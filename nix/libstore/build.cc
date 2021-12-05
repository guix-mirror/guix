#include "config.h"

#include "references.hh"
#include "pathlocks.hh"
#include "misc.hh"
#include "globals.hh"
#include "local-store.hh"
#include "util.hh"
#include "archive.hh"
#include "affinity.hh"
#include "builtins.hh"

#include <map>
#include <sstream>
#include <algorithm>

#include <limits.h>
#include <time.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <cstring>
#include <stdint.h>

#include <pwd.h>
#include <grp.h>

#include <zlib.h>

#if HAVE_BZLIB_H
# include <bzlib.h>
#endif

/* Includes required for chroot support. */
#if HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#if HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#if HAVE_SYS_SYSCALL_H
#include <sys/syscall.h>
#endif
#if HAVE_SCHED_H
#include <sched.h>
#endif


#define CHROOT_ENABLED HAVE_CHROOT && HAVE_SYS_MOUNT_H && defined(MS_BIND) && defined(MS_PRIVATE)
#define CLONE_ENABLED defined(CLONE_NEWNS)

#if defined(SYS_pivot_root)
#define pivot_root(new_root, put_old) (syscall(SYS_pivot_root, new_root,put_old))
#endif

#if CHROOT_ENABLED
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <netinet/ip.h>
#endif

#if __linux__
#include <sys/personality.h>
#endif

#if HAVE_STATVFS
#include <sys/statvfs.h>
#endif


namespace nix {

using std::map;


/* Forward definition. */
class Worker;


/* A pointer to a goal. */
class Goal;
class DerivationGoal;
typedef std::shared_ptr<Goal> GoalPtr;
typedef std::weak_ptr<Goal> WeakGoalPtr;

struct CompareGoalPtrs {
    bool operator() (const GoalPtr & a, const GoalPtr & b);
};

/* Set of goals. */
typedef set<GoalPtr, CompareGoalPtrs> Goals;
typedef list<WeakGoalPtr> WeakGoals;

/* A map of paths to goals (and the other way around). */
typedef map<Path, WeakGoalPtr> WeakGoalMap;



class Goal : public std::enable_shared_from_this<Goal>
{
public:
    typedef enum {ecBusy, ecSuccess, ecFailed, ecNoSubstituters, ecIncompleteClosure} ExitCode;

protected:

    /* Backlink to the worker. */
    Worker & worker;

    /* Goals that this goal is waiting for. */
    Goals waitees;

    /* Goals waiting for this one to finish.  Must use weak pointers
       here to prevent cycles. */
    WeakGoals waiters;

    /* Number of goals we are/were waiting for that have failed. */
    unsigned int nrFailed;

    /* Number of substitution goals we are/were waiting for that
       failed because there are no substituters. */
    unsigned int nrNoSubstituters;

    /* Number of substitution goals we are/were waiting for that
       failed because othey had unsubstitutable references. */
    unsigned int nrIncompleteClosure;

    /* Name of this goal for debugging purposes. */
    string name;

    /* Whether the goal is finished. */
    ExitCode exitCode;

    Goal(Worker & worker) : worker(worker)
    {
        nrFailed = nrNoSubstituters = nrIncompleteClosure = 0;
        exitCode = ecBusy;
    }

    virtual ~Goal()
    {
        trace("goal destroyed");
    }

public:
    virtual void work() = 0;

    void addWaitee(GoalPtr waitee);

    virtual void waiteeDone(GoalPtr waitee, ExitCode result);

    virtual void handleChildOutput(int fd, const string & data)
    {
        abort();
    }

    virtual void handleEOF(int fd)
    {
        abort();
    }

    void trace(const format & f);

    string getName()
    {
        return name;
    }

    ExitCode getExitCode()
    {
        return exitCode;
    }

    /* Callback in case of a timeout.  It should wake up its waiters,
       get rid of any running child processes that are being monitored
       by the worker (important!), etc. */
    virtual void timedOut() = 0;

    virtual string key() = 0;

protected:
    void amDone(ExitCode result);
};


bool CompareGoalPtrs::operator() (const GoalPtr & a, const GoalPtr & b) {
    string s1 = a->key();
    string s2 = b->key();
    return s1 < s2;
}


/* A mapping used to remember for each child process to what goal it
   belongs, and file descriptors for receiving log data and output
   path creation commands. */
struct Child
{
    WeakGoalPtr goal;
    set<int> fds;
    bool respectTimeouts;
    bool inBuildSlot;
    time_t lastOutput; /* time we last got output on stdout/stderr */
    time_t timeStarted;
};

typedef map<pid_t, Child> Children;


/* The worker class. */
class Worker
{
private:

    /* Note: the worker should only have strong pointers to the
       top-level goals. */

    /* The top-level goals of the worker. */
    Goals topGoals;

    /* Goals that are ready to do some work. */
    WeakGoals awake;

    /* Goals waiting for a build slot. */
    WeakGoals wantingToBuild;

    /* Child processes currently running. */
    Children children;

    /* Number of build slots occupied.  This includes local builds and
       substitutions but not remote builds via the build hook. */
    unsigned int nrLocalBuilds;

    /* Maps used to prevent multiple instantiations of a goal for the
       same derivation / path. */
    WeakGoalMap derivationGoals;
    WeakGoalMap substitutionGoals;

    /* Goals waiting for busy paths to be unlocked. */
    WeakGoals waitingForAnyGoal;

    /* Goals sleeping for a few seconds (polling a lock). */
    WeakGoals waitingForAWhile;

    /* Last time the goals in `waitingForAWhile' where woken up. */
    time_t lastWokenUp;

public:

    /* Set if at least one derivation had a BuildError (i.e. permanent
       failure). */
    bool permanentFailure;

    /* Set if at least one derivation had a timeout. */
    bool timedOut;

    LocalStore & store;

    std::shared_ptr<Agent> hook;
    std::shared_ptr<Agent> substituter;

    Worker(LocalStore & store);
    ~Worker();

    /* Make a goal (with caching). */
    GoalPtr makeDerivationGoal(const Path & drvPath, const StringSet & wantedOutputs, BuildMode buildMode = bmNormal);
    GoalPtr makeSubstitutionGoal(const Path & storePath, bool repair = false);

    /* Remove a dead goal. */
    void removeGoal(GoalPtr goal);

    /* Wake up a goal (i.e., there is something for it to do). */
    void wakeUp(GoalPtr goal);

    /* Return the number of local build and substitution processes
       currently running (but not remote builds via the build
       hook). */
    unsigned int getNrLocalBuilds();

    /* Registers a running child process.  `inBuildSlot' means that
       the process counts towards the jobs limit. */
    void childStarted(GoalPtr goal, pid_t pid,
        const set<int> & fds, bool inBuildSlot, bool respectTimeouts);

    /* Unregisters a running child process.  `wakeSleepers' should be
       false if there is no sense in waking up goals that are sleeping
       because they can't run yet (e.g., there is no free build slot,
       or the hook would still say `postpone'). */
    void childTerminated(pid_t pid, bool wakeSleepers = true);

    /* Put `goal' to sleep until a build slot becomes available (which
       might be right away). */
    void waitForBuildSlot(GoalPtr goal);

    /* Wait for any goal to finish.  Pretty indiscriminate way to
       wait for some resource that some other goal is holding. */
    void waitForAnyGoal(GoalPtr goal);

    /* Wait for a few seconds and then retry this goal.  Used when
       waiting for a lock held by another process.  This kind of
       polling is inefficient, but POSIX doesn't really provide a way
       to wait for multiple locks in the main select() loop. */
    void waitForAWhile(GoalPtr goal);

    /* Loop until the specified top-level goals have finished. */
    void run(const Goals & topGoals);

    /* Wait for input to become available. */
    void waitForInput();

    unsigned int exitStatus();
};


//////////////////////////////////////////////////////////////////////


void addToWeakGoals(WeakGoals & goals, GoalPtr p)
{
    // FIXME: necessary?
    // FIXME: O(n)
    foreach (WeakGoals::iterator, i, goals)
        if (i->lock() == p) return;
    goals.push_back(p);
}


void Goal::addWaitee(GoalPtr waitee)
{
    waitees.insert(waitee);
    addToWeakGoals(waitee->waiters, shared_from_this());
}


void Goal::waiteeDone(GoalPtr waitee, ExitCode result)
{
    assert(waitees.find(waitee) != waitees.end());
    waitees.erase(waitee);

    trace(format("waitee `%1%' done; %2% left") %
        waitee->name % waitees.size());

    if (result == ecFailed || result == ecNoSubstituters || result == ecIncompleteClosure) ++nrFailed;

    if (result == ecNoSubstituters) ++nrNoSubstituters;

    if (result == ecIncompleteClosure) ++nrIncompleteClosure;

    if (waitees.empty() || (result == ecFailed && !settings.keepGoing)) {

        /* If we failed and keepGoing is not set, we remove all
           remaining waitees. */
        foreach (Goals::iterator, i, waitees) {
            GoalPtr goal = *i;
            WeakGoals waiters2;
            foreach (WeakGoals::iterator, j, goal->waiters)
                if (j->lock() != shared_from_this()) waiters2.push_back(*j);
            goal->waiters = waiters2;
        }
        waitees.clear();

        worker.wakeUp(shared_from_this());
    }
}


void Goal::amDone(ExitCode result)
{
    trace("done");
    assert(exitCode == ecBusy);
    assert(result == ecSuccess || result == ecFailed || result == ecNoSubstituters || result == ecIncompleteClosure);
    exitCode = result;
    foreach (WeakGoals::iterator, i, waiters) {
        GoalPtr goal = i->lock();
        if (goal) goal->waiteeDone(shared_from_this(), result);
    }
    waiters.clear();
    worker.removeGoal(shared_from_this());
}


void Goal::trace(const format & f)
{
    debug(format("%1%: %2%") % name % f);
}



//////////////////////////////////////////////////////////////////////


/* Restore default handling of SIGPIPE, otherwise some programs will
   randomly say "Broken pipe". */
static void restoreSIGPIPE()
{
    struct sigaction act, oact;
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    if (sigaction(SIGPIPE, &act, &oact)) throw SysError("resetting SIGPIPE");
}


//////////////////////////////////////////////////////////////////////


class UserLock
{
private:
    /* POSIX locks suck.  If we have a lock on a file, and we open and
       close that file again (without closing the original file
       descriptor), we lose the lock.  So we have to be *very* careful
       not to open a lock file on which we are holding a lock. */
    static PathSet lockedPaths; /* !!! not thread-safe */

    Path fnUserLock;
    AutoCloseFD fdUserLock;

    string user;
    uid_t uid;
    gid_t gid;
    std::vector<gid_t> supplementaryGIDs;

public:
    UserLock();
    ~UserLock();

    void acquire();
    void release();

    void kill();

    string getUser() { return user; }
    uid_t getUID() { return uid; }
    uid_t getGID() { return gid; }
    std::vector<gid_t> getSupplementaryGIDs() { return supplementaryGIDs; }

    bool enabled() { return uid != 0; }

};


PathSet UserLock::lockedPaths;


UserLock::UserLock()
{
    uid = gid = 0;
}


UserLock::~UserLock()
{
    release();
}


void UserLock::acquire()
{
    assert(uid == 0);

    assert(settings.buildUsersGroup != "");

    /* Get the members of the build-users-group. */
    struct group * gr = getgrnam(settings.buildUsersGroup.c_str());
    if (!gr)
        throw Error(format("the group `%1%' specified in `build-users-group' does not exist")
            % settings.buildUsersGroup);
    gid = gr->gr_gid;

    /* Copy the result of getgrnam. */
    Strings users;
    for (char * * p = gr->gr_mem; *p; ++p) {
        debug(format("found build user `%1%'") % *p);
        users.push_back(*p);
    }

    if (users.empty())
        throw Error(format("the build users group `%1%' has no members")
            % settings.buildUsersGroup);

    /* Find a user account that isn't currently in use for another
       build. */
    foreach (Strings::iterator, i, users) {
        debug(format("trying user `%1%'") % *i);

        struct passwd * pw = getpwnam(i->c_str());
        if (!pw)
            throw Error(format("the user `%1%' in the group `%2%' does not exist")
                % *i % settings.buildUsersGroup);

        createDirs(settings.nixStateDir + "/userpool");

        fnUserLock = (format("%1%/userpool/%2%") % settings.nixStateDir % pw->pw_uid).str();

        if (lockedPaths.find(fnUserLock) != lockedPaths.end())
            /* We already have a lock on this one. */
            continue;

        AutoCloseFD fd = open(fnUserLock.c_str(), O_RDWR | O_CREAT, 0600);
        if (fd == -1)
            throw SysError(format("opening user lock `%1%'") % fnUserLock);
        closeOnExec(fd);

        if (lockFile(fd, ltWrite, false)) {
            fdUserLock = fd.borrow();
            lockedPaths.insert(fnUserLock);
            user = *i;
            uid = pw->pw_uid;

            /* Sanity check... */
            if (uid == getuid() || uid == geteuid())
                throw Error(format("the build user should not be a member of `%1%'")
                    % settings.buildUsersGroup);

            /* Get the list of supplementary groups of this build user.  This
               is usually either empty or contains a group such as "kvm".  */
            supplementaryGIDs.resize(10);
            int ngroups = supplementaryGIDs.size();
            int err = getgrouplist(pw->pw_name, pw->pw_gid,
                supplementaryGIDs.data(), &ngroups);
            if (err == -1)
                throw Error(format("failed to get list of supplementary groups for ‘%1%’") % pw->pw_name);

            supplementaryGIDs.resize(ngroups);

            return;
        }
    }

    throw Error(format("all build users are currently in use; "
        "consider creating additional users and adding them to the `%1%' group")
        % settings.buildUsersGroup);
}


void UserLock::release()
{
    if (uid == 0) return;
    fdUserLock.close(); /* releases lock */
    assert(lockedPaths.find(fnUserLock) != lockedPaths.end());
    lockedPaths.erase(fnUserLock);
    fnUserLock = "";
    uid = 0;
}


void UserLock::kill()
{
    assert(enabled());
    killUser(uid);
}

//////////////////////////////////////////////////////////////////////


typedef map<string, string> HashRewrites;


string rewriteHashes(string s, const HashRewrites & rewrites)
{
    foreach (HashRewrites::const_iterator, i, rewrites) {
        assert(i->first.size() == i->second.size());
        size_t j = 0;
        while ((j = s.find(i->first, j)) != string::npos) {
            debug(format("rewriting @ %1%") % j);
            s.replace(j, i->second.size(), i->second);
        }
    }
    return s;
}


//////////////////////////////////////////////////////////////////////


typedef enum {rpAccept, rpDecline, rpPostpone} HookReply;

class SubstitutionGoal;

class DerivationGoal : public Goal
{
private:
    /* The path of the derivation. */
    Path drvPath;

    /* The specific outputs that we need to build.  Empty means all of
       them. */
    StringSet wantedOutputs;

    /* Whether additional wanted outputs have been added. */
    bool needRestart;

    /* Whether to retry substituting the outputs after building the
       inputs. */
    bool retrySubstitution;

    /* The derivation stored at drvPath. */
    Derivation drv;

    /* The remainder is state held during the build. */

    /* Locks on the output paths. */
    PathLocks outputLocks;

    /* All input paths (that is, the union of FS closures of the
       immediate input paths). */
    PathSet inputPaths;

    /* Referenceable paths (i.e., input and output paths). */
    PathSet allPaths;

    /* Outputs that are already valid.  If we're repairing, these are
       the outputs that are valid *and* not corrupt. */
    PathSet validPaths;

    /* Outputs that are corrupt or not valid. */
    PathSet missingPaths;

    /* User selected for running the builder. */
    UserLock buildUser;

    /* The process ID of the builder. */
    Pid pid;

    /* The temporary directory. */
    Path tmpDir;

    /* The path of the temporary directory in the sandbox. */
    Path tmpDirInSandbox;

    /* File descriptor for the log file. */
    FILE * fLogFile;
    gzFile   gzLogFile;
#if HAVE_BZLIB_H
    BZFILE * bzLogFile;
#endif
    AutoCloseFD fdLogFile;

    /* Number of bytes received from the builder's stdout/stderr. */
    unsigned long logSize;

    /* Pipe for the builder's standard output/error. */
    Pipe builderOut;

    /* The build hook. */
    std::shared_ptr<Agent> hook;

    /* Whether we're currently doing a chroot build. */
    bool useChroot;

    Path chrootRootDir;

    /* RAII object to delete the chroot directory. */
    std::shared_ptr<AutoDelete> autoDelChroot;

    /* All inputs that are regular files. */
    PathSet regularInputPaths;

    /* Whether this is a fixed-output derivation. */
    bool fixedOutput;

    typedef void (DerivationGoal::*GoalState)();
    GoalState state;

    /* Stuff we need to pass to runChild(). */
    typedef map<Path, Path> DirsInChroot; // maps target path to source path
    DirsInChroot dirsInChroot;
    typedef map<string, string> Environment;
    Environment env;

    /* Hash rewriting. */
    HashRewrites rewritesToTmp, rewritesFromTmp;
    typedef map<Path, Path> RedirectedOutputs;
    RedirectedOutputs redirectedOutputs;

    BuildMode buildMode;

    /* If we're repairing without a chroot, there may be outputs that
       are valid but corrupt.  So we redirect these outputs to
       temporary paths. */
    PathSet redirectedBadOutputs;

    /* The current round, if we're building multiple times. */
    unsigned int curRound = 1;

    unsigned int nrRounds;

    /* Path registration info from the previous round, if we're
       building multiple times. Since this contains the hash, it
       allows us to compare whether two rounds produced the same
       result. */
    ValidPathInfos prevInfos;

    BuildResult result;

public:
    DerivationGoal(const Path & drvPath, const StringSet & wantedOutputs, Worker & worker, BuildMode buildMode = bmNormal);
    ~DerivationGoal();

    void timedOut() override;

    string key()
    {
        /* Ensure that derivations get built in order of their name,
           i.e. a derivation named "aardvark" always comes before
           "baboon". And substitution goals always happen before
           derivation goals (due to "b$"). */
        return "b$" + storePathToName(drvPath) + "$" + drvPath;
    }

    void work();

    Path getDrvPath()
    {
        return drvPath;
    }

    /* Add wanted outputs to an already existing derivation goal. */
    void addWantedOutputs(const StringSet & outputs);

    BuildResult getResult() { return result; }

private:
    /* The states. */
    void init();
    void haveDerivation();
    void outputsSubstituted();
    void closureRepaired();
    void inputsRealised();
    void tryToBuild();
    void buildDone();

    /* Is the build hook willing to perform the build? */
    HookReply tryBuildHook();

    /* Start building a derivation. */
    void startBuilder();

    /* Run the builder's process. */
    void runChild();

    friend int childEntry(void *);

    /* Check that the derivation outputs all exist and register them
       as valid. */
    void registerOutputs();

    /* Open a log file and a pipe to it. */
    Path openLogFile();

    /* Close the log file. */
    void closeLogFile();

    /* Delete the temporary directory, if we have one. */
    void deleteTmpDir(bool force);

    /* Callback used by the worker to write to the log. */
    void handleChildOutput(int fd, const string & data);
    void handleEOF(int fd);

    /* Return the set of (in)valid paths. */
    PathSet checkPathValidity(bool returnValid, bool checkHash);

    /* Abort the goal if `path' failed to build. */
    bool pathFailed(const Path & path);

    /* Forcibly kill the child process, if any. */
    void killChild();

    Path addHashRewrite(const Path & path);

    void repairClosure();

    void done(BuildResult::Status status, const string & msg = "");
};


DerivationGoal::DerivationGoal(const Path & drvPath, const StringSet & wantedOutputs, Worker & worker, BuildMode buildMode)
    : Goal(worker)
    , wantedOutputs(wantedOutputs)
    , needRestart(false)
    , retrySubstitution(false)
    , fLogFile(0)
    , gzLogFile(0)
#if HAVE_BZLIB_H
    , bzLogFile(0)
#endif
    , useChroot(false)
    , buildMode(buildMode)
{
    this->drvPath = drvPath;
    state = &DerivationGoal::init;
    name = (format("building of `%1%'") % drvPath).str();
    trace("created");

    /* Prevent the .chroot directory from being
       garbage-collected. (See isActiveTempFile() in gc.cc.) */
    worker.store.addTempRoot(drvPath);
}


DerivationGoal::~DerivationGoal()
{
    /* Careful: we should never ever throw an exception from a
       destructor. */
    try { killChild(); } catch (...) { ignoreException(); }
    try { deleteTmpDir(false); } catch (...) { ignoreException(); }
    try { closeLogFile(); } catch (...) { ignoreException(); }
}


void DerivationGoal::killChild()
{
    if (pid != -1) {
        worker.childTerminated(pid);

        if (buildUser.enabled()) {
            /* If we're using a build user, then there is a tricky
               race condition: if we kill the build user before the
               child has done its setuid() to the build user uid, then
               it won't be killed, and we'll potentially lock up in
               pid.wait().  So also send a conventional kill to the
               child. */
            ::kill(-pid, SIGKILL); /* ignore the result */
            buildUser.kill();
            pid.wait(true);
        } else
            pid.kill();

        assert(pid == -1);
    }

    /* If there was a build hook involved, remove it from the worker's
       children.  */
    if (hook && hook->pid != -1) {
	worker.childTerminated(hook->pid);
    }
    hook.reset();
}


void DerivationGoal::timedOut()
{
    if (settings.printBuildTrace)
        printMsg(lvlError, format("@ build-failed %1% - timeout") % drvPath);
    killChild();
    done(BuildResult::TimedOut);
}


void DerivationGoal::work()
{
    (this->*state)();
}


void DerivationGoal::addWantedOutputs(const StringSet & outputs)
{
    /* If we already want all outputs, there is nothing to do. */
    if (wantedOutputs.empty()) return;

    if (outputs.empty()) {
        wantedOutputs.clear();
        needRestart = true;
    } else
        foreach (StringSet::const_iterator, i, outputs)
            if (wantedOutputs.find(*i) == wantedOutputs.end()) {
                wantedOutputs.insert(*i);
                needRestart = true;
            }
}


void DerivationGoal::init()
{
    trace("init");

    if (settings.readOnlyMode)
        throw Error(format("cannot build derivation `%1%' - no write access to the store") % drvPath);

    /* The first thing to do is to make sure that the derivation
       exists.  If it doesn't, it may be created through a
       substitute. */
    if (buildMode == bmNormal && worker.store.isValidPath(drvPath)) {
        haveDerivation();
        return;
    }

    addWaitee(worker.makeSubstitutionGoal(drvPath));

    state = &DerivationGoal::haveDerivation;
}


void DerivationGoal::haveDerivation()
{
    trace("loading derivation");

    if (nrFailed != 0) {
        printMsg(lvlError, format("cannot build missing derivation ‘%1%’") % drvPath);
        done(BuildResult::MiscFailure);
        return;
    }

    /* `drvPath' should already be a root, but let's be on the safe
       side: if the user forgot to make it a root, we wouldn't want
       things being garbage collected while we're busy. */
    worker.store.addTempRoot(drvPath);

    assert(worker.store.isValidPath(drvPath));

    /* Get the derivation. */
    drv = derivationFromPath(worker.store, drvPath);

    foreach (DerivationOutputs::iterator, i, drv.outputs)
        worker.store.addTempRoot(i->second.path);

    /* Check what outputs paths are not already valid. */
    PathSet invalidOutputs = checkPathValidity(false, buildMode == bmRepair);

    /* If they are all valid, then we're done. */
    if (invalidOutputs.size() == 0 && buildMode == bmNormal) {
        done(BuildResult::AlreadyValid);
        return;
    }

    /* Check whether any output previously failed to build.  If so,
       don't bother. */
    foreach (PathSet::iterator, i, invalidOutputs)
        if (pathFailed(*i)) return;

    /* We are first going to try to create the invalid output paths
       through substitutes.  If that doesn't work, we'll build
       them. */
    if (settings.useSubstitutes && substitutesAllowed(drv))
        foreach (PathSet::iterator, i, invalidOutputs)
            addWaitee(worker.makeSubstitutionGoal(*i, buildMode == bmRepair));

    if (waitees.empty()) /* to prevent hang (no wake-up event) */
        outputsSubstituted();
    else
        state = &DerivationGoal::outputsSubstituted;
}


void DerivationGoal::outputsSubstituted()
{
    trace("all outputs substituted (maybe)");

    if (nrFailed > 0 && nrFailed > nrNoSubstituters + nrIncompleteClosure && !settings.tryFallback)
        throw Error(format("some substitutes for the outputs of derivation `%1%' failed (usually happens due to networking issues); try `--fallback' to build derivation from source ") % drvPath);

    /*  If the substitutes form an incomplete closure, then we should
        build the dependencies of this derivation, but after that, we
        can still use the substitutes for this derivation itself. */
    if (nrIncompleteClosure > 0 && !retrySubstitution) retrySubstitution = true;

    nrFailed = nrNoSubstituters = nrIncompleteClosure = 0;

    if (needRestart) {
        needRestart = false;
        haveDerivation();
        return;
    }

    unsigned int nrInvalid = checkPathValidity(false, buildMode == bmRepair).size();
    if (buildMode == bmNormal && nrInvalid == 0) {
        done(BuildResult::Substituted);
        return;
    }
    if (buildMode == bmRepair && nrInvalid == 0) {
        repairClosure();
        return;
    }
    if (buildMode == bmCheck && nrInvalid > 0)
        throw Error(format("some outputs of `%1%' are not valid, so checking is not possible") % drvPath);

    /* Otherwise, at least one of the output paths could not be
       produced using a substitute.  So we have to build instead. */

    /* Make sure checkPathValidity() from now on checks all
       outputs. */
    wantedOutputs = PathSet();

    /* The inputs must be built before we can build this goal. */
    foreach (DerivationInputs::iterator, i, drv.inputDrvs)
        addWaitee(worker.makeDerivationGoal(i->first, i->second, buildMode == bmRepair ? bmRepair : bmNormal));

    foreach (PathSet::iterator, i, drv.inputSrcs)
        addWaitee(worker.makeSubstitutionGoal(*i));

    if (waitees.empty()) /* to prevent hang (no wake-up event) */
        inputsRealised();
    else
        state = &DerivationGoal::inputsRealised;
}


void DerivationGoal::repairClosure()
{
    /* If we're repairing, we now know that our own outputs are valid.
       Now check whether the other paths in the outputs closure are
       good.  If not, then start derivation goals for the derivations
       that produced those outputs. */

    /* Get the output closure. */
    PathSet outputClosure;
    foreach (DerivationOutputs::iterator, i, drv.outputs) {
        if (!wantOutput(i->first, wantedOutputs)) continue;
        computeFSClosure(worker.store, i->second.path, outputClosure);
    }

    /* Filter out our own outputs (which we have already checked). */
    foreach (DerivationOutputs::iterator, i, drv.outputs)
        outputClosure.erase(i->second.path);

    /* Get all dependencies of this derivation so that we know which
       derivation is responsible for which path in the output
       closure. */
    PathSet inputClosure;
    computeFSClosure(worker.store, drvPath, inputClosure);
    std::map<Path, Path> outputsToDrv;
    foreach (PathSet::iterator, i, inputClosure)
        if (isDerivation(*i)) {
            Derivation drv = derivationFromPath(worker.store, *i);
            foreach (DerivationOutputs::iterator, j, drv.outputs)
                outputsToDrv[j->second.path] = *i;
        }

    /* Check each path (slow!). */
    PathSet broken;
    foreach (PathSet::iterator, i, outputClosure) {
        if (worker.store.pathContentsGood(*i)) continue;
        printMsg(lvlError, format("found corrupted or missing path `%1%' in the output closure of `%2%'") % *i % drvPath);
        Path drvPath2 = outputsToDrv[*i];
        if (drvPath2 == "")
            addWaitee(worker.makeSubstitutionGoal(*i, true));
        else
            addWaitee(worker.makeDerivationGoal(drvPath2, PathSet(), bmRepair));
    }

    if (waitees.empty()) {
        done(BuildResult::AlreadyValid);
        return;
    }

    state = &DerivationGoal::closureRepaired;
}


void DerivationGoal::closureRepaired()
{
    trace("closure repaired");
    if (nrFailed > 0)
        throw Error(format("some paths in the output closure of derivation ‘%1%’ could not be repaired") % drvPath);
    done(BuildResult::AlreadyValid);
}


void DerivationGoal::inputsRealised()
{
    trace("all inputs realised");

    if (nrFailed != 0) {
        printMsg(lvlError,
            format("cannot build derivation `%1%': %2% dependencies couldn't be built")
            % drvPath % nrFailed);
        done(BuildResult::DependencyFailed);
        return;
    }

    if (retrySubstitution) {
        haveDerivation();
        return;
    }

    /* Gather information necessary for computing the closure and/or
       running the build hook. */

    /* The outputs are referenceable paths. */
    foreach (DerivationOutputs::iterator, i, drv.outputs) {
        debug(format("building path `%1%'") % i->second.path);
        allPaths.insert(i->second.path);
    }

    /* Determine the full set of input paths. */

    /* First, the input derivations. */
    foreach (DerivationInputs::iterator, i, drv.inputDrvs) {
        /* Add the relevant output closures of the input derivation
           `*i' as input paths.  Only add the closures of output paths
           that are specified as inputs. */
        assert(worker.store.isValidPath(i->first));
        Derivation inDrv = derivationFromPath(worker.store, i->first);
        foreach (StringSet::iterator, j, i->second)
            if (inDrv.outputs.find(*j) != inDrv.outputs.end())
                computeFSClosure(worker.store, inDrv.outputs[*j].path, inputPaths);
            else
                throw Error(
                    format("derivation `%1%' requires non-existent output `%2%' from input derivation `%3%'")
                    % drvPath % *j % i->first);
    }

    /* Second, the input sources. */
    foreach (PathSet::iterator, i, drv.inputSrcs)
        computeFSClosure(worker.store, *i, inputPaths);

    debug(format("added input paths %1%") % showPaths(inputPaths));

    allPaths.insert(inputPaths.begin(), inputPaths.end());

    /* Is this a fixed-output derivation? */
    fixedOutput = true;
    for (auto & i : drv.outputs)
	if (i.second.hash == "") fixedOutput = false;

    /* Don't repeat fixed-output derivations since they're already
       verified by their output hash.*/
    nrRounds = fixedOutput ? 1 : settings.get("build-repeat", 0) + 1;

    /* Okay, try to build.  Note that here we don't wait for a build
       slot to become available, since we don't need one if there is a
       build hook. */
    state = &DerivationGoal::tryToBuild;
    worker.wakeUp(shared_from_this());
}


static bool canBuildLocally(const string & platform)
{
    return platform == settings.thisSystem
#if __linux__
        || (platform == "i686-linux" && settings.thisSystem == "x86_64-linux")
        || (platform == "armhf-linux" && settings.thisSystem == "aarch64-linux")
#endif
        ;
}


static string get(const StringPairs & map, const string & key, const string & def = "")
{
    StringPairs::const_iterator i = map.find(key);
    return i == map.end() ? def : i->second;
}


bool willBuildLocally(const Derivation & drv)
{
    return get(drv.env, "preferLocalBuild") == "1" && canBuildLocally(drv.platform);
}


bool substitutesAllowed(const Derivation & drv)
{
    return get(drv.env, "allowSubstitutes", "1") == "1";
}


void DerivationGoal::tryToBuild()
{
    trace("trying to build");

    /* Check for the possibility that some other goal in this process
       has locked the output since we checked in haveDerivation().
       (It can't happen between here and the lockPaths() call below
       because we're not allowing multi-threading.)  If so, put this
       goal to sleep until another goal finishes, then try again. */
    foreach (DerivationOutputs::iterator, i, drv.outputs)
        if (pathIsLockedByMe(i->second.path)) {
            debug(format("putting derivation `%1%' to sleep because `%2%' is locked by another goal")
                % drvPath % i->second.path);
            worker.waitForAnyGoal(shared_from_this());
            return;
        }

    /* Obtain locks on all output paths.  The locks are automatically
       released when we exit this function or the client crashes.  If we
       can't acquire the lock, then continue; hopefully some other
       goal can start a build, and if not, the main loop will sleep a
       few seconds and then retry this goal. */
    if (!outputLocks.lockPaths(outputPaths(drv), "", false)) {
        worker.waitForAWhile(shared_from_this());
        return;
    }

    /* Now check again whether the outputs are valid.  This is because
       another process may have started building in parallel.  After
       it has finished and released the locks, we can (and should)
       reuse its results.  (Strictly speaking the first check can be
       omitted, but that would be less efficient.)  Note that since we
       now hold the locks on the output paths, no other process can
       build this derivation, so no further checks are necessary. */
    validPaths = checkPathValidity(true, buildMode == bmRepair);
    if (buildMode != bmCheck && validPaths.size() == drv.outputs.size()) {
        debug(format("skipping build of derivation `%1%', someone beat us to it") % drvPath);
        outputLocks.setDeletion(true);
        done(BuildResult::AlreadyValid);
        return;
    }

    missingPaths = outputPaths(drv);
    if (buildMode != bmCheck)
        foreach (PathSet::iterator, i, validPaths) missingPaths.erase(*i);

    /* If any of the outputs already exist but are not valid, delete
       them. */
    foreach (DerivationOutputs::iterator, i, drv.outputs) {
        Path path = i->second.path;
        if (worker.store.isValidPath(path)) continue;
        if (!pathExists(path)) continue;
        debug(format("removing invalid path `%1%'") % path);
        deletePath(path);
    }

    /* Check again whether any output previously failed to build,
       because some other process may have tried and failed before we
       acquired the lock. */
    foreach (DerivationOutputs::iterator, i, drv.outputs)
        if (pathFailed(i->second.path)) return;

    /* Don't do a remote build if the derivation has the attribute
       `preferLocalBuild' set.  Also, check and repair modes are only
       supported for local builds. */
    bool buildLocally = buildMode != bmNormal || willBuildLocally(drv);

    /* Is the build hook willing to accept this job? */
    if (!buildLocally) {
        switch (tryBuildHook()) {
            case rpAccept:
                /* Yes, it has started doing so.  Wait until we get
                   EOF from the hook. */
                state = &DerivationGoal::buildDone;
                return;
            case rpPostpone:
                /* Not now; wait until at least one child finishes or
                   the wake-up timeout expires. */
                worker.waitForAWhile(shared_from_this());
                outputLocks.unlock();
                return;
            case rpDecline:
                /* We should do it ourselves. */
                break;
        }
    }

    /* Make sure that we are allowed to start a build.  If this
       derivation prefers to be done locally, do it even if
       maxBuildJobs is 0. */
    unsigned int curBuilds = worker.getNrLocalBuilds();
    if (curBuilds >= settings.maxBuildJobs && !(buildLocally && curBuilds == 0)) {
        worker.waitForBuildSlot(shared_from_this());
        outputLocks.unlock();
        return;
    }

    try {

        /* Okay, we have to build. */
        startBuilder();

    } catch (BuildError & e) {
        printMsg(lvlError, e.msg());
        outputLocks.unlock();
        buildUser.release();
        if (settings.printBuildTrace)
            printMsg(lvlError, format("@ build-failed %1% - %2% %3%")
                % drvPath % 0 % e.msg());
        worker.permanentFailure = true;
        done(BuildResult::InputRejected, e.msg());
        return;
    }

    /* This state will be reached when we get EOF on the child's
       log pipe. */
    state = &DerivationGoal::buildDone;
}


void replaceValidPath(const Path & storePath, const Path tmpPath)
{
    /* We can't atomically replace storePath (the original) with
       tmpPath (the replacement), so we have to move it out of the
       way first.  We'd better not be interrupted here, because if
       we're repairing (say) Glibc, we end up with a broken system. */
    Path oldPath = (format("%1%.old-%2%-%3%") % storePath % getpid() % rand()).str();
    if (pathExists(storePath))
        rename(storePath.c_str(), oldPath.c_str());
    if (rename(tmpPath.c_str(), storePath.c_str()) == -1)
        throw SysError(format("moving `%1%' to `%2%'") % tmpPath % storePath);
    if (pathExists(oldPath))
        deletePath(oldPath);
}


MakeError(NotDeterministic, BuildError)


void DerivationGoal::buildDone()
{
    trace("build done");

    /* Since we got an EOF on the logger pipe, the builder is presumed
       to have terminated.  In fact, the builder could also have
       simply have closed its end of the pipe --- just don't do that
       :-) */
    int status;
    pid_t savedPid;
    if (hook) {
        savedPid = hook->pid;
        status = hook->pid.wait(true);
    } else {
        /* !!! this could block! security problem! solution: kill the
           child */
        savedPid = pid;
        status = pid.wait(true);
    }

    debug(format("builder process for `%1%' finished") % drvPath);

    /* So the child is gone now. */
    worker.childTerminated(savedPid);

    /* Close the read side of the logger pipe. */
    if (hook) {
        hook->builderOut.readSide.close();
        hook->fromAgent.readSide.close();
    }
    else builderOut.readSide.close();

    /* Close the log file. */
    closeLogFile();

    /* When running under a build user, make sure that all processes
       running under that uid are gone.  This is to prevent a
       malicious user from leaving behind a process that keeps files
       open and modifies them after they have been chown'ed to
       root. */
    if (buildUser.enabled()) buildUser.kill();

    bool diskFull = false;

    try {

        /* Check the exit status. */
        if (!statusOk(status)) {

            /* Heuristically check whether the build failure may have
               been caused by a disk full condition.  We have no way
               of knowing whether the build actually got an ENOSPC.
               So instead, check if the disk is (nearly) full now.  If
               so, we don't mark this build as a permanent failure. */
#if HAVE_STATVFS
            unsigned long long required = 8ULL * 1024 * 1024; // FIXME: make configurable
            struct statvfs st;
            if (statvfs(settings.nixStore.c_str(), &st) == 0 &&
                (unsigned long long) st.f_bavail * st.f_bsize < required)
                diskFull = true;
            if (statvfs(tmpDir.c_str(), &st) == 0 &&
                (unsigned long long) st.f_bavail * st.f_bsize < required)
                diskFull = true;
#endif

            deleteTmpDir(false);

            /* Move paths out of the chroot for easier debugging of
               build failures. */
            if (useChroot && buildMode == bmNormal)
                foreach (PathSet::iterator, i, missingPaths)
                    if (pathExists(chrootRootDir + *i))
                        rename((chrootRootDir + *i).c_str(), i->c_str());

            if (diskFull)
                printMsg(lvlError, "note: build failure may have been caused by lack of free disk space");

            throw BuildError(format("builder for `%1%' %2%")
                % drvPath % statusToString(status));
        }

        /* Compute the FS closure of the outputs and register them as
           being valid. */
        registerOutputs();

        /* Delete unused redirected outputs (when doing hash rewriting). */
        foreach (RedirectedOutputs::iterator, i, redirectedOutputs)
            if (pathExists(i->second)) deletePath(i->second);

        /* Delete the chroot (if we were using one). */
        autoDelChroot.reset(); /* this runs the destructor */

        deleteTmpDir(true);

        /* Repeat the build if necessary. */
        if (curRound++ < nrRounds) {
            outputLocks.unlock();
            buildUser.release();
            state = &DerivationGoal::tryToBuild;
            worker.wakeUp(shared_from_this());
            return;
        }

        /* It is now safe to delete the lock files, since all future
           lockers will see that the output paths are valid; they will
           not create new lock files with the same names as the old
           (unlinked) lock files. */
        outputLocks.setDeletion(true);
        outputLocks.unlock();

    } catch (BuildError & e) {
        if (!hook)
            printMsg(lvlError, e.msg());
        outputLocks.unlock();
        buildUser.release();

        BuildResult::Status st = BuildResult::MiscFailure;

        if (hook && WIFEXITED(status) && WEXITSTATUS(status) == 101) {
            if (settings.printBuildTrace)
                printMsg(lvlError, format("@ build-failed %1% - timeout") % drvPath);
            st = BuildResult::TimedOut;
        }

        else if (hook && (!WIFEXITED(status) || WEXITSTATUS(status) != 100)) {
            if (settings.printBuildTrace)
                printMsg(lvlError, format("@ hook-failed %1% - %2% %3%")
                    % drvPath % status % e.msg());
        }

        else {
            if (settings.printBuildTrace)
                printMsg(lvlError, format("@ build-failed %1% - %2% %3%")
                    % drvPath % 1 % e.msg());

            st =
                statusOk(status) ? BuildResult::OutputRejected :
                fixedOutput || diskFull ? BuildResult::TransientFailure :
                BuildResult::PermanentFailure;

            /* Register the outputs of this build as "failed" so we
               won't try to build them again (negative caching).
               However, don't do this for fixed-output derivations,
               since they're likely to fail for transient reasons
               (e.g., fetchurl not being able to access the network).
               Hook errors (like communication problems with the
               remote machine) shouldn't be cached either. */
            if (settings.cacheFailure && !fixedOutput && !diskFull)
                foreach (DerivationOutputs::iterator, i, drv.outputs)
                    worker.store.registerFailedPath(i->second.path);
        }

        done(st, e.msg());
        return;
    }

    /* Release the build user, if applicable. */
    buildUser.release();

    if (settings.printBuildTrace)
        printMsg(lvlError, format("@ build-succeeded %1% -") % drvPath);

    done(BuildResult::Built);
}


HookReply DerivationGoal::tryBuildHook()
{
    if (!settings.useBuildHook) return rpDecline;

    if (!worker.hook) {
	Strings args = {
	    "offload",
	    settings.thisSystem.c_str(),
            (format("%1%") % settings.maxSilentTime).str().c_str(),
            (format("%1%") % settings.printBuildTrace).str().c_str(),
            (format("%1%") % settings.buildTimeout).str().c_str()
	};

        worker.hook = std::make_shared<Agent>(settings.guixProgram, args);
    }

    /* Tell the hook about system features (beyond the system type)
       required from the build machine.  (The hook could parse the
       drv file itself, but this is easier.) */
    Strings features = tokenizeString<Strings>(get(drv.env, "requiredSystemFeatures"));
    foreach (Strings::iterator, i, features) checkStoreName(*i); /* !!! abuse */

    /* Send the request to the hook. */
    writeLine(worker.hook->toAgent.writeSide, (format("%1% %2% %3% %4%")
        % (worker.getNrLocalBuilds() < settings.maxBuildJobs ? "1" : "0")
        % drv.platform % drvPath % concatStringsSep(",", features)).str());

    /* Read the first line of input, which should be a word indicating
       whether the hook wishes to perform the build. */
    string reply;
    while (true) {
        string s = readLine(worker.hook->fromAgent.readSide);
        if (string(s, 0, 2) == "# ") {
            reply = string(s, 2);
            break;
        }
        s += "\n";
        writeToStderr(s);
    }

    debug(format("hook reply is `%1%'") % reply);

    if (reply == "decline" || reply == "postpone")
        return reply == "decline" ? rpDecline : rpPostpone;
    else if (reply != "accept")
        throw Error(format("bad hook reply `%1%'") % reply);

    printMsg(lvlTalkative, format("using hook to build path(s) %1%") % showPaths(missingPaths));

    hook = worker.hook;
    worker.hook.reset();

    /* Tell the hook all the inputs that have to be copied to the
       remote system.  This unfortunately has to contain the entire
       derivation closure to ensure that the validity invariant holds
       on the remote system.  (I.e., it's unfortunate that we have to
       list it since the remote system *probably* already has it.) */
    PathSet allInputs;
    allInputs.insert(inputPaths.begin(), inputPaths.end());
    computeFSClosure(worker.store, drvPath, allInputs);

    string s;
    foreach (PathSet::iterator, i, allInputs) { s += *i; s += ' '; }
    writeLine(hook->toAgent.writeSide, s);

    /* Tell the hooks the missing outputs that have to be copied back
       from the remote system. */
    s = "";
    foreach (PathSet::iterator, i, missingPaths) { s += *i; s += ' '; }
    writeLine(hook->toAgent.writeSide, s);

    hook->toAgent.writeSide.close();

    /* Create the log file and pipe. */
    Path logFile = openLogFile();

    set<int> fds;
    fds.insert(hook->fromAgent.readSide);
    fds.insert(hook->builderOut.readSide);
    worker.childStarted(shared_from_this(), hook->pid, fds, false, true);

    if (settings.printBuildTrace)
        printMsg(lvlError, format("@ build-started %1% - %2% %3% %4%")
            % drvPath % drv.platform % logFile % hook->pid);

    return rpAccept;
}


void chmod_(const Path & path, mode_t mode)
{
    if (chmod(path.c_str(), mode) == -1)
        throw SysError(format("setting permissions on `%1%'") % path);
}


int childEntry(void * arg)
{
    ((DerivationGoal *) arg)->runChild();
    return 1;
}


void DerivationGoal::startBuilder()
{
    auto f = format(
        buildMode == bmRepair ? "repairing path(s) %1%" :
        buildMode == bmCheck ? "checking path(s) %1%" :
        nrRounds > 1 ? "building path(s) %1% (round %2%/%3%)" :
        "building path(s) %1%");
    f.exceptions(boost::io::all_error_bits ^ boost::io::too_many_args_bit);
    startNest(nest, lvlInfo, f % showPaths(missingPaths) % curRound % nrRounds);

    /* Note: built-in builders are *not* running in a chroot environment so
       that we can easily implement them in Guile without having it as a
       derivation input (they are running under a separate build user,
       though).  */
    useChroot = settings.useChroot && !isBuiltin(drv);

    /* Construct the environment passed to the builder. */
    env.clear();

    /* Most shells initialise PATH to some default (/bin:/usr/bin:...) when
       PATH is not set.  We don't want this, so we fill it in with some dummy
       value. */
    env["PATH"] = "/path-not-set";

    /* Set HOME to a non-existing path to prevent certain programs from using
       /etc/passwd (or NIS, or whatever) to locate the home directory (for
       example, wget looks for ~/.wgetrc).  I.e., these tools use /etc/passwd
       if HOME is not set, but they will just assume that the settings file
       they are looking for does not exist if HOME is set but points to some
       non-existing path. */
    Path homeDir = "/homeless-shelter";
    env["HOME"] = homeDir;

    /* Tell the builder where the store is.  Usually they
       shouldn't care, but this is useful for purity checking (e.g.,
       the compiler or linker might only want to accept paths to files
       in the store or in the build directory). */
    env["NIX_STORE"] = settings.nixStore;

    /* The maximum number of cores to utilize for parallel building. */
    env["NIX_BUILD_CORES"] = (format("%d") % settings.buildCores).str();

    /* Add all bindings specified in the derivation. */
    foreach (StringPairs::iterator, i, drv.env)
        env[i->first] = i->second;

    /* Create a temporary directory where the build will take
       place. */
    auto drvName = storePathToName(drvPath);
    tmpDir = createTempDir("", "guix-build-" + drvName, false, false, 0700);

    if (useChroot) {
	/* Make the build directory seen by the build process a sub-directory.
	   That way, "/tmp/guix-build-foo.drv-0" is root-owned, and thus its
	   permissions cannot be changed by the build process, while
	   "/tmp/guix-build-foo.drv-0/top" is owned by the build user.  This
	   cannot be done when !useChroot because then $NIX_BUILD_TOP would
	   be inaccessible to the build user by its full file name.

	   If the build user could make the build directory world-writable,
	   then an attacker could create in it a hardlink to a root-owned file
	   such as /etc/shadow.  If 'keepFailed' is true, the daemon would
	   then chown that hardlink to the user, giving them write access to
	   that file.  */
	tmpDir += "/top";
	if (mkdir(tmpDir.c_str(), 0700) == 1)
	    throw SysError("creating top-level build directory");
    }

    /* In a sandbox, for determinism, always use the same temporary
       directory. */
    tmpDirInSandbox = useChroot ? canonPath("/tmp", true) + "/guix-build-" + drvName + "-0" : tmpDir;

    /* For convenience, set an environment pointing to the top build
       directory. */
    env["NIX_BUILD_TOP"] = tmpDirInSandbox;

    /* Also set TMPDIR and variants to point to this directory. */
    env["TMPDIR"] = env["TEMPDIR"] = env["TMP"] = env["TEMP"] = tmpDirInSandbox;

    /* Explicitly set PWD to prevent problems with chroot builds.  In
       particular, dietlibc cannot figure out the cwd because the
       inode of the current directory doesn't appear in .. (because
       getdents returns the inode of the mount point). */
    env["PWD"] = tmpDirInSandbox;

    /* *Only* if this is a fixed-output derivation, propagate the
       values of the environment variables specified in the
       `impureEnvVars' attribute to the builder.  This allows for
       instance environment variables for proxy configuration such as
       `http_proxy' to be easily passed to downloaders like
       `fetchurl'.  Passing such environment variables from the caller
       to the builder is generally impure, but the output of
       fixed-output derivations is by definition pure (since we
       already know the cryptographic hash of the output). */
    if (fixedOutput) {
        Strings varNames = tokenizeString<Strings>(get(drv.env, "impureEnvVars"));
        foreach (Strings::iterator, i, varNames) env[*i] = getEnv(*i);
    }

    /* The `exportReferencesGraph' feature allows the references graph
       to be passed to a builder.  This attribute should be a list of
       pairs [name1 path1 name2 path2 ...].  The references graph of
       each `pathN' will be stored in a text file `nameN' in the
       temporary build directory.  The text files have the format used
       by `nix-store --register-validity'.  However, the deriver
       fields are left empty. */
    string s = get(drv.env, "exportReferencesGraph");
    Strings ss = tokenizeString<Strings>(s);
    if (ss.size() % 2 != 0)
        throw BuildError(format("odd number of tokens in `exportReferencesGraph': `%1%'") % s);
    for (Strings::iterator i = ss.begin(); i != ss.end(); ) {
        string fileName = *i++;
        checkStoreName(fileName); /* !!! abuse of this function */

        /* Check that the store path is valid. */
        Path storePath = *i++;
        if (!isInStore(storePath))
            throw BuildError(format("`exportReferencesGraph' contains a non-store path `%1%'")
                % storePath);
        storePath = toStorePath(storePath);
        if (!worker.store.isValidPath(storePath))
            throw BuildError(format("`exportReferencesGraph' contains an invalid path `%1%'")
                % storePath);

        /* If there are derivations in the graph, then include their
           outputs as well.  This is useful if you want to do things
           like passing all build-time dependencies of some path to a
           derivation that builds a NixOS DVD image. */
        PathSet paths, paths2;
        computeFSClosure(worker.store, storePath, paths);
        paths2 = paths;

        foreach (PathSet::iterator, j, paths2) {
            if (isDerivation(*j)) {
                Derivation drv = derivationFromPath(worker.store, *j);
                foreach (DerivationOutputs::iterator, k, drv.outputs)
                    computeFSClosure(worker.store, k->second.path, paths);
            }
        }

        /* Write closure info to `fileName'. */
        writeFile(tmpDir + "/" + fileName,
            worker.store.makeValidityRegistration(paths, false, false));
    }


    /* If `build-users-group' is not empty, then we have to build as
       one of the members of that group. */
    if (settings.buildUsersGroup != "") {
        buildUser.acquire();
        assert(buildUser.getUID() != 0);
        assert(buildUser.getGID() != 0);

        /* Make sure that no other processes are executing under this
           uid. */
        buildUser.kill();

        /* Change ownership of the temporary build directory. */
        if (chown(tmpDir.c_str(), buildUser.getUID(), buildUser.getGID()) == -1)
            throw SysError(format("cannot change ownership of '%1%'") % tmpDir);
    }

    if (useChroot) {
#if CHROOT_ENABLED
        /* Create a temporary directory in which we set up the chroot
           environment using bind-mounts.  We put it in the store
           to ensure that we can create hard-links to non-directory
           inputs in the fake store in the chroot (see below). */
        chrootRootDir = drvPath + ".chroot";
        if (pathExists(chrootRootDir)) deletePath(chrootRootDir);

        /* Clean up the chroot directory automatically. */
        autoDelChroot = std::shared_ptr<AutoDelete>(new AutoDelete(chrootRootDir));

        printMsg(lvlChatty, format("setting up chroot environment in `%1%'") % chrootRootDir);

        if (mkdir(chrootRootDir.c_str(), 0750) == -1)
            throw SysError(format("cannot create ‘%1%’") % chrootRootDir);

        if (chown(chrootRootDir.c_str(), 0, buildUser.getGID()) == -1)
            throw SysError(format("cannot change ownership of ‘%1%’") % chrootRootDir);

        /* Create a writable /tmp in the chroot.  Many builders need
           this.  (Of course they should really respect $TMPDIR
           instead.) */
        Path chrootTmpDir = chrootRootDir + "/tmp";
        createDirs(chrootTmpDir);
        chmod_(chrootTmpDir, 01777);

        /* Create a /etc/passwd with entries for the build user and the
           nobody account.  The latter is kind of a hack to support
           Samba-in-QEMU. */
        createDirs(chrootRootDir + "/etc");

        writeFile(chrootRootDir + "/etc/passwd",
            (format(
                "nixbld:x:%1%:%2%:Nix build user:/:/noshell\n"
                "nobody:x:65534:65534:Nobody:/:/noshell\n")
                % (buildUser.enabled() ? buildUser.getUID() : getuid())
                % (buildUser.enabled() ? buildUser.getGID() : getgid())).str());

        /* Declare the build user's group so that programs get a consistent
           view of the system (e.g., "id -gn"). */
        writeFile(chrootRootDir + "/etc/group",
            (format("nixbld:!:%1%:\n")
                % (buildUser.enabled() ? buildUser.getGID() : getgid())).str());

        /* Create /etc/hosts with localhost entry. */
        if (!fixedOutput)
            writeFile(chrootRootDir + "/etc/hosts", "127.0.0.1 localhost\n");

        /* Bind-mount a user-configurable set of directories from the
           host file system. */
        PathSet dirs = tokenizeString<StringSet>(settings.get("build-chroot-dirs", string(DEFAULT_CHROOT_DIRS)));
        PathSet dirs2 = tokenizeString<StringSet>(settings.get("build-extra-chroot-dirs", string("")));
        dirs.insert(dirs2.begin(), dirs2.end());
        for (auto & i : dirs) {
            size_t p = i.find('=');
            if (p == string::npos)
                dirsInChroot[i] = i;
            else
                dirsInChroot[string(i, 0, p)] = string(i, p + 1);
        }
        dirsInChroot[tmpDirInSandbox] = tmpDir;

        /* Make the closure of the inputs available in the chroot,
           rather than the whole store.  This prevents any access
           to undeclared dependencies.  Directories are bind-mounted,
           while other inputs are hard-linked (since only directories
           can be bind-mounted).  !!! As an extra security
           precaution, make the fake store only writable by the
           build user. */
        Path chrootStoreDir = chrootRootDir + settings.nixStore;
        createDirs(chrootStoreDir);
        chmod_(chrootStoreDir, 01775);

        if (chown(chrootStoreDir.c_str(), 0, buildUser.getGID()) == -1)
            throw SysError(format("cannot change ownership of ‘%1%’") % chrootStoreDir);

        foreach (PathSet::iterator, i, inputPaths) {
            struct stat st;
            if (lstat(i->c_str(), &st))
                throw SysError(format("getting attributes of path `%1%'") % *i);
            if (S_ISDIR(st.st_mode))
                dirsInChroot[*i] = *i;
            else {
                Path p = chrootRootDir + *i;
                if (link(i->c_str(), p.c_str()) == -1) {
                    /* Hard-linking fails if we exceed the maximum
                       link count on a file (e.g. 32000 of ext3),
                       which is quite possible after a `nix-store
                       --optimise'. */
                    if (errno != EMLINK)
                        throw SysError(format("linking `%1%' to `%2%'") % p % *i);
                    StringSink sink;
                    dumpPath(*i, sink);
                    StringSource source(sink.s);
                    restorePath(p, source);
                }

                regularInputPaths.insert(*i);
            }
        }

        /* If we're repairing, checking or rebuilding part of a
           multiple-outputs derivation, it's possible that we're
           rebuilding a path that is in settings.dirsInChroot
           (typically the dependencies of /bin/sh).  Throw them
           out. */
        for (auto & i : drv.outputs)
            dirsInChroot.erase(i.second.path);

#else
        throw Error("chroot builds are not supported on this platform");
#endif
    }

    else {

        if (pathExists(homeDir))
            throw Error(format("directory `%1%' exists; please remove it") % homeDir);

        /* We're not doing a chroot build, but we have some valid
           output paths.  Since we can't just overwrite or delete
           them, we have to do hash rewriting: i.e. in the
           environment/arguments passed to the build, we replace the
           hashes of the valid outputs with unique dummy strings;
           after the build, we discard the redirected outputs
           corresponding to the valid outputs, and rewrite the
           contents of the new outputs to replace the dummy strings
           with the actual hashes. */
        if (validPaths.size() > 0)
            foreach (PathSet::iterator, i, validPaths)
                addHashRewrite(*i);

        /* If we're repairing, then we don't want to delete the
           corrupt outputs in advance.  So rewrite them as well. */
        if (buildMode == bmRepair)
            foreach (PathSet::iterator, i, missingPaths)
                if (worker.store.isValidPath(*i) && pathExists(*i)) {
                    addHashRewrite(*i);
                    redirectedBadOutputs.insert(*i);
                }
    }


    /* Run the builder. */
    printMsg(lvlChatty, format("executing builder `%1%'") % drv.builder);

    /* Create the log file. */
    Path logFile = openLogFile();

    /* Create a pipe to get the output of the builder. */
    builderOut.create();

    /* Fork a child to build the package.  Note that while we
       currently use forks to run and wait for the children, it
       shouldn't be hard to use threads for this on systems where
       fork() is unavailable or inefficient.

       If we're building in a chroot, then also set up private
       namespaces for the build:

       - The PID namespace causes the build to start as PID 1.
         Processes outside of the chroot are not visible to those on
         the inside, but processes inside the chroot are visible from
         the outside (though with different PIDs).

       - The private mount namespace ensures that all the bind mounts
         we do will only show up in this process and its children, and
         will disappear automatically when we're done.

       - The private network namespace ensures that the builder cannot
         talk to the outside world (or vice versa).  It only has a
         private loopback interface.

       - The IPC namespace prevents the builder from communicating
         with outside processes using SysV IPC mechanisms (shared
         memory, message queues, semaphores).  It also ensures that
         all IPC objects are destroyed when the builder exits.

       - The UTS namespace ensures that builders see a hostname of
         localhost rather than the actual hostname.
    */
#if __linux__
    if (useChroot) {
	char stack[32 * 1024];
	int flags = CLONE_NEWPID | CLONE_NEWNS | CLONE_NEWIPC | CLONE_NEWUTS | SIGCHLD;
	if (!fixedOutput) flags |= CLONE_NEWNET;
	/* Ensure proper alignment on the stack.  On aarch64, it has to be 16
	   bytes.  */
	pid = clone(childEntry,
		    (char *)(((uintptr_t)stack + sizeof(stack) - 8) & ~(uintptr_t)0xf),
		    flags, this);
	if (pid == -1)
	    throw SysError("cloning builder process");
    } else
#endif
    {
        pid = fork();
        if (pid == 0) runChild();
    }

    if (pid == -1) throw SysError("unable to fork");

    /* parent */
    pid.setSeparatePG(true);
    builderOut.writeSide.close();
    worker.childStarted(shared_from_this(), pid,
        singleton<set<int> >(builderOut.readSide), true, true);

    /* Check if setting up the build environment failed. */
    string msg = readLine(builderOut.readSide);
    if (!msg.empty()) throw Error(msg);

    if (settings.printBuildTrace) {
        printMsg(lvlError, format("@ build-started %1% - %2% %3% %4%")
            % drvPath % drv.platform % logFile % pid);
    }

}

/* Return true if the operating system kernel part of SYSTEM1 and SYSTEM2 (the
   bit that comes after the hyphen in system types such as "i686-linux") is
   the same.  */
static bool sameOperatingSystemKernel(const std::string& system1, const std::string& system2)
{
    auto os1 = system1.substr(system1.find("-"));
    auto os2 = system2.substr(system2.find("-"));
    return os1 == os2;
}

void DerivationGoal::runChild()
{
    /* Warning: in the child we should absolutely not make any SQLite
       calls! */

    try { /* child */

        _writeToStderr = 0;

        restoreAffinity();

        commonChildInit(builderOut);

#if CHROOT_ENABLED
        if (useChroot) {
            /* Initialise the loopback interface. */
            AutoCloseFD fd(socket(PF_INET, SOCK_DGRAM, IPPROTO_IP));
            if (fd == -1) throw SysError("cannot open IP socket");

            struct ifreq ifr;
            strcpy(ifr.ifr_name, "lo");
            ifr.ifr_flags = IFF_UP | IFF_LOOPBACK | IFF_RUNNING;
            if (ioctl(fd, SIOCSIFFLAGS, &ifr) == -1)
                throw SysError("cannot set loopback interface flags");

            fd.close();

            /* Set the hostname etc. to fixed values. */
            char hostname[] = "localhost";
            if (sethostname(hostname, sizeof(hostname)) == -1)
                throw SysError("cannot set host name");
            char domainname[] = "(none)"; // kernel default
            if (setdomainname(domainname, sizeof(domainname)) == -1)
                throw SysError("cannot set domain name");

            /* Make all filesystems private.  This is necessary
               because subtrees may have been mounted as "shared"
               (MS_SHARED).  (Systemd does this, for instance.)  Even
               though we have a private mount namespace, mounting
               filesystems on top of a shared subtree still propagates
               outside of the namespace.  Making a subtree private is
               local to the namespace, though, so setting MS_PRIVATE
               does not affect the outside world. */
            if (mount(0, "/", 0, MS_REC|MS_PRIVATE, 0) == -1) {
                throw SysError("unable to make ‘/’ private mount");
            }

            /* Bind-mount chroot directory to itself, to treat it as a
               different filesystem from /, as needed for pivot_root. */
            if (mount(chrootRootDir.c_str(), chrootRootDir.c_str(), 0, MS_BIND, 0) == -1)
                throw SysError(format("unable to bind mount ‘%1%’") % chrootRootDir);

            /* Set up a nearly empty /dev, unless the user asked to
               bind-mount the host /dev. */
            Strings ss;
            if (dirsInChroot.find("/dev") == dirsInChroot.end()) {
                createDirs(chrootRootDir + "/dev/shm");
                createDirs(chrootRootDir + "/dev/pts");
                ss.push_back("/dev/full");
#ifdef __linux__
                if (pathExists("/dev/kvm"))
                    ss.push_back("/dev/kvm");
#endif
                ss.push_back("/dev/null");
                ss.push_back("/dev/random");
                ss.push_back("/dev/tty");
                ss.push_back("/dev/urandom");
                ss.push_back("/dev/zero");
                createSymlink("/proc/self/fd", chrootRootDir + "/dev/fd");
                createSymlink("/proc/self/fd/0", chrootRootDir + "/dev/stdin");
                createSymlink("/proc/self/fd/1", chrootRootDir + "/dev/stdout");
                createSymlink("/proc/self/fd/2", chrootRootDir + "/dev/stderr");
            }

            /* Fixed-output derivations typically need to access the
               network, so give them access to /etc/resolv.conf and so
               on. */
            if (fixedOutput) {
                ss.push_back("/etc/resolv.conf");
                ss.push_back("/etc/nsswitch.conf");
                ss.push_back("/etc/services");
                ss.push_back("/etc/hosts");
            }

            for (auto & i : ss) dirsInChroot[i] = i;

            /* Bind-mount all the directories from the "host"
               filesystem that we want in the chroot
               environment. */
            foreach (DirsInChroot::iterator, i, dirsInChroot) {
                struct stat st;
                Path source = i->second;
                Path target = chrootRootDir + i->first;
                if (source == "/proc") continue; // backwards compatibility
                debug(format("bind mounting `%1%' to `%2%'") % source % target);
                if (stat(source.c_str(), &st) == -1)
                    throw SysError(format("getting attributes of path `%1%'") % source);
                if (S_ISDIR(st.st_mode))
                    createDirs(target);
                else {
                    createDirs(dirOf(target));
                    writeFile(target, "");
                }
                if (mount(source.c_str(), target.c_str(), "", MS_BIND, 0) == -1)
                    throw SysError(format("bind mount from `%1%' to `%2%' failed") % source % target);
            }

            /* Bind a new instance of procfs on /proc to reflect our
               private PID namespace. */
            createDirs(chrootRootDir + "/proc");
            if (mount("none", (chrootRootDir + "/proc").c_str(), "proc", 0, 0) == -1)
                throw SysError("mounting /proc");

            /* Mount a new tmpfs on /dev/shm to ensure that whatever
               the builder puts in /dev/shm is cleaned up automatically. */
            if (pathExists("/dev/shm") && mount("none", (chrootRootDir + "/dev/shm").c_str(), "tmpfs", 0, 0) == -1)
                throw SysError("mounting /dev/shm");

            /* Mount a new devpts on /dev/pts.  Note that this
               requires the kernel to be compiled with
               CONFIG_DEVPTS_MULTIPLE_INSTANCES=y (which is the case
               if /dev/ptx/ptmx exists). */
            if (pathExists("/dev/pts/ptmx") &&
                !pathExists(chrootRootDir + "/dev/ptmx")
                && dirsInChroot.find("/dev/pts") == dirsInChroot.end())
            {
                if (mount("none", (chrootRootDir + "/dev/pts").c_str(), "devpts", 0, "newinstance,mode=0620") == -1)
                    throw SysError("mounting /dev/pts");
                createSymlink("/dev/pts/ptmx", chrootRootDir + "/dev/ptmx");

                /* Make sure /dev/pts/ptmx is world-writable.  With some
                   Linux versions, it is created with permissions 0.  */
                chmod_(chrootRootDir + "/dev/pts/ptmx", 0666);
            }

            /* Do the chroot(). */
            if (chdir(chrootRootDir.c_str()) == -1)
                throw SysError(format("cannot change directory to '%1%'") % chrootRootDir);

            if (mkdir("real-root", 0) == -1)
                throw SysError("cannot create real-root directory");

            if (pivot_root(".", "real-root") == -1)
                throw SysError(format("cannot pivot old root directory onto '%1%'") % (chrootRootDir + "/real-root"));

            if (chroot(".") == -1)
                throw SysError(format("cannot change root directory to '%1%'") % chrootRootDir);

            if (umount2("real-root", MNT_DETACH) == -1)
                throw SysError("cannot unmount real root filesystem");

            if (rmdir("real-root") == -1)
                throw SysError("cannot remove real-root directory");
        }
#endif

        if (chdir(tmpDirInSandbox.c_str()) == -1)
            throw SysError(format("changing into `%1%'") % tmpDir);

        /* Close all other file descriptors. */
        closeMostFDs(set<int>());

#if __linux__
        /* Change the personality to 32-bit if we're doing an
           i686-linux build on an x86_64-linux machine. */
        struct utsname utsbuf;
        uname(&utsbuf);
        if (drv.platform == "i686-linux" &&
            (settings.thisSystem == "x86_64-linux" ||
             (!strcmp(utsbuf.sysname, "Linux") && !strcmp(utsbuf.machine, "x86_64")))) {
            if (personality(PER_LINUX32) == -1)
                throw SysError("cannot set i686-linux personality");
        }

        if (drv.platform == "armhf-linux" &&
            (settings.thisSystem == "aarch64-linux" ||
             (!strcmp(utsbuf.sysname, "Linux") && !strcmp(utsbuf.machine, "aarch64")))) {
            if (personality(PER_LINUX32) == -1)
                throw SysError("cannot set armhf-linux personality");
        }

        /* Impersonate a Linux 2.6 machine to get some determinism in
           builds that depend on the kernel version. */
        if ((drv.platform == "i686-linux" || drv.platform == "x86_64-linux") && settings.impersonateLinux26) {
            int cur = personality(0xffffffff);
            if (cur != -1) personality(cur | 0x0020000 /* == UNAME26 */);
        }

        /* Disable address space randomization for improved
           determinism. */
        int cur = personality(0xffffffff);
        if (cur != -1) personality(cur | ADDR_NO_RANDOMIZE);
#endif

        /* Fill in the environment. */
        Strings envStrs;
        foreach (Environment::const_iterator, i, env)
            envStrs.push_back(rewriteHashes(i->first + "=" + i->second, rewritesToTmp));

        /* If we are running in `build-users' mode, then switch to the
           user we allocated above.  Make sure that we drop all root
           privileges.  Note that above we have closed all file
           descriptors except std*, so that's safe.  Also note that
           setuid() when run as root sets the real, effective and
           saved UIDs. */
        if (buildUser.enabled()) {
            /* Preserve supplementary groups of the build user, to allow
               admins to specify groups such as "kvm".  */
            if (setgroups(buildUser.getSupplementaryGIDs().size(),
                          buildUser.getSupplementaryGIDs().data()) == -1)
                throw SysError("cannot set supplementary groups of build user");

            if (setgid(buildUser.getGID()) == -1 ||
                getgid() != buildUser.getGID() ||
                getegid() != buildUser.getGID())
                throw SysError("setgid failed");

            if (setuid(buildUser.getUID()) == -1 ||
                getuid() != buildUser.getUID() ||
                geteuid() != buildUser.getUID())
                throw SysError("setuid failed");
        }

        restoreSIGPIPE();

        /* Indicate that we managed to set up the build environment. */
        writeFull(STDERR_FILENO, "\n");

        /* Execute the program.  This should not return. */
        if (isBuiltin(drv)) {
            try {
                logType = ltFlat;

		auto buildDrv = lookupBuiltinBuilder(drv.builder);
                if (buildDrv != NULL) {
		    /* Check what the output file name is.  When doing a
		       'bmCheck' build, the output file name is different from
		       that specified in DRV due to hash rewriting.  */
		    Path output = drv.outputs["out"].path;
		    auto redirected = redirectedOutputs.find(output);
		    if (redirected != redirectedOutputs.end())
			output = redirected->second;

                    buildDrv(drv, drvPath, output);
		}
                else
                    throw Error(format("unsupported builtin function '%1%'") % string(drv.builder, 8));
                _exit(0);
            } catch (std::exception & e) {
                writeFull(STDERR_FILENO, "error: " + string(e.what()) + "\n");
                _exit(1);
            }
        }

        /* Fill in the arguments. */
        Strings args;
        string builderBasename = baseNameOf(drv.builder);
        args.push_back(builderBasename);
        foreach (Strings::iterator, i, drv.args)
            args.push_back(rewriteHashes(*i, rewritesToTmp));

	/* If DRV targets the same operating system kernel, try to execute it:
	   there might be binfmt_misc set up for user-land emulation of other
	   architectures.  However, if it targets a different operating
	   system--e.g., "i586-gnu" vs. "x86_64-linux"--do not try executing
	   it: the ELF file for that OS is likely indistinguishable from a
	   native ELF binary and it would just crash at run time.  */
	int error;
	if (sameOperatingSystemKernel(drv.platform, settings.thisSystem)) {
	    execve(drv.builder.c_str(), stringsToCharPtrs(args).data(),
		   stringsToCharPtrs(envStrs).data());
	    error = errno;
	} else {
	    error = ENOEXEC;
	}

	/* Right platform?  Check this after we've tried 'execve' to allow for
	   transparent emulation of different platforms with binfmt_misc
	   handlers that invoke QEMU.  */
	if (error == ENOEXEC && !canBuildLocally(drv.platform)) {
	    if (settings.printBuildTrace)
		printMsg(lvlError, format("@ unsupported-platform %1% %2%") % drvPath % drv.platform);
	    throw Error(
		format("a `%1%' is required to build `%3%', but I am a `%2%'")
		% drv.platform % settings.thisSystem % drvPath);
	}

	errno = error;
        throw SysError(format("executing `%1%'") % drv.builder);

    } catch (std::exception & e) {
        writeFull(STDERR_FILENO, "while setting up the build environment: " + string(e.what()) + "\n");
        _exit(1);
    }

    abort(); /* never reached */
}


/* Parse a list of reference specifiers.  Each element must either be
   a store path, or the symbolic name of the output of the derivation
   (such as `out'). */
PathSet parseReferenceSpecifiers(const Derivation & drv, string attr)
{
    PathSet result;
    Paths paths = tokenizeString<Paths>(attr);
    foreach (Strings::iterator, i, paths) {
        if (isStorePath(*i))
            result.insert(*i);
        else if (drv.outputs.find(*i) != drv.outputs.end())
            result.insert(drv.outputs.find(*i)->second.path);
        else throw BuildError(
            format("derivation contains an invalid reference specifier `%1%'")
            % *i);
    }
    return result;
}


void DerivationGoal::registerOutputs()
{
    /* When using a build hook, the build hook can register the output
       as valid (by doing `nix-store --import').  If so we don't have
       to do anything here. */
    if (hook) {
        bool allValid = true;
        foreach (DerivationOutputs::iterator, i, drv.outputs)
            if (!worker.store.isValidPath(i->second.path)) allValid = false;
        if (allValid) return;
    }

    ValidPathInfos infos;

    /* Set of inodes seen during calls to canonicalisePathMetaData()
       for this build's outputs.  This needs to be shared between
       outputs to allow hard links between outputs. */
    InodesSeen inodesSeen;

    Path checkSuffix = "-check";

    /* Check whether the output paths were created, and grep each
       output path to determine what other paths it references.  Also make all
       output paths read-only. */
    foreach (DerivationOutputs::iterator, i, drv.outputs) {
        Path path = i->second.path;
        if (missingPaths.find(path) == missingPaths.end()) continue;

        Path actualPath = path;
        if (useChroot) {
            actualPath = chrootRootDir + path;
            if (pathExists(actualPath)) {
                /* Move output paths from the chroot to the store. */
                if (buildMode == bmRepair)
                    replaceValidPath(path, actualPath);
                else
                    if (buildMode != bmCheck && rename(actualPath.c_str(), path.c_str()) == -1)
                        throw SysError(format("moving build output `%1%' from the chroot to the store") % path);
            }
            if (buildMode != bmCheck) actualPath = path;
        } else {
            Path redirected = redirectedOutputs[path];
            if (buildMode == bmRepair
                && redirectedBadOutputs.find(path) != redirectedBadOutputs.end()
                && pathExists(redirected))
                replaceValidPath(path, redirected);
            if (buildMode == bmCheck && redirected != "")
                actualPath = redirected;
        }

        struct stat st;
        if (lstat(actualPath.c_str(), &st) == -1) {
            if (errno == ENOENT)
                throw BuildError(
                    format("builder for `%1%' failed to produce output path `%2%'")
                    % drvPath % path);
            throw SysError(format("getting attributes of path `%1%'") % actualPath);
        }

#ifndef __CYGWIN__
        /* Check that the output is not group or world writable, as
           that means that someone else can have interfered with the
           build.  Also, the output should be owned by the build
           user. */
        if ((!S_ISLNK(st.st_mode) && (st.st_mode & (S_IWGRP | S_IWOTH))) ||
            (buildUser.enabled() && st.st_uid != buildUser.getUID()))
            throw BuildError(format("suspicious ownership or permission on `%1%'; rejecting this build output") % path);
#endif

        /* Apply hash rewriting if necessary. */
        bool rewritten = false;
        if (!rewritesFromTmp.empty()) {
            printMsg(lvlError, format("warning: rewriting hashes in `%1%'; cross fingers") % path);

            /* Canonicalise first.  This ensures that the path we're
               rewriting doesn't contain a hard link to /etc/shadow or
               something like that. */
            canonicalisePathMetaData(actualPath, buildUser.enabled() ? buildUser.getUID() : -1, inodesSeen);

            /* FIXME: this is in-memory. */
            StringSink sink;
            dumpPath(actualPath, sink);
            deletePath(actualPath);
            sink.s = rewriteHashes(sink.s, rewritesFromTmp);
            StringSource source(sink.s);
            restorePath(actualPath, source);

            rewritten = true;
        }

        startNest(nest, lvlTalkative,
            format("scanning for references inside `%1%'") % path);

        /* Check that fixed-output derivations produced the right
           outputs (i.e., the content hash should match the specified
           hash). */
        if (i->second.hash != "") {

            bool recursive; HashType ht; Hash h;
            i->second.parseHashInfo(recursive, ht, h);

            if (!recursive) {
                /* The output path should be a regular file without
                   execute permission. */
                if (!S_ISREG(st.st_mode) || (st.st_mode & S_IXUSR) != 0)
                    throw BuildError(
                        format("output path `%1% should be a non-executable regular file") % path);
            }

            /* Check the hash. */
            Hash h2 = recursive ? hashPath(ht, actualPath).first : hashFile(ht, actualPath);
            if (h != h2) {
		if (settings.printBuildTrace)
		    printMsg(lvlError, format("@ hash-mismatch %1% %2% %3% %4%")
			     % path % i->second.hashAlgo
			     % printHash16or32(h) % printHash16or32(h2));
                throw BuildError(format("hash mismatch for store item '%1%'") % path);
	    }
        }

        /* Get rid of all weird permissions.  This also checks that
           all files are owned by the build user, if applicable. */
        canonicalisePathMetaData(actualPath,
            buildUser.enabled() && !rewritten ? buildUser.getUID() : -1, inodesSeen);

        /* For this output path, find the references to other paths
           contained in it.  Compute the SHA-256 NAR hash at the same
           time.  The hash is stored in the database so that we can
           verify later on whether nobody has messed with the store. */
        HashResult hash;
        PathSet references = scanForReferences(actualPath, allPaths, hash);

        if (buildMode == bmCheck) {
            if (!store->isValidPath(path)) continue;
            ValidPathInfo info = worker.store.queryPathInfo(path);
            if (hash.first != info.hash) {
                if (settings.keepFailed) {
                    Path dst = path + checkSuffix;
                    if (pathExists(dst)) deletePath(dst);
                    if (rename(actualPath.c_str(), dst.c_str()))
                        throw SysError(format("renaming `%1%' to `%2%'") % actualPath % dst);
                    throw Error(format("derivation `%1%' may not be deterministic: output `%2%' differs from ‘%3%’")
                        % drvPath % path % dst);
                } else
                    throw Error(format("derivation `%1%' may not be deterministic: output `%2%' differs")
                        % drvPath % path);
            }

            if (settings.printBuildTrace)
                printMsg(lvlError, format("@ build-succeeded %1% -") % drvPath);

            continue;
        }

        /* For debugging, print out the referenced and unreferenced
           paths. */
        foreach (PathSet::iterator, i, inputPaths) {
            PathSet::iterator j = references.find(*i);
            if (j == references.end())
                debug(format("unreferenced input: `%1%'") % *i);
            else
                debug(format("referenced input: `%1%'") % *i);
        }

        /* Enforce `allowedReferences' and friends. */
        auto checkRefs = [&](const string & attrName, bool allowed, bool recursive) {
            if (drv.env.find(attrName) == drv.env.end()) return;

            PathSet spec = parseReferenceSpecifiers(drv, get(drv.env, attrName));

            PathSet used;
            if (recursive) {
                /* Our requisites are the union of the closures of our references. */
                for (auto & i : references)
                    /* Don't call computeFSClosure on ourselves. */
                    if (actualPath != i)
                        computeFSClosure(worker.store, i, used);
            } else
                used = references;

            for (auto & i : used)
                if (allowed) {
                    if (spec.find(i) == spec.end())
                        throw BuildError(format("output (`%1%') is not allowed to refer to path `%2%'") % actualPath % i);
                } else {
                    if (spec.find(i) != spec.end())
                        throw BuildError(format("output (`%1%') is not allowed to refer to path `%2%'") % actualPath % i);
                }
        };

        checkRefs("allowedReferences", true, false);
        checkRefs("allowedRequisites", true, true);
        checkRefs("disallowedReferences", false, false);
        checkRefs("disallowedRequisites", false, true);

        if (curRound == nrRounds) {
            worker.store.optimisePath(path); // FIXME: combine with scanForReferences()

            worker.store.markContentsGood(path);
        }

        ValidPathInfo info;
        info.path = path;
        info.hash = hash.first;
        info.narSize = hash.second;
        info.references = references;
        info.deriver = drvPath;
        infos.push_back(info);
    }

    /* Compare the result with the previous round, and report which
       path is different, if any.*/
    if (curRound > 1 && prevInfos != infos) {
        assert(prevInfos.size() == infos.size());
        for (auto i = prevInfos.begin(), j = infos.begin(); i != prevInfos.end(); ++i, ++j)
            if (!(*i == *j)) {
                Path prev = i->path + checkSuffix;
                if (pathExists(prev))
                    throw NotDeterministic(
                        format("output ‘%1%’ of ‘%2%’ differs from ‘%3%’ from previous round")
                        % i->path % drvPath % prev);
                else
                    throw NotDeterministic(
                        format("output ‘%1%’ of ‘%2%’ differs from previous round")
                        % i->path % drvPath);
            }
        assert(false); // shouldn't happen
    }

    if (settings.keepFailed) {
        for (auto & i : drv.outputs) {
            Path prev = i.second.path + checkSuffix;
            if (pathExists(prev)) deletePath(prev);
            if (curRound < nrRounds) {
                Path dst = i.second.path + checkSuffix;
                if (rename(i.second.path.c_str(), dst.c_str()))
                    throw SysError(format("renaming ‘%1%’ to ‘%2%’") % i.second.path % dst);
            }
        }

    }

    if (curRound < nrRounds) {
        prevInfos = infos;
        return;
    }

    /* Register each output path as valid, and register the sets of
       paths referenced by each of them.  If there are cycles in the
       outputs, this will fail. */
    worker.store.registerValidPaths(infos);
}


string drvsLogDir = "drvs";


Path DerivationGoal::openLogFile()
{
    logSize = 0;

    if (!settings.keepLog) return "";

    string baseName = baseNameOf(drvPath);

    /* Create a log file. */
    Path dir = (format("%1%/%2%/%3%/") % settings.nixLogDir % drvsLogDir % string(baseName, 0, 2)).str();
    createDirs(dir);

    switch (settings.logCompression)
      {
      case COMPRESSION_GZIP: {
        Path logFileName = (format("%1%/%2%.gz") % dir % string(baseName, 2)).str();
        AutoCloseFD fd = open(logFileName.c_str(), O_CREAT | O_WRONLY | O_TRUNC, 0666);
        if (fd == -1) throw SysError(format("creating log file `%1%'") % logFileName);
        closeOnExec(fd);

	/* Note: FD will be closed by 'gzclose'.  */
        if (!(gzLogFile = gzdopen(fd.borrow(), "w")))
            throw Error(format("cannot open compressed log file `%1%'") % logFileName);

        gzbuffer(gzLogFile, 32768);
        gzsetparams(gzLogFile, Z_BEST_COMPRESSION, Z_DEFAULT_STRATEGY);

        return logFileName;
      }

#if HAVE_BZLIB_H
      case COMPRESSION_BZIP2: {
        Path logFileName = (format("%1%/%2%.bz2") % dir % string(baseName, 2)).str();
        AutoCloseFD fd = open(logFileName.c_str(), O_CREAT | O_WRONLY | O_TRUNC, 0666);
        if (fd == -1) throw SysError(format("creating log file `%1%'") % logFileName);
        closeOnExec(fd);

        if (!(fLogFile = fdopen(fd.borrow(), "w")))
            throw SysError(format("opening file `%1%'") % logFileName);

        int err;
        if (!(bzLogFile = BZ2_bzWriteOpen(&err, fLogFile, 9, 0, 0)))
            throw Error(format("cannot open compressed log file `%1%'") % logFileName);

        return logFileName;
      }
#endif

      case COMPRESSION_NONE: {
        Path logFileName = (format("%1%/%2%") % dir % string(baseName, 2)).str();
        fdLogFile = open(logFileName.c_str(), O_CREAT | O_WRONLY | O_TRUNC, 0666);
        if (fdLogFile == -1) throw SysError(format("creating log file `%1%'") % logFileName);
        closeOnExec(fdLogFile);
        return logFileName;
      }
    }

    abort();
}


void DerivationGoal::closeLogFile()
{
    if (gzLogFile) {
	int err;
	err = gzclose(gzLogFile);
	gzLogFile = NULL;
	if (err != Z_OK) throw Error(format("cannot close compressed log file (gzip error = %1%)") % err);
    }
#if HAVE_BZLIB_H
    else if (bzLogFile) {
        int err;
        BZ2_bzWriteClose(&err, bzLogFile, 0, 0, 0);
        bzLogFile = 0;
        if (err != BZ_OK) throw Error(format("cannot close compressed log file (BZip2 error = %1%)") % err);
    }
#endif

    if (fLogFile) {
        fclose(fLogFile);
        fLogFile = 0;
    }

    fdLogFile.close();
}


static void _chown(const Path & path, uid_t uid, gid_t gid)
{
    checkInterrupt();

    if (lchown(path.c_str(), uid, gid) == -1) {
	throw SysError(format("change owner and group of `%1%'") % path);
    }
    struct stat st = lstat(path);
    if (S_ISDIR(st.st_mode)) {
        for (auto & i : readDirectory(path))
            _chown(path + "/" + i.name, uid, gid);
    }
}


void DerivationGoal::deleteTmpDir(bool force)
{
    if (tmpDir != "") {
	// When useChroot is true, tmpDir looks like
	// "/tmp/guix-build-foo.drv-0/top".  Its parent is root-owned.
	string top;
	if (useChroot) {
	    if (baseNameOf(tmpDir) != "top") abort();
	    top = dirOf(tmpDir);
	} else top = tmpDir;

        if (settings.keepFailed && !force) {
            printMsg(lvlError,
                format("note: keeping build directory `%2%'")
                % drvPath % top);
            chmod(tmpDir.c_str(), 0755);

            // Change the ownership if clientUid is set. Never change the
            // ownership or the group to "root" for security reasons.
            if (settings.clientUid != (uid_t) -1 && settings.clientUid != 0) {
                _chown(tmpDir, settings.clientUid,
                       settings.clientGid != 0 ? settings.clientGid : -1);

		if (top != tmpDir) {
		    // Rename tmpDir to its parent, with an intermediate step.
		    string pivot = top + ".pivot";
		    if (rename(top.c_str(), pivot.c_str()) == -1)
			throw SysError("pivoting failed build tree");
		    if (rename((pivot + "/top").c_str(), top.c_str()) == -1)
			throw SysError("renaming failed build tree");
		    rmdir(pivot.c_str());
		}
            }
        }
        else {
            deletePath(tmpDir);
	    if (top != tmpDir) rmdir(dirOf(tmpDir).c_str());
	}
        tmpDir = "";
    }
}


void DerivationGoal::handleChildOutput(int fd, const string & data)
{
    string prefix;

    if (settings.multiplexedBuildOutput) {
	/* Print a prefix that allows clients to determine whether a message
	   comes from the daemon or from a build process, and in the latter
	   case, which build process it comes from.  The PID here matches the
	   one given in "@ build-started" traces; it's shorter that the
	   derivation file name, hence this choice.  */
	prefix = "@ build-log "
	    + std::to_string(pid < 0 ? hook->pid : pid)
	    + " " + std::to_string(data.size()) + "\n";
    }

    if ((hook && fd == hook->builderOut.readSide) ||
        (!hook && fd == builderOut.readSide))
    {
        logSize += data.size();
        if (settings.maxLogSize && logSize > settings.maxLogSize) {
            printMsg(lvlError,
                format("%1% killed after writing more than %2% bytes of log output")
                % getName() % settings.maxLogSize);
            timedOut(); // not really a timeout, but close enough
            return;
        }
        if (verbosity >= settings.buildVerbosity)
            writeToStderr(prefix + data);

	if (gzLogFile) {
	    if (data.size() > 0) {
		int count, err;
		count = gzwrite(gzLogFile, data.data(), data.size());
		if (count == 0) throw Error(format("cannot write to compressed log file (gzip error = %1%)") % gzerror(gzLogFile, &err));
	    }
#if HAVE_BZLIB_H
	} else if (bzLogFile) {
            int err;
            BZ2_bzWrite(&err, bzLogFile, (unsigned char *) data.data(), data.size());
            if (err != BZ_OK) throw Error(format("cannot write to compressed log file (BZip2 error = %1%)") % err);
#endif
        } else if (fdLogFile != -1)
            writeFull(fdLogFile, data);
    }

    if (hook && fd == hook->fromAgent.readSide)
        writeToStderr(prefix + data);
}


void DerivationGoal::handleEOF(int fd)
{
    worker.wakeUp(shared_from_this());
}


PathSet DerivationGoal::checkPathValidity(bool returnValid, bool checkHash)
{
    PathSet result;
    foreach (DerivationOutputs::iterator, i, drv.outputs) {
        if (!wantOutput(i->first, wantedOutputs)) continue;
        bool good =
            worker.store.isValidPath(i->second.path) &&
            (!checkHash || worker.store.pathContentsGood(i->second.path));
        if (good == returnValid) result.insert(i->second.path);
    }
    return result;
}


bool DerivationGoal::pathFailed(const Path & path)
{
    if (!settings.cacheFailure) return false;

    if (!worker.store.hasPathFailed(path)) return false;

    printMsg(lvlError, format("builder for `%1%' failed previously (cached)") % path);

    if (settings.printBuildTrace)
        printMsg(lvlError, format("@ build-failed %1% - cached") % drvPath);

    done(BuildResult::CachedFailure);

    return true;
}


Path DerivationGoal::addHashRewrite(const Path & path)
{
    string h1 = string(path, settings.nixStore.size() + 1, 32);
    string h2 = string(printHash32(hashString(htSHA256, "rewrite:" + drvPath + ":" + path)), 0, 32);
    Path p = settings.nixStore + "/" + h2 + string(path, settings.nixStore.size() + 33);
    if (pathExists(p)) deletePath(p);
    assert(path.size() == p.size());
    rewritesToTmp[h1] = h2;
    rewritesFromTmp[h2] = h1;
    redirectedOutputs[path] = p;
    printMsg(lvlChatty, format("output '%1%' redirected to '%2%'")
	     % path % p);
    return p;
}


void DerivationGoal::done(BuildResult::Status status, const string & msg)
{
    result.status = status;
    result.errorMsg = msg;
    amDone(result.success() ? ecSuccess : ecFailed);
    if (result.status == BuildResult::TimedOut)
        worker.timedOut = true;
    if (result.status == BuildResult::PermanentFailure || result.status == BuildResult::CachedFailure)
        worker.permanentFailure = true;
}


//////////////////////////////////////////////////////////////////////


class SubstitutionGoal : public Goal
{
    friend class Worker;

private:
    /* The store path that should be realised through a substitute. */
    Path storePath;

    /* Path info returned by the substituter's query info operation. */
    SubstitutablePathInfo info;

    /* Lock on the store path. */
    std::shared_ptr<PathLocks> outputLock;

    /* Whether to try to repair a valid path. */
    bool repair;

    /* Location where we're downloading the substitute.  Differs from
       storePath when doing a repair. */
    Path destPath;

    typedef void (SubstitutionGoal::*GoalState)();
    GoalState state;

    /* The substituter. */
    std::shared_ptr<Agent> substituter;

    /* Either the empty string, or the status phrase returned by the
       substituter.  */
    string status;

    void tryNext();

public:
    SubstitutionGoal(const Path & storePath, Worker & worker, bool repair = false);
    ~SubstitutionGoal();

    void timedOut();

    string key()
    {
        /* "a$" ensures substitution goals happen before derivation
           goals. */
        return "a$" + storePathToName(storePath) + "$" + storePath;
    }

    void work();

    /* The states. */
    void init();
    void gotInfo();
    void referencesValid();
    void tryToRun();
    void finished();

    /* Callback used by the worker to write to the log. */
    void handleChildOutput(int fd, const string & data);
    void handleEOF(int fd);

    Path getStorePath() { return storePath; }
};


SubstitutionGoal::SubstitutionGoal(const Path & storePath, Worker & worker, bool repair)
    : Goal(worker)
    , repair(repair)
{
    this->storePath = storePath;
    state = &SubstitutionGoal::init;
    name = (format("substitution of `%1%'") % storePath).str();
    trace("created");
}


SubstitutionGoal::~SubstitutionGoal()
{
    if (substituter) worker.childTerminated(substituter->pid);
}


void SubstitutionGoal::timedOut()
{
    if (settings.printBuildTrace)
        printMsg(lvlError, format("@ substituter-failed %1% timeout") % storePath);
    if (substituter) {
        pid_t savedPid = substituter->pid;
	substituter.reset();
        worker.childTerminated(savedPid);
    }
    amDone(ecFailed);
}


void SubstitutionGoal::work()
{
    (this->*state)();
}


void SubstitutionGoal::init()
{
    trace("init");

    worker.store.addTempRoot(storePath);

    /* If the path already exists we're done. */
    if (!repair && worker.store.isValidPath(storePath)) {
        amDone(ecSuccess);
        return;
    }

    if (settings.readOnlyMode)
        throw Error(format("cannot substitute path `%1%' - no write access to the store") % storePath);

    tryNext();
}


void SubstitutionGoal::tryNext()
{
    trace("trying substituter");

    SubstitutablePathInfos infos;
    PathSet dummy(singleton<PathSet>(storePath));
    worker.store.querySubstitutablePathInfos(dummy, infos);
    SubstitutablePathInfos::iterator k = infos.find(storePath);
    if (k == infos.end()) {
        /* None left.  Terminate this goal and let someone else deal
           with it. */
        debug(format("path `%1%' is required, but there is no substituter that can build it") % storePath);
        /* Hack: don't indicate failure if there were no substituters.
           In that case the calling derivation should just do a
           build. */
        amDone(ecNoSubstituters);
	return;
    }

    /* Found a substitute.  */
    info = k->second;

    /* To maintain the closure invariant, we first have to realise the
       paths referenced by this one. */
    foreach (PathSet::iterator, i, info.references)
        if (*i != storePath) /* ignore self-references */
            addWaitee(worker.makeSubstitutionGoal(*i));

    if (waitees.empty()) /* to prevent hang (no wake-up event) */
        referencesValid();
    else
        state = &SubstitutionGoal::referencesValid;
}


void SubstitutionGoal::referencesValid()
{
    trace("all references realised");

    if (nrFailed > 0) {
        debug(format("some references of path `%1%' could not be realised") % storePath);
        amDone(nrNoSubstituters > 0 || nrIncompleteClosure > 0 ? ecIncompleteClosure : ecFailed);
        return;
    }

    foreach (PathSet::iterator, i, info.references)
        if (*i != storePath) /* ignore self-references */
            assert(worker.store.isValidPath(*i));

    state = &SubstitutionGoal::tryToRun;
    worker.wakeUp(shared_from_this());
}


void SubstitutionGoal::tryToRun()
{
    trace("trying to run");

    /* Make sure that we are allowed to start a build.  Note that even
       is maxBuildJobs == 0 (no local builds allowed), we still allow
       a substituter to run.  This is because substitutions cannot be
       distributed to another machine via the build hook. */
    if (worker.getNrLocalBuilds() >= (settings.maxBuildJobs == 0 ? 1 : settings.maxBuildJobs)) {
        worker.waitForBuildSlot(shared_from_this());
        return;
    }

    /* Maybe a derivation goal has already locked this path
       (exceedingly unlikely, since it should have used a substitute
       first, but let's be defensive). */
    outputLock.reset(); // make sure this goal's lock is gone
    if (pathIsLockedByMe(storePath)) {
        debug(format("restarting substitution of `%1%' because it's locked by another goal")
            % storePath);
        worker.waitForAnyGoal(shared_from_this());
        return; /* restart in the tryToRun() state when another goal finishes */
    }

    /* Acquire a lock on the output path. */
    outputLock = std::shared_ptr<PathLocks>(new PathLocks);
    if (!outputLock->lockPaths(singleton<PathSet>(storePath), "", false)) {
        worker.waitForAWhile(shared_from_this());
        return;
    }

    /* Check again whether the path is invalid. */
    if (!repair && worker.store.isValidPath(storePath)) {
        debug(format("store path `%1%' has become valid") % storePath);
        outputLock->setDeletion(true);
        amDone(ecSuccess);
        return;
    }

    printMsg(lvlInfo, format("fetching path `%1%'...") % storePath);

    destPath = repair ? storePath + ".tmp" : storePath;

    /* Remove the (stale) output path if it exists. */
    if (pathExists(destPath))
        deletePath(destPath);

    if (!worker.substituter) {
	const Strings args = { "substitute", "--substitute" };
	const std::map<string, string> env = {
	    { "_NIX_OPTIONS",
	      settings.pack() + "deduplicate="
	      + (settings.autoOptimiseStore ? "yes" : "no")
	    }
	};
	worker.substituter = std::make_shared<Agent>(settings.guixProgram, args, env);
    }

    /* Borrow the worker's substituter.  */
    if (!substituter) substituter.swap(worker.substituter);

    /* Send the request to the substituter.  */
    writeLine(substituter->toAgent.writeSide,
	      (format("substitute %1% %2%") % storePath % destPath).str());

    set<int> fds;
    fds.insert(substituter->fromAgent.readSide);
    fds.insert(substituter->builderOut.readSide);
    worker.childStarted(shared_from_this(), substituter->pid, fds, true, true);

    state = &SubstitutionGoal::finished;

    if (settings.printBuildTrace)
	/* The second element in the message used to be the name of the
	   substituter but we're left with only one.  */
        printMsg(lvlError, format("@ substituter-started %1% substitute") % storePath);
}


void SubstitutionGoal::finished()
{
    trace("substitute finished");

    /* Remove the 'guix substitute' process from the list of children.  */
    worker.childTerminated(substituter->pid);

    /* If max-jobs > 1, the worker might have created a new 'substitute'
       process in the meantime.  If that is the case, terminate ours;
       otherwise, give it back to the worker.  */
    if (worker.substituter) {
	substituter.reset ();
    } else {
	worker.substituter.swap(substituter);
    }

    /* Check the exit status and the build result. */
    HashResult hash;
    try {
	auto statusList = tokenizeString<vector<string> >(status);

	if (statusList.empty()) {
            throw SubstError(format("fetching path `%1%' (empty status: '%2%')")
			     % storePath % status);
	} else if (statusList[0] == "hash-mismatch") {
	    if (settings.printBuildTrace) {
		auto hashType = statusList[1];
		auto expectedHash = statusList[2];
		auto actualHash = statusList[3];
		printMsg(lvlError, format("@ hash-mismatch %1% %2% %3% %4%")
			 % storePath
			 % hashType % expectedHash % actualHash);
	    }
	    throw SubstError(format("hash mismatch for substituted item `%1%'") % storePath);
	} else if (statusList[0] == "success") {
	    if (!pathExists(destPath))
		throw SubstError(format("substitute did not produce path `%1%'") % destPath);

	    std::string hashStr = statusList[1];
	    size_t n = hashStr.find(':');
	    if (n == string::npos)
		throw Error(format("bad hash from substituter: %1%") % hashStr);

	    HashType hashType = parseHashType(string(hashStr, 0, n));
	    switch (hashType) {
	    case htUnknown:
		throw Error(format("unknown hash algorithm in `%1%'") % hashStr);
	    case htSHA256:
		hash.first = parseHash16or32(hashType, string(hashStr, n + 1));
		if (!string2Int(statusList[2], hash.second))
		    throw Error(format("invalid nar size for '%1%' substitute") % storePath);
		break;
	    default:
		/* The database only stores SHA256 hashes, so compute it.  */
		hash = hashPath(htSHA256, destPath);
		break;
	    }
	}
	else
            throw SubstError(format("fetching path `%1%' (status: '%2%')")
                % storePath % status);

    } catch (SubstError & e) {

        printMsg(lvlInfo, e.msg());

        if (settings.printBuildTrace) {
            printMsg(lvlError, format("@ substituter-failed %1% %2% %3%")
                % storePath % status % e.msg());
        }

	amDone(ecFailed);
        return;
    }

    if (repair) replaceValidPath(storePath, destPath);

    /* Note: 'guix substitute' takes care of resetting timestamps and of
       deduplicating 'destPath', so no need to do it here.  */

    ValidPathInfo info2;
    info2.path = storePath;
    info2.hash = hash.first;
    info2.narSize = hash.second;
    info2.references = info.references;
    info2.deriver = info.deriver;
    worker.store.registerValidPath(info2);

    outputLock->setDeletion(true);
    outputLock.reset();

    worker.store.markContentsGood(storePath);

    printMsg(lvlChatty,
        format("substitution of path `%1%' succeeded") % storePath);

    if (settings.printBuildTrace)
        printMsg(lvlError, format("@ substituter-succeeded %1%") % storePath);

    amDone(ecSuccess);
}


void SubstitutionGoal::handleChildOutput(int fd, const string & data)
{
    if (verbosity >= settings.buildVerbosity
	&& fd == substituter->fromAgent.readSide) {
	writeToStderr(data);
	/* Don't write substitution output to a log file for now.  We
	   probably should, though. */
    }

    if (fd == substituter->builderOut.readSide) {
	/* DATA may consist of several lines.  Process them one by one.  */
	string input = data;
	while (!input.empty()) {
	    /* Process up to the first newline.  */
	    size_t end = input.find_first_of("\n");
	    string trimmed = (end != string::npos) ? input.substr(0, end) : input;

	    /* Update the goal's state accordingly.  */
	    if (status == "") {
		status = trimmed;
		worker.wakeUp(shared_from_this());
	    } else {
		printMsg(lvlError, format("unexpected substituter message '%1%'") % input);
	    }

	    input = (end != string::npos) ? input.substr(end + 1) : "";
	}
    }
}


void SubstitutionGoal::handleEOF(int fd)
{
    worker.wakeUp(shared_from_this());
}



//////////////////////////////////////////////////////////////////////


static bool working = false;


Worker::Worker(LocalStore & store)
    : store(store)
{
    /* Debugging: prevent recursive workers. */
    if (working) abort();
    working = true;
    nrLocalBuilds = 0;
    lastWokenUp = 0;
    permanentFailure = false;
    timedOut = false;
}


Worker::~Worker()
{
    working = false;

    /* Explicitly get rid of all strong pointers now.  After this all
       goals that refer to this worker should be gone.  (Otherwise we
       are in trouble, since goals may call childTerminated() etc. in
       their destructors). */
    topGoals.clear();
}


GoalPtr Worker::makeDerivationGoal(const Path & path,
    const StringSet & wantedOutputs, BuildMode buildMode)
{
    GoalPtr goal = derivationGoals[path].lock();
    if (!goal) {
        goal = GoalPtr(new DerivationGoal(path, wantedOutputs, *this, buildMode));
        derivationGoals[path] = goal;
        wakeUp(goal);
    } else
        (dynamic_cast<DerivationGoal *>(goal.get()))->addWantedOutputs(wantedOutputs);
    return goal;
}


GoalPtr Worker::makeSubstitutionGoal(const Path & path, bool repair)
{
    GoalPtr goal = substitutionGoals[path].lock();
    if (!goal) {
        goal = GoalPtr(new SubstitutionGoal(path, *this, repair));
        substitutionGoals[path] = goal;
        wakeUp(goal);
    }
    return goal;
}


static void removeGoal(GoalPtr goal, WeakGoalMap & goalMap)
{
    /* !!! inefficient */
    for (WeakGoalMap::iterator i = goalMap.begin();
         i != goalMap.end(); )
        if (i->second.lock() == goal) {
            WeakGoalMap::iterator j = i; ++j;
            goalMap.erase(i);
            i = j;
        }
        else ++i;
}


void Worker::removeGoal(GoalPtr goal)
{
    nix::removeGoal(goal, derivationGoals);
    nix::removeGoal(goal, substitutionGoals);
    if (topGoals.find(goal) != topGoals.end()) {
        topGoals.erase(goal);
        /* If a top-level goal failed, then kill all other goals
           (unless keepGoing was set). */
        if (goal->getExitCode() == Goal::ecFailed && !settings.keepGoing)
            topGoals.clear();
    }

    /* Wake up goals waiting for any goal to finish. */
    foreach (WeakGoals::iterator, i, waitingForAnyGoal) {
        GoalPtr goal = i->lock();
        if (goal) wakeUp(goal);
    }

    waitingForAnyGoal.clear();
}


void Worker::wakeUp(GoalPtr goal)
{
    goal->trace("woken up");
    addToWeakGoals(awake, goal);
}


unsigned Worker::getNrLocalBuilds()
{
    return nrLocalBuilds;
}


void Worker::childStarted(GoalPtr goal,
    pid_t pid, const set<int> & fds, bool inBuildSlot,
    bool respectTimeouts)
{
    Child child;
    child.goal = goal;
    child.fds = fds;
    child.timeStarted = child.lastOutput = time(0);
    child.inBuildSlot = inBuildSlot;
    child.respectTimeouts = respectTimeouts;
    children[pid] = child;
    if (inBuildSlot) nrLocalBuilds++;
}


void Worker::childTerminated(pid_t pid, bool wakeSleepers)
{
    assert(pid != -1); /* common mistake */

    Children::iterator i = children.find(pid);
    assert(i != children.end());

    if (i->second.inBuildSlot) {
        assert(nrLocalBuilds > 0);
        nrLocalBuilds--;
    }

    children.erase(pid);

    if (wakeSleepers) {

        /* Wake up goals waiting for a build slot. */
        foreach (WeakGoals::iterator, i, wantingToBuild) {
            GoalPtr goal = i->lock();
            if (goal) wakeUp(goal);
        }

        wantingToBuild.clear();
    }
}


void Worker::waitForBuildSlot(GoalPtr goal)
{
    debug("wait for build slot");
    if (getNrLocalBuilds() < settings.maxBuildJobs)
        wakeUp(goal); /* we can do it right away */
    else
        addToWeakGoals(wantingToBuild, goal);
}


void Worker::waitForAnyGoal(GoalPtr goal)
{
    debug("wait for any goal");
    addToWeakGoals(waitingForAnyGoal, goal);
}


void Worker::waitForAWhile(GoalPtr goal)
{
    debug("wait for a while");
    addToWeakGoals(waitingForAWhile, goal);
}


void Worker::run(const Goals & _topGoals)
{
    foreach (Goals::iterator, i,  _topGoals) topGoals.insert(*i);

    startNest(nest, lvlDebug, format("entered goal loop"));

    while (1) {

        checkInterrupt();

        /* Call every wake goal (in the ordering established by
           CompareGoalPtrs). */
        while (!awake.empty() && !topGoals.empty()) {
            Goals awake2;
            for (auto & i : awake) {
                GoalPtr goal = i.lock();
                if (goal) awake2.insert(goal);
            }
            awake.clear();
            for (auto & goal : awake2) {
                checkInterrupt();
                goal->work();
                if (topGoals.empty()) break; // stuff may have been cancelled
            }
        }

        if (topGoals.empty()) break;

        /* Wait for input. */
        if (!children.empty() || !waitingForAWhile.empty())
            waitForInput();
        else {
            if (awake.empty() && settings.maxBuildJobs == 0) throw Error(
                "unable to start any build; either increase `--max-jobs' "
                "or enable distributed builds");
            assert(!awake.empty());
        }
    }

    /* If --keep-going is not set, it's possible that the main goal
       exited while some of its subgoals were still active.  But if
       --keep-going *is* set, then they must all be finished now. */
    assert(!settings.keepGoing || awake.empty());
    assert(!settings.keepGoing || wantingToBuild.empty());
    assert(!settings.keepGoing || children.empty());
}


void Worker::waitForInput()
{
    printMsg(lvlVomit, "waiting for children");

    /* Process output from the file descriptors attached to the
       children, namely log output and output path creation commands.
       We also use this to detect child termination: if we get EOF on
       the logger pipe of a build, we assume that the builder has
       terminated. */

    bool useTimeout = false;
    struct timeval timeout;
    timeout.tv_usec = 0;
    time_t before = time(0);

    /* If we're monitoring for silence on stdout/stderr, or if there
       is a build timeout, then wait for input until the first
       deadline for any child. */
    assert(sizeof(time_t) >= sizeof(long));
    time_t nearest = LONG_MAX; // nearest deadline
    foreach (Children::iterator, i, children) {
        if (!i->second.respectTimeouts) continue;
        if (settings.maxSilentTime != 0)
            nearest = std::min(nearest, i->second.lastOutput + settings.maxSilentTime);
        if (settings.buildTimeout != 0)
            nearest = std::min(nearest, i->second.timeStarted + settings.buildTimeout);
    }
    if (nearest != LONG_MAX) {
        timeout.tv_sec = std::max((time_t) 1, nearest - before);
        useTimeout = true;
        printMsg(lvlVomit, format("sleeping %1% seconds") % timeout.tv_sec);
    }

    /* If we are polling goals that are waiting for a lock, then wake
       up after a few seconds at most. */
    if (!waitingForAWhile.empty()) {
        useTimeout = true;
        if (lastWokenUp == 0)
            printMsg(lvlError, "waiting for locks or build slots...");
        if (lastWokenUp == 0 || lastWokenUp > before) lastWokenUp = before;
        timeout.tv_sec = std::max((time_t) 1, (time_t) (lastWokenUp + settings.pollInterval - before));
    } else lastWokenUp = 0;

    using namespace std;
    /* Use select() to wait for the input side of any logger pipe to
       become `available'.  Note that `available' (i.e., non-blocking)
       includes EOF. */
    fd_set fds;
    FD_ZERO(&fds);
    int fdMax = 0;
    foreach (Children::iterator, i, children) {
        foreach (set<int>::iterator, j, i->second.fds) {
            FD_SET(*j, &fds);
            if (*j >= fdMax) fdMax = *j + 1;
        }
    }

    if (select(fdMax, &fds, 0, 0, useTimeout ? &timeout : 0) == -1) {
        if (errno == EINTR) return;
        throw SysError("waiting for input");
    }

    time_t after = time(0);

    /* Process all available file descriptors. */

    /* Since goals may be canceled from inside the loop below (causing
       them go be erased from the `children' map), we have to be
       careful that we don't keep iterators alive across calls to
       timedOut(). */
    set<pid_t> pids;
    foreach (Children::iterator, i, children) pids.insert(i->first);

    foreach (set<pid_t>::iterator, i, pids) {
        checkInterrupt();
        Children::iterator j = children.find(*i);
        if (j == children.end()) continue; // child destroyed
        GoalPtr goal = j->second.goal.lock();
        assert(goal);

        set<int> fds2(j->second.fds);
        foreach (set<int>::iterator, k, fds2) {
            if (FD_ISSET(*k, &fds)) {
                unsigned char buffer[4096];
                ssize_t rd = read(*k, buffer, sizeof(buffer));
                if (rd == -1) {
                    if (errno != EINTR)
                        throw SysError(format("reading from %1%")
                            % goal->getName());
                } else if (rd == 0) {
                    debug(format("%1%: got EOF") % goal->getName());
                    goal->handleEOF(*k);
                    j->second.fds.erase(*k);
                } else {
                    printMsg(lvlVomit, format("%1%: read %2% bytes")
                        % goal->getName() % rd);
                    string data((char *) buffer, rd);
                    j->second.lastOutput = after;
                    goal->handleChildOutput(*k, data);
                }
            }
        }

        if (goal->getExitCode() == Goal::ecBusy &&
            settings.maxSilentTime != 0 &&
            j->second.respectTimeouts &&
            after - j->second.lastOutput >= (time_t) settings.maxSilentTime)
        {
            printMsg(lvlError,
                format("%1% timed out after %2% seconds of silence")
                % goal->getName() % settings.maxSilentTime);
            goal->timedOut();
        }

        else if (goal->getExitCode() == Goal::ecBusy &&
            settings.buildTimeout != 0 &&
            j->second.respectTimeouts &&
            after - j->second.timeStarted >= (time_t) settings.buildTimeout)
        {
            printMsg(lvlError,
                format("%1% timed out after %2% seconds")
                % goal->getName() % settings.buildTimeout);
            goal->timedOut();
        }
    }

    if (!waitingForAWhile.empty() && lastWokenUp + settings.pollInterval <= after) {
        lastWokenUp = after;
        foreach (WeakGoals::iterator, i, waitingForAWhile) {
            GoalPtr goal = i->lock();
            if (goal) wakeUp(goal);
        }
        waitingForAWhile.clear();
    }
}


unsigned int Worker::exitStatus()
{
    return timedOut ? 101 : (permanentFailure ? 100 : 1);
}


//////////////////////////////////////////////////////////////////////


void LocalStore::buildPaths(const PathSet & drvPaths, BuildMode buildMode)
{
    startNest(nest, lvlDebug,
        format("building %1%") % showPaths(drvPaths));

    Worker worker(*this);

    Goals goals;
    foreach (PathSet::const_iterator, i, drvPaths) {
        DrvPathWithOutputs i2 = parseDrvPathWithOutputs(*i);
        if (isDerivation(i2.first))
            goals.insert(worker.makeDerivationGoal(i2.first, i2.second, buildMode));
        else
            goals.insert(worker.makeSubstitutionGoal(*i, buildMode));
    }

    worker.run(goals);

    PathSet failed;
    foreach (Goals::iterator, i, goals)
        if ((*i)->getExitCode() == Goal::ecFailed) {
            DerivationGoal * i2 = dynamic_cast<DerivationGoal *>(i->get());
            if (i2) failed.insert(i2->getDrvPath());
            else failed.insert(dynamic_cast<SubstitutionGoal *>(i->get())->getStorePath());
        }

    if (!failed.empty())
        throw Error(format("build of %1% failed") % showPaths(failed), worker.exitStatus());
}


void LocalStore::ensurePath(const Path & path)
{
    /* If the path is already valid, we're done. */
    if (isValidPath(path)) return;

    Worker worker(*this);
    GoalPtr goal = worker.makeSubstitutionGoal(path);
    Goals goals = singleton<Goals>(goal);

    worker.run(goals);

    if (goal->getExitCode() != Goal::ecSuccess)
        throw Error(format("path `%1%' does not exist and cannot be created") % path, worker.exitStatus());
}


void LocalStore::repairPath(const Path & path)
{
    Worker worker(*this);
    GoalPtr goal = worker.makeSubstitutionGoal(path, true);
    Goals goals = singleton<Goals>(goal);

    worker.run(goals);

    if (goal->getExitCode() != Goal::ecSuccess) {
        /* Since substituting the path didn't work, if we have a valid
           deriver, then rebuild the deriver. */
        Path deriver = queryDeriver(path);
        if (deriver != "" && isValidPath(deriver)) {
            goals.clear();
            goals.insert(worker.makeDerivationGoal(deriver, StringSet(), bmRepair));
            worker.run(goals);
        } else
            throw Error(format("cannot repair path `%1%'") % path, worker.exitStatus());
    }
}


}
