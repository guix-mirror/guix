#include "store-api.hh"
#include "globals.hh"
#include "util.hh"

#include <climits>


namespace nix {


GCOptions::GCOptions()
{
    action = gcDeleteDead;
    ignoreLiveness = false;
    maxFreed = ULLONG_MAX;
}


bool isInStore(const Path & path)
{
    return isInDir(path, settings.nixStore);
}


bool isStorePath(const Path & path)
{
    return isInStore(path)
        && path.find('/', settings.nixStore.size() + 1) == Path::npos;
}


void assertStorePath(const Path & path)
{
    if (!isStorePath(path))
        throw Error(format("path `%1%' is not in the store") % path);
}


Path toStorePath(const Path & path)
{
    if (!isInStore(path))
        throw Error(format("path `%1%' is not in the store") % path);
    Path::size_type slash = path.find('/', settings.nixStore.size() + 1);
    if (slash == Path::npos)
        return path;
    else
        return Path(path, 0, slash);
}


string storePathToName(const Path & path)
{
    assertStorePath(path);
    return string(path, settings.nixStore.size() + 34);
}


void checkStoreName(const string & name)
{
    string validChars = "+-._?=";
    /* Disallow names starting with a dot for possible security
       reasons (e.g., "." and ".."). */
    if (string(name, 0, 1) == ".")
        throw Error(format("invalid name: `%1%'") % name);
    foreach (string::const_iterator, i, name)
        if (!((*i >= 'A' && *i <= 'Z') ||
              (*i >= 'a' && *i <= 'z') ||
              (*i >= '0' && *i <= '9') ||
              validChars.find(*i) != string::npos))
        {
            throw Error(format("invalid character `%1%' in name `%2%'")
                % *i % name);
        }
}


/* Store paths have the following form:

   <store>/<h>-<name>

   where

   <store> = the location of the store, usually /gnu/store
   
   <name> = a human readable name for the path, typically obtained
     from the name attribute of the derivation, or the name of the
     source file from which the store path is created.  For derivation
     outputs other than the default "out" output, the string "-<id>"
     is suffixed to <name>.
     
   <h> = base-32 representation of the first 160 bits of a SHA-256
     hash of <s>; the hash part of the store name
     
   <s> = the string "<type>:sha256:<h2>:<store>:<name>";
     note that it includes the location of the store as well as the
     name to make sure that changes to either of those are reflected
     in the hash (e.g. you won't get /nix/store/<h>-name1 and
     /nix/store/<h>-name2 with equal hash parts).
     
   <type> = one of:
     "text:<r1>:<r2>:...<rN>"
       for plain text files written to the store using
       addTextToStore(); <r1> ... <rN> are the references of the
       path.
     "source"
       for paths copied to the store using addToStore() when recursive
       = true and hashAlgo = "sha256"
     "output:<id>"
       for either the outputs created by derivations, OR paths copied
       to the store using addToStore() with recursive != true or
       hashAlgo != "sha256" (in that case "source" is used; it's
       silly, but it's done that way for compatibility).  <id> is the
       name of the output (usually, "out").

   <h2> = base-16 representation of a SHA-256 hash of:
     if <type> = "text:...":
       the string written to the resulting store path
     if <type> = "source":
       the serialisation of the path from which this store path is
       copied, as returned by hashPath()
     if <type> = "output:out":
       for non-fixed derivation outputs:
         the derivation (see hashDerivationModulo() in
         primops.cc)
       for paths copied by addToStore() or produced by fixed-output
       derivations:
         the string "fixed:out:<rec><algo>:<hash>:", where
           <rec> = "r:" for recursive (path) hashes, or "" or flat
             (file) hashes
           <algo> = "md5", "sha1" or "sha256"
           <hash> = base-16 representation of the path or flat hash of
             the contents of the path (or expected contents of the
             path for fixed-output derivations)

   It would have been nicer to handle fixed-output derivations under
   "source", e.g. have something like "source:<rec><algo>", but we're
   stuck with this for now...

   The main reason for this way of computing names is to prevent name
   collisions (for security).  For instance, it shouldn't be feasible
   to come up with a derivation whose output path collides with the
   path for a copied source.  The former would have a <s> starting with
   "output:out:", while the latter would have a <2> starting with
   "source:".
*/


Path makeStorePath(const string & type,
    const Hash & hash, const string & name)
{
    /* e.g., "source:sha256:1abc...:/nix/store:foo.tar.gz" */
    string s = type + ":sha256:" + printHash(hash) + ":"
        + settings.nixStore + ":" + name;

    checkStoreName(name);

    return settings.nixStore + "/"
        + printHash32(compressHash(hashString(htSHA256, s), 20))
        + "-" + name;
}


Path makeOutputPath(const string & id,
    const Hash & hash, const string & name)
{
    return makeStorePath("output:" + id, hash,
        name + (id == "out" ? "" : "-" + id));
}


Path makeFixedOutputPath(bool recursive,
    HashType hashAlgo, Hash hash, string name)
{
    return hashAlgo == htSHA256 && recursive
        ? makeStorePath("source", hash, name)
        : makeStorePath("output:out", hashString(htSHA256,
                "fixed:out:" + (recursive ? (string) "r:" : "") +
                printHashType(hashAlgo) + ":" + printHash(hash) + ":"),
            name);
}


Path computeStorePathForText(const string & name, const string & s,
    const PathSet & references)
{
    Hash hash = hashString(htSHA256, s);
    /* Stuff the references (if any) into the type.  This is a bit
       hacky, but we can't put them in `s' since that would be
       ambiguous. */
    string type = "text";
    foreach (PathSet::const_iterator, i, references) {
        type += ":";
        type += *i;
    }
    return makeStorePath(type, hash, name);
}


/* Return a string accepted by decodeValidPathInfo() that
   registers the specified paths as valid.  Note: it's the
   responsibility of the caller to provide a closure. */
string StoreAPI::makeValidityRegistration(const PathSet & paths,
    bool showDerivers, bool showHash)
{
    string s = "";
    
    foreach (PathSet::iterator, i, paths) {
        s += *i + "\n";

        ValidPathInfo info = queryPathInfo(*i);

        if (showHash) {
            s += printHash(info.hash) + "\n";
            s += (format("%1%\n") % info.narSize).str();
        }

        Path deriver = showDerivers ? info.deriver : "";
        s += deriver + "\n";

        s += (format("%1%\n") % info.references.size()).str();

        foreach (PathSet::iterator, j, info.references)
            s += *j + "\n";
    }

    return s;
}

string showPaths(const PathSet & paths)
{
    string s;
    foreach (PathSet::const_iterator, i, paths) {
        if (s.size() != 0) s += ", ";
        s += "`" + *i + "'";
    }
    return s;
}


void exportPaths(StoreAPI & store, const Paths & paths,
    bool sign, Sink & sink)
{
    foreach (Paths::const_iterator, i, paths) {
        writeInt(1, sink);
        store.exportPath(*i, sign, sink);
    }
    writeInt(0, sink);
}

Path readStorePath(Source & from)
{
    Path path = readString(from);
    assertStorePath(path);
    return path;
}


template<class T> T readStorePaths(Source & from)
{
    T paths = readStrings<T>(from);
    foreach (typename T::iterator, i, paths) assertStorePath(*i);
    return paths;
}

template PathSet readStorePaths(Source & from);

}


#include "local-store.hh"
#include "serialise.hh"


namespace nix {


std::shared_ptr<StoreAPI> store;


std::shared_ptr<StoreAPI> openStore(bool reserveSpace)
{
    return std::shared_ptr<StoreAPI>(new LocalStore(reserveSpace));
}


}
