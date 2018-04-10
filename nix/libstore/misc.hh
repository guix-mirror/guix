#pragma once

#include "derivations.hh"


namespace nix {


/* Read a derivation, after ensuring its existence through
   ensurePath(). */
Derivation derivationFromPath(StoreAPI & store, const Path & drvPath);

/* Place in `paths' the set of all store paths in the file system
   closure of `storePath'; that is, all paths than can be directly or
   indirectly reached from it.  `paths' is not cleared.  If
   `flipDirection' is true, the set of paths that can reach
   `storePath' is returned; that is, the closures under the
   `referrers' relation instead of the `references' relation is
   returned. */
void computeFSClosure(StoreAPI & store, const Path & path,
    PathSet & paths, bool flipDirection = false,
    bool includeOutputs = false, bool includeDerivers = false);

/* Return the path corresponding to the output identifier `id' in the
   given derivation. */
Path findOutput(const Derivation & drv, string id);

bool willBuildLocally(const Derivation & drv);

bool substitutesAllowed(const Derivation & drv);


}
