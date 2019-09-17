/* GNU Guix --- Functional package management for GNU
   Copyright (C) 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>

   This file is part of GNU Guix.

   GNU Guix is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or (at
   your option) any later version.

   GNU Guix is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.  */

#include <builtins.hh>
#include <util.hh>
#include <globals.hh>

#include <unistd.h>
#include <cstdlib>

namespace nix {

static void builtinDownload(const Derivation &drv,
			    const std::string &drvPath,
			    const std::string &output)
{
    /* Invoke 'guix perform-download'.  */
    Strings args;
    args.push_back("perform-download");
    args.push_back(drvPath);

    /* Close all other file descriptors. */
    closeMostFDs(set<int>());

    const char *const argv[] =
      {
	  "guix", "perform-download", drvPath.c_str(), output.c_str(), NULL
      };

    /* Tell the script what the store file name is, so that
       'strip-store-file-name' (used for instance to determine the URL of
       content-addressed mirrors) works correctly.  */
    setenv("NIX_STORE", settings.nixStore.c_str(), 1);

    /* Tell it about options such as "print-extended-build-trace".  */
    setenv("_NIX_OPTIONS", settings.pack().c_str(), 1);

    const string program = settings.guixProgram;
    execv(program.c_str(), (char *const *) argv);

    throw SysError(format("failed to run download program '%1%'") % program);
}

static const std::map<std::string, derivationBuilder> builtins =
{
    { "download", builtinDownload }
};

derivationBuilder lookupBuiltinBuilder(const std::string & name)
{
    if (name.substr(0, 8) == "builtin:")
    {
	auto realName = name.substr(8);
	auto builder = builtins.find(realName);
	return builder == builtins.end() ? NULL : builder->second;
    }
    else
	return NULL;
}

std::list<std::string> builtinBuilderNames()
{
    std::list<std::string> result;
    for(auto&& iter: builtins)
    {
	result.push_back(iter.first);
    }
    return result;
}

}
