;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.
(define-module (gnu packages minetest)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system minetest)
  #:use-module ((guix licenses) #:prefix license:))

(define-public (minetest-topic topic-id)
  "Return an URL (as a string) pointing to the forum topic with
numeric identifier TOPIC-ID on the official Minetest forums."
  (string-append "https://forum.minetest.net/viewtopic.php?t="
                 (number->string topic-id)))

(define-public minetest-basic-materials
  (package
    (name "minetest-basic-materials")
    ;; Upstream uses dates as version numbers.
    (version "2021-01-30")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/VanessaE/basic_materials.git")
             (commit "e72665b2ed98d7be115779a32d35e6d9ffa231bd")))
       (sha256
        (base32 "0v6l3lrjgshy4sccjhfhmfxc3gk0cdy73qb02i9wd2vw506v5asx"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (home-page (minetest-topic 21000))
    (synopsis "Some \"basic\" materials and items for other Minetest mods to use")
    (description
     "The Minetest mod \"basic_materials\" provides a small selection of
\"basic\" materials and items that other mods should use when possible -- things
like steel bars and chains, wire, plastic strips and sheets, and more.")
    (license
     (list license:cc-by-sa4.0 license:lgpl3))
    (properties `((upstream-name . "VanessaE/basic_materials")))))

(define-public minetest-mesecons
  ;; The release on ContentDB does not have its own version number.
  (let ((commit "db5879706d04d3480bc4863ce0c03fa73e5f10c7")
        (revision "0"))
  (package
    (name "minetest-mesecons")
    (version (git-version "1.2.1" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minetest-mods/mesecons")
             (commit commit)))
       (sha256
        (base32 "04m9s9l3frw1lgki41hgvjsw2zkrvfv0sy750b6j12arzb3lv645"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (home-page "https://mesecons.net")
    (synopsis
     "Digital circuitry for Minetest, including wires, buttons and lights")
    (description
     "Mesecons is a mod for Minetest implementing various items related
to digital circuitry, such as wires, buttons, lights and programmable
controllers.  Among other things, there are also pistons, solar panels,
pressure plates and note blocks.

Mesecons has a similar goal to Redstone in Minecraft, but works in its own way,
with different rules and mechanics.")
    ;; LGPL for code, CC-BY-SA for textures.
    ;; The README.md and COPYING.txt disagree about the "+" in license:lgpl3+.
    ;; For now, assume README.md is correct.  Upstream has been asked to
    ;; correct the inconsistency:
    ;; <https://github.com/minetest-mods/mesecons/issues/575>.
    (license (list license:lgpl3+ license:cc-by-sa3.0))
    (properties `((upstream-name . "Jeija/mesecons"))))))

(define-public minetest-pipeworks
  (package
    (name "minetest-pipeworks")
    ;; Upstream uses dates as version numbers.
    (version "2021-04-14-1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/VanessaE/pipeworks")
             (commit "db6d1bd9c109e1e543b97cc3fa8a11400da23bcd")))
       (sha256
        (base32 "1flhcnf17dn1v86kcg47a1n4cb0lybd11ncxrkxn3wmf10ibsrm0"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (propagated-inputs
     `(("minetest-basic-materials" ,minetest-basic-materials)))
    (home-page (minetest-topic 2155))
    (synopsis "Pipes, item-transport tubes and related devices for Minetest")
    (description
     "Pipeworks is a mod for Minetest implementing 3D pipes and tubes for
transporting liquids and items and some related devices.  Pipes and tubes can
go horizontally or vertically.  Item tubes can also be used for sorting items
and extracting items from chests or putting items in chests.  Autocrafters can
automatically follow craft recipes to make new items and can be fed by item
tubes.  Deployers can place items in the world as a player would.  Node
breakers simulate a player punching a node.")
    ;; CC-BY-SA for textures, LGPL for code
    (license (list license:cc-by-sa4.0 license:lgpl3))
    (properties `((upstream-name . "VanessaE/pipeworks")))))

(define-public minetest-unifieddyes
  (package
    (name "minetest-unifieddyes")
    ;; Upstream uses dates as version numbers.
    (version "2021-04-20-1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/VanessaE/unifieddyes")
             (commit "ff3b2d30fa0df5c7181fdd401b989de6271c3bb3")))
       (sha256
        (base32
         "0rba9n192xcpmxwnq7ixb6mn32gkpic247j3w4mwinrqcyscacsv"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (propagated-inputs
     `(("minetest-basic-materials" ,minetest-basic-materials)))
    (home-page (minetest-topic 2178))
    (synopsis
     "Unified Dyes expands the standard dye set of Minetest to up to 256 colours")
    (description "The purpose of this mod originally was to supply a complete
set of colours for Minetest mod authors to use for colourised nodes or
reference in recipes.  Since the advent of the default dyes mod in the standard
Minetest game, this mod has become an extension of the default mod an a library
for general colour handling.")
    (license license:gpl2+)
    (properties `((upstream-name . "VanessaE/unifieddyes")))))
