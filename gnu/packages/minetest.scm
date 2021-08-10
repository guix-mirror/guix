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

(define-public minetest-coloredwood
  (package
    (name "minetest-coloredwood")
    ;; Upstream uses dates as version numbers.
    (version "2021-04-14-1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/VanessaE/coloredwood")
             (commit "be4df6fc889419155bed8638bbb752493e78cbd5")))
       (sha256
        (base32 "1swirfk6b4xfbiwv8adyw5yl2lyfpp8ymfipzq9ivyvmif8nl3ki"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (propagated-inputs
     `(("minetest-unifieddyes" ,minetest-unifieddyes)))
    (home-page (minetest-topic 2411))
    (synopsis "Painted wood in Minetest")
    (description
     "This Minetest mod provides hundreds of colours of wood and fences to
Minetest, using Unified Dyes.  If the \"moreblocks\" mod is active,
coloured and cut wood shapes are provided as well.")
    (license
     ;; LGPL for code, CC-BY-SA for textures
     (list license:cc-by-sa4.0 license:lgpl3))
    (properties `((upstream-name . "VanessaE/coloredwood")))))

(define-public minetest-ethereal
  ;; ContentDB release 2021-07-28 is slightly ahead of the
  ;; initial version 1.29 -- i.e., some released changes have been
  ;; made to version 1.29 without a corresponding version bump.
  (let ((commit "7670c1da9274901f57f6682384af2b3bae005a86")
        (revision "0"))
    (package
      (name "minetest-ethereal")
      (version (git-version "1.29" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://notabug.org/TenPlus1/ethereal")
               (commit commit)))
         (sha256
          (base32 "1hal8bq4fydsip7s8rqz4vlaaqy9rhzxmryd0j2qnqm9286yjgkk"))
         (file-name (git-file-name name version))))
      (build-system minetest-mod-build-system)
      (home-page (minetest-topic 14638))
      (synopsis "The Ethereal mod adds many new biomes to Minetest")
      (description
       "The Ethereal Minetest mod uses the v7 map generator to add many new
biomes to the world.  It adds new trees, plants, food items, tweaks and some
special items, intending to make an interesting adventure.")
      ;; CC0: some textures
      (license (list license:cc0 license:expat))
      (properties `((upstream-name . "TenPlus1/ethereal"))))))

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

(define-public minetest-technic
  (package
    (name "minetest-technic")
    ;; Upstream doesn't keep version numbers, so use the release
    ;; date on ContentDB instead.
    (version "2021-04-15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minetest-mods/technic")
             (commit "1c219487d3f4dd03c01ff9aa1f298c7c18c7e189")))
       (sha256
        (base32 "1k9hdgzp7jnhsk6rgrlrv1lr5xrmh8ln4wv6r25v6f0fwbyj57sf"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (propagated-inputs
     `(("minetest-pipeworks" ,minetest-pipeworks)
       ("minetest-basic-materials" ,minetest-basic-materials)))
    (home-page (minetest-topic 2538))
    (synopsis "Machinery and automation for Minetest")
    (description
     "This Minetest mod adds machinery and automation to Minetest.
It adds various ores that can be processed for constructing various
machinery, such as power generators, force field emitters, quarries
and a workshop for repairing tools.  Most machines are electrically
powered.")
    ;; CC BY-SA 3.0: some texture
    ;; WTFPL: some textures
    ;; CC BY-SA3.0: some textures
    ;; CC BY-SA4.0: some sounds
    (license (list license:lgpl2.1+ license:cc-by-sa3.0 license:cc-by-sa4.0
                   license:wtfpl2))
    (properties `((upstream-name . "RealBadAngel/technic")))))

(define-public minetest-throwing
  ;; The latest release on ContentDB is ahead of the latet
  ;; tagged commit.
  (let ((commit "31f0cf5f868673dc82f24ddc432b45c9cd282d27")
        (revision "0"))
    (package
      (name "minetest-throwing")
      (version (git-version "1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/minetest-mods/throwing")
               (commit commit)))
         (sha256
          (base32 "1s5kkr6rxxv2dhbbjzv62gw1s617hnpjavw1v9fv11v3mgigdfjb"))
         (file-name (git-file-name name version))))
      (build-system minetest-mod-build-system)
      (home-page (minetest-topic 16365))
      (synopsis "API for throwing things in Minetest")
      (description
       "This Minetest mod provides an API for registering throwable things and
throwing things like arrows.  However, this mod does not provide an actual
arrow and bow, but @code{minetest-throwing-arrows} does.")
      (license license:mpl2.0)
      (properties `((upstream-name . "Palige/throwing"))))))

(define-public minetest-throwing-arrows
  ;; There is only one tagged commit (version 1.1),
  ;; there are no releases on ContentDB and the latest
  ;; commit has a compatibility fix for Minetest 5.4.0-dev.
  (let ((commit "059cc897af0aebfbd2c54ac5588f2b842f44f159")
        (revision "0"))
    (package
      (name "minetest-throwing-arrows")
      (version (git-version "1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/minetest-mods/throwing_arrows")
               (commit commit)))
         (sha256
          (base32 "0m2pmccpfxn878zd00pmrpga2h6gknz4f3qprck0fq94mksmwqs3"))
         (file-name (git-file-name name version))))
      (build-system minetest-mod-build-system)
      (propagated-inputs
       `(("minetest-throwing" ,minetest-throwing)))
      (home-page (minetest-topic 16365))
      (synopsis "Arrows and bows for Minetest")
      (description
       ;; TRANSLATORS: "throwing" is the name of a Minetest mod and should
       ;; not be translated.
       "This mod adds arrows and bows to Minetest.  It is a compatible
replacement for the throwing mod by PilzAdam that uses the throwing API.")
      (license license:mpl2.0))))

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
        (base32 "0rba9n192xcpmxwnq7ixb6mn32gkpic247j3w4mwinrqcyscacsv"))
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

(define-public minetest-unified-inventory
  (package
    (name "minetest-unified-inventory")
    ;; Upstream doesn't keep version numbers, so use the release title
    ;; on ContentDB instead.
    (version "2021-03-25-1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minetest-mods/unified_inventory")
             (commit "c044f5e3b08f0c68ab028d757b2fa63d9a1b0370")))
       (sha256
        (base32 "198g945gzbfl0kps46gwjw0c601l3b3wvn4c7dw8manskri1jr4g"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (home-page (minetest-topic 12767))
    (synopsis "Replace the default inventory in Minetest and add a crafting guide")
    (description
     "The Unified Inventory Minetest mod relaces the default survival an
creative inventory.  It includes a node, item and tool browser, a crafting
guide, a trash and refill slot for creative mode, bags and waypoints for keeping
track of important locations.")
    ;; CC-BY: some textures and icons
    ;; CC-BY-SA: some textures and icons
    ;; LGLPL2.1+: code and some textures
    ;; GPL2+: some textures
    ;; GPL3: bags.lua
    ;; GFDL: some icons
    ;; public domain, CC0: some icons
    (license (list license:gpl3 license:gpl2+ license:lgpl2.1+ license:cc-by3.0
                   license:cc-by4.0 license:cc-by-sa3.0 license:public-domain
                   license:cc0 license:fdl1.2+))
    (properties `((upstream-name . "RealBadAngel/unified_inventory")))))
