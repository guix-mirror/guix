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
