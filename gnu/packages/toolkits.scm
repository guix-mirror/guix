;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;;
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

(define-module (gnu packages toolkits)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages sdl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download))

(define-public imgui
  (package
    (name "imgui")
    (version "1.86")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ocornut/imgui")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02a7b05zrka20jhzag2jb4jl624i1m456bsv69jb9zgys2p9dv1n"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Contains fonts and other unneeded files.
                  (delete-file-recursively "misc")))))
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test suite
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'adjust-includes
           (lambda _
             (substitute* (find-files "." "(\\.cpp|\\.mm)$")
               (("#include <SDL")
                "#include <SDL2/SDL"))))
         (delete 'configure)
         (replace 'build
           (lambda _
             ;; Build main library.
             (invoke ,(cc-for-target) "-I" (getcwd)
                     "-g" "-O2" "-fPIC" "-shared"
                     "-lGL" "-lSDL2" "-lglfw"
                     "-o" "libimgui.so"
                     "imgui.cpp"
                     "imgui_draw.cpp"
                     ;; Include the supported backends.
                     "backends/imgui_impl_glfw.cpp"
                     "backends/imgui_impl_sdl.cpp"
                     "backends/imgui_impl_opengl2.cpp"
                     "backends/imgui_impl_opengl3.cpp")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc"))
                    (header? (cut string-suffix? ".h" <>))
                    (imgui-headers (scandir "." header?))
                    (backend-headers (find-files
                                      "backends"
                                      "(glfw|opengl|sdl|vulkan).*\\.h$")))
               (install-file "libimgui.so" (string-append out "/lib"))
               ;; Install headers.
               (for-each (lambda (f)
                           (install-file f (string-append out "/include/imgui")))
                         imgui-headers)
               (for-each (lambda (f)
                           (install-file f (string-append
                                            out "/include/imgui/backends")))
                         backend-headers)
               ;; Install examples.
               (copy-recursively
                "examples" (string-append
                            doc "/share/imgui/examples"))))))))
    (inputs (list glfw mesa sdl2))
    (home-page "https://github.com/ocornut/imgui")
    (synopsis "Graphical user interface library for C++")
    (description "@code{dear imgui} (also know as ImGui) is a graphical user
interface library for C++.  It outputs vertex buffers that can be rendered in
a 3D application.  It is fast, renderer agnostic and self-contained.")
    (license license:expat)))
