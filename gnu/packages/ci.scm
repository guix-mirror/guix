;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017, 2020, 2021 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
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

(define-module (gnu packages ci)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages uglifyjs)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu))

(define-public cuirass
  (let ((commit "9f08035f942a1e78f92e2db886d7837b0ab98b2f")
        (revision "11"))
    (package
      (name "cuirass")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.savannah.gnu.org/git/guix/guix-cuirass.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0jrp0hngbmlg5vmfr93j86lxgk2zm5d424dx0c29ldgfr8i7bwcz"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build utils)
                    (guix build gnu-build-system)
                    (ice-9 rdelim)
                    (ice-9 popen))
         #:configure-flags '("--localstatedir=/var") ;for /var/log/cuirass
         ;; XXX: HTTP tests fail on aarch64 due to Fibers errors, disable them
         ;; on that architecture for now.
         #:tests? ,(let ((s (or (%current-target-system)
                                (%current-system))))
                     (not (string-prefix? "aarch64" s)))
         #:parallel-tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-before 'bootstrap 'fix-version-gen
             (lambda _
              (patch-shebang "build-aux/git-version-gen")

              (call-with-output-file ".tarball-version"
                (lambda (port)
                  (display ,version port)))))
           (add-before 'check 'set-PATH-for-tests
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((pg (assoc-ref inputs "ephemeralpg"))
                     (path (getenv "PATH")))
                 (setenv "PATH" (string-append pg "/bin:" path)))))
           ;; Disable the remote tests that require a Guix daemon connection.
           (add-before 'check 'disable-remote-tests
             (lambda _
               (substitute* "Makefile.am"
                 (("tests/remote.scm") ""))))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; Wrap the 'cuirass' command to refer to the right modules.
               (let* ((out    (assoc-ref outputs "out"))
                      (avahi  (assoc-ref inputs "guile-avahi"))
                      (gcrypt (assoc-ref inputs "guile-gcrypt"))
                      (json   (assoc-ref inputs "guile-json"))
                      (zmq    (assoc-ref inputs "guile-simple-zmq"))
                      (squee  (assoc-ref inputs "guile-squee"))
                      (git    (assoc-ref inputs "guile-git"))
                      (bytes  (assoc-ref inputs "guile-bytestructures"))
                      (fibers (assoc-ref inputs "guile-fibers"))
                      (zlib   (assoc-ref inputs "guile-zlib"))
                      (matd   (assoc-ref inputs "guile-mastodon"))
                      (tls    (assoc-ref inputs "gnutls"))
                      (mail   (assoc-ref inputs "mailutils"))
                      (guix   (assoc-ref inputs "guix"))
                      (deps   (list avahi gcrypt json zmq squee git bytes
                                    fibers zlib matd tls mail guix))
                      (guile  (assoc-ref inputs "guile"))
                      (effective
                       (read-line
                        (open-pipe* OPEN_READ
                                    (string-append guile "/bin/guile")
                                    "-c" "(display (effective-version))")))
                      (mods
                       (string-drop-right  ;drop trailing colon
                        (string-join deps
                                     (string-append "/share/guile/site/"
                                                    effective ":")
                                     'suffix)
                        1))
                      (objs
                       (string-drop-right
                        (string-join deps
                                     (string-append "/lib/guile/" effective
                                                    "/site-ccache:")
                                     'suffix)
                        1)))
                 ;; Make sure 'cuirass' can find the relevant Guile modules.
                 (wrap-program (string-append out "/bin/cuirass")
                   `("PATH" ":" prefix (,(string-append out "/bin")))
                   `("GUILE_LOAD_PATH" ":" prefix (,mods))
                   `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,objs)))))))))
      (inputs
       (list guile-3.0-latest
             guile-avahi
             guile-fibers
             guile-gcrypt
             guile-json-4
             guile-simple-zmq
             guile-squee
             guile-git
             guile-zlib
             guile-mastodon
             gnutls
             mailutils
             ;; FIXME: this is propagated by "guile-git", but it needs to be among
             ;; the inputs to add it to GUILE_LOAD_PATH.
             guile-bytestructures
             guix))
      (native-inputs
       (list autoconf automake pkg-config texinfo ephemeralpg))
      (native-search-paths
       ;; For HTTPS access, Cuirass itself honors these variables, with the
       ;; same semantics as Git and OpenSSL (respectively).
       (list (search-path-specification
              (variable "GIT_SSL_CAINFO")
              (file-type 'regular)
              (separator #f)                      ;single entry
              (files '("etc/ssl/certs/ca-certificates.crt")))
             (search-path-specification
              (variable "SSL_CERT_DIR")
              (files '("etc/ssl/certs")))))
      (synopsis "Continuous integration system")
      (description
       "Cuirass is a continuous integration tool using GNU Guix.  It is
intended as a replacement for Hydra.")
      (home-page "https://guix.gnu.org/cuirass/")
      (license l:gpl3+))))

(define-public laminar
  (package
    (name "laminar")
    (version "1.1")
    (source
     (origin (method url-fetch)
             (uri (string-append "https://github.com/ohwgiles/laminar/archive/"
                                 version
                                 ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "1lzfmfjygmbdr2n1q49kwwffw8frz5y6iczhdz5skwmzwg0chbsf"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; TODO Can't build tests
       #:configure-flags
       (list "-DCMAKE_CXX_STANDARD=17"
             ;; "-DBUILD_TESTS=true" TODO: objcopy: js/stPskyUS: can't add
             ;; section '.note.GNU-stack': file format not recognized
             (string-append "-DLAMINAR_VERSION=" ,version))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-CMakeLists.txt
           (lambda _
             (substitute* "CMakeLists.txt"
               (("file\\(DOWNLOAD.*\n$")
                "# file download removed by Guix --")
               (("install\\(FILES etc/laminar.service DESTINATION \\$\\{SYSTEMD\\_UNITDIR\\}\\)")
                "")
               (("install\\(FILES \\$\\{CMAKE\\_CURRENT\\_BINARY\\_DIR\\}\\/laminar\\.service DESTINATION \\$\\{SYSTEMD\\_UNITDIR\\}\\)")
                "")
               (("install\\(FILES etc/laminar\\.conf DESTINATION \\/etc\\)") "")
               (("\\/usr\\/") ""))
             #t))
         (add-after 'configure 'copy-in-javascript-and-css
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (use-modules (ice-9 popen))

             (mkdir-p "../build/js")
             (for-each (lambda (name)
                         (let* ((file
                                 (assoc-ref inputs (string-append name ".js")))
                                (port
                                 (open-pipe* OPEN_READ "uglifyjs" file))
                                (destination
                                 (string-append
                                  "../build/js/" name ".min.js")))

                           (call-with-output-file destination
                             (lambda (output-port)
                               (dump-port port output-port)))

                           (let ((exit (close-pipe port)))
                             (unless (zero? exit)
                               (error "uglifyjs failed" exit)))))

                       '("vue"
                         "vue-router"
                         "Chart"))

             ;; ansi_up.js isn't minified
             (copy-file (assoc-ref inputs "ansi_up.js")
                        "../build/js/ansi_up.js")

             #t)))))
    (inputs
     (list capnproto rapidjson sqlite boost zlib))
    (native-inputs
     `(("googletest" ,googletest)
       ("uglifyjs" ,node-uglify-js)

       ("vue.js"
        ,(origin (method url-fetch)
                 (uri (string-append "https://raw.githubusercontent.com/"
                                     "vuejs/vue/v2.6.12/dist/vue.js"))
                 (sha256
                  (base32
                   "1mq2dn6yqbmzar77xf4x2bvvanf9xc9nwfq06sksl5zmr300m7qm"))))
       ("vue-router.js"
        ,(origin (method url-fetch)
                 (uri (string-append "https://raw.githubusercontent.com/"
                                     "vuejs/vue-router/v3.4.8/dist/vue-router.js"))
                 (sha256
                  (base32
                   "1hkrbgzhpnrsb4zdafslqagy1vkac6bkdj7kh49js2lhkp9z4nj5"))))
       ("ansi_up.js"
        ,(origin (method url-fetch)
                 (uri (string-append "https://raw.githubusercontent.com/"
                                     "drudru/ansi_up/v4.0.4/ansi_up.js"))
                 (sha256
                  (base32
                   "1dx8wn38ds8d01kkih26fx1yrisg3kpz61qynjr4zil03ap0hrlr"))))
       ("Chart.js"
        ,(origin (method url-fetch)
                 (uri (string-append "https://github.com/chartjs/Chart.js/"
                                     "releases/download/v2.7.2/Chart.js"))
                 (sha256
                  (base32
                   "05m3gk6hqjx92j20drnk7q075qpjraywqaf25lnglmsgsgpiqsr7"))))))
    (synopsis "Lightweight continuous integration service")
    (description
     "Laminar is a lightweight and modular continuous integration service.  It
doesn't have a configuration web UI instead uses version-controllable
configuration files and scripts.

Laminar encourages the use of existing tools such as bash and cron instead of
reinventing them.")
    (home-page "https://laminar.ohwg.net/")
    (license l:gpl3+)))
