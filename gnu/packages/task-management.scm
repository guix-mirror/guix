;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Tomáš Čech <sleep_walker@suse.cz>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 LibreMiami <packaging-guix@libremiami.org>
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

(define-module (gnu packages task-management)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python))

(define-public clikan
  (let ((commit "55ab29e68263c6fed2844aef96fbebacda3eba9b")
        (revision "1"))
    (package
      (name "clikan")
      (version
       (git-version "0.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kitplummer/clikan")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1nyx80z53xxlbhpb5k22jnv4jajxqhjm0gz7qb18w9pqqlrvkqd4"))))
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs outputs tests? #:allow-other-keys)
               (setenv "HOME" (getenv "TEMP"))
               (when tests?
                 (add-installed-pythonpath inputs outputs)
                 (invoke "pytest" "-vv")))))))
      (native-inputs
       `(("pytest" ,python-pytest)
         ("click" ,python-click)))
      (inputs
       `(("click" ,python-click)
         ("click-default-group" ,python-click-default-group)
         ("pyyaml" ,python-pyyaml)
         ("rich" ,python-rich)))
      (home-page "https://github.com/kitplummer/clikan")
      (synopsis "Command-line kanban utility")
      (description
       "Clikan is a super simple command-line utility for tracking tasks
following the Japanese kanban (boarding) style.")
      (license license:expat))))

(define-public t-todo-manager
  ;; Last release is more than 10 years old.  Using latest commit.
  (let ((changeset "89ad444c000b")
        (revision "97"))
    (package
      (name "t-todo-manager")
      (version (git-version "1.2.0" revision changeset))
      (source
       (origin
         (method hg-fetch)
         (uri (hg-reference
               (url "https://hg.stevelosh.com/t")
               (changeset changeset)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32 "0c8zn7l0xq65wp07h7mxnb5ww56d1443l2vkjvx5sj6wpcchfn0s"))))
      (build-system python-build-system)
      (native-inputs
       (list python-cram))
      (synopsis "Command-line todo list manager")
      (description
       "@command{t} is a command-line todo list manager for people that want
to finish tasks, not organize them.")
      (home-page "https://stevelosh.com/projects/t/")
      (license license:expat))))

(define-public taskwarrior
  (package
    (name "taskwarrior")
    (version "2.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://taskwarrior.org/download/task-" version ".tar.gz"))
       (sha256 (base32
                "0kq8n2y4srax48yp7shz7ngac0q75dnvdbr9z9f9ldyqncr61ah0"))))
    (build-system cmake-build-system)
    (inputs
     (list gnutls
           `(,util-linux "lib")))
    (arguments
     `(#:tests? #f ; No tests implemented.
       #:phases
       (modify-phases %standard-phases
         (delete 'install-license-files)))) ; Already installed by package
     (home-page "https://taskwarrior.org")
    (synopsis "Command line task manager")
    (description
     "Taskwarrior is a command-line task manager following the Getting Things
Done time management method.  It supports network synchronization, filtering
and querying data, exposing task data in multiple formats to other tools.")
    (license license:expat)))

(define-public dstask
  (package
    (name "dstask")
    (version "0.25")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/naggie/dstask")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m83zc2zqvpcbjng92jvlnk0biw4krv12wjvjas66jbbk3sjghcy"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/naggie/dstask"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               (invoke "go" "build" "-o" "dstask" "cmd/dstask/main.go")
               (invoke "go" "build" "-o" "dstask-import"
                       "cmd/dstask-import/main.go"))))
         (replace 'install
           (lambda* (#:key import-path outputs #:allow-other-keys)
             (with-directory-excursion (string-append "src/" import-path)
               (let* ((out (assoc-ref outputs "out"))
                      (bindir (string-append out "/bin"))
                      (zsh-completion (string-append
                                       out "/share/zsh/site-functions/_dstask"))
                      (bash-completion
                       (string-append
                        out "/share/bash-completion/completions/_dstask")))
                 (install-file "dstask" bindir)
                 (install-file "dstask-import" bindir)
                 (install-file ".dstask-bash-completions.sh" bash-completion)
                 (install-file ".dstask-zsh-completions.sh" zsh-completion))))))))
    (synopsis "CLI-based TODO manager with git-based sync + markdown notes per task")
    (description "dstask is a personal task tracker that uses git for
synchronization.  It offers a note command to attach a Markdown based note to
a task.")
    (home-page "https://github.com/naggie/dstask")
    (license license:expat)))

(define-public blanket
  (package
    (name "blanket")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rafaelmardojai/blanket/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00i821zqfbigxmc709322r16z75qsw4rg23yhv35gza9sl65bzkg"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:tests? #f ;the "Validate appstream file" test fails
       #:phases
       (modify-phases %standard-phases
         (add-after 'wrap 'wrap-libs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out               (assoc-ref outputs "out"))
                    (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                    (gst-plugin-path   (getenv "GST_PLUGIN_SYSTEM_PATH"))
                    (python-path       (getenv "GUIX_PYTHONPATH")))
               (wrap-program (string-append out "/bin/blanket")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                 `("GUIX_PYTHONPATH" ":" prefix (,python-path))))
             #t)))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     (list appstream-glib
           gsettings-desktop-schemas
           gst-plugins-bad
           gst-plugins-good ;for ScaleTempo plugin
           gtk+
           libhandy
           python
           python-gst
           python-pygobject))
    (home-page "https://github.com/rafaelmardojai/blanket")
    (synopsis "Ambient sound and noise player")
    (description
     "Blanket provides different ambient sounds and types of noise to listen
to with the goal of improving your focus and enhancing your productivity.
You can also use it to fall asleep in a noisy environment.")
    (license license:gpl3+)))

(define-public todoman
  (package
    (name "todoman")
    (version "4.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "todoman" version))
        (sha256
          (base32 "1j2h5cv8wnmw41fpz1ggsgi599qhk184cas9kgd92glj3m4alg6f"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* '("tests/test_cli.py" "tests/test_formatter.py")
               (("tests\\.helpers") "helpers"))))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv" "tests" "-k"
                       (string-append
                        ;; Test expects wrong output string.
                        "not test_bad_start_date "
                        ;; Unknown failure
                        "and not test_default_command_args"))))))))
    (native-inputs
      (list python-setuptools-scm
            python-pytest
            python-pytest-cov
            python-freezegun))
    (propagated-inputs
      (list python-atomicwrites
            python-click
            python-click-log
            python-dateutil
            python-humanize
            python-icalendar
            python-parsedatetime
            python-pyxdg
            python-urwid))
    (home-page "https://todoman.readthedocs.io/")
    (synopsis "CalDav-based todo manager")
    (description "Todoman is a simple, standards-based, cli todo (aka: task)
manager.  Todos are stored into icalendar files, which means you can sync
them via CalDAV using, for example, @code{vdirsyncer}.")
    (license license:isc)))

