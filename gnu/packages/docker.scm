;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages docker)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages check)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages virtualization))

(define-public python-docker-py
  (package
    (name "python-docker-py")
    (version "1.10.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "docker-py" version))
       (sha256
        (base32
         "05f49f6hnl7npmi7kigg0ibqk8s3fhzx1ivvz1kqvlv4ay3paajc"))))
    (build-system python-build-system)
    ;; TODO: Tests require a running Docker daemon.
    (arguments '(#:tests? #f))
    (inputs
     `(("python-requests" ,python-requests)
       ("python-six" ,python-six)
       ("python-websocket-client" ,python-websocket-client)))
    (home-page "https://github.com/docker/docker-py/")
    (synopsis "Python client for Docker")
    (description "Docker-Py is a Python client for the Docker container
management tool.")
    (license license:asl2.0)))

(define-public python-dockerpty
  (package
    (name "python-dockerpty")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dockerpty" version))
       (sha256
        (base32
         "0za6rr349641wv76ww9l3zcic2xyxrirlxpnzl4296h897648455"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-six" ,python-six)))
    (home-page "https://github.com/d11wtq/dockerpty")
    (synopsis "Python library to use the pseudo-TTY of a Docker container")
    (description "Docker PTY provides the functionality needed to operate the
pseudo-terminal (PTY) allocated to a Docker container using the Python
client.")
    (license license:asl2.0)))

(define-public docker-compose
  (package
    (name "docker-compose")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "docker-compose" version))
       (sha256
        (base32
         "0ksg7hm2yvc977968dixxisrhcmvskzpcx3pz0v1kazrdqp7xakr"))))
    (build-system python-build-system)
    ;; TODO: Tests require running Docker daemon.
    (arguments '(#:tests? #f))
    (inputs
     `(("python-docker-py" ,python-docker-py)
       ("python-dockerpty" ,python-dockerpty)
       ("python-docopt" ,python-docopt)
       ("python-jsonschema" ,python-jsonschema)
       ("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests-2.7)
       ("python-six" ,python-six)
       ("python-texttable" ,python-texttable)
       ("python-websocket-client" ,python-websocket-client)))
    (home-page "https://www.docker.com/")
    (synopsis "Multi-container orchestration for Docker")
    (description "Docker Compose is a tool for defining and running
multi-container Docker applications.  A Compose file is used to configure an
application’s services.  Then, using a single command, the containers are
created and all the services are started as specified in the configuration.")
    (license license:asl2.0)))

(define-public python-docker-pycreds
  (package
    (name "python-docker-pycreds")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "docker-pycreds" version))
        (sha256
         (base32
          "1zxvam1q22qb0jf48553nnncnfrcp88ag4xa0qmq6vr0imn9a3lb"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-versioning
           (lambda _
             (substitute* "test-requirements.txt"
               (("3.0.2") ,(package-version python-pytest))
               (("2.3.1") ,(package-version python-pytest-cov))
               (("2.4.1") ,(package-version python-flake8)))
             #t)))))
    (native-inputs
     `(("python-flake8" ,python-flake8)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "https://github.com/shin-/dockerpy-creds")
    (synopsis
     "Python bindings for the Docker credentials store API")
    (description
     "Docker-Pycreds contains the Python bindings for the docker credentials
store API.  It allows programmers to interact with a Docker registry using
Python without keeping their credentials in a Docker configuration file.")
    (license license:asl2.0)))

(define-public containerd
  (package
    (name "containerd")
    (version "1.2.1")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/containerd/containerd.git")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "16zn6p1ky3yrgn53z8h9wza53ch91fj47wj5xgz6w4c57j30f66p"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/containerd/containerd"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'chdir
           (lambda _
             (chdir "src/github.com/containerd/containerd")
             #t))
         (add-after 'chdir 'patch-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; TODO: Patch "socat", "unpigz".
             (substitute* "./runtime/v1/linux/runtime.go"
              (("defaultRuntime[ \t]*=.*")
               (string-append "defaultRuntime = \""
                              (assoc-ref inputs "runc")
                              "/sbin/runc\"\n"))
              (("defaultShim[ \t]*=.*")
               (string-append "defaultShim = \""
                              (assoc-ref outputs "out")
                              "/bin/containerd-shim\"\n")))
            (substitute* "./vendor/github.com/containerd/go-runc/runc.go"
              (("DefaultCommand[ \t]*=.*")
               (string-append "DefaultCommand = \""
                              (assoc-ref inputs "runc")
                              "/sbin/runc\"\n")))
             #t))
         (replace 'build
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (apply invoke "make" make-flags)))
         (replace 'install
           (lambda* (#:key outputs (make-flags '()) #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (apply invoke "make" (string-append "DESTDIR=" out) "install"
                      make-flags)))))))
    (inputs
     `(("btrfs-progs" ,btrfs-progs)
       ("libseccomp" ,libseccomp)
       ("runc" ,runc)))
    (native-inputs
     `(("go" ,go)
       ("pkg-config" ,pkg-config)))
    (synopsis "Container runtime")
    (description "This package provides the container daemon for Docker.
It includes image transfer and storage, container execution and supervision,
network attachments.")
    (home-page "http://containerd.io/")
    (license license:asl2.0)))
