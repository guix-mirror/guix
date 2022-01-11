;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Hugo Lecomte <hugo.lecomte@inria.fr>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
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

(define-module (gnu packages jupyter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages docker))

(define-public python-jupyter-protocol
  (package
    (name "python-jupyter-protocol")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jupyter_protocol" version))
              (sha256
               (base32
                "1bk3as5yw9y5nmq6l15nr46aby34phmvsx9kxgqnm5pd5q2b5h57"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-dateutil python-jupyter-core python-pyzmq
           python-traitlets))
    (native-inputs
     (list python-ipykernel python-ipython python-mock python-pytest))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter protocol implementation")
    (description
     "This Python library is an experimental implementation of the
@uref{https://jupyter-client.readthedocs.io/en/latest/messaging.html, Jupyter
protocol} to be used by both clients and kernels.")
    (license license:bsd-3)
    (properties '((upstream-name . "jupyter_protocol")))))

(define-public python-jupyter-kernel-mgmt
  (package
    (name "python-jupyter-kernel-mgmt")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jupyter_kernel_mgmt" version))
              (sha256
               (base32
                "0i7a78dn89ca8h0a42giyxwcmk6y4wrdr7q8h2ax9vybb84c795q"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-dateutil
           python-entrypoints
           python-jupyter-core
           python-jupyter-protocol
           python-pyzmq
           python-traitlets))
    (native-inputs
     (list python-ipykernel python-ipython python-mock python-pytest))
    (home-page "https://jupyter.org")
    (synopsis "Discover, launch, and communicate with Jupyter kernels")
    (description
     "This package is an experimental refactoring of the machinery for
launching and using Jupyter kernels.")
    (license license:bsd-3)
    (properties '((upstream-name . "jupyter_kernel_mgmt")))))

(define-public python-jupyter-kernel-test
  (package
    (name "python-jupyter-kernel-test")
    (version "0.3")
    (home-page "https://github.com/jupyter/jupyter_kernel_test")
    (source (origin
              ;; PyPI has a ".whl" file but not a proper source release.
              ;; Thus, fetch code from Git.
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00iy74i4i8is6axb9vlsm0b9wxkvyyxnbl8r0i4gaj3xd788jm83"))))
    (build-system python-build-system)
    (arguments
     ;; The repo doesn't contain a "setup.py" file so install files manually.
     '(#:phases (modify-phases %standard-phases
                  (delete 'build)
                  (delete 'check)
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (version (python-version (assoc-ref inputs "python")))
                             (pydir (string-append out "/lib/python"
                                                   version "/site-packages/"
                                                   "jupyter_kernel_test")))
                        (for-each (lambda (file)
                                    (install-file file pydir))
                                  (find-files "jupyter_kernel_test"
                                              "\\.py$"))
                        #t))))))
    (propagated-inputs
     (list python-jupyter-kernel-mgmt python-jupyter-protocol
           python-jsonschema))
    (synopsis "Test Jupyter kernels")
    (description
     "@code{jupyter_kernel_test} is a tool for testing Jupyter kernels.  It
tests kernels for successful code execution and conformance with the
@uref{https://jupyter-client.readthedocs.io/en/latest/messaging.html, Jupyter
Messaging Protocol}.")
    (license license:bsd-3)))

(define-public xeus
  (package
    (name "xeus")
    (version "0.23.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/QuantStack/xeus")
                    (commit version)))
              (sha256
               (base32
                "1m1b6z1538r7mv2ggn7bdbd9570ja7cadplq64zl8rgl2c8vdi2a"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_STATIC_LIBS=OFF"
                           "-DDISABLE_ARCH_NATIVE=ON" ;no '-march=native'
                           "-DBUILD_TESTING=ON")))
    (native-inputs
     (list pkg-config
           ;; The following inputs are used by the test suite.
           googletest
           python-pytest
           python-3
           python-jupyter-kernel-test
           python-jupyter-client))
    (inputs
     (list xtl
           json-modern-cxx
           cppzmq
           zeromq
           openssl
           `(,util-linux "lib")))         ;libuuid
    (home-page "https://quantstack.net/xeus")
    (synopsis "C++ implementation of the Jupyter Kernel protocol")
    (description
     "@code{xeus} is a library meant to facilitate the implementation of
kernels for Jupyter.  It takes the burden of implementing the Jupyter Kernel
protocol so developers can focus on implementing the interpreter part of the
kernel.

Several Jupyter kernels are built upon @code{xeus}, such as @code{xeus-cling},
a kernel for the C++ programming language, and @code{xeus-python}, an
alternative Python kernel for Jupyter.")
    (license license:bsd-3)))

(define-public python-jupyterlab-pygments
  (package
    (name "python-jupyterlab-pygments")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlab_pygments" version))
       (sha256
        (base32
         "0ij14mmnc39nmf84i0av6j9glazjic7wzv1qyhr0j5966s3s1kfg"))))
    (build-system python-build-system)
    (arguments '(#:tests? #false)) ; there are no tests
    (propagated-inputs
     (list python-pygments))
    (home-page "https://jupyter.org")
    (synopsis "Pygments theme using JupyterLab CSS variables")
    (description
     "This package contains a syntax coloring theme for pygments making use of
the JupyterLab CSS variables.")
    (license license:bsd-3)))

(define-public python-jupyter-packaging
  (package
    (name "python-jupyter-packaging")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_packaging" version))
       (sha256
        (base32
         "0r015c0m713d19asmpimsw6bk2sqv2lpd2nccgjzjdj5h1crg0bg"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-deprecation python-packaging python-setuptools
           python-tomlkit python-wheel))
    (native-inputs
     (list python-pypa-build python-coverage python-pytest
           python-pytest-cov python-pytest-mock))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter packaging utilities")
    (description "This package provides tools to help build and install
Jupyter Python packages that require a pre-build step that may include
JavaScript build steps.")
    (license license:bsd-3)))

(define-public python-jupyter-server
  (package
    (name "python-jupyter-server")
    (version "1.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_server" version))
       (sha256
        (base32
         "1gvjbsw5nl94hz02rnkr4g4kkvh9fz7i45vz17hzwyvdpj7bd8yk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (let ((home (string-append (getcwd) "/guix-home")))
                 (setenv "HOME" home))
               ;; Add jupyter-server executable to PATH.
               (setenv "PATH"
                       (string-append (assoc-ref outputs "out") "/bin:"
                                      (getenv "PATH")))
               (with-directory-excursion "jupyter_server"
                 ;; The pytest fixtures are only loaded when the file is
                 ;; called conftest.py.
                 (rename-file "pytest_plugin.py" "conftest.py")
                 (invoke "pytest" "-vv"
                         ;; Fails with internal server error
                         "-k" "not test_list_formats"
                         ;; Integration tests require a server.
                         "-m" "not integration_test"))))))))
    (propagated-inputs
     (list python-anyio
           python-argon2-cffi
           python-ipython-genutils
           python-jinja2
           python-jupyter-client
           python-jupyter-core
           python-nbconvert
           python-nbformat
           python-prometheus-client
           python-pyzmq
           python-requests-unixsocket
           python-send2trash
           python-terminado
           python-tornado-6
           python-traitlets
           python-websocket-client))
    (native-inputs
     (list python-coverage
           python-ipykernel
           python-pytest
           python-pytest-console-scripts
           python-pytest-cov
           python-pytest-mock
           python-pytest-tornasync
           python-requests))
    (home-page "https://jupyter.org")
    (synopsis "Core services, APIs, and REST endpoints for Jupyter web applications")
    (description
     "This package provides the backend—i.e. core services, APIs, and REST
endpoints—to Jupyter web applications.")
    (license license:expat)))

(define-public python-jupyterlab-widgets
  (package
    (name "python-jupyterlab-widgets")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlab_widgets" version))
       (sha256
        (base32
         "0y7vhhas3qndiypcpcfnhrj9n92v2w4hdc86nn620s9h9nl2j6jw"))))
    (build-system python-build-system)
    (native-inputs
     (list python-jupyter-packaging python-setuptools))
    (home-page "https://github.com/jupyter-widgets/ipywidgets")
    (synopsis "Interactive widgets for Jupyter Notebooks")
    (description "ipywidgets, also known as jupyter-widgets or simply widgets,
are interactive HTML widgets for Jupyter notebooks and the IPython kernel.")
    (license license:bsd-3)))

(define-public python-nbclient
  (package
    (name "python-nbclient")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbclient" version))
       (sha256
        (base32
         "172q4r6mq0lg394di0pc6ipvniy14jg38wkdsj48r366609jf5yv"))))
    (build-system python-build-system)
    ;; Tests require a kernel via python-ipykernel, and also tools from
    ;; nbconvert.
    (arguments '(#:tests? #false))
    (propagated-inputs
     (list python-async-generator python-jupyter-client python-nbformat
           python-nest-asyncio python-traitlets))
    (native-inputs
     (list python-black
           python-bumpversion
           python-check-manifest
           python-codecov
           python-coverage
           python-flake8
           ;; ("python-ipykernel" ,python-ipykernel)
           ;; ("python-ipython" ,python-ipython)
           ;; ("python-ipywidgets" ,python-ipywidgets)
           python-mypy
           python-pip
           python-pytest
           python-pytest-cov
           python-setuptools
           python-testpath
           python-tox
           python-twine
           python-wheel
           python-xmltodict))
    (home-page "https://jupyter.org")
    (synopsis "Client library for executing notebooks")
    (description
     "This package provides a client library for executing notebooks. Formerly
nbconvert's @code{ExecutePreprocessor.}")
    (license license:bsd-3)))

(define-public repo2docker
  (package
    (name "repo2docker")
    (version "2021.08.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jupyterhub/repo2docker/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "111irpghzys0s5ixs8paskz7465cls1sm9d5bg45a15jklcw84a9"))))
    (outputs '("out" "doc"))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'patch-shebangs 'fix-install-miniforge
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (substitute* (find-files
                                      out "^(install-miniforge|install-nix|\
nix-shell-wrapper|repo2docker-entrypoint)")
                          (("^#!(.*)/bin/bash")
                           "#!/bin/bash"))
                        (substitute* (find-files out "^freeze\\.py$")
                          (("^#!(.*)/bin/python3")
                           "#!/bin/python3\n")))))
                  (add-after 'install 'make-doc
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "doc"))
                             (doc (string-append out "/share/doc/"
                                                 ,(package-name this-package))))
                        (setenv "PYTHONPATH"
                                (string-append (getcwd) ":"
                                               (getenv "GUIX_PYTHONPATH")))
                        ;; Don't treat warnings as errors.
                        (substitute* "docs/Makefile"
                          (("(SPHINXOPTS[[:blank:]]+= )-W" _ group)
                           group))
                        (with-directory-excursion "docs"
                          (invoke  "make" "html")
                          (copy-recursively "build/html"
                                            (string-append doc "/html")))))))))
    (inputs
     (list python-traitlets
           python-toml
           python-semver
           python-ruamel.yaml
           python-requests
           python-json-logger
           python-jinja2
           python-escapism
           python-docker))
    (native-inputs
     (list python-sphinx python-entrypoints python-recommonmark
           python-sphinxcontrib-autoprogram python-pydata-sphinx-theme))
    (home-page "https://repo2docker.readthedocs.io/en/latest/index.html#")
    (synopsis "Generate docker images from repositories")
    (description
     "repo2docker fetches a repository (from GitHub, GitLab, Zenodo, Figshare,
Dataverse installations, a Git repository or a local directory) and builds a
container image in which the code can be executed.  The image build process is
based on the configuration files found in the repository.  repo2docker can be
used to explore a repository locally by building and executing the constructed
image of the repository, or as a means of building images that are pushed to a
Docker registry.")
    (license license:bsd-3)))

(define-public python-bash-kernel
  (package
   (name "python-bash-kernel")
   (version "0.7.2")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "bash_kernel" version))
            (sha256
             (base32
              "0w0nbr3iqqsgpk83rgd0f5b02462bkyj2n0h6i9dwyc1vpnq9350"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'bash-references
          (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "bash_kernel/kernel.py"
               (("\"bash\"")
                (string-append "\"" (assoc-ref inputs "bash") "/bin/bash\""))
               (("\\['bash', ")
                (string-append "['" (assoc-ref inputs "bash") "/bin/bash', ")))
             #t))
        (add-after 'install 'install-kernelspec
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (setenv "HOME" "/tmp")
              (invoke "python" "-m" "bash_kernel.install" "--prefix" out)
              #t))))))
   (inputs
     (list bash))
   (propagated-inputs
     (list python-pexpect python-ipykernel python-jupyter-client))
   (home-page "https://github.com/takluyver/bash_kernel")
   (synopsis "Jupyter kernel for Bash")
   (description "A bash shell kernel for Jupyter.")
   (license license:expat)))

(define-public python-sparqlkernel
  (package
    (name "python-sparqlkernel")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sparqlkernel" version))
              (sha256
               (base32
                "004v22nyi5cnpxq4fiws89p7i5wcnzv45n3n70axdd6prh6rkapx"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'no-custom-css
           (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "sparqlkernel/install.py"
                (("install_custom_css\\( destd, PKGNAME \\)") ""))
              #t))
         (add-after 'install 'install-kernelspec
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "HOME" "/tmp")
               (add-installed-pythonpath inputs outputs)
               (invoke
                 (string-append out "/bin/jupyter-sparqlkernel")
                 "install"
                 (string-append "--InstallKernelSpec.prefix=" out))
               #t))))))
    (native-inputs
     (list python-traitlets python-jupyter-client python-notebook
           python-ipykernel python-html5lib-0.9))
    (propagated-inputs
     (list python-sparqlwrapper python-pygments))
    (home-page "https://github.com/paulovn/sparql-kernel")
    (synopsis "Jupyter kernel for SPARQL")
    (description "This module installs a Jupyter kernel for SPARQL.  It allows
sending queries to an SPARQL endpoint and fetching & presenting the results in
a notebook.")
    (license license:bsd-3)))

(define-public python-voila
  (package
    (name "python-voila")
    (version "0.2.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "voila" version))
       (sha256
        (base32
         "0krfc95yjlhjdmrsladhy6lpf4xs1zw49nmkyl4pkykndglvwa1m"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-jupyter-client python-jupyter-server python-nbclient
           python-nbconvert))
    (native-inputs
     (list python-ipywidgets
           python-jupyter-packaging
           python-matplotlib
           python-mock
           python-pytest
           python-pytest-tornasync
           python-setuptools
           python-tornado-6))
    (home-page "https://github.com/voila-dashboards/voila")
    (synopsis "Render live Jupyter notebooks with interactive widgets")
    (description
     "Voilà turns Jupyter notebooks into standalone web applications.  Unlike
the usual HTML-converted notebooks, each user connecting to the Voilà tornado
application gets a dedicated Jupyter kernel which can execute the callbacks to
changes in Jupyter interactive widgets.")
    (license license:bsd-3)))
