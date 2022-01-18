;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2021, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2017, 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018, 2019, 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Alex ter Weele <alex.ter.weele@gmail.com>
;;; Copyright © 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 Raphaël Mélotte <raphael.melotte@mind.be>
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

(define-module (gnu packages monitoring)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages image)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rrdtool)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web))

(define-public nagios
  (package
    (name "nagios")
    (version "4.4.6")
    ;; XXX: Nagios 4.2.x and later bundle a copy of AngularJS.
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/nagios/nagios-4.x/nagios-"
                    version "/nagios-" version ".tar.gz"))
              (sha256
               (base32
                "1x5hb97zbvkm73q53ydp1gwj8nnznm72q9c4rm6ny7phr995l3db"))
              (modules '((guix build utils)))
              (snippet
               ;; Ensure reproducibility.
               '(begin
                  (substitute* (find-files "cgi" "\\.c$")
                    (("__DATE__") "\"1970-01-01\"")
                    (("__TIME__") "\"00:00:00\""))
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     (list unzip))
    (inputs
     `(("zlib" ,zlib)
       ("libpng-apng" ,libpng)
       ("gd" ,gd)
       ("perl" ,perl)
       ("mailutils" ,mailutils)))
    (arguments
     '(#:configure-flags (list "--sysconfdir=/etc"

                               ;; 'include/locations.h.in' defines file
                               ;; locations, and many things go directly under
                               ;; LOCALSTATEDIR, hence the extra '/nagios'.
                               "--localstatedir=/var/nagios"

                               (string-append
                                "--with-mail="
                                (assoc-ref %build-inputs "mailutils")
                                "/bin/mail"))
       #:make-flags '("all")
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'do-not-chown-to-nagios
                    (lambda _
                      ;; Makefiles do 'install -o nagios -g nagios', which
                      ;; doesn't work for us.
                      (substitute* (find-files "." "^Makefile$")
                        (("-o nagios -g nagios")
                         ""))
                      #t))
                  (add-before 'build 'do-not-create-sysconfdir
                    (lambda _
                      ;; Don't try to create /var upon 'make install'.
                      (substitute* "Makefile"
                        (("\\$\\(INSTALL\\).*\\$\\(LOGDIR\\).*$" all)
                         (string-append "# " all))
                        (("\\$\\(INSTALL\\).*\\$\\(CHECKRESULTDIR\\).*$" all)
                         (string-append "# " all))
                        (("chmod g\\+s.*" all)
                         (string-append "# " all)))
                      #t))
                  (add-before 'build 'set-html/php-directory
                    (lambda _
                      ;; Install HTML and PHP files under 'share/nagios/html'
                      ;; instead of just 'share/'.
                      (substitute* '("html/Makefile" "Makefile")
                        (("HTMLDIR=.*$")
                         "HTMLDIR = $(datarootdir)/nagios/html\n"))
                      #t)))
       #:tests? #f))                             ;no 'check' target or similar
    (home-page "https://www.nagios.org/")
    (synopsis "Host, service, and network monitoring program")
    (description
     "Nagios is a host, service, and network monitoring program written in C.
CGI programs are included to allow you to view the current status, history,
etc. via a Web interface.  Features include:

@itemize
@item Monitoring of network services (via SMTP, POP3, HTTP, PING, etc).
@item Monitoring of host resources (processor load, disk usage, etc.).
@item A plugin interface to allow for user-developed service monitoring
  methods.
@item Ability to define network host hierarchy using \"parent\" hosts,
  allowing detection of and distinction between hosts that are down
  and those that are unreachable.
@item Notifications when problems occur and get resolved (via email,
  pager, or user-defined method).
@item Ability to define event handlers for proactive problem resolution.
@item Automatic log file rotation/archiving.
@item Optional web interface for viewing current network status,
  notification and problem history, log file, etc.
@end itemize\n")
    (license license:gpl2)))

(define-public zabbix-agentd
  (package
    (name "zabbix-agentd")
    (version "5.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://cdn.zabbix.com/zabbix/sources/stable/"
             (version-major+minor version) "/zabbix-" version ".tar.gz"))
       (sha256
        (base32 "100n1rv7r4pqagxxifzpcza5bhrr2fklzx7gndxwiyq4597p1jvn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-agent"
             "--enable-ipv6"
             (string-append "--with-iconv="
                            (assoc-ref %build-inputs "libiconv"))
             (string-append "--with-libpcre="
                            (assoc-ref %build-inputs "pcre")))))
    (inputs
     (list libiconv pcre))
    (home-page "https://www.zabbix.com/")
    (synopsis "Distributed monitoring solution (client-side agent)")
    (description "This package provides a distributed monitoring
solution (client-side agent)")
    (license license:gpl2)))

(define-public zabbix-server
  (package
    (inherit zabbix-agentd)
    (name "zabbix-server")
    (outputs '("out" "front-end" "schema"))
    (arguments
     (substitute-keyword-arguments
         `(#:phases
           (modify-phases %standard-phases
             (add-after 'install 'install-front-end
               (lambda* (#:key outputs #:allow-other-keys)
                 (let* ((php (string-append (assoc-ref outputs "front-end")
                                            "/share/zabbix/php"))
                        (front-end-conf (string-append php "/conf"))
                        (etc (string-append php "/etc")))
                   (mkdir-p php)
                   (copy-recursively "ui" php)
                   ;; Make front-end write config to ‘/etc/zabbix’ directory.
                   (rename-file front-end-conf
                                (string-append front-end-conf "-example"))
                   (symlink "/etc/zabbix" front-end-conf))
                 #t))
             (add-after 'install 'install-schema
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((database-directory
                        (string-append (assoc-ref outputs "schema")
                                       "/database")))
                   (for-each delete-file
                             (find-files "database" "Makefile\\.in|\\.am$"))
                   (mkdir-p database-directory)
                   (copy-recursively "database" database-directory))
                 #t)))
           ,@(package-arguments zabbix-agentd))
       ((#:configure-flags flags)
        `(cons* "--enable-server"
                "--with-postgresql"
                (string-append "--with-libevent="
                               (assoc-ref %build-inputs "libevent"))
                "--with-net-snmp"
                (string-append "--with-gnutls="
                               (assoc-ref %build-inputs "gnutls"))
                "--with-libcurl"
                (string-append "--with-zlib="
                               (assoc-ref %build-inputs "zlib"))
                ,flags))))
    (inputs
     (modify-inputs (package-inputs zabbix-agentd)
       (prepend curl
                libevent
                gnutls
                postgresql
                zlib
                net-snmp
                curl)))
    (synopsis "Distributed monitoring solution (server-side)")
    (description "This package provides a distributed monitoring
solution (server-side)")))

(define-public zabbix-cli
  (package
    (name "zabbix-cli")
    (version "2.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/unioslo/zabbix-cli")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wzmrn8p09ksqhhgawr179c4az7p2liqr0l4q2dra62bxliawyqz"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'use-absolute-ncurses
                    (lambda _
                      (substitute* "bin/zabbix-cli"
                        (("'clear'")
                         (string-append "'" (which "clear") "'")))))
                  (add-after 'unpack 'patch-setup.py
                    (lambda _
                      ;; Install data_files to $out/share instead of /usr/share.
                      (substitute* "setup.py"
                        (("/usr/") "")))))))
    (inputs
     `(("clear" ,ncurses)
       ("python-requests" ,python-requests)))
    (home-page "https://github.com/unioslo/zabbix-cli")
    (synopsis "Command-line interface to Zabbix")
    (description
     "@command{zabbix-cli} is a command-line client for the Zabbix
monitoring system.  It can configure and display various aspects of Zabbix
through a text-based interface.")
    (license license:gpl3+)))

(define-public python-pyzabbix
  (package
    (name "python-pyzabbix")
    (version "1.0.0")
    (home-page "https://github.com/lukecyca/pyzabbix")
    ;; No tests on PyPI, use the git checkout.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url home-page) (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "146pv8bj6pv8max1lkm07560b9zcc268c927kff6rcib47qxfnn2"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch
                    (lambda _
                      ;; Permit newer versions of httpretty.
                      (substitute* "setup.py"
                        (("httpretty<0\\.8\\.7")
                         "httpretty"))))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (invoke "nosetests")
                          (format #t "test suite not run~%")))))))
    (native-inputs
     ;; For tests.
     (list python-httpretty python-nose))
    (propagated-inputs
     (list python-requests python-semantic-version))
    (synopsis "Python interface to the Zabbix API")
    (description
     "@code{pyzabbix} is a Python module for working with the Zabbix API.")
    (license license:lgpl2.1+)))

(define-public darkstat
  (package
    (name "darkstat")
    (version "3.0.719")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://unix4lyfe.org/darkstat/darkstat-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1mzddlim6dhd7jhr4smh0n2fa511nvyjhlx76b03vx7phnar1bxf"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))          ; no tests
    (inputs
     (list libpcap zlib))
    (home-page "https://unix4lyfe.org/darkstat/")
    (synopsis "Network statistics gatherer")
    (description
     "@command{darkstat} is a packet sniffer that runs as a background process,
gathers all sorts of statistics about network usage, and serves them over
HTTP.  Features:

@itemize
@item Traffic graphs, reports per host, shows ports for each host.
@item Embedded web-server with deflate compression.
@item Asynchronous reverse DNS resolution using a child process.
@item Small.  Portable.  Single-threaded.  Efficient.
@item Supports IPv6.
@end itemize")
    (license license:gpl2)))

(define-public python-whisper
  (package
    (name "python-whisper")
    (version "1.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "whisper" version))
       (sha256
        (base32
         "1bk29w09zcpsv8hp0g0al7nwrxa07z0ycls3mbh83wfavk83aprl"))))
    (build-system python-build-system)
    (native-inputs (list python-six))
    (home-page "http://graphiteapp.org/")
    (synopsis "Fixed size round-robin style database for Graphite")
    (description "Whisper is one of three components within the Graphite
project.  Whisper is a fixed-size database, similar in design and purpose to
RRD (round-robin-database).  It provides fast, reliable storage of numeric
data over time.  Whisper allows for higher resolution (seconds per point) of
recent data to degrade into lower resolutions for long-term retention of
historical data.")
    (license license:asl2.0)))

(define-public python-carbon
  (package
    (name "python-carbon")
    (version "1.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "carbon" version))
       (sha256
        (base32
         "1wb91fipk1niciffq5xwqbh8g7rl7ghdam4m97cjbig12i5qr4cm"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Don't install to /opt
         (add-after 'unpack 'do-not-install-to-/opt
           (lambda _ (setenv "GRAPHITE_NO_PREFIX" "1") #t)))))
    (propagated-inputs
     (list python-cachetools python-txamqp python-urllib3 python-whisper))
    (home-page "http://graphiteapp.org/")
    (synopsis "Backend data caching and persistence daemon for Graphite")
    (description "Carbon is a backend data caching and persistence daemon for
Graphite.  Carbon is responsible for receiving metrics over the network,
caching them in memory for \"hot queries\" from the Graphite-Web application,
and persisting them to disk using the Whisper time-series library.")
    (license license:asl2.0)))

(define-public graphite-web
  (package
    (name "graphite-web")
    (version "1.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "graphite-web" version))
       (sha256
        (base32
         "1l5a5rry9cakqxamvlx4xq63jifmncb6815bg9vy7fg1zyd3pjxk"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f               ;XXX: not in PyPI release & requires database
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.py"
               ;; Allow newer versions of django-tagging.
               (("django-tagging==")
                "django-tagging>="))
             #t))
         ;; Don't install to /opt
         (add-after 'unpack 'do-not-install-to-/opt
           (lambda _ (setenv "GRAPHITE_NO_PREFIX" "1") #t)))))
    (propagated-inputs
     (list python-cairocffi
           python-pytz
           python-whisper
           python-django-2.2
           python-django-tagging
           python-scandir
           python-urllib3
           python-pyparsing
           python-txamqp))
    (home-page "https://graphiteapp.org/")
    (synopsis "Scalable realtime graphing system")
    (description "Graphite is a scalable real-time graphing system that does
two things: store numeric time-series data, and render graphs of this data on
demand.")
    (license license:asl2.0)))

(define-public python-prometheus-client
  (package
    (name "python-prometheus-client")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "prometheus_client" version))
       (sha256
        (base32 "1ni2yv4ixwz32nz39ckia76lvggi7m19y5f702w5qczbnfi29kbi"))))
    (build-system python-build-system)
    (arguments
     '(;; No included tests.
       #:tests? #f))
    (propagated-inputs
     (list python-twisted))
    (home-page
     "https://github.com/prometheus/client_python")
    (synopsis "Python client for the Prometheus monitoring system")
    (description
     "The @code{prometheus_client} package supports exposing metrics from
software written in Python, so that they can be scraped by a Prometheus
service.

Metrics can be exposed through a standalone web server, or through Twisted,
WSGI and the node exporter textfile collector.")
    (license license:asl2.0)))

(define-public go-github-com-prometheus-node-exporter
  (package
    (name "go-github-com-prometheus-node-exporter")
    (version "0.18.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/prometheus/node_exporter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s3sp1gj86p7npxl38hkgs6ymd3wjjmc5hydyg1b5wh0x3yvpx07"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/prometheus/node_exporter"))
    (synopsis "Prometheus exporter for hardware and OS metrics")
    (description "Prometheus exporter for metrics exposed by *NIX kernels,
written in Go with pluggable metric collectors.")
    (home-page "https://github.com/prometheus/node_exporter")
    (license license:asl2.0)))

(define-public temper-exporter
  (let ((commit "a87bbab19c05609d62d9e4c7941178700c1ef84d")
        (revision "0"))
    (package
      (name "temper-exporter")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yrro/temper-exporter")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0jk3ydi8s14q5kyl9j3gm2zrnwlb1jwjqpg5vqrgkbm9jrldrabc"))))
      (build-system python-build-system)
      (arguments
       '(#:tests? #f                    ; One test failure:
                                        ; test/test_exporter.py:33:
                                        ; AssertionError
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-setup.py
             (lambda _
               (substitute* "setup.py"
                 (("git_ref = .*\n") "git_ref = ''\n"))
               #t))
           (add-after 'install 'install-udev-rules
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "debian/prometheus-temper-exporter.udev"
                             (string-append (assoc-ref outputs "out")
                                            "/lib/udev/rules.d"))
               #t)))))
      (inputs
       (list python-prometheus-client python-pyudev))
      (native-inputs
       (list python-pytest python-pytest-mock python-pytest-runner))
      (home-page "https://github.com/yrro/temper-exporter")
      (synopsis "Prometheus exporter for PCSensor TEMPer sensor devices")
      (description
       "This package contains a Prometheus exporter for the TEMPer sensor
devices.")
      (license license:expat))))

(define-public fswatch
  (package
    (name "fswatch")
    (version "1.16.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                      (url "https://github.com/emcrisostomo/fswatch")
                      (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zsvc8arza2ypnnmv4m0qfpnldmy1zh10q6wss05ibmanslfj2ql"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake gettext-minimal libtool))
    (arguments
     (list #:configure-flags
           #~(list "--disable-static")))
    (synopsis "File system monitor")
    (description "This package provides a file system monitor.")
    (home-page "https://github.com/emcrisostomo/fswatch")
    (license license:gpl3+)))

(define-public collectd
  (package
    (name "collectd")
    (version "5.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://storage.googleapis.com/collectd-tarballs/collectd-"
                    version
                    ".tar.bz2"))
              (sha256
               (base32
                "1mh97afgq6qgmpvpr84zngh58m0sl1b4wimqgvvk376188q09bjv"))
              (patches (search-patches "collectd-5.11.0-noinstallvar.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--localstatedir=/var" "--sysconfdir=/etc")
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'autoreconf
                    (lambda _
                      ;; Required because of patched sources.
                      (invoke "autoreconf" "-vfi"))))))
    (inputs
     (list rrdtool curl libyajl))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://collectd.org/")
    (synopsis "Collect system and application performance metrics periodically")
    (description
     "collectd gathers metrics from various sources such as the operating system,
applications, log files and external devices, and stores this information or
makes it available over the network.  Those statistics can be used to monitor
systems, find performance bottlenecks (i.e., performance analysis) and predict
future system load (i.e., capacity planning).")
    ;; license:expat for the daemon in src/daemon/ and some plugins,
    ;; license:gpl2 for other plugins
    (license (list license:expat license:gpl2))))

(define-public hostscope
  (package
    (name "hostscope")
    (version "8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.maier-komor.de/hostscope/hostscope-V"
                    version ".tgz"))
              (sha256
               (base32
                "0jw6yij8va0f292g4xkf9lp9sxkzfgv67ajw49g3vq42q47ld7cv"))))
    (build-system gnu-build-system)
    (inputs (list ncurses))
    (arguments '(#:tests? #f)) ;; No included tests.
    (home-page "http://www.maier-komor.de/hostscope.html")
    (properties `((release-monitoring-url . ,home-page)))
    (synopsis
     "System monitoring tool for multiple hosts")
    (description
     "HostScope displays key system metrics of Linux hosts, such as detailed
CPU load, speed and temperature, I/O rates of network interfaces, I/O rates of
disks, and user process summary information.  All metrics are multicast on the
LAN, if wanted, and clients can switch between multiple hosts on the network.
Hostscope features a bridge to Influx DB.  So Grafana can be used to visualize
the recorded data over time.")
    (license license:gpl3+)))

(define-public fatrace
  (package
    (name "fatrace")
    (version "0.16.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/martinpitt/fatrace")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bxz6v1z0icp716jnv3knjyqp8bv6xnkz8gqd8z3g2b6yxj5xff3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; tests need root to run as root,
         ;; and there is no make target for them:
         (delete 'check))
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output))))
    (synopsis "File access events monitor")
    (description "This package provides a utility to report system wide file
access events from all running processes.  Its main purpose is to find
processes which keep waking up the disk unnecessarily and thus prevent some
power saving.")
    (home-page "https://github.com/martinpitt/fatrace")
    (license license:gpl3+)))
