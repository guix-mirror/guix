;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2019 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu services cgit)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (repository-cgit-configuration
            cgit-configuration
            %cgit-configuration-nginx
            cgit-configuration-nginx-config
            opaque-cgit-configuration
            cgit-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the Cgit a web frontend for
;;; Git repositories written in C.
;;;
;;; Note: fields of <cgit-configuration> and <repository-cgit-configuration>
;;; should be specified in the specific order.
;;;
;;; Code:

(define %cgit-configuration-nginx
  (nginx-server-configuration
   (root cgit)
   (locations
    (list
     (nginx-location-configuration
      (uri "@cgit")
      (body '("fastcgi_param SCRIPT_FILENAME $document_root/lib/cgit/cgit.cgi;"
              "fastcgi_param PATH_INFO $uri;"
              "fastcgi_param QUERY_STRING $args;"
              "fastcgi_param HTTP_HOST $server_name;"
              "fastcgi_pass 127.0.0.1:9000;")))))
   (try-files (list "$uri" "@cgit"))
   (listen '("80"))
   (ssl-certificate #f)
   (ssl-certificate-key #f)))


;;;
;;; Serialize <cgit-configuration>
;;;

(define (uglify-field-name field-name)
  (string-delete #\? (symbol->string field-name)))

(define (serialize-field field-name val)
  #~(format #f "~a=~a\n" #$(uglify-field-name field-name) #$val))

(define (serialize-string field-name val)
  (if (and (string? val) (string=? val ""))
      ""
      (serialize-field field-name val)))

(define (serialize-list field-name val)
  (if (null? val) "" (serialize-field field-name (string-join val))))

(define robots-list? list?)

(define (serialize-robots-list field-name val)
  (if (null? val) "" (serialize-field field-name (string-join val ", "))))

(define (integer? val)
  (exact-integer? val))

(define (serialize-integer field-name val)
  (serialize-field field-name (number->string val)))

(define (serialize-boolean field-name val)
  (serialize-integer field-name (if val 1 0)))

(define (serialize-repository-cgit-configuration x)
  (serialize-configuration x repository-cgit-configuration-fields))

(define (repository-cgit-configuration-list? val)
  (list? val))

(define (serialize-repository-cgit-configuration-list field-name val)
  #~(string-append
     #$@(map serialize-repository-cgit-configuration val)))

(define (file-object? val)
  (or (file-like? val) (string? val)))
(define (serialize-file-object field-name val)
  (serialize-string field-name val))

(define (project-list? val)
  (or (list? val)
      (file-object? val)))


;;;
;;; Serialize <nginx-server-configuration>
;;;

(define (nginx-server-configuration-list? val)
  (and (list? val) (and-map nginx-server-configuration? val)))

(define (serialize-nginx-server-configuration-list field-name val)
  "")


;;;
;;; Serialize <repository-cgit-configuration>
;;;

(define (serialize-repo-field field-name val)
  #~(format #f "repo.~a=~a\n" #$(uglify-field-name field-name) #$val))

(define (serialize-repo-list field-name val)
  (if (null? val) "" (serialize-repo-field field-name (string-join val))))

(define repo-boolean? boolean?)

(define (serialize-repo-integer field-name val)
  (serialize-repo-field field-name (number->string val)))

(define (serialize-repo-boolean field-name val)
  (serialize-repo-integer field-name (if val 1 0)))
(define-maybe repo-boolean)

(define repo-list? list?)

(define repo-string? string?)

(define (serialize-repo-string field-name val)
  (if (string=? val "") "" (serialize-repo-field field-name val)))

(define repo-file-object? file-object?)
(define serialize-repo-file-object serialize-repo-string)

(define module-link-path? list?)

(define (serialize-module-link-path field-name val)
  (if (null? val) ""
      (match val
        ((path text)
         (format #f "repo.module-link.~a=~a\n" path text)))))

(define (serialize-project-list _ val)
  (if (null? val) ""
      (serialize-field
       'project-list
       (if (file-object? val)
           val
           (plain-file "project-list" (string-join val "\n"))))))

(define (serialize-extra-options extra-options)
  (string-join extra-options "\n" 'suffix))

(define repository-directory? string?)

(define (serialize-repository-directory _ val)
  (if (string=? val "") "" (format #f "scan-path=~a\n" val)))

(define mimetype-alist? list?)

(define (serialize-mimetype-alist field-name val)
  (format #f "# Mimetypes\n~a"
          (string-join
           (map (match-lambda
                  ((extension mimetype)
                   (format #f "mimetype.~a=~a"
                           (symbol->string extension) mimetype)))
                val) "\n")))

(define-configuration repository-cgit-configuration
  (snapshots
   (repo-list '())
   "A mask of snapshot formats for this repo that cgit generates links for,
restricted by the global @code{snapshots} setting.")
  (source-filter
   (repo-file-object "")
   "Override the default @code{source-filter}.")
  (url
   (repo-string "")
   "The relative URL used to access the repository.")
  (about-filter
   (repo-file-object "")
   "Override the default @code{about-filter}.")
  (branch-sort
   (repo-string "")
   "Flag which, when set to @samp{age}, enables date ordering in the branch
ref list, and when set to @samp{name} enables ordering by branch name.")
  (clone-url
   (repo-list '())
   "A list of URLs which can be used to clone repo.")
  (commit-filter
   (repo-file-object "")
   "Override the default @code{commit-filter}.")
  (commit-sort
   (repo-string "")
   "Flag which, when set to @samp{date}, enables strict date ordering in the
commit log, and when set to @samp{topo} enables strict topological ordering.")
  (defbranch
   (repo-string "")
   "The name of the default branch for this repository.  If no such branch
exists in the repository, the first branch name (when sorted) is used as
default instead.  By default branch pointed to by HEAD, or \"master\" if there
is no suitable HEAD.")
  (desc
   (repo-string "")
   "The value to show as repository description.")
  (homepage
   (repo-string "")
   "The value to show as repository homepage.")
  (email-filter
   (repo-file-object "")
   "Override the default @code{email-filter}.")
  (enable-commit-graph?
   (maybe-repo-boolean 'disabled)
   "A flag which can be used to disable the global setting
@code{enable-commit-graph?}.")
  (enable-log-filecount?
   (maybe-repo-boolean 'disabled)
   "A flag which can be used to disable the global setting
@code{enable-log-filecount?}.")
  (enable-log-linecount?
   (maybe-repo-boolean 'disabled)
   "A flag which can be used to disable the global setting
@code{enable-log-linecount?}.")
  (enable-remote-branches?
   (maybe-repo-boolean 'disabled)
   "Flag which, when set to @code{#t}, will make cgit display remote
branches in the summary and refs views.")
  (enable-subject-links?
   (maybe-repo-boolean 'disabled)
   "A flag which can be used to override the global setting
@code{enable-subject-links?}.")
  (enable-html-serving?
   (maybe-repo-boolean 'disabled)
   "A flag which can be used to override the global setting
@code{enable-html-serving?}.")
  (hide?
   (repo-boolean #f)
   "Flag which, when set to @code{#t}, hides the repository from the
repository index.")
  (ignore?
   (repo-boolean #f)
   "Flag which, when set to @samp{#t}, ignores the repository.")
  (logo
   (repo-file-object "")
   "URL which specifies the source of an image which will be used as a
logo on this repo’s pages.")
  (logo-link
   (repo-string "")
   "URL loaded when clicking on the cgit logo image.")
  (owner-filter
   (repo-file-object "")
   "Override the default @code{owner-filter}.")
  (module-link
   (repo-string "")
   "Text which will be used as the formatstring for a hyperlink when a
submodule is printed in a directory listing.  The arguments for the
formatstring are the path and SHA1 of the submodule commit.")
  (module-link-path
   (module-link-path '())
   "Text which will be used as the formatstring for a hyperlink when a
submodule with the specified subdirectory path is printed in a directory
listing.")
  (max-stats
   (repo-string "")
   "Override the default maximum statistics period.")
  (name
   (repo-string "")
   "The value to show as repository name.")
  (owner
   (repo-string "")
   "A value used to identify the owner of the repository.")
  (path
   (repo-string "")
   "An absolute path to the repository directory.")
  (readme
   (repo-string "")
   "A path (relative to repo) which specifies a file to include verbatim
as the \"About\" page for this repo.")
  (section
   (repo-string "")
   "The name of the current repository section - all repositories defined
after this option will inherit the current section name.")
  (extra-options
   (repo-list '())
   "Extra options will be appended to cgitrc file."))

;; Generate a <cgit-configuration> record, which may include a list of
;; <repository-cgit-configuration>, <nginx-server-configuration>, <package>.
(define-configuration cgit-configuration
  (package
   (package cgit)
   "The CGIT package.")
  (nginx
   (nginx-server-configuration-list (list %cgit-configuration-nginx))
   "NGINX configuration.")
  (about-filter
   (file-object "")
   "Specifies a command which will be invoked to format the content of about
pages (both top-level and for each repository).")
  (agefile
   (string "")
   "Specifies a path, relative to each repository path, which can be used to
specify the date and time of the youngest commit in the repository.")
  (auth-filter
   (file-object "")
   "Specifies a command that will be invoked for authenticating repository
access.")
  (branch-sort
   (string "name")
   "Flag which, when set to @samp{age}, enables date ordering in the branch
ref list, and when set @samp{name} enables ordering by branch name.")
  (cache-root
   (string "/var/cache/cgit")
   "Path used to store the cgit cache entries.")
  (cache-static-ttl
   (integer -1)
   "Number which specifies the time-to-live, in minutes, for the cached
version of repository pages accessed with a fixed SHA1.")
  (cache-dynamic-ttl
   (integer 5)
   "Number which specifies the time-to-live, in minutes, for the cached
version of repository pages accessed without a fixed SHA1.")
  (cache-repo-ttl
   (integer 5)
   "Number which specifies the time-to-live, in minutes, for the cached
version of the repository summary page.")
  (cache-root-ttl
   (integer 5)
   "Number which specifies the time-to-live, in minutes, for the cached
version of the repository index page.")
  (cache-scanrc-ttl
   (integer 15)
   "Number which specifies the time-to-live, in minutes, for the result of
scanning a path for Git repositories.")
  (cache-about-ttl
   (integer 15)
   "Number which specifies the time-to-live, in minutes, for the cached
version of the repository about page.")
  (cache-snapshot-ttl
   (integer 5)
   "Number which specifies the time-to-live, in minutes, for the cached
version of snapshots.")
  (cache-size
   (integer 0)
   "The maximum number of entries in the cgit cache. When set to
@samp{0}, caching is disabled.")
  (case-sensitive-sort?
   (boolean #t)
   "Sort items in the repo list case sensitively.")
  (clone-prefix
   (list '())
   "List of common prefixes which, when combined with a repository URL,
generates valid clone URLs for the repository.")
  (clone-url
   (list '())
   "List of @code{clone-url} templates.")
  (commit-filter
   (file-object "")
   "Command which will be invoked to format commit messages.")
  (commit-sort
   (string "git log")
   "Flag which, when set to @samp{date}, enables strict date ordering in the
commit log, and when set to @samp{topo} enables strict topological
ordering.")
  (css
   (file-object "/share/cgit/cgit.css")
   "URL which specifies the css document to include in all cgit pages.")
  (email-filter
   (file-object "")
   "Specifies a command which will be invoked to format names and email
address of committers, authors, and taggers, as represented in various
places throughout the cgit interface.")
  (embedded?
   (boolean #f)
   "Flag which, when set to @samp{#t}, will make cgit generate a HTML
fragment suitable for embedding in other HTML pages.")
  (enable-commit-graph?
   (boolean #f)
   "Flag which, when set to @samp{#t}, will make cgit print an ASCII-art
commit history graph to the left of the commit messages in the
repository log page.")
  (enable-filter-overrides?
   (boolean #f)
   "Flag which, when set to @samp{#t}, allows all filter settings to be
overridden in repository-specific cgitrc files.")
  (enable-follow-links?
   (boolean #f)
   "Flag which, when set to @samp{#t}, allows users to follow a file in the
log view.")
  (enable-http-clone?
   (boolean #t)
   "If set to @samp{#t}, cgit will act as an dumb HTTP endpoint for Git
clones.")
  (enable-index-links?
   (boolean #f)
   "Flag which, when set to @samp{#t}, will make cgit generate extra links
\"summary\", \"commit\", \"tree\" for each repo in the repository index.")
  (enable-index-owner?
   (boolean #t)
   "Flag which, when set to @samp{#t}, will make cgit display the owner of
each repo in the repository index.")
  (enable-log-filecount?
   (boolean #f)
   "Flag which, when set to @samp{#t}, will make cgit print the number of
modified files for each commit on the repository log page.")
  (enable-log-linecount?
   (boolean #f)
   "Flag which, when set to @samp{#t}, will make cgit print the number of
added and removed lines for each commit on the repository log page.")
  (enable-remote-branches?
   (boolean #f)
   "Flag which, when set to @code{#t}, will make cgit display remote
branches in the summary and refs views.")
  (enable-subject-links?
   (boolean #f)
   "Flag which, when set to @code{1}, will make cgit use the subject of
the parent commit as link text when generating links to parent commits
in commit view.")
  (enable-html-serving?
   (boolean #f)
   "Flag which, when set to @samp{#t}, will make cgit use the subject of the
parent commit as link text when generating links to parent commits in
commit view.")
  (enable-tree-linenumbers?
   (boolean #t)
   "Flag which, when set to @samp{#t}, will make cgit generate linenumber
links for plaintext blobs printed in the tree view.")
  (enable-git-config?
   (boolean #f)
   "Flag which, when set to @samp{#f}, will allow cgit to use Git config to
set any repo specific settings.")
  (favicon
   (file-object "/favicon.ico")
   "URL used as link to a shortcut icon for cgit.")
  (footer
   (string "")
   "The content of the file specified with this option will be included
verbatim at the bottom of all pages (i.e. it replaces the standard
\"generated by...\" message).")
  (head-include
   (string "")
   "The content of the file specified with this option will be included
verbatim in the HTML HEAD section on all pages.")
  (header
   (string "")
   "The content of the file specified with this option will be included
verbatim at the top of all pages.")
  (include
   (file-object "")
   "Name of a configfile to include before the rest of the current config-
file is parsed.")
  (index-header
   (string "")
   "The content of the file specified with this option will be included
verbatim above the repository index.")
  (index-info
   (string "")
   "The content of the file specified with this option will be included
verbatim below the heading on the repository index page.")
  (local-time?
   (boolean #f)
   "Flag which, if set to @samp{#t}, makes cgit print commit and tag times
in the servers timezone.")
  (logo
   (file-object "/share/cgit/cgit.png")
   "URL which specifies the source of an image which will be used as a logo
on all cgit pages.")
  (logo-link
   (string "")
   "URL loaded when clicking on the cgit logo image.")
  (owner-filter
   (file-object "")
   "Command which will be invoked to format the Owner column of the main
page.")
  (max-atom-items
   (integer 10)
   "Number of items to display in atom feeds view.")
  (max-commit-count
   (integer 50)
   "Number of entries to list per page in \"log\" view.")
  (max-message-length
   (integer 80)
   "Number of commit message characters to display in \"log\" view.")
  (max-repo-count
   (integer 50)
   "Specifies the number of entries to list per page on the repository index
page.")
  (max-repodesc-length
   (integer 80)
   "Specifies the maximum number of repo description characters to display
on the repository index page.")
  (max-blob-size
   (integer 0)
   "Specifies the maximum size of a blob to display HTML for in KBytes.")
  (max-stats
   (string "")
   "Maximum statistics period.  Valid values are @samp{week},@samp{month},
@samp{quarter} and @samp{year}.")
  (mimetype
   (mimetype-alist '((gif "image/gif")
                     (html "text/html")
                     (jpg "image/jpeg")
                     (jpeg "image/jpeg")
                     (pdf "application/pdf")
                     (png "image/png")
                     (svg "image/svg+xml")))
   "Mimetype for the specified filename extension.")
  (mimetype-file
   (file-object "")
   "Specifies the file to use for automatic mimetype lookup.")
  (module-link
   (string "")
   "Text which will be used as the formatstring for a hyperlink when a
submodule is printed in a directory listing.")
  (nocache?
   (boolean #f)
   "If set to the value @samp{#t} caching will be disabled.")
  (noplainemail?
   (boolean #f)
   "If set to @samp{#t} showing full author email addresses will be
disabled.")
  (noheader?
   (boolean #f)
   "Flag which, when set to @samp{#t}, will make cgit omit the standard
header on all pages.")
  (project-list
   (project-list '())
   "A list of subdirectories inside of @code{repository-directory}, relative
to it, that should loaded as Git repositories.  An empty list means that all
subdirectories will be loaded.")
  (readme
   (file-object "")
   "Text which will be used as default value for @code{cgit-repo-readme}.")
  (remove-suffix?
   (boolean #f)
   "If set to @code{#t} and @code{repository-directory} is enabled, if any
repositories are found with a suffix of @code{.git}, this suffix will be
removed for the URL and name.")
  (renamelimit
   (integer -1)
   "Maximum number of files to consider when detecting renames.")
  (repository-sort
   (string "")
   "The way in which repositories in each section are sorted.")
  (robots
   (robots-list (list "noindex" "nofollow"))
   "Text used as content for the @code{robots} meta-tag.")
  (root-desc
   (string "a fast webinterface for the git dscm")
   "Text printed below the heading on the repository index page.")
  (root-readme
   (string "")
   "The content of the file specified with this option will be included
verbatim below the \"about\" link on the repository index page.")
  (root-title
   (string "")
   "Text printed as heading on the repository index page.")
  (scan-hidden-path
   (boolean #f)
   "If set to @samp{#t} and repository-directory is enabled,
repository-directory will recurse into directories whose name starts with a
period.  Otherwise, repository-directory will stay away from such directories,
considered as \"hidden\".  Note that this does not apply to the \".git\"
directory in non-bare repos.")
  (snapshots
   (list '())
   "Text which specifies the default set of snapshot formats that cgit
generates links for.")
  (repository-directory
   (repository-directory "/srv/git")
   "Name of the directory to scan for repositories (represents
@code{scan-path}).")
  (section
   (string "")
   "The name of the current repository section - all repositories defined
after this option will inherit the current section name.")
  (section-sort
   (string "")
   "Flag which, when set to @samp{1}, will sort the sections on the repository
listing by name.")
  (section-from-path
   (integer 0)
   "A number which, if defined prior to repository-directory, specifies how
many path elements from each repo path to use as a default section name.")
  (side-by-side-diffs?
   (boolean #f)
   "If set to @samp{#t} shows side-by-side diffs instead of unidiffs per
default.")
  (source-filter
   (file-object "")
   "Specifies a command which will be invoked to format plaintext blobs in the
tree view.")
  (summary-branches
   (integer 10)
   "Specifies the number of branches to display in the repository \"summary\"
view.")
  (summary-log
   (integer 10)
   "Specifies the number of log entries to display in the repository
\"summary\" view.")
  (summary-tags
   (integer 10)
   "Specifies the number of tags to display in the repository \"summary\"
view.")
  (strict-export
   (string "")
   "Filename which, if specified, needs to be present within the repository
for cgit to allow access to that repository.")
  (virtual-root
   (string "/")
   "URL which, if specified, will be used as root for all cgit links.")
  (repositories
   (repository-cgit-configuration-list '())
   "A list of @dfn{cgit-repo} records to use with config.")
  (extra-options
   (list '())
   "Extra options will be appended to cgitrc file."))

;; This distinguishes fields whose order matters, and makes sure further
;; changes won't inadvertently change the order.
(define (serialize-cgit-configuration config)
  (define (rest? field)
    (not (memq (configuration-field-name field)
               '(project-list
                 extra-options
                 repository-directory
                 repositories))))
  #~(string-append
     #$(let ((rest (filter rest? cgit-configuration-fields)))
         (serialize-configuration config rest))
     #$(serialize-project-list
        'project-list
        (cgit-configuration-project-list config))
     #$(serialize-extra-options
        (cgit-configuration-extra-options config))
     #$(serialize-repository-directory
        'repository-directory
        (cgit-configuration-repository-directory config))
     #$(serialize-repository-cgit-configuration-list
        'repositories
        (cgit-configuration-repositories config))))

(define-configuration opaque-cgit-configuration
  (cgit
   (package cgit)
   "The cgit package.")
  (cgitrc
   (string (configuration-missing-field 'opaque-cgit-configuration 'cgitrc))
   "The contents of the @code{cgitrc} to use.")
  (cache-root
   (string "/var/cache/cgit")
   "Path used to store the cgit cache entries.")
  (nginx
   (nginx-server-configuration-list (list %cgit-configuration-nginx))
   "NGINX configuration."))

(define (cgit-activation config)
  "Return the activation gexp for CONFIG."
  (let* ((opaque-config? (opaque-cgit-configuration? config))
         (config-str
          (if opaque-config?
              (opaque-cgit-configuration-cgitrc config)
              (serialize-cgit-configuration config))))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$(if opaque-config?
                       (opaque-cgit-configuration-cache-root config)
                       (cgit-configuration-cache-root config)))
        (copy-file #$(mixed-text-file "cgitrc" config-str)
                   "/etc/cgitrc"))))

(define (cgit-configuration-nginx-config config)
  (if (opaque-cgit-configuration? config)
      (opaque-cgit-configuration-nginx config)
      (cgit-configuration-nginx config)))

(define cgit-service-type
  (service-type
   (name 'cgit)
   (extensions
    (list (service-extension activation-service-type
                             cgit-activation)
          (service-extension nginx-service-type
                             cgit-configuration-nginx-config)

          ;; Make sure fcgiwrap is instantiated.
          (service-extension fcgiwrap-service-type
                             (const #t))))
   (default-value (cgit-configuration))
   (description
    "Run the cgit web interface, which allows users to browse Git
repositories.")))

(define (generate-cgit-documentation)
  (generate-documentation
   `((cgit-configuration
      ,cgit-configuration-fields
      (repositories repository-cgit-configuration))
     (repository-cgit-configuration
      ,repository-cgit-configuration-fields))
   'cgit-configuration))
