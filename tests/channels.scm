;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-channels)
  #:use-module (guix channels)
  #:use-module (guix profiles)
  #:use-module ((guix build syscalls) #:select (mkdtemp!))
  #:use-module (guix tests)
  #:use-module (guix store)
  #:use-module ((guix grafts) #:select (%graft?))
  #:use-module (guix derivations)
  #:use-module (guix sets)
  #:use-module (guix gexp)
  #:use-module ((guix diagnostics)
                #:select (error-location?
                          error-location location-line
                          formatted-message?
                          formatted-message-string
                          formatted-message-arguments))
  #:use-module ((guix build utils) #:select (which))
  #:use-module (git)
  #:use-module (guix git)
  #:use-module (guix git-authenticate)
  #:use-module (guix openpgp)
  #:use-module (guix tests git)
  #:use-module (guix tests gnupg)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match))

(define (gpg+git-available?)
  (and (which (git-command))
       (which (gpg-command)) (which (gpgconf-command))))

(define commit-id-string
  (compose oid->string commit-id))


(test-begin "channels")

(define* (make-instance #:key
                        (name 'fake)
                        (commit "cafebabe")
                        (spec #f))
  (define instance-dir (mkdtemp! "/tmp/checkout.XXXXXX"))
  (when spec
    (call-with-output-file (string-append instance-dir "/.guix-channel")
      (lambda (port) (write spec port))))
  (checkout->channel-instance instance-dir
                              #:commit commit
                              #:name name))

(define instance--boring (make-instance))
(define instance--unsupported-version
  (make-instance #:spec
                 '(channel (version 42) (dependencies whatever))))
(define instance--no-deps
  (make-instance #:spec
                 '(channel (version 0))))
(define instance--sub-directory
  (make-instance #:spec
                 '(channel (version 0) (directory "modules"))))
(define instance--simple
  (make-instance #:spec
                 '(channel
                   (version 0)
                   (dependencies
                    (channel
                     (name test-channel)
                     (url "https://example.com/test-channel"))))))
(define instance--with-dupes
  (make-instance #:spec
                 '(channel
                   (version 0)
                   (dependencies
                    (channel
                     (name test-channel)
                     (url "https://example.com/test-channel"))
                    (channel
                     (name test-channel)
                     (url "https://example.com/test-channel")
                     (commit "abc1234"))
                    (channel
                     (name test-channel)
                     (url "https://example.com/test-channel-elsewhere"))))))

(define channel-instance-metadata
  (@@ (guix channels) channel-instance-metadata))
(define channel-metadata-directory
  (@@ (guix channels) channel-metadata-directory))
(define channel-metadata-dependencies
  (@@ (guix channels) channel-metadata-dependencies))


(test-equal "channel-instance-metadata returns default if .guix-channel does not exist"
  '("/" ())
  (let ((metadata (channel-instance-metadata instance--boring)))
    (list (channel-metadata-directory metadata)
          (channel-metadata-dependencies metadata))))

(test-equal "channel-instance-metadata and default dependencies"
  '()
  (channel-metadata-dependencies (channel-instance-metadata instance--no-deps)))

(test-equal "channel-instance-metadata and directory"
  "/modules"
  (channel-metadata-directory
   (channel-instance-metadata instance--sub-directory)))

(test-equal "channel-instance-metadata rejects unsupported version"
  1                              ;line number in the generated '.guix-channel'
  (guard (c ((and (message-condition? c) (error-location? c))
             (location-line (error-location c))))
    (channel-instance-metadata instance--unsupported-version)))

(test-assert "channel-instance-metadata returns <channel-metadata>"
  (every (@@ (guix channels) channel-metadata?)
         (map channel-instance-metadata
              (list instance--no-deps
                    instance--simple
                    instance--with-dupes))))

(test-assert "channel-instance-metadata dependencies are channels"
  (let ((deps ((@@ (guix channels) channel-metadata-dependencies)
               (channel-instance-metadata instance--simple))))
    (match deps
      (((? channel? dep)) #t)
      (_ #f))))

(test-assert "latest-channel-instances includes channel dependencies"
  (let* ((channel (channel
                   (name 'test)
                   (url "test")))
         (test-dir (channel-instance-checkout instance--simple)))
    (mock ((guix git) update-cached-checkout
           (lambda* (url #:key ref starting-commit)
             (match url
               ("test" (values test-dir "caf3cabba9e" #f))
               (_      (values (channel-instance-checkout instance--no-deps)
                               "abcde1234" #f)))))
          (with-store store
            (let ((instances (latest-channel-instances store (list channel))))
              (and (eq? 2 (length instances))
                   (lset= eq?
                          '(test test-channel)
                          (map (compose channel-name channel-instance-channel)
                               instances))))))))

(test-assert "latest-channel-instances excludes duplicate channel dependencies"
  (let* ((channel (channel
                   (name 'test)
                   (url "test")))
         (test-dir (channel-instance-checkout instance--with-dupes)))
    (mock ((guix git) update-cached-checkout
           (lambda* (url #:key ref starting-commit)
             (match url
               ("test" (values test-dir "caf3cabba9e" #f))
               (_      (values (channel-instance-checkout instance--no-deps)
                               "abcde1234" #f)))))
          (with-store store
            (let ((instances (latest-channel-instances store (list channel))))
              (and (= 2 (length instances))
                   (lset= eq?
                          '(test test-channel)
                          (map (compose channel-name channel-instance-channel)
                               instances))
                   ;; only the most specific channel dependency should remain,
                   ;; i.e. the one with a specified commit.
                   (find (lambda (instance)
                           (and (eq? (channel-name
                                      (channel-instance-channel instance))
                                     'test-channel)
                                (string=? (channel-commit
                                           (channel-instance-channel instance))
                                          "abc1234")))
                         instances)))))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-channel-instances #:validate-pull"
  'descendant

  ;; Make sure the #:validate-pull procedure receives the right values.
  (let/ec return
    (with-temporary-git-repository directory
        '((add "a.txt" "A")
          (commit "first commit")
          (add "b.scm" "#t")
          (commit "second commit"))
      (with-repository directory repository
        (let* ((commit1 (find-commit repository "first"))
               (commit2 (find-commit repository "second"))
               (spec    (channel (url (string-append "file://" directory))
                                 (name 'foo)))
               (new     (channel (inherit spec)
                                 (commit (oid->string (commit-id commit2)))))
               (old     (channel (inherit spec)
                                 (commit (oid->string (commit-id commit1))))))
          (define (validate-pull channel current commit relation)
            (return (and (eq? channel old)
                         (string=? (oid->string (commit-id commit2))
                                   current)
                         (string=? (oid->string (commit-id commit1))
                                   commit)
                         relation)))

          (with-store store
            ;; Attempt a downgrade from NEW to OLD.
            (latest-channel-instances store (list old)
                                      #:current-channels (list new)
                                      #:validate-pull validate-pull)))))))

(test-assert "channel-instances->manifest"
  ;; Compute the manifest for a graph of instances and make sure we get a
  ;; derivation graph that mirrors the instance graph.  This test also ensures
  ;; we don't try to access Git repositores at all at this stage.
  (let* ((spec      (lambda deps
                      `(channel (version 0)
                                (dependencies
                                 ,@(map (lambda (dep)
                                          `(channel
                                            (name ,dep)
                                            (url "http://example.org")))
                                        deps)))))
         (guix      (make-instance #:name 'guix))
         (instance0 (make-instance #:name 'a))
         (instance1 (make-instance #:name 'b #:spec (spec 'a)))
         (instance2 (make-instance #:name 'c #:spec (spec 'b)))
         (instance3 (make-instance #:name 'd #:spec (spec 'c 'a))))
    (%graft? #f)                                    ;don't try to build stuff

    ;; Create 'build-self.scm' so that GUIX is recognized as the 'guix' channel.
    (let ((source (channel-instance-checkout guix)))
      (mkdir (string-append source "/build-aux"))
      (call-with-output-file (string-append source
                                            "/build-aux/build-self.scm")
        (lambda (port)
          (write '(begin
                    (use-modules (guix) (gnu packages bootstrap))

                    (lambda _
                      (package->derivation %bootstrap-guile)))
                 port))))

    (with-store store
      (let ()
        (define manifest
          (run-with-store store
            (channel-instances->manifest (list guix
                                               instance0 instance1
                                               instance2 instance3))))

        (define entries
          (manifest-entries manifest))

        (define (depends? drv in out)
          ;; Return true if DRV depends (directly or indirectly) on all of IN
          ;; and none of OUT.
          (let ((set (list->set
                      (requisites store
                                  (list (derivation-file-name drv)))))
                (in  (map derivation-file-name in))
                (out (map derivation-file-name out)))
            (and (every (cut set-contains? set <>) in)
                 (not (any (cut set-contains? set <>) out)))))

        (define (lookup name)
          (run-with-store store
            (lower-object
             (manifest-entry-item
              (manifest-lookup manifest
                               (manifest-pattern (name name)))))))

        (let ((drv-guix (lookup "guix"))
              (drv0     (lookup "a"))
              (drv1     (lookup "b"))
              (drv2     (lookup "c"))
              (drv3     (lookup "d")))
          (and (depends? drv-guix '() (list drv0 drv1 drv2 drv3))
               (depends? drv0
                         (list) (list drv1 drv2 drv3))
               (depends? drv1
                         (list drv0) (list drv2 drv3))
               (depends? drv2
                         (list drv1) (list drv3))
               (depends? drv3
                         (list drv2 drv0) (list))))))))

(unless (which (git-command)) (test-skip 1))
(test-equal "channel-news, no news"
  '()
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "the commit"))
    (with-repository directory repository
      (let ((channel (channel (url (string-append "file://" directory))
                              (name 'foo)))
            (latest  (reference-name->oid repository "HEAD")))
        (channel-news-for-commit channel (oid->string latest))))))

(unless (which (git-command)) (test-skip 1))
(test-assert "channel-news, one entry"
  (with-temporary-git-repository directory
      `((add ".guix-channel"
             ,(object->string
               '(channel (version 0)
                         (news-file "news.scm"))))
        (commit "first commit")
        (add "src/a.txt" "A")
        (commit "second commit")
        (tag "tag-for-first-news-entry")
        (add "news.scm"
             ,(lambda (repository)
                (let ((previous
                       (reference-name->oid repository "HEAD")))
                  (object->string
                   `(channel-news
                     (version 0)
                     (entry (commit ,(oid->string previous))
                            (title (en "New file!")
                                   (eo "Nova dosiero!"))
                            (body (en "Yeah, a.txt."))))))))
        (commit "third commit")
        (add "src/b.txt" "B")
        (commit "fourth commit")
        (add "news.scm"
             ,(lambda (repository)
                (let ((second
                       (commit-id
                        (find-commit repository "second commit")))
                      (previous
                       (reference-name->oid repository "HEAD")))
                  (object->string
                   `(channel-news
                     (version 0)
                     (entry (commit ,(oid->string previous))
                            (title (en "Another file!"))
                            (body (en "Yeah, b.txt.")))
                     (entry (tag "tag-for-first-news-entry")
                            (title (en "Old news.")
                                   (eo "Malnovaĵoj."))
                            (body (en "For a.txt"))))))))
        (commit "fifth commit"))
    (with-repository directory repository
      (define (find-commit* message)
        (oid->string (commit-id (find-commit repository message))))

      (let ((channel (channel (url (string-append "file://" directory))
                              (name 'foo)))
            (commit1 (find-commit* "first commit"))
            (commit2 (find-commit* "second commit"))
            (commit3 (find-commit* "third commit"))
            (commit4 (find-commit* "fourth commit"))
            (commit5 (find-commit* "fifth commit")))
        ;; First try fetching all the news up to a given commit.
        (and (null? (channel-news-for-commit channel commit2))
             (lset= string=?
                    (map channel-news-entry-commit
                         (channel-news-for-commit channel commit5))
                    (list commit2 commit4))
             (lset= equal?
                    (map channel-news-entry-title
                         (channel-news-for-commit channel commit5))
                    '((("en" . "Another file!"))
                      (("en" . "Old news.") ("eo" . "Malnovaĵoj."))))
             (lset= string=?
                    (map channel-news-entry-commit
                         (channel-news-for-commit channel commit3))
                    (list commit2))

             ;; Now fetch news entries that apply to a commit range.
             (lset= string=?
                    (map channel-news-entry-commit
                         (channel-news-for-commit channel commit3 commit1))
                    (list commit2))
             (lset= string=?
                    (map channel-news-entry-commit
                         (channel-news-for-commit channel commit5 commit3))
                    (list commit4))
             (lset= string=?
                    (map channel-news-entry-commit
                         (channel-news-for-commit channel commit5 commit1))
                    (list commit4 commit2))
             (lset= equal?
                    (map channel-news-entry-tag
                         (channel-news-for-commit channel commit5 commit1))
                    '(#f "tag-for-first-news-entry")))))))

(unless (which (git-command)) (test-skip 1))
(test-assert "channel-news, annotated tag"
  (with-temporary-git-repository directory
      `((add ".guix-channel"
             ,(object->string
               '(channel (version 0)
                         (news-file "news.scm"))))
        (add "src/a.txt" "A")
        (commit "first commit")
        (tag "tag-for-first-news-entry"
             "This is an annotated tag.")
        (add "news.scm"
             ,(lambda (repository)
                (let ((previous
                       (reference-name->oid repository "HEAD")))
                  (object->string
                   `(channel-news
                     (version 0)
                     (entry (tag "tag-for-first-news-entry")
                            (title (en "New file!"))
                            (body (en "Yeah, a.txt."))))))))
        (commit "second commit"))
    (with-repository directory repository
      (define (find-commit* message)
        (oid->string (commit-id (find-commit repository message))))

      (let ((channel (channel (url (string-append "file://" directory))
                              (name 'foo)))
            (commit1 (find-commit* "first commit"))
            (commit2 (find-commit* "second commit")))
        (and (null? (channel-news-for-commit channel commit1))
             (lset= equal?
                    (map channel-news-entry-title
                         (channel-news-for-commit channel commit2))
                    '((("en" . "New file!"))))
             (lset= string=?
                    (map channel-news-entry-tag
                         (channel-news-for-commit channel commit2))
                    (list "tag-for-first-news-entry"))
             ;; This is an annotated tag, but 'channel-news-entry-commit'
             ;; should give us the commit ID, not the ID of the annotated tag
             ;; object.
             (lset= string=?
                    (map channel-news-entry-commit
                         (channel-news-for-commit channel commit2))
                    (list commit1)))))))

(unless (which (git-command)) (test-skip 1))
(test-assert "latest-channel-instances, missing introduction for 'guix'"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "first commit")
        (add "b.scm" "#t")
        (commit "second commit"))
    (with-repository directory repository
      (let* ((commit1 (find-commit repository "first"))
             (commit2 (find-commit repository "second"))
             (channel (channel (url (string-append "file://" directory))
                               (name 'guix))))

        (guard (c ((formatted-message? c)
                   (->bool (string-contains (formatted-message-string c)
                                            "introduction"))))
          (with-store store
            ;; Attempt a downgrade from NEW to OLD.
            (latest-channel-instances store (list channel))
            #f))))))

(unless (gpg+git-available?) (test-skip 1))
(test-equal "authenticate-channel, wrong first commit signer"
  #t
  (with-fresh-gnupg-setup (list %ed25519-public-key-file
                                %ed25519-secret-key-file
                                %ed25519-2-public-key-file
                                %ed25519-2-secret-key-file)
    (with-temporary-git-repository directory
        `((add ".guix-channel"
               ,(object->string
                 '(channel (version 0)
                           (keyring-reference "master"))))
          (add ".guix-authorizations"
               ,(object->string
                 `(authorizations (version 0)
                                  ((,(key-fingerprint
                                      %ed25519-public-key-file)
                                    (name "Charlie"))))))
          (add "signer.key" ,(call-with-input-file %ed25519-public-key-file
                               get-string-all))
          (commit "first commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file)))
          (add "random" ,(random-text))
          (commit "second commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file))))
      (with-repository directory repository
        (let* ((commit1 (find-commit repository "first"))
               (commit2 (find-commit repository "second"))
               (intro   (make-channel-introduction
                         (commit-id-string commit1)
                         (openpgp-public-key-fingerprint
                          (read-openpgp-packet
                           %ed25519-2-public-key-file)))) ;different key
               (channel (channel (name 'example)
                                 (url (string-append "file://" directory))
                                 (introduction intro))))
          (guard (c ((formatted-message? c)
                     (and (string-contains (formatted-message-string c)
                                           "initial commit")
                          (equal? (formatted-message-arguments c)
                                  (list
                                   (oid->string (commit-id commit1))
                                   (key-fingerprint %ed25519-public-key-file)
                                   (key-fingerprint
                                    %ed25519-2-public-key-file))))))
            (authenticate-channel channel directory
                                  (commit-id-string commit2)
                                  #:keyring-reference-prefix "")
            'failed))))))

(unless (gpg+git-available?) (test-skip 1))
(test-equal "authenticate-channel, .guix-authorizations"
  #t
  (with-fresh-gnupg-setup (list %ed25519-public-key-file
                                %ed25519-secret-key-file
                                %ed25519-2-public-key-file
                                %ed25519-2-secret-key-file)
    (with-temporary-git-repository directory
        `((add ".guix-channel"
               ,(object->string
                 '(channel (version 0)
                           (keyring-reference "channel-keyring"))))
          (add ".guix-authorizations"
               ,(object->string
                 `(authorizations (version 0)
                                  ((,(key-fingerprint
                                      %ed25519-public-key-file)
                                    (name "Charlie"))))))
          (commit "zeroth commit")
          (add "a.txt" "A")
          (commit "first commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file)))
          (add "b.txt" "B")
          (commit "second commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file)))
          (add "c.txt" "C")
          (commit "third commit"
                  (signer ,(key-fingerprint %ed25519-2-public-key-file)))
          (branch "channel-keyring")
          (checkout "channel-keyring")
          (add "signer.key" ,(call-with-input-file %ed25519-public-key-file
                               get-string-all))
          (add "other.key" ,(call-with-input-file %ed25519-2-public-key-file
                              get-string-all))
          (commit "keyring commit")
          (checkout "master"))
      (with-repository directory repository
        (let* ((commit1 (find-commit repository "first"))
               (commit2 (find-commit repository "second"))
               (commit3 (find-commit repository "third"))
               (intro   (make-channel-introduction
                         (commit-id-string commit1)
                         (openpgp-public-key-fingerprint
                          (read-openpgp-packet
                           %ed25519-public-key-file))))
               (channel (channel (name 'example)
                                 (url (string-append "file://" directory))
                                 (introduction intro))))
          ;; COMMIT1 and COMMIT2 are fine.
          (and (authenticate-channel channel directory
                                     (commit-id-string commit2)
                                     #:keyring-reference-prefix "")

               ;; COMMIT3 is signed by an unauthorized key according to its
               ;; parent's '.guix-authorizations' file.
               (guard (c ((unauthorized-commit-error? c)
                          (and (oid=? (git-authentication-error-commit c)
                                      (commit-id commit3))
                               (bytevector=?
                                (openpgp-public-key-fingerprint
                                 (unauthorized-commit-error-signing-key c))
                                (openpgp-public-key-fingerprint
                                 (read-openpgp-packet
                                  %ed25519-2-public-key-file))))))
                 (authenticate-channel channel directory
                                       (commit-id-string commit3)
                                       #:keyring-reference-prefix "")
                 'failed)))))))

(unless (gpg+git-available?) (test-skip 1))
(test-equal "latest-channel-instances, authenticate dependency"
  #t
  ;; Make sure that a channel dependency that has an introduction is
  ;; authenticated.  This test checks that an authentication error is raised
  ;; as it should when authenticating the dependency.
  (with-fresh-gnupg-setup (list %ed25519-public-key-file
                                %ed25519-secret-key-file)
    (with-temporary-git-repository dependency-directory
        `((add ".guix-channel"
               ,(object->string
                 '(channel (version 0)
                           (keyring-reference "master"))))
          (add ".guix-authorizations"
               ,(object->string
                 `(authorizations (version 0) ())))
          (add "signer.key" ,(call-with-input-file %ed25519-public-key-file
                               get-string-all))
          (commit "zeroth commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file)))
          (add "foo.txt" "evil")
          (commit "unsigned commit"))
      (with-repository dependency-directory dependency
        (let* ((commit0 (find-commit dependency "zeroth"))
               (commit1 (find-commit dependency "unsigned"))
               (intro   `(channel-introduction
                          (version 0)
                          (commit ,(commit-id-string commit0))
                          (signer ,(openpgp-format-fingerprint
                                    (openpgp-public-key-fingerprint
                                     (read-openpgp-packet
                                      %ed25519-public-key-file)))))))
          (with-temporary-git-repository directory
              `((add ".guix-channel"
                     ,(object->string
                       `(channel (version 0)
                                 (dependencies
                                  (channel
                                   (name test-channel)
                                   (url ,dependency-directory)
                                   (introduction ,intro))))))
                (commit "single commit"))
            (let ((channel (channel (name 'test) (url directory))))
              (guard (c ((unsigned-commit-error? c)
                         (oid=? (git-authentication-error-commit c)
                                (commit-id commit1))))
                (with-store store
                  (latest-channel-instances store (list channel))
                  'failed)))))))))

(test-end "channels")
