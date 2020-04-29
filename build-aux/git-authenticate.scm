;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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

;;;
;;; Authenticate a range of commits.
;;;

(use-modules (git)
             (guix git)
             (guix gnupg)
             (guix utils)
             ((guix build utils) #:select (mkdir-p))
             (guix i18n)
             (guix progress)
             (srfi srfi-1)
             (srfi srfi-11)
             (srfi srfi-26)
             (srfi srfi-34)
             (srfi srfi-35)
             (rnrs io ports)
             (ice-9 match)
             (ice-9 format)
             (ice-9 pretty-print))


(define %committers
  ;; List of committers.  These are the user names found on
  ;; <https://savannah.gnu.org/project/memberlist.php?group=guix> along with
  ;; the fingerprint of the signing (sub)key.
  ;;
  ;; TODO: Replace this statically-defined list by an in-repo list.
  '(("andreas"
     "AD17 A21E F8AE D8F1 CC02  DBD9 F7D5 C9BF 765C 61E3")
    ("ajgrf"
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")
    ("alexvong1995"
     "306F CB8F 2C01 C25D 29D3  0556 61EF 502E F602 52F2")
    ("alezost"
     "4FB9 9F49 2B12 A365 7997  E664 8246 0C08 2A0E E98F")
    ("ambrevar"
     "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F")
    ("apteryx"
     "27D5 86A4 F890 0854 329F  F09F 1260 E464 82E6 3562")
    ("arunisaac"
     "7F73 0343 F2F0 9F3C 77BF  79D3 2E25 EE8B 6180 2BB3")
    ("atheia"
     ;; primary: "3B12 9196 AE30 0C3C 0E90  A26F A715 5567 3271 9948"
     "9A2B 401E D001 0650 1584  BAAC 8BC4 F447 6E8A 8E00")
    ("bandali"
     ;; primary: "BE62 7373 8E61 6D6D 1B3A  08E8 A21A 0202 4881 6103"
     "39B3 3C8D 9448 0D2D DCC2  A498 8B44 A0CD C7B9 56F2")
    ("bavier"
     ;; primary: "34FF 38BC D151 25A6 E340  A0B5 3453 2F9F AFCA 8B8E"
     "A0C5 E352 2EF8 EF5C 64CD  B7F0 FD73 CAC7 19D3 2566")
    ("beffa"
     "3774 8024 880F D3FF DCA2  C9AB 5893 6E0E 2F1B 5A4C")
    ("benwoodcroft"
     "BCF8 F737 2CED 080A 67EB  592D 2A6A D9F4 AAC2 0DF6")
    ("biscuolo"
     "45CC 63B8 5258 C9D5 5F34  B239 D37D 0EA7 CECC 3912")
    ("boskovits"
     "7988 3B9F 7D6A 4DBF 3719  0367 2506 A96C CF63 0B21")
    ("brettgilio"
     "DFC0 C7F7 9EE6 0CA7 AE55  5E19 6722 43C4 A03F 0EEE")
    ("carl"
     ;; primary: "0401 7A2A 6D9A 0CCD C81D  8EC2 96AB 007F 1A7E D999"
     "09CD D25B 5244 A376 78F6  EEA8 0CC5 2153 1979 91A5")
    ("cbaines"
     "3E89 EEE7 458E 720D 9754  E0B2 5E28 A33B 0B84 F577")
    ("civodul"
     "3CE4 6455 8A84 FDC6 9DB4  0CFB 090B 1199 3D9A EBB5")
    ("cwebber"
     "510A 8628 E2A7 7678 8F8C  709C 4BC0 2592 5FF8 F4D3")
    ("dannym"
     ;; primary: "295A F991 6F46 F8A1 34B0  29DA 8086 3842 F0FE D83B"
     "76CE C6B1 7274 B465 C02D  B3D9 E71A 3554 2C30 BAA5")
    ("davexunit"
     "B3C0 DB4D AD73 BA5D 285E  19AE 5143 0234 CEFD 87C3")
    ("davexunit (2nd)"                            ;FIXME: to be confirmed!
     "8CCB A7F5 52B9 CBEA E1FB  2915 8328 C747 0FF1 D807")
    ("daviwil"
     "53C4 1E6E 41AA FE55 335A  CA5E 446A 2ED4 D940 BF14")
    ("dvc"
     "6909 6DFD D702 8BED ACC5  884B C5E0 51C7 9C0B ECDB")
    ("dvc (old)"
     "5F43 B681 0437 2F4B A898  A64B 33B9 E9FD E28D 2C23")
    ("efraim"
     "A28B F40C 3E55 1372 662D  14F7 41AA E7DC CA3D 8351")
    ("efraim (old)"
     "9157 41FE B22F A4E3 3B6E  8F8D F4C1 D391 7EAC EE93")
    ("glv"
     ;; primary: "2453 02B1 BAB1 F867 FDCA  96BC 8F3F 861F 82EB 7A9A"
     "CBC5 9C66 EC27 B971 7940  6B3E 6BE8 208A DF21 FE3F")
    ("hoebjo"
     "2219 43F4 9E9F 276F 9499  3382 BF28 6CB6 593E 5FFD")
    ("htgoebel"
     "B943 509D 633E 80DD 27FC  4EED 634A 8DFF D3F6 31DF")
    ("ipetkov"
     "7440 26BA 7CA3 C668 E940  1D53 0B43 1E98 3705 6942")
    ("iyzsong"
     ;; primary: "66A5 6D9C 9A98 BE7F 719A  B401 2652 5665 AE72 7D37"
     "0325 78A6 8298 94E7 2AA2  66F5 D415 BF25 3B51 5976")

    ;; https://lists.gnu.org/archive/html/guix-devel/2018-04/msg00229.html
    ("janneke (old)"
     "DB34 CB51 D25C 9408 156F  CDD6 A12F 8797 8D70 1B99")
    ("janneke"
     "1A85 8392 E331 EAFD B8C2  7FFB F3C1 A0D9 C1D6 5273")

    ("jlicht"
     ;; primary: "1BA4 08C5 8BF2 0EA7 3179  635A 865D C0A3 DED9 B5D0"
     "E31D 9DDE EBA5 4A14 8A20  4550 DA45 97F9 47B4 1025")
    ("jmd"
     "8797 A26D 0854 2EAB 0285  A290 8A67 719C 2DE8 27B3")
    ("kkebreau"
     "83B6 703A DCCA 3B69 4BCE  2DA6 E6A5 EE3C 1946 7A0D")
    ("leungbk"
     "45E5 75FA 53EA 8BD6 1BCE  0B4E 3ADC 75F0 13D6 78F9")
    ("lfam"
     ;; primary: "4F71 6F9A 8FA2 C80E F1B5  E1BA 5E35 F231 DE1A C5E0"
     "B051 5948 F1E7 D3C1 B980  38A0 2646 FA30 BACA 7F08")
    ("lsl88"
     "2AE3 1395 932B E642 FC0E  D99C 9BED 6EDA 32E5 B0BC")
    ("marusich"
     "CBF5 9755 CBE7 E7EF EF18  3FB1 DD40 9A15 D822 469D")
    ("mbakke"
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")
    ("mhw"
     "D919 0965 CE03 199E AF28  B3BE 7CEF 2984 7562 C516")
    ("mothacehe"
     "4008 6A7E 0252 9B60 31FB  8607 8354 7635 3176 9CA6")
    ("mthl"
     "F2A3 8D7E EB2B 6640 5761  070D 0ADE E100 9460 4D37")
    ("nckx"
     ;; primary: "F5BC 5534 C36F 0087 B39D  36EF 1C9D C4FE B9DB 7C4B"
     "F5DA 2032 4B87 3D0B 7A38  7672 0DB0 FF88 4F55 6D79")
    ("nckx (revoked; not compromised)"
     ;; primary: "F5BC 5534 C36F 0087 B39D  36EF 1C9D C4FE B9DB 7C4B"
     "7E8F AED0 0944 78EF 72E6  4D16 D889 B0F0 18C5 493C")
    ("niedzejkob"
     "E576 BFB2 CF6E B13D F571  33B9 E315 A758 4613 1564")
    ("ngz"
     "ED0E F1C8 E126 BA83 1B48  5FE9 DA00 B4F0 48E9 2F2D")
    ("pelzflorian"
     "CEF4 CB91 4856 BA38 0A20  A7E2 3008 88CB 39C6 3817")
    ("pgarlick"
     ;; primary: "B68B DF22 73F9 DA0E 63C1  8A32 515B F416 9242 D600"
     "C699 ED09 E51B CE89 FD1D  A078 AAC7 E891 896B 568A")
    ("phant0mas"
     "3A86 380E 58A8 B942 8D39  60E1 327C 1EF3 8DF5 4C32")
    ("reepca"
     "74D6 A930 F44B 9B84 9EA5  5606 C166 AA49 5F7F 189C")
    ("rekado"
     "BCA6 89B6 3655 3801 C3C6  2150 197A 5888 235F ACAC")
    ("rhelling"
     "0154 E1B9 1CC9 D9EF 7764  8DE7 F3A7 27DB 44FC CA36")
    ("roelj (old)"
     "17CB 2812 EB63 3DFF 2C7F  0452 C3EC 1DCA 8430 72E1")
    ("roelj"
     ;; From commit cc51c03ff867d4633505354819c6d88af88bf919 (March 2020).
     ;; See <https://lists.gnu.org/archive/html/guix-devel/2020-03/msg00070.html>.
     "F556 FD94 FB8F 8B87 79E3  6832 CBD0 CD51 38C1 9AFC")
    ("roptat (old)"
     "B5FA E628 5B41 3728 B2A0  FAED 4311 1F45 2008 6A0C")
    ("roptat"
     ;; From commit 2cbede5935eb6a40173bbdf30a9ad22bf7574c22 (Jan. 2020).  See
     ;; <https://lists.gnu.org/archive/html/guix-devel/2020-01/msg00499.html>.
     "1EFB 0909 1F17 D28C CBF9  B13A 53D4 57B2 D636 EE82")
    ("samplet"
     ;; primary: "D6B0 C593 DA8C 5EDC A44C  7A58 C336 91F7 1188 B004"
     "A02C 2D82 0EF4 B25B A6B5  1D90 2AC6 A5EC 1C35 7C59")
    ("sleep_walker"
     "77DD AD2D 97F5 31BB C0F3  C7FD DFB5 EB09 AA62 5423")
    ("snape"
     "F494 72F4 7A59 00D5 C235  F212 89F9 6D48 08F3 59C7")
    ("steap"
     "4E26 CCE9 578E 0828 9855  BDD4 1C79 95D2 D5A3 8336")
    ("taylanub"
     "9ADE 9ECF 2B19 C180 9C99  5CEA A1F4 CFCC 5283 6BAC")

    ;; https://lists.gnu.org/archive/html/guix-devel/2017-03/msg00826.html
    ("thomasd"
     ;; primary: "1DD1 681F E285 E07F 11DC  0C59 2E15 A6BC D77D 54FD"
     "3D2C DA58 819C 08C2 A649  D43D 5C3B 064C 724A 5726")
    ("thomasd (old)"
     "A5C5 92EA 606E 7106 A6A3  BC08 98B2 1575 91E1 2B08")

    ("toothbrush"
     "D712 1D73 A40A 7264 9E43  ED7D F284 6B1A 0D32 C442")
    ("vagrantc"
     "6580 7361 3BFC C5C7 E2E4  5D45 DC51 8FC8 7F97 16AA")
    ("wigust"
     ;; primary: "C955 CC5D C048 7FB1 7966  40A9 199A F6A3 67E9 4ABB"
     "7238 7123 8EAC EB63 4548  5857 167F 8EA5 001A FA9C")
    ("wingo"
     "FF47 8FB2 64DE 32EC 2967  25A3 DDC0 F535 8812 F8F2")))

(define %authorized-signing-keys
  ;; Fingerprint of authorized signing keys.
  (map (match-lambda
         ((name fingerprint)
          (string-filter char-set:graphic fingerprint)))
       %committers))

(define %commits-with-bad-signature
  ;; Commits with a known-bad signature.
  '("6a34f4ccc8a5d4a48e25ad3c9c512f8634928b91"))  ;2016-12-29

(define %unsigned-commits
  ;; Commits lacking a signature.
  '())

(define-syntax-rule (with-temporary-files file1 file2 exp ...)
  (call-with-temporary-output-file
   (lambda (file1 port1)
     (call-with-temporary-output-file
      (lambda (file2 port2)
        exp ...)))))

(define (commit-signing-key repo commit-id)
  "Return the OpenPGP key ID that signed COMMIT-ID (an OID).  Raise an
exception if the commit is unsigned or has an invalid signature."
  (let-values (((signature signed-data)
                (catch 'git-error
                  (lambda ()
                    (commit-extract-signature repo commit-id))
                  (lambda _
                    (values #f #f)))))
    (if (not signature)
        (raise (condition
                (&message
                 (message (format #f (G_ "commit ~a lacks a signature")
                                  commit-id)))))
        (begin
          (with-fluids ((%default-port-encoding "UTF-8"))
            (with-temporary-files data-file signature-file
              (call-with-output-file data-file
                (cut display signed-data <>))
              (call-with-output-file signature-file
                (cut display signature <>))

              (let-values (((status data)
                            (with-error-to-port (%make-void-port "w")
                              (lambda ()
                                (gnupg-verify* signature-file data-file
                                               #:key-download 'always)))))
                (match status
                  ('invalid-signature
                   ;; There's a signature but it's invalid.
                   (raise (condition
                           (&message
                            (message (format #f (G_ "signature verification failed \
for commit ~a")
                                             (oid->string commit-id)))))))
                  ('missing-key
                   (raise (condition
                           (&message
                            (message (format #f (G_ "could not authenticate \
commit ~a: key ~a is missing")
                                             (oid->string commit-id)
                                             data))))))
                  ('valid-signature
                   (match data
                     ((fingerprint . user)
                      fingerprint)))))))))))

(define (authenticate-commit repository commit)
  "Authenticate COMMIT from REPOSITORY and return the signing key fingerprint.
Raise an error when authentication fails."
  (define id
    (commit-id commit))

  (define signing-key
    (commit-signing-key repository id))

  (unless (member signing-key %authorized-signing-keys)
    (raise (condition
            (&message
             (message (format #f (G_ "commit ~a not signed by an authorized \
key: ~a")
                              (oid->string id) signing-key))))))

  signing-key)

(define* (authenticate-commits repository commits
                               #:key (report-progress (const #t)))
  "Authenticate COMMITS, a list of commit objects, calling REPORT-PROGRESS for
each of them.  Return an alist showing the number of occurrences of each key."
  (parameterize ((current-keyring (string-append (config-directory)
                                                 "/keyrings/channels/guix.kbx")))
    (fold (lambda (commit stats)
            (report-progress)
            (let ((signer (authenticate-commit repository commit)))
              (match (assoc signer stats)
                (#f          (cons `(,signer . 1) stats))
                ((_ . count) (cons `(,signer . ,(+ count 1))
                                   (alist-delete signer stats))))))
          '()
          commits)))

(define commit-short-id
  (compose (cut string-take <> 7) oid->string commit-id))


;;;
;;; Caching.
;;;

(define (authenticated-commit-cache-file)
  "Return the name of the file that contains the cache of
previously-authenticated commits."
  (string-append (cache-directory) "/authentication/channels/guix"))

(define (previously-authenticated-commits)
  "Return the previously-authenticated commits as a list of commit IDs (hex
strings)."
  (catch 'system-error
    (lambda ()
      (call-with-input-file (authenticated-commit-cache-file)
        read))
    (lambda args
      (if (= ENOENT (system-error-errno args))
          '()
          (apply throw args)))))

(define (cache-authenticated-commit commit-id)
  "Record in ~/.cache COMMIT-ID and its closure as authenticated (only
COMMIT-ID is written to cache, though)."
  (define %max-cache-length
    ;; Maximum number of commits in cache.
    200)

  (let ((lst  (delete-duplicates
               (cons commit-id (previously-authenticated-commits))))
        (file (authenticated-commit-cache-file)))
    (mkdir-p (dirname file))
    (with-atomic-file-output file
      (lambda (port)
        (let ((lst (if (> (length lst) %max-cache-length)
                       (take lst %max-cache-length) ;truncate
                       lst)))
          (chmod port #o600)
          (display ";; List of previously-authenticated commits.\n\n"
                   port)
          (pretty-print lst port))))))


;;;
;;; Entry point.
;;;

(define (git-authenticate args)
  (define repository
    (repository-open "."))

  (let loop ((args args))
    (match args
      ((_ start end)
       (define start-commit
         (commit-lookup repository (string->oid start)))
       (define end-commit
         (commit-lookup repository (string->oid end)))

       (define authenticated-commits
         ;; Previously-authenticated commits that don't need to be checked
         ;; again.
         (filter-map (lambda (id)
                       (false-if-exception
                        (commit-lookup repository (string->oid id))))
                     (previously-authenticated-commits)))

       (define commits
         ;; Commits to authenticate, excluding the closure of
         ;; AUTHENTICATED-COMMITS.
         (commit-difference end-commit start-commit
                            authenticated-commits))

       (define reporter
         (progress-reporter/bar (length commits)))

       (format #t (G_ "Authenticating ~a to ~a (~a commits)...~%")
               (commit-short-id start-commit)
               (commit-short-id end-commit)
               (length commits))

       (let ((stats (call-with-progress-reporter reporter
                      (lambda (report)
                        (authenticate-commits repository commits
                                              #:report-progress report)))))
         (cache-authenticated-commit (oid->string (commit-id end-commit)))

         (unless (null? stats)
           (format #t (G_ "Signing statistics:~%"))
           (for-each (match-lambda
                       ((signer . count)
                        (format #t "  ~a ~10d~%" signer count)))
                     (sort stats
                           (match-lambda*
                             (((_ . count1) (_ . count2))
                              (> count1 count2))))))))
      ((command start)
       (let* ((head (repository-head repository))
              (end  (reference-target head)))
         (loop (list command start (oid->string end)))))
      (_
       (format (current-error-port)
               (G_ "Usage: git-authenticate START [END]

Authenticate commits START to END or the current head.\n"))))))

;;; Local Variables:
;;; eval: (put 'with-temporary-files 'scheme-indent-function 2)
;;; End:
