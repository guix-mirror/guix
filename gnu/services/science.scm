(define-module (gnu services science)
  #:export (<rshiny-configuration>
            rshiny-configuration
            rshiny-configuration?
            rshiny-configuration-package
            rshiny-configuration-binary
            rshiny-shepherd-service
            rshiny-service-type))

(use-modules (gnu)
             (guix records)
             (ice-9 match))
(use-service-modules shepherd)
(use-package-modules cran)

(define-record-type* <rshiny-configuration>
  rshiny-configuration
  make-rshiny-configuration
  rshiny-configuration?
  (package          rshiny-configuration-package    ; package
                    (default r-shiny))
  (binary           rshiny-configuration-binary     ; string
                    (default "rshiny")))

(define rshiny-shepherd-service
  (match-lambda
    (($ <rshiny-configuration> package binary)
     (list
       (shepherd-service
         (documentation (string-append "R-Shiny service for " binary))
         (provision (list (symbol-append 'rshiny- (string->symbol
                                                    (string-take binary 9)))))
         (requirement '(networking))
         (start
           #~(exec-command
               (list
                 #$(string-append "/run/current-system/profile/bin/" binary))
               ;#:log-file #$(string-append "/var/log/" binary ".log") ; kills shepherd
               #:environment-variables
               (list "R_LIBS_USER=/run/current-system/profile/site-library/")))
         (stop #~(make-kill-destructor)))))))

(define rshiny-service-type
  (service-type
    (name 'rshiny)
    (extensions
      (list
        (service-extension shepherd-root-service-type
                           rshiny-shepherd-service)
        (service-extension profile-service-type
                           ;; We want the package installed so that it
                           ;; pulls in the propagated inputs as well.
                           (lambda (config)
                             (list
                               (rshiny-configuration-package config))))))
    (description
     "Run an R-Shiny webapp as a Guix Service.")))
