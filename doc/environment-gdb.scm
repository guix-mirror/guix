(use-modules (guix)
             (gnu packages gdb)
             (gnu packages autotools)
             (gnu packages texinfo))

;; Augment the package definition of GDB with the build tools
;; needed when developing GDB (and which are not needed when
;; simply installing it.)
(package (inherit gdb)
  (native-inputs `(("autoconf" ,autoconf-2.64)
                   ("automake" ,automake)
                   ("texinfo" ,texinfo)
                   ,@(package-native-inputs gdb))))
