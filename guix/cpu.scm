;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix cpu)
  #:use-module (guix sets)
  #:use-module (guix memoization)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:export (current-cpu
            cpu?
            cpu-architecture
            cpu-family
            cpu-model
            cpu-flags

            cpu->gcc-architecture))

;;; Commentary:
;;;
;;; This module provides tools to determine the micro-architecture supported
;;; by the CPU and to map it to a name known to GCC's '-march'.
;;;
;;; Code:

;; CPU description.
(define-record-type <cpu>
  (cpu architecture family model flags)
  cpu?
  (architecture cpu-architecture)                 ;string, from 'uname'
  (family       cpu-family)                       ;integer
  (model        cpu-model)                        ;integer
  (flags        cpu-flags))                       ;set of strings

(define current-cpu
  (mlambda ()
    "Return a <cpu> record representing the host CPU."
    (define (prefix? prefix)
      (lambda (str)
        (string-prefix? prefix str)))

    (call-with-input-file "/proc/cpuinfo"
      (lambda (port)
        (let loop ((family #f)
                   (model #f))
          (match (read-line port)
            ((? eof-object?)
             #f)
            ((? (prefix? "cpu family") str)
             (match (string-tokenize str)
               (("cpu" "family" ":" family)
                (loop (string->number family) model))))
            ((? (prefix? "model") str)
             (match (string-tokenize str)
               (("model" ":" model)
                (loop family (string->number model)))
               (_
                (loop family model))))
            ((? (prefix? "flags") str)
             (match (string-tokenize str)
               (("flags" ":" flags ...)
                (cpu (utsname:machine (uname))
                     family model (list->set flags)))))
            (_
             (loop family model))))))))

(define (cpu->gcc-architecture cpu)
  "Return the architecture name, suitable for GCC's '-march' flag, that
corresponds to CPU, a record as returned by 'current-cpu'."
  (match (cpu-architecture cpu)
    ("x86_64"
     ;; Transcribed from GCC's 'host_detect_local_cpu' in driver-i386.c.
     (or (and (= 6 (cpu-family cpu))              ;the "Pentium Pro" family
              (letrec-syntax ((model (syntax-rules (=>)
                                       ((_) #f)
                                       ((_ (candidate => integers ...) rest
                                           ...)
                                        (or (and (= (cpu-model cpu) integers)
                                                 candidate)
                                            ...
                                            (model rest ...))))))
                (model ("bonnel" => #x1c #x26)
                       ("silvermont" => #x37 #x4a #x4d #x5a #x5d)
                       ("core2" => #x0f #x17 #x1d)
                       ("nehalem" => #x1a #x1e #x1f #x2e)
                       ("westmere" => #x25 #x2c #x2f)
                       ("sandybridge" => #x2a #x2d)
                       ("ivybridge" => #x3a #x3e)
                       ("haswell" => #x3c #x3f #x45 #x46)
                       ("broadwell" => #x3d #x47 #x4f #x56)
                       ("skylake" => #x4e #x5e #x8e #x9e)
                       ("skylake-avx512" => #x55) ;TODO: cascadelake
                       ("knl" => #x57)
                       ("cannonlake" => #x66)
                       ("knm" => #x85))))

         ;; Fallback case for non-Intel processors or for Intel processors not
         ;; recognized above.
         (letrec-syntax ((if-flags (syntax-rules (=>)
                                     ((_)
                                      #f)
                                     ((_ (flags ... => name) rest ...)
                                      (if (every (lambda (flag)
                                                   (set-contains? (cpu-flags cpu)
                                                                  flag))
                                                 '(flags ...))
                                          name
                                          (if-flags rest ...))))))
           (if-flags ("avx512" => "knl")
                     ("adx" => "broadwell")
                     ("avx2" => "haswell")
                     ;; TODO: tigerlake, cooperlake, etc.
                     ("avx" => "sandybridge")
                     ("sse4_2" "gfni" => "tremont")
                     ("sse4_2" "sgx" => "goldmont-plus")
                     ("sse4_2" "xsave" => "goldmont")
                     ("sse4_2" "movbe" => "silvermont")
                     ("sse4_2" => "nehalem")
                     ("ssse3" "movbe" => "bonnell")
                     ("ssse3" => "core2")))

         ;; TODO: Recognize AMD models (bdver*, znver*, etc.)?

         "x86_64"))
    (architecture
     ;; TODO: AArch64.
     architecture)))
