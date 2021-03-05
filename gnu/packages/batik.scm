;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (gnu packages batik)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages xml))

(define-public java-w3c-smil-3.0
  (package
    (name "java-w3c-smil")
    (version "3.0")
    (source #f)
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "w3c-smil.jar"
       #:source-dir "."
       #:tests? #f ; No tests exist.
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             ;; https://www.w3.org/TR/SMIL3/smil-timing.html#q142
             (mkdir-p "org/w3c/dom/smil")
             (call-with-output-file "org/w3c/dom/smil/ElementTimeControl.java"
               (lambda (port)
                 (format port "
package org.w3c.dom.smil;

import org.w3c.dom.DOMException;

public interface ElementTimeControl {
    public boolean  beginElement();

    public boolean  beginElementAt(float offset);

    public boolean endElement();

    public boolean endElementAt(float offset);
}
")))
             (call-with-output-file "org/w3c/dom/smil/TimeEvent.java"
               (lambda (port)
                 (format port "
package org.w3c.dom.smil;

import org.w3c.dom.events.Event;
import org.w3c.dom.views.AbstractView;

public interface TimeEvent extends Event {
    public AbstractView getView();

    public int getDetail();

    public void initTimeEvent(String typeArg,
                              AbstractView viewArg,
                              int detailArg);

}
")))
             #t)))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://www.w3.org/Style/CSS/SAC/")
    (synopsis "W3C SAC interface for CSS parsers in Java")
    (description "This package provides a SAC interface by the W3C.
SAC is an interface for CSS parsers.")
    (license license:w3c)))

(define-public java-w3c-svg-1.0
  (package
    (name "java-w3c-svg")
    (version "20010904")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://www.w3.org/TR/2001/REC-SVG-" version
                            "/java-binding.zip"))
        (sha256
         (base32
          "0gnxvx51bg6ijplf6l2q0i1m07101f7fickawshfygnsdjqfdnbp"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "w3c-svg.jar"
       #:source-dir "."
       #:tests? #f ; No tests exist.
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "unzip" source)))
         (add-after 'unpack 'patch-interface
           (lambda _
             ;; Make it compatible with batik.
             ;; This is equivalent to usingxml commons externals'
             ;; "externals" part from https://xerces.apache.org/mirrors.cgi
             (substitute* "SVGFEConvolveMatrixElement.java"
              (("public SVGAnimatedLength[ ]*getKernelUnitLength")
               "public SVGAnimatedNumber getKernelUnitLength"))
             (substitute* "SVGFEMorphologyElement.java"
              (("public SVGAnimatedLength[ ]*getRadius")
               "public SVGAnimatedNumber getRadius"))
             (call-with-output-file "EventListenerInitializer.java"
               (lambda (port)
                 (format port "
// License: http://www.apache.org/licenses/LICENSE-2.0
package org.w3c.dom.svg;
public interface EventListenerInitializer {
    public void initializeEventListeners(SVGDocument doc);
}

")))
             #t)))))
    (propagated-inputs
     `(("java-w3c-smil" ,java-w3c-smil-3.0)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://www.w3.org/Style/CSS/SAC/")
    (synopsis "W3C SVG 1.0 interface")
    (description "This package provides a SVG 1.0 interface.")
    (license license:w3c)))

(define-public java-w3c-svg
  (package
    (inherit java-w3c-svg-1.0)
    (version "20110816")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://www.w3.org/TR/2011/REC-SVG11-" version
                            "/java-binding.zip"))
        (sha256
         (base32
          "0jicqcrxav8ggs37amgvvwgc2f0qp1c5wns4rb2i3si83s2m09ns"))))
    (arguments
     (substitute-keyword-arguments (package-arguments java-w3c-svg-1.0)
       ((#:phases phases)
       `(modify-phases ,phases
          (delete 'patch-interface)))))
    (propagated-inputs
     `())
    (synopsis "W3C SVG interface")
    (description "This package provides a SVG interface.")))

(define-public java-w3c-sac
  (package
    (name "java-w3c-sac")
    (version "1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.w3.org/2002/06/sacjava-" version
                            ".zip"))
        (sha256
         (base32
          "1djp2nnzf8jchnwz1ij9i5jfx4cg1ryf3lbw133yzjy0wkhcla52"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "w3c-sac.jar"
       #:source-dir "sac-1.3"
       #:tests? #f ; No tests exist.
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "unzip" source))))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://www.w3.org/Style/CSS/SAC/")
    (synopsis "W3C SAC interface for CSS parsers in Java")
    (description "This package provides a SAC interface by the W3C.
SAC is an interface for CSS parsers.")
    (license license:w3c)))

(define-public java-xmlgraphics-commons
  (package
    (name "java-xmlgraphics-commons")
    (version "2.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "mirror://apache/xmlgraphics/commons/source/xmlgraphics-commons-"
              version "-src.tar.gz"))
        (sha256
         (base32 "18gndjwzdd6vhp59fn8cwn6i6q0v0ym8nmwn65rcykdrzz0nvl72"))
        (modules '((guix build utils)))
        (snippet
         `(begin
            (delete-file-recursively "lib")
            #t))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jar-main"
       #:test-target "junit"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             (substitute* "build.xml"
              (("<attribute name=\"Build-Id\" value=\"[^\"]*\"")
               "<attribute name=\"Build-Id\" value=\"\""))
             #t))
         (add-before 'build 'prepare-build-directories
           (lambda _
             (mkdir "lib")
             (mkdir "lib/build")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-share (string-append out "/share/java")))
               (for-each (lambda (name)
                           (install-file name out-share))
                         (find-files "build"
                                     "xmlgraphics-commons.*\\.jar$"))
               #t))))))
    (native-inputs
     `(("java-apache-xml-commons-resolver" ,java-apache-xml-commons-resolver)
       ("java-asm" ,java-asm)
       ("java-cglib" ,java-cglib)
       ("java-hamcrest" ,java-hamcrest-core)
       ("java-junit" ,java-junit)
       ("java-mockito" ,java-mockito-1)
       ("java-objenesis" ,java-objenesis)))
    (propagated-inputs
     `(("java-commons-io" ,java-commons-io)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)))
    (home-page "https://xmlgraphics.apache.org/commons/")
    (synopsis "XMLGraphics constants")
    (description "This package provides XMLGraphics constants (originally
from @code{batik}).")
    (license license:asl2.0)))
