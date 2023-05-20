;; package.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(defpackage :sgl-automata
  (:nicknames :sgla)
  (:use #:cl
        #:alexandria
        #:simple-gl
        #:3d-vectors
        #:3d-matrices)
  (:export #:create-1d-cellular-automata
           #:cellular-automata
           #:2d-automata-viewer
           #:1d-cellular-automata
           #:1d-cellular-automata-wrapping

           #:create-2d-cellular-automata
           #:2d-cellular-automata
           #:game-of-life
           #:hashlife
           #:create-game-of-life
           #:create-hashlife
           #:create-random-game-of-life
           ))
