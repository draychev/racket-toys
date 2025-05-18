#!/usr/bin/env racket
#lang racket/gui

;; latin-to-cyrillic.rkt — live Latin → Cyrillic (Bulgarian phonetic) converter
;; ─────────────────────────────────────────────────────────────────────────────
;; • Run:     racket latin-to-cyrillic.rkt
;; • Compile: raco exe latin-to-cyrillic.rkt
;;
;; Capitalization aware: typing `Sh`, `SH`, or `sh` yields `Ш`, `Ш`, or `ш`.
;; Requires only racket/gui and racket/string (present in every install).

(require racket/string)

;; ─────────────────────────────────────────────────────────────────────────────
;; 1. Mapping table – longest Latin sequences first so they win the match.
;; ─────────────────────────────────────────────────────────────────────────────
(define mapping
  '(("sht" . "щ") ("zh" . "ж") ("ch" . "ч") ("sh" . "ш")
                  ("ts" . "ц")  ("ya" . "я") ("q"  . "я") ("yu" . "ю")
                  ("a"  . "а")  ("b"  . "б") ("v" . "в") ("g"  . "г")
                  ("d"  . "д")  ("e"  . "е") ("z" . "з") ("i"  . "и")
                  ("y"  . "й")  ("j"  . "й") ("k" . "к") ("l"  . "л")
                  ("m" . "м")
                  ("n"  . "н")  ("o"  . "о") ("p" . "п") ("r"  . "р")
                  ("s"  . "с")  ("t"  . "т") ("u" . "у") ("f"  . "ф")
                  ("h"  . "х")  ("'"  . "ъ") ("y" . "ь")))

(define sorted-map
  (sort mapping (λ (a b) (> (string-length (car a))
                            (string-length (car b))))))

;; ─────────────────────────────────────────────────────────────────────────────
;; 2. Helper predicates for capitalization handling
;; ─────────────────────────────────────────────────────────────────────────────
(define (all-uppercase? s) (string=? s (string-upcase s)))
(define (first-uppercase? s)
  (and (positive? (string-length s))
       (char-upper-case? (string-ref s 0))))

(define (apply-case latin cyr)
  (cond [(all-uppercase? latin)        (string-upcase cyr)]
        [(first-uppercase? latin)      (string-upcase cyr)]
        [else                          cyr]))

;; ─────────────────────────────────────────────────────────────────────────────
;; 3. Transliteration engine (case‐preserving)
;; ─────────────────────────────────────────────────────────────────────────────
(define (latin->cyr s)
  (for/fold ([txt s]) ([pair (in-list sorted-map)])
    (define latin (car pair))
    (define cyr   (cdr pair))
    ;; Racket regexps need a colon after mode flags: (?i:...)
    (define pat (regexp (string-append "(?i:" latin ")")))
    (regexp-replace* pat txt
                     (λ (orig . _) (apply-case orig cyr)))))

;; ─────────────────────────────────────────────────────────────────────────────
;; 4. Minimal GUI
;; ─────────────────────────────────────────────────────────────────────────────
(define frame (new frame%
                   [label "Latin → Cyrillic (BG phonetic)"]
                   [width 520]
                   [height 180]))

(define input-field
  (new text-field%
       [parent frame]
       [label "Latin:"]
       [style '(single)]
       [callback
        (λ (self _evt)
          (send output-field set-value
                (latin->cyr (send self get-value))))]))

(define output-field
  (new text-field%
       [parent frame]
       [label "Cyrillic:"]
       [style '(single)]))

(send output-field enable #f) ; make read‐only

(new button%
     [parent frame]
     [label "Copy Cyrillic to clipboard"]
     [callback
      (λ (_1 _2)
        (send the-clipboard set-clipboard-string
              (send output-field get-value)
              0))]) ; 0 = plain‐text format

(send frame show #t)

(module+ main (void))
