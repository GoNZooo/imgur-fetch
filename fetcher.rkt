#lang racket

(require net/url
         racket/cmdline)

; Originally I'd hoped to use Racket's HTML parser to
; clean things up a little in this one, but it's not happening.
; Looking at the docs (http://docs.racket-lang.org/html/) it
; should be easy for anyone and everyone to see why it's not
; going to make anything cleaner and more elegant.

; Function that grabs the source for a site corresponding to the supplied url.
(define (get-source url)
  (call/input-url (string->url url) get-pure-port port->string))

(define (download-file url [path ""])
  (define (get-file-bytes)
    (call/input-url (string->url url) get-pure-port port->bytes))
  (define filename (last (string-split url "/")))
  
  (if (equal? filename " ")
      #f
      (call-with-output-file (string-append path "/" filename)
        (lambda (output-port) (write-bytes file-bytes output-port))
        #:exists 'replace))))

(define (extract-full-res-urls src)
  (regexp-match* #rx"href=\"([a-zA-Z0-9:/\\.]*?)\" target=\"_blank\">View full resolution</a>" src #:match-select cadr))

(define (get-author-name src)
  (second (regexp-match #rx"By <a href=\".*?\">([a-zA-Z0-9-]*?)</a>" src)))

(define (create-author-directory author)
  (when (not (directory-exists? author))
    (make-directory author)))

(define (download-images image-urls author)
  (for-each (lambda (image-url)
              (download-file image-url author))
            image-urls))

(module+ main

         (define album-url (command-line #:args (album-url) album-url))
         
         (let* ([src (get-source album-url)]
                [urls (extract-full-res-urls src)]
                [author (get-author-name src)])

           (create-author-directory author)
           (download-images urls author)))