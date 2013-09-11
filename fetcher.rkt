#lang racket

(require net/url
         racket/cmdline)

; Function that grabs the source for a site corresponding to the supplied url.
(define (get-source url)
  (call/input-url (string->url url) get-pure-port port->string))

(define (source->source-file source path)
  (call-with-output-file path (lambda (output-port) (write-string source output-port)) #:exists 'replace))

(define (url->source-file url path)
  (source->source-file (get-source url) path))

(define (download-file url [path ""])
  (let ([file-bytes (call/input-url (string->url url) get-pure-port port->bytes)]
        [filename (last (string-split url "/"))])
    (if (equal? filename " ")
        #f
        (call-with-output-file (string-append path "/" filename) (lambda (output-port) (write-bytes file-bytes output-port)) #:exists 'replace))))

(define (extract-full-res-urls src)
  (regexp-match* #rx"href=\"([a-zA-Z0-9:/\\.]*?)\" target=\"_blank\">View full resolution</a>" src #:match-select cadr))

(define (get-author-name src)
  (second (regexp-match #rx"By <a href=\".*?\">([a-zA-Z0-9-]*?)</a>" src)))

(module+ main

         (define album-url (command-line #:args (album-url) album-url))
         
         (let* ([src (get-source album-url)]
                [urls (extract-full-res-urls src)]
                [author (get-author-name src)])

           (when (not (directory-exists? author))
             (make-directory author))
           
           (for-each (lambda (image-url)
                       (download-file image-url author))
                     urls)))