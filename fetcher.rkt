#lang racket

(require net/url
         racket/cmdline

         "path-config.rkt")

; Originally I'd hoped to use Racket's HTML parser to
; clean things up a little in this one, but it's not happening.
; Looking at the docs (http://docs.racket-lang.org/html/) it
; should be easy for anyone and everyone to see why it's not
; going to make anything cleaner and more elegant.

; Function that grabs the source for a site corresponding to the supplied url.
(define (get-source url)
  (call/input-url (string->url url) get-pure-port port->string))

(define (extract-full-res-urls src)
  (regexp-match* #rx"href=\"([a-zA-Z0-9:/\\.]*?)\" target=\"_blank\">View full resolution</a>" src #:match-select cadr))

(define (get-author-name src)
  (second (regexp-match #rx"By <a href=\".*?\">([a-zA-Z0-9-]*?)</a>" src)))

(define (download-images image-urls author path)

  (define (create-author-directory)
    (when (not (directory-exists? (build-path path author)))
      (make-directory (build-path path author))))
  
  (define (download-file url)
    (define (get-file-bytes)
      (call/input-url (string->url url) get-pure-port port->bytes))
    (define filename (last (string-split url "/")))
  
    (if (equal? filename " ")
        #f
        (call-with-output-file (build-path path author filename)
          (lambda (output-port) (write-bytes (get-file-bytes) output-port))
          #:exists 'replace)))

  (create-author-directory)
  (for-each (lambda (image-url)
              (download-file image-url))
            image-urls))

(module+ main
  ; Set our current path to the base path
  ; in path-config.rkt, change if -d flag present
  (define path (get-download-base-path))
  
  (define album-url
    (command-line
     #:program
     "imgur-fetch"
     
     #:once-each
     [("-d" "--directory") ; list-of flags
      download-directory ; variable to put it in
      "Set download directory" ; help-text
      (set! path download-directory)] ; S-exp to eval if flag is present
     
     #:args (album-url) ; general arg, always supplied and without flag
     album-url)) ; always return album-url
  
  (let* ([src (get-source album-url)]
         [urls (extract-full-res-urls src)]
         [author (get-author-name src)])

    (download-images urls author path)))