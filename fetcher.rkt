#lang racket

(require net/url
         racket/cmdline

         "path-config.rkt")

; Originally I'd hoped to use Racket's HTML parser to
; clean things up a little in this one, but it's not happening.
; Looking at the docs (http://docs.racket-lang.org/html/), it
; should be easy for anyone and everyone to see why it's not
; going to make anything cleaner and more elegant.

(define (get-source url)
  (call/input-url (string->url url) get-pure-port port->string))

(define (extract-full-res-urls src)
  (regexp-match*
   #rx"href=\"([a-zA-Z0-9:/\\.]*?)\" target=\"_blank\">View full resolution</a>"
   src
   #:match-select cadr))

; Extract author name from source HTML.
; If not possible, return #f
(define (get-author-name src)
  (let ([result (regexp-match
                 #rx"By <a href=\".*?\">([a-zA-Z0-9-]*?)</a>"
                 src)])
    (if (false? result)
        result
        (second result))))

(define (download-images image-urls author album-name path)
  (define album-path "")
  (if (equal? author "")
      (set! album-path (build-path path album-name))
      (set! album-path (build-path path author album-name)))
  
  (define (create-directories)
    (make-directory* album-path)) 
  
  (define (download-file url)
    (define (get-file-bytes)
      (call/input-url (string->url url) get-pure-port port->bytes))
    
    (define filename (last (string-split url "/")))
  
    (call-with-output-file (build-path album-path filename)
      (lambda (output-port) (write-bytes (get-file-bytes) output-port))
      #:exists 'replace))

  (create-directories)
  (for-each download-file image-urls)
  album-path)

(define (get-album-name album-url)
  (last (string-split album-url "/")))

(define (assemble-command-line-flags)
  ; Set our current path to the base path
  ; in path-config.rkt, change if -d flag present
  (define path (get-download-base-path))
  
  (define album-url
    (command-line
     #:program
     "imgur-fetch"
     
     #:once-each
     [("--directory" "-d") ; list-of flags
      download-directory ; variable to put it in
      "Set download directory" ; help-text
      (set! path download-directory)] ; S-exp to eval if flag is present
     
     #:args (album-url) ; general arg, always supplied and without flag
     album-url)) ; always return album-url
  (values path album-url))
  
(module+ main
  (define-values (base-path album-url) (assemble-command-line-flags))
  
  (let* ([src (get-source album-url)]
         [urls (extract-full-res-urls src)]
         [author (get-author-name src)]
         [album-name (get-album-name album-url)])

    (when (false? author)
      (set! author ""))
    (let ([final-download-path (download-images urls author album-name base-path)])
      (printf "Downloaded album '~a' to ~a~n"
              (get-album-name album-url) final-download-path))))
