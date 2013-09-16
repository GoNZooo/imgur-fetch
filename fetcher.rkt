#lang racket

(require net/url
         racket/cmdline
         net/head

         "path-config.rkt")

; Originally I'd hoped to use Racket's HTML parser to
; clean things up a little in this one, but it's not happening.
; Looking at the docs (http://docs.racket-lang.org/html/), it
; should be easy for anyone and everyone to see why it's not
; going to make anything cleaner and more elegant.

(define (get-source url)
  (call/input-url (string->url url) get-pure-port port->string))

(define (extract-full-res-urls src)
  (let ([p #rx"href=\"(/download/[0-9A-Za-z]*?)\">Download full resolution</a>"])
    (regexp-match* p src #:match-select cadr)))

; Extract author name from source HTML.
; If not possible, return the empty string.
(define (get-author-name src)
  (let* ([p #rx"By <a href=\".*?\">([a-zA-Z0-9-]*?)</a>"]
         [result (regexp-match p src)])
    (if (false? result)
        ""
        (second result))))

(define (download-images image-urls author album-name path)
  (define album-path
    (if (equal? author "")
        (build-path path album-name)
        (build-path path author album-name)))
  
  (define (create-directories)
    (make-directory* album-path))

  (define (download-file url)
    (define (get-file-bytes+header)
      (call/input-url (string->url url)
                      get-impure-port
                      (lambda (input-port)
                        ; Extract header from impure port
                        (let ([header (purify-port input-port)])
                          (values header (port->bytes input-port))))))
    
    (define-values (header file-bytes) (get-file-bytes+header))

    (define base-filename
      (last (string-split url "/")))

    (define (get-file-extension)
      (define subtype
        (last (string-split (extract-field "content-type" header) "/")))
      (case subtype
        [("jpeg") ".jpg"]
        [("png") ".png"]
        [("gif") ".gif"]
        [else ".na"]))
    
    (call-with-output-file
        (build-path album-path (string-append base-filename (get-file-extension)))
      (lambda (output-port) (write-bytes file-bytes output-port))
      #:exists 'replace))
  
  (create-directories)
  (for-each
   (lambda (dl-url)
     (download-file (string-append "http://imgur.com/" dl-url)))
   image-urls)
  
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
         [final-download-path (download-images (extract-full-res-urls src)
                                               (get-author-name src)
                                               (get-album-name album-url)
                                               base-path)])
    (printf "Downloaded album '~a' to ~a~n"
            (get-album-name album-url) final-download-path)))
