#lang racket

(provide download-base-path
         get-download-base-path)

(define download-base-path
  "~/Pictures/imgur-fetch/")

(define (get-download-base-path)
  (expand-user-path download-base-path))