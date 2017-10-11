(use-modules (web client)
             (web response)
             (srfi srfi-1)
             (srfi srfi-4)
             (srfi srfi-9)
             (srfi srfi-13)
             (rnrs bytevectors)
             (rnrs hashtables)
             (ice-9 pretty-print)
             (ice-9 hash-table)
             (json))

(define %app-id "")
(define %app-secret "")
(define %token "")
(define %token-type "")

(define (pp-hashtable h)
  (if (hash-table? h)
      (hash-for-each (lambda (key value)
                       (format #t "~a => ~a~%" key value)) h)
      (display "Not a hash-table!")))

(define (get-fb-access-token)
  (let ((res (http-get
              (string-append "https://graph.facebook.com/oauth/access_token?client_id="
                             %app-id
                             "&client_secret="
                             %app-secret
                             "&grant_type=client_credentials")
              #:streaming? #t)))
    ;; (display res)
    (if (equal? (response-code res) 200)
          (let ((j (json-string->scm (utf8->string (read-response-body res)))))
            ;; (pp-hashtable j)
            (set! %token (hash-ref j "access_token"))
            (set! %token-type (hash-ref j "token_type")))
        (begin  ;; Request failed
          (display "Something when wrong!")(newline)
          (display res)
          ))))

(define (get-kodkollektivet-events)
  (let ((res (http-get
              (string-append "https://graph.facebook.com/v2.10/kodkollektivet/events?access_token="
                             %token)

              #:streaming? #t)))
        (if (equal? (response-code res) 200)
          (let ((j (json-string->scm (utf8->string (read-response-body res)))))
            ;; (pp-hashtable j)
            (hash-ref j "data")))))

(get-fb-access-token)
(display %token)
(newline)
(display %token-type)
(newline)
(newline)
(newline)

(map (lambda (x) (pp-hashtable x)) (get-kodkollektivet-events))
