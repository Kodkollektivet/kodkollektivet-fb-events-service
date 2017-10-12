;; TODO: Handle errors somehow

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

(define (pp-event-list lst)
  "Pretty print a list of scm objects"
  (map
   (lambda(x)
     (display (scm->json x #:pretty #t)))
   lst))

(define (app-variables-set?)
  "Returns #true is app id/secret is set."
  (and
   (not (string-null? %app-id))
   (not (string-null? %app-secret))))

(define (tokens-set?)
  "Returns #true if token variables are set"
  (and
   (not (string-null? %token))
   (not (string-null? %token-type))))

(define (fb-access-token-link)
  "Returns the link to get the token."
  (string-append "https://graph.facebook.com/oauth/access_token?client_id="
                 %app-id
                 "&client_secret="
                 %app-secret
                 "&grant_type=client_credentials"))

(define (fb-event-link)
  "Returns the link to our events."
  (string-append "https://graph.facebook.com/v2.10/kodkollektivet/events?access_token="
                 %token))

(define (get-fb-access-token!)
  (if (app-variables-set?)
      (let ((res (http-get (fb-access-token-link) #:streaming? #t)))
        (if (equal? (response-code res) 200)
            (let ((j (json-string->scm (utf8->string (read-response-body res)))))
              (set! %token (hash-ref j "access_token"))
              (set! %token-type (hash-ref j "token_type")))
            (begin  ;; Request failed
              (display "Something when wrong!")(newline)
              (display res))))))

(define (events->scm)
  "Returns a list with scm objects (hashtable)."
  (if (tokens-set?)
      (let ((res (http-get (fb-event-link) #:streaming? #t)))
        (if (equal? (response-code res) 200)
            (let ((j (json-string->scm (utf8->string (read-response-body res)))))
              (hash-ref j "data"))))))

(define (events->json-string)
  (if (tokens-set?)
      (let ((res (http-get (fb-event-link) #:streaming? #t)))
        (if (equal? (response-code res) 200)
            (let ((j (json-string->scm (utf8->string (read-response-body res)))))
              (scm->json-string (hash-ref j "data")))))))

(define (events->file)
  (if (tokens-set?)
      (let ((res (http-get (fb-event-link) #:streaming? #t)))
        (if (equal? (response-code res) 200)
            (let ((j (json-string->scm (utf8->string (read-response-body res)))))
              (define o (open-output-file "events.txt"))
              (map
               (lambda(x)
                 (write (scm->json-string x) o)
                 (newline o))
               (hash-ref j "data"))
              (close-output-port o))))))

(define (event-id event)
  "Return event id."
  (hash-ref event "id"))

(get-fb-access-token!)

(define (scm-events->file fname lst)
  "Write a list of scm objects to file, each item on a new line."
  (define o (open-output-file fname))
  (map (lambda (x)
         (display (scm->json-string x) o)
         (newline o))
       lst)
  (close-output-port o))

(scm-events->file "./events.txt" (events->scm))
