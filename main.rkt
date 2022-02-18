#lang racket/base

(require racket/bool
         racket/cmdline
         racket/date
         racket/exn
         racket/function
         racket/list
         racket/match
         racket/port
         racket/logging
         racket/format
         racket/path
         racket/string
         db
         openssl/sha1
         net/url
         web-server/web-server
         web-server/http
         web-server/servlet-env
         web-server/templates
         "vendored/mime.rkt")

(define schema-version 2)
(define db-conn (make-parameter #f))

(define-logger rpaste)

(define-syntax-rule (send-output a ...)
  (λ (op)
    (parameterize ([current-output-port op])
      a ...)
    (void)))

(define site-title "rpaste")

(define (start req)
  (log-rpaste-info "~a ~a" (request-method req) (request-uri req))
  (match (request-method req)
    [#"HEAD" (make-head (start (struct-copy request req [method #"GET"])))]
    [#"GET" (route-get req)]
    [#"POST" (make-paste req)]
    [m (mk-bad (format "Method ~a not allowed." m)
               #:code 405
               #:message #"Method Not Allowed")]))

(define (route-get req)
  (match (map path/param-path (url-path (request-uri req)))
    [(or (? null?)
         (list "" ...)
         (list "list"))
     (list-pastes req)]
    [(list "form") (paste-form req)]
    [(list-rest "static" paths) (send-static req)]
    [_ (show-paste req)]))

(define (send-static req)
  (define res #f)
  (with-handlers ([exn:fail:filesystem? (λ (ex) (set! res (mk-bad
                                                           (exn->string ex)
                                                           #:code 404
                                                           #:message #"Not Found")))])
    (define pth (apply build-path (map path/param-path (url-path (request-uri req)))))
    (define ip (open-input-file pth))
    (set! res (mk-gud (curry copy-port ip) #:mime (path->mime-type pth))))
  res)

(define (make-head res)
  (struct-copy response res [output void]))

(define (mk-bad txt #:code [code 404] #:message [msg #"Not found"] #:mime [mime #"text/plain"])
  (response/output (λ (op)
                     (write-string txt op)
                     (void))
                   #:code code
                   #:message msg
                   #:mime-type mime))

(define (mk-gud fn #:mime [mime #"text/plain; charset=utf-8"])
  (response/output fn #:mime-type mime))

(define (epoch->rfc2822 epoch)
  (parameterize ([date-display-format 'rfc2822])
    (date->string (seconds->date epoch #f) #t)))

(define (site-baseurl req)
  (define h (request-headers/raw req))
  (define s
    (cond
      [(headers-assq #"Forwarded" h) => (compose1 bytes->string/utf-8 header-value)]
      [(headers-assq #"X-Forwarded-For" h) => (compose1 bytes->string/utf-8 header-value)]
      [(getenv "APP_URL") => identity]
      [(headers-assq #"Host" h) => (compose1 bytes->string/utf-8 header-value)]
      [else (format "~a:~a" (request-host-ip req) (request-host-port req))]))
  (match (string-append (string-trim s "/" #:repeat? #t) "/") 
    [(regexp "https?://.*" (list m)) m]
    [no-scheme (string-append "http://" no-scheme)]))

(define (list-pastes req)
  (define rows (query-rows (db-conn) "SELECT key, timestamp FROM Pastes ORDER BY timestamp DESC"))
  (define address (site-baseurl req))
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (list (string->bytes/utf-8 (include-template "templates/homepage.html")))))

(define (paste-form req)
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (list (string->bytes/utf-8 (include-template "templates/form.html")))))

(define (show-paste req)
  (define p (map path/param-path (url-path (request-uri req))))
  (if (null? p)
      (mk-bad "Not found [no paste specified with /your_paste_here] :(")
      (let ([rows (query-rows (db-conn)
                              "SELECT paste FROM Pastes WHERE key = ?"
                              (car p))])
        (if (null? rows)
            (mk-bad (format "No paste with key [~a] :(" (car p)))
            (mk-gud (send-output (write-bytes (vector-ref (car rows) 0))))))))

(define (make-paste req)
  (define raw (request-bindings/raw req))
  (define pf (implies raw (bindings-assq #"p" raw)))
  (if (binding:form? pf)
      (let* ([data (binding:form-value pf)]
             [hash (sha1 (open-input-bytes data))])
        (query-exec (db-conn)
                    "INSERT OR IGNORE INTO Pastes (key, paste, timestamp) VALUES (?, ?, ?)"
                    hash data (current-seconds))
        (if (and raw (bindings-assq #"redirect" raw))
            (redirect-to (format "/~a" hash))
            (mk-gud (send-output (write-string (string-append hash "\n"))))))
      (response/output (send-output (write-string "Bad request. Need payload p=..."))
                       #:code 400
                       #:message #"Bad request")))

(define (fun)
  (define listen-port (make-parameter 8080))
  (define listen-ip (make-parameter #f))
  (define (normalize-path-string s)
    (path->string (simple-form-path (string->path s))))
  (define database (make-parameter (normalize-path-string (format "./rpaste~a.sqlite3" schema-version))
                                   normalize-path-string))
  (command-line
   #:once-each
   [("-p" "--port") port-string "Port to listen on"
                    (listen-port (string->number port-string))]
   [("--ip") ip-string "IP to listen on"
             (listen-ip ip-string)]
   [("-d" "--database") database-string "Database to use"
                        (database database-string)])
  (log-rpaste-debug "(listen-port) = ~a, (listen-ip) = ~a, (database) = ~a"
                    (listen-port)
                    (listen-ip)
                    (database))
  (db-conn
   (virtual-connection
    (connection-pool
     (thunk (sqlite3-connect #:database (string->path (database))
                             #:mode 'create)))))
  (query-exec (db-conn) #<<END
CREATE TABLE IF NOT EXISTS Pastes
  ("id" INTEGER PRIMARY KEY  AUTOINCREMENT  NOT NULL,
   "key" TEXT NOT NULL  UNIQUE,
   "paste" BLOB NOT NULL,
   "timestamp" INTEGER NOT NULL)
END
              )
  (serve/servlet start
                 #:stateless? #t
                 #:listen-ip (listen-ip)
                 #:port (listen-port)
                 #:servlet-path "/"
                 #:servlet-regexp #rx""
                 #:command-line? #t
                 #:server-root-path "."))

(module+ main
  (with-logging-to-port (current-error-port)
    fun
    #:logger rpaste-logger 'debug 'rpaste))
