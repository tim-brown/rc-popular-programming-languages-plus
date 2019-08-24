#lang racket
(require json net/url html)

(provide wikimedia-api:query-list
         wikimedia-api:scheme
         wikimedia-api:host
         wikimedia-api:path
         wikimedia-api:index-path
         wikimedia-api:query-url
         wikimedia-api:index-query-url
         wikimedia-api:query-categorymembers
         wikimedia-api-logger
         
         wikimedia-api:ns=0?)

(define-logger  wikimedia-api)
(current-logger wikimedia-api-logger)

(let* ((prxy (getenv "http_proxy")) (prxy-url (and prxy (string->url prxy))))
  (when prxy-url
    (current-proxy-servers
     (list
      (list (url-scheme prxy-url)
            (url-host prxy-url)
            (url-port prxy-url))))
    (log-info "current-proxy-servers set to ~s~%" (current-proxy-servers))))

(define wikimedia-api:scheme (make-parameter "http"))
(define wikimedia-api:host (make-parameter "rosettacode.org"))
(define wikimedia-api:path
  (make-parameter (list (make-path/param "mw" empty)
                        (make-path/param "api.php" empty))))
(define wikimedia-api:index-path
  (make-parameter (list (make-path/param "mw" empty) (make-path/param "index.php" empty))))

(define (wikimedia-api:query-url q)
  (make-url (wikimedia-api:scheme) #f (wikimedia-api:host) #f #t (wikimedia-api:path) q #f))

(define (wikimedia-api:index-query-url q)
  (make-url (wikimedia-api:scheme) #f (wikimedia-api:host) #f #t (wikimedia-api:index-path) q #f))

(define (fetch-json url)
  ; better to read the JSON directly using this function... but that means that if there is
  ; a problem (like a redirect); then we'll need to read again -- an hardship suffered only
  ; exceptionally
  (with-handlers
      ([exn:fail? (λ (x) (error "fetch-json general falure caught: ~s" x))]
       [exn? (λ (x) (error "fetch-json general exception caught: ~s" x))])
    (with-handlers
        ([exn:fail:read? (λ (x)
                           (define port-contents (port->string (get-pure-port url)))
                           (log-error "fetch-json read failure: ~s port-contents: ~s"
                                      (exn-message x) port-contents)
                           (define hdoc (read-html-as-xml (open-input-string port-contents)))
                           hdoc)])
      (read-json (get-pure-port url)))))

(define (wikimedia-api:query-list Q)
  (define result-key (string->symbol (cdr (assoc 'list Q))))
  (define (inr q rslt (depth 0))
    (log-debug "inner query: ~s" q)
    (match (fetch-json (wikimedia-api:query-url q))
      #;[x (printf "query-list [~a] ~s ->~% ~s~%~%" depth q (hash-set* x
                                                                     'query '...
                                                                    ; 'warnings '...
                                                                     )) (failure-cont)]
      [(hash-table ('query qry-rslt) ((or 'continue 'query-continue) qc))
       (log-debug "continues: qc ~s" qc)
       (define r+ (cons qry-rslt rslt))
       (define q+
         (for*/fold ((q+ q)) ((qc-lists (in-hash-values qc)) ([qc-k qc-v] (in-hash qc-lists)))
           (cons (cons qc-k qc-v) (remove qc-k q+ (λ (v x) (eq? v (car x)))))))
       (inr q+ r+ (add1 depth))]
      [(or
        (hash-table ('query qry-rslt))
        (hash-table ('batchcomplete "") ('continue _) ('query qry-rslt))
        )
       (cons qry-rslt rslt)]
      [(hash-table ('error (or (hash-table ('info error-info) _ ...) error-info)))
       (error 'wikimedia-api:query-list "~a" error-info)]
      [unmatched (error 'wikimedia-api:query-list "unmatched result: ~s" unmatched)]))
  (define rlst-parts (reverse (inr Q null 0)))
  (for/fold ((list-members null)) ((r rlst-parts))
    (define part-members (hash-ref r result-key))
    (append list-members part-members)))

(define (wikimedia-api:query-categorymembers category . more-query-alist)
  (define qry
    `((action . "query")
      (rawcontinue . "")
      (list   . "categorymembers")
      (cmtitle . ,(match category [(regexp "^Category:" (list c)) c]
                    [c (string-append "Category:" c)]))
      (cmlimit . "500")
      (format  . "json")))  
  (wikimedia-api:query-list (append qry more-query-alist)))

(define wikimedia-api:ns=0? (match-lambda [(hash-table ('ns 0) _ ...) #t] [_ #f]))

(module+ test
  (wikimedia-api:query-categorymembers "Programming Languages"))