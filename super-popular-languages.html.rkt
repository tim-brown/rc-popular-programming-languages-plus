#lang racket

(require "wikimedia-api.rkt")

(require racket/date
         xml
         xml/path
         json
         (only-in srfi/1 lset-difference lset-union)
         net/url)

(date-display-format 'iso-8601)
(and (putenv "TZ" "GMT") (void))

(define current-max-programming-languages (make-parameter 2000))

#;(define proglangs-qry `((action . "query")
                        (list   . "categorymembers")
                        (cmtitle . "Category:Programming_Languages")
                        (cmlimit . ,(number->string (current-max-programming-languages)))
                        (format  . "json")))

(define categories-url (wikimedia-api:index-query-url `((title . "Special:Categories")
                                                        (limit . "5000"))))

(define extract-html-body
  (match-lambda
    [(list-no-order (element _ _ 'html _ (list-no-order (element _ _ 'body _ bdy) rest ...))
                    toplevel-rest ...)
     `(div ((class "body")) ,@(map xml->xexpr bdy))]
    [(and `(div ((class ,(or "body" "unmatched"))) ,_ ...) pre-baked) pre-baked]
    [(app ~s else) `(div ((class "unmatched")) (pre ,else))]))

(define (get-programming-languages)
  (match (wikimedia-api:query-categorymembers "Programming_Languages")
    [(? jsexpr? js)
     (for/set ([h (in-list js)])
       (match (hash-ref h 'title)
         ((regexp "^.........(.*)" (list _ ttl-9)) ttl-9)
         (else (string-append "unmatched title: " else))))]
    [(app extract-html-body else) else]))

(define category-rx #px"title=\"Category:(.+?)\".+\\(([0-9]+(,[0-9][0-9][0-9])*) member")

(define (fetch-result)
  (match (get-programming-languages)
    [(? set? p-ls)
     (for*/list ([l (in-port read-line (get-pure-port categories-url))]
                 [res (in-value (regexp-match category-rx l))]
                 #:when res #:when (set-member? p-ls (cadr res)))
       (cons (cadr res) (string->number (regexp-replace* "," (caddr res) ""))))]
    [else else]))

(define (string<?/titles a b)
  (let/ec ec (string<? (hash-ref a 'title (lambda () (ec #f)))
                       (hash-ref b 'title (lambda () (ec #t))))))

(define (string=?/titles a b)
  (let/ec ec (string=? (hash-ref a 'title (lambda () (ec #f)))
                       (hash-ref b 'title (lambda () (ec #f))))))

(define (fetch/build-results-table)
  (define result (fetch-result))
  (define sorted-result (sort result > #:key cdr))
  
  (define racket-cats (wikimedia-api:query-categorymembers "Racket"
                                                           #;'(cmsort . "sortkey")
                                                           #;'(cmstartsortkeyprefix . "C")
                                                           #;'(cmendsortkeyprefix . "D")
                                                           ))
  (define racket-cats:ns-0 (filter ns=0? racket-cats))

  (define other-languages '("Go" "Perl 6" "Julia" "Phix" "Python" "Kotlin" "Perl"))
  (define other-implemented
    (for/hash ((lang other-languages))
      (define lang-cats (filter ns=0? (wikimedia-api:query-categorymembers lang)))
      (define lang-not-racket (lset-difference string=?/titles lang-cats racket-cats:ns-0))
      (values lang lang-not-racket)))
  
  (define not-rkt
    (sort (apply lset-union string=?/titles (hash-values other-implemented)) string<?/titles))
  
  (define (result->rows result)
    (define-values (rows _)
      (for/fold ((lst null) (last-cnt 0))
        ([lang (in-list sorted-result)]
         [place (in-naturals 1)])
        (define cnt (cdr lang))
        (define row `(tr ((class ,(car lang)))
                         (td ,(if (= cnt last-cnt) "=" (~a place)))
                         (td ,(~a cnt))
                         (td ,(car lang))))
        (values (cons row lst) cnt)))
    (reverse rows))
  
  (define (result->table r)
    (define result-rows (result->rows r))
    `(table
      (tr (th "#") (th "Count") (th "Name"))
      "\n"
      ,@result-rows))
  
  (log-info "result is:~a jsexpr?:~a" result (jsexpr? result))
  
  (define competition-tbl
    `(table
      (thead
       (tr (th "Task not implemented in Racket")
           ,@(for/list ((l other-languages)) `(th ,l)))
       ,@(for/list ((n not-rkt))
           `(tr (th ((class "notracket"))
                    (a ((href ,(format "https://rosettacode.org/?curid=~a" (hash-ref n 'pageid))))
                       ,(hash-ref n 'title)))
                ,@(for/list ((l other-languages))
                    (if (member n (hash-ref other-implemented l))
                        `(td ((class "linkdot"))
                             (a ((href ,(format "https://rosettacode.org/?curid=~a#~a"
                                                (hash-ref n 'pageid) l)))
                                bullet))
                        `(td nbsp))))))))
  
  (match result
    [(? dict? (app result->table tbl1))
     (list `(table (tr (td ,tbl1) (td ,competition-tbl))))]
    [(app extract-html-body else) (list else "\n" contact-tim)]))

;;---------------------------------------------------------------------------------------------------
(define contact-tim
  '(div "Contact "(a ((href "mailto:tim@timb.net"))" tim@timb.net")
        ", who might be able to clean this up..."))

(define (generate-xml-doc)
  `(html
    (head
     (link ((rel "stylesheet") (type "text/css") (href "/css/super-popular.css")))"\n"
     (meta ((charset "UTF-8")))
     (title "Popular Programming Languages: Rosetta Code"))
    "\n"
    (body
     (h1 "Rosetta Code: Popular Programming Languages") "\n"
     (h2 "Generated: ",(date->string (current-date) #t)) "\n"
     ,@(fetch/build-results-table))))

(define (generate-report)
  (void (write-xexpr (generate-xml-doc) #:insert-newlines? #t)))

;; (with-output-to-file "super-popular.html" #:exists 'replace generate-report)
(generate-report)
;;---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit)
  (check-equal? (url->string categories-url)
                (string-append "http://rosettacode.org/mw/index.php?"
                               "title=Special%3ACategories&limit=5000"))
  
  ;(get-programming-languages)
  ;(fetch/build-results-table)
  
  #;(define cats
      (for*/list ([l (in-port read-line (get-pure-port categories-url))]
                  #:unless (regexp-match #px"Basic language learning"           l)
                  #:unless (regexp-match #px"Encyclopedia"                      l)
                  #:unless (regexp-match #px"Implementations"                   l)
                  #:unless (regexp-match #px"Language Implementations"          l)
                  #:unless (regexp-match #px"Language users"                    l)
                  #:unless (regexp-match #px"Maintenance/OmitCategoriesCreated" l)
                  #:unless (regexp-match #px"Programming Languages"             l)
                  #:unless (regexp-match #px"Programming Tasks"                 l)
                  #:unless (regexp-match #px"RCTemplates"                       l)
                  #:unless (regexp-match #px"Solutions by Library"              l)
                  #:unless (regexp-match #px"Solutions by Programming Language" l)
                  #:unless (regexp-match #px"Solutions by Programming Task"     l)
                  #:unless (regexp-match #px"Unimplemented tasks by language"   l)
                  #:unless (regexp-match #px"WikiStubs"                         l)
                  #:unless (regexp-match #px"Examples needing attention"	l)
                  #:unless (regexp-match #px"Impl needed"			l)	
                  [res (in-value (regexp-match category-rx l))]
                  #:when res)
        (format "~s ~s~%" l res))))

