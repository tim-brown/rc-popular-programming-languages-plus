#lang racket

(require "wikimedia-api.rkt")

(require racket/date
         xml
         xml/path
         json
         (only-in srfi/1 lset-difference lset-union)
         net/url)

(define category-rx #px"title=\"Category:(.+?)\".+\\(([0-9]+(,[0-9][0-9][0-9])*) member")

(date-display-format 'iso-8601)
(and (putenv "TZ" "GMT") (void))

(define contact-tim
  '(div "Contact "(a ((href "mailto:tim@timb.net"))" tim@timb.net")
        ", who might be able to clean this up..."))

(define categories-url (wikimedia-api:index-query-url `((title . "Special:Categories")
                                                        (limit . "5000"))))

(define extract-html-body
  (match-lambda
    [(list-no-order (element _ _ 'html _ (list-no-order (element _ _ 'body _ bdy) _rest ...))
                    _root-rest ...)
     `(div ((class "body")) ,@(map xml->xexpr bdy))]
    [(and `(div ((class ,(or "body" "unmatched"))) ,_ ...) pre-baked) pre-baked]
    [(app ~s else) `(div ((class "unmatched")) (pre ,else))]))

(define (category-members-jsexpr->programming-language-set js)
  (for/set ([h (in-list js)])
    (match (hash-ref h 'title)
      ((regexp "^.........(.*)" (list _ ttl-9)) ttl-9)
      (else (string-append "unmatched title: " else)))))
  
(define (get-programming-languages)
  (let ((query-result (wikimedia-api:query-categorymembers "Programming_Languages")))
    (if (jsexpr? query-result)
        (category-members-jsexpr->programming-language-set query-result)
        (extract-html-body query-result))))

(define (fetch-result)
  (match (get-programming-languages)
    [(? set? p-ls)
     (for*/list ([l (in-port read-line (get-pure-port categories-url))]
                 [res (in-value (regexp-match category-rx l))]
                 #:when res #:when (set-member? p-ls (cadr res)))
       (cons (cadr res) (string->number (regexp-replace* "," (caddr res) ""))))]
    [else else]))

(define (string<?/titles a b)
  (let/ec ec (string<? (hash-ref a 'title (λ () (ec #f)))
                       (hash-ref b 'title (λ () (ec #t))))))

(define (string=?/titles a b)
  (let/ec ec (string=? (hash-ref a 'title (λ () (ec #f)))
                       (hash-ref b 'title (λ () (ec #f))))))

(define (build-language-list)
  #f)

(define (result->rows result)
    (define-values (rows _)
      (for/fold ((lst null) (last-cnt 0))
                ([lang (in-list result)]
                 [place (in-naturals 1)])
        (define cnt (cdr lang))
        (define row `(tr ((class ,(car lang)))
                         (td ,(if (= cnt last-cnt) "=" (~a place)))
                         (td ,(~a cnt))
                         (td ,(car lang))))
        (values (cons row lst) cnt)))
    (reverse rows))

  (define (result->table result)
    `(table (tr (th "#") (th "Count") (th "Name")) "\n"
      ,@(result->rows result)))

(define (make-competition-table other-languages
                                tasks-not-implemented-in-racket
                                lang->non-racket-tasks)
  `(table
    (thead
     (tr (th "Task not implemented in Racket") (th "#") ,@(for/list ((l other-languages)) `(th ,l)))
     ,@(for/list ((n tasks-not-implemented-in-racket))
         (let* ((dot-candidates
                (for/list ((l other-languages))
                  (and (member n (hash-ref lang->non-racket-tasks l))
                       `(td ((class "linkdot"))
                            (a ((href ,(format "https://rosettacode.org/?curid=~a#~a"
                                               (hash-ref n 'pageid) l)))
                               bullet)))))
                (priority-class (string-append "p" (number->string (- (length other-languages)
                                                                      (length dot-candidates))))))
           `(tr ((class ,priority-class))
                (th ((class "notracket"))
                    (a ((href ,(format "https://rosettacode.org/?curid=~a" (hash-ref n 'pageid))))
                       ,(hash-ref n 'title)))
                ,@(cons `(td ,(number->string (length (filter values dot-candidates))))
                        (map (λ (maybe-dot) (or maybe-dot `(td nbsp))) dot-candidates))))))))

(define (fetch/build-results-table)
  (define sorted-result
    (let ((result (fetch-result)))
      (log-info "result is:~a jsexpr?:~a" result (jsexpr? result))
      (sort result > #:key cdr)))
  
  (define racket-cats (wikimedia-api:query-categorymembers "Racket"))
  (define racket-cats:ns-0 (filter wikimedia-api:ns=0? racket-cats))

  (define other-languages (for/list ((l (sequence-map car sorted-result))
                                     #:break (equal? l "Racket")) l))
  (define langauge->tasks-implemented-in-language-not-racket
    (for/hash ((lang other-languages))
      (define lang-cats (filter wikimedia-api:ns=0? (wikimedia-api:query-categorymembers lang)))
      (define lang-not-racket (lset-difference string=?/titles lang-cats racket-cats:ns-0))
      (values lang lang-not-racket)))
  
  (define tasks-not-implemented-in-racket
    (sort (apply lset-union string=?/titles
                 (hash-values langauge->tasks-implemented-in-language-not-racket))
          string<?/titles))
  
  (define competition-tbl (make-competition-table other-languages
                                                  tasks-not-implemented-in-racket
                                                  langauge->tasks-implemented-in-language-not-racket))

  (if (dict? sorted-result)
      `((table (tr (td ,(result->table sorted-result)) (td ,competition-tbl))))
      (list (extract-html-body sorted-result) "\n" contact-tim)))

;;---------------------------------------------------------------------------------------------------
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

(define (write-report xdoc)
  (void (write-xexpr xdoc #:insert-newlines? #t)))

(let ((report (generate-xml-doc)))
  (with-output-to-file "super-popular.html" #:exists 'replace (λ () (write-report report)))
  (write-report report))
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

