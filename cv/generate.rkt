#lang racket

(require xml)

(define (venue->long-name venue)
  (match (second venue)
    ['asplos "International Conference on Architectural Support for Programming Languages and Operating Systems"]
    ['aaai "AAAI Conference on Artificial Intelligence"]
    ['vldb "International Conference on Very Large Data Bases"]
    ['popl "Symposium on Principles of Programming Languages"]
    ['icfp "International Conference on Functional Programming"]
    ['pldi "Programming Language Design and Implementation"]
    ['scheme "Scheme Workshop"]
    ['spsm "ACM CCS Workshop on Security and Privacy in Smartphones and Mobile Devices"]
    ['soups "Symposium on Usable Privacy and Security"]
    ['hipc "International Conference on High Performance Computing, Data, and Analytics"]
    ['eduhipc "Workshop on Education for High Performance Computing"]
    ['bar "Workshop on Binary Analysis Research"]
    ['tfp "Symposium on Trends in Functional Programming"]
    ['wflp "Workshop on Functional and Constraint Logic Programming"]
    ['pearc "Practice and Experience in Advanced Research Computing"]
    ['jfp "Journal of Functional Programming"]
    ['isc "ISC High Performance"]
    ['ics "International Conference on Supercomputing"]
    ['ia3 "Workshop on Irregular Applications: Architectures and Algorithms"]
    ['hps "Workshop on High Performance Storage"]
    ['hpdc "International ACM Symposium on High-Performance Parallel and Distributed Computing"]
    ['cc "International Conference on Compiler Construction"]
    ['cluster "IEEE International Conference on Cluster Computing"]
    ['csf "IEEE Computer Security Foundations Symposium"]
    ['atc "USENIX ATC"]
    ['utah "University of Utah"]
    [_ "???"]))

(define (venue->short-name venue)
  (match (second venue)
         ['asplos "ASPLOS"]
	 ['aaai "AAAI"]
	 ['vldb "VLDB"]
	 ['popl "POPL"]
         ['icfp "ICFP"]
         ['pldi "PLDI"]
         ['scheme "SW"]
         ['spsm "SPSM"]
         ['soups "SOUPS" ]
         ['hipc "HiPC" ]
	 ['eduhipc "EduHiPC" ]
         ['isc "ISC"]
	 ['pearc "PEARC"]
         ['ics "ICS"]
	 ['hps "HPS"]
	 ['ia3 "IA3"]
	 ['hpdc "HPDC"]
         ['bar "BAR"]
         ['tfp "TFP"]
         ['jfp "JFP"]
         ['wflp "WFLP"]
         ['cc "CC"]
	 ['cluster "CLUSTER"]
         ['csf "CSF"]
	 ['atc "USENIX ATC"]
         ['utah "U of U"]
         [_ "??"]))

(define (pub->date-ord pub)
  (define dcl (first (filter (lambda (p) (and (list? p) (equal? 'date (car p)))) pub)))
  (if (< (length dcl) 3)
      (* 100 (second dcl))
      (+ (* 100 (third dcl)) (second dcl))))

(define (date-name date)
  (if (= 3 (length date))
      (format "~a ~a"
              (list-ref '(months Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec) (second date))
              (third date))
      (format "~a" (second date))))

(define (date->year date)
  (last date))

(define (pub-is-student? pub)
  (and (> (date->year (pub->date pub)) 2019)
       (not (equal? (first (pub->authors pub)) "Thomas Gilray"))
       (not (equal? (first (pub->authors pub)) "Sidharth Kumar"))
       (not (equal? (first (pub->authors pub)) "Kristopher Micinski"))))

(define (pub->title pub)
  (define lst (filter (lambda (p) (and (list? p) (> (length p) 1) (equal? 'title (car p)))) pub))
  (if (null? lst)
      #f
      (second (first lst))))

(define (pub->authors pub)
  (rest (first (filter (lambda (p) (and (list? p) (equal? 'authors (car p)))) pub))))

(define (pub->date pub)
  (first (filter (lambda (p) (and (list? p) (equal? 'date (car p)))) pub)))

(define (pub->venue pub)
  (first (filter (lambda (p) (and (list? p) (or (equal? 'conf (car p))
                                                (equal? 'work (car p))
                                                (equal? 'journal (car p))
                                                (equal? 'phd (car p)))))
                 pub)))

(define (pub->selected? pub)
  (not (null? (filter (lambda (p) (and (list? p) (equal? 'selected (car p)))) pub))))

(define (pub->insubmission? pub)
  (not (null? (filter (lambda (p) (and (list? p) (equal? 'submitted (car p)))) pub))))

(define (pub->awards pub)
  (define awards (filter (lambda (p) (and (list? p) (equal? 'awards (car p)))) pub))
  (if (null? awards)
      '()
      (rest (first awards))))

(define (pub->talk pub)
  (define talks (filter (lambda (p) (and (list? p) (equal? 'talk (car p)))) pub))
  (if (= 1 (length talks))
      (second (first talks))
      #f))

(define (pub->pdfs pub)
  (filter (lambda (p) (and (list? p) (or (equal? 'pdf (car p))
                                         (equal? 'conf-pdf (car p))
                                         (equal? 'journal-pdf (car p))
                                         (equal? 'invited-journal-pdf (car p))
                                         (equal? 'extended-pdf (car p)))))
          pub))


;; Read in pubs
(define pubs (filter pub->title (with-input-from-file "pubs.sexpr" read)))

;; Write out website's main publication list
(define pub->li
  (lambda (pub)
    (define venue (pub->venue pub))
    (define other-authors
      (filter (lambda (x) (not (equal? x "Thomas Gilray")))
              (pub->authors pub)))
    `(li ()
         (span ([class "title"]) ,(pub->title pub))
         ,@(match (length other-authors)
                  [0 '(". ")]
                  [1 (list (string-append " with " (first other-authors) ". "))]
                  [_ (list (string-join
                            other-authors
                            ", "
                            #:before-first " with "
                            #:before-last ", and "
                            #:after-last ". "))])
         ,(string-append (venue->long-name venue) ". ")
	 "("
	 (b ,(venue->short-name venue))
	 ,(if (>= (length venue) 3) (format "—~a% acceptance" (last venue)) "") 
	 ") " 
	 ,(date-name (pub->date pub))
	 ". "
	 #;
         ,(if (< (length venue) 3)
              (format "(~a) ~a. "
                      (venue->short-name venue)
                      (date-name (pub->date pub)))
              (format "(~a—~a% acceptance) ~a. "
                      (venue->short-name venue)
                      (last venue)
                      (date-name (pub->date pub))))
         ,@(map (lambda (awd) `(b () ,awd ".")) (pub->awards pub))
         ,(if (pub->insubmission? pub) " (In submission) " " ")
         ,@(map (lambda (pdfcl)
                  `(a ([href ,(string-append "/pdf/" (second pdfcl))])
                      ,(match (first pdfcl)
                              ['pdf "(pdf)"]
                              ['extended-pdf "(extended version)"]
                              ['journal-pdf "(journal version)"]
                              ['invited-journal-pdf "(invited journal version)"]
                              ['conf-pdf "(conference version)"])))
                (pub->pdfs pub)))))

(with-output-to-file
 "all-pubs.html"
 (lambda ()
   (map (lambda (li)
          (display (xexpr->string li)))
        (map pub->li 
             (sort pubs > #:key pub->date-ord)))
   (void))
 #:exists 'replace)

(with-output-to-file
 "sel-pubs.html"
 (lambda ()
   (map (lambda (li)
          (display (xexpr->string li)))
        (map pub->li 
             (filter pub->selected?
                     (sort pubs > #:key pub->date-ord))))
   (void))
 #:exists 'replace)

(define (print-latex-pubs filename pubs-list)
  (with-output-to-file
   filename
   (lambda ()
     (map (lambda (pub)
            (define venue (pub->venue pub))
            (display (format "\\paper.~a \\textit{~a.}\n~a\n~a.\n\\\\(~a~a) ~a. ~a\n\\\\~a"
			     (if (pub-is-student? pub) "$\\dagger$" "")
                             (pub->title pub)
                             (let ([authors (map (lambda (auth) (if (equal? auth "Thomas Gilray") "\\textbf{Thomas Gilray}" auth))
                                                 (pub->authors pub))])
                               (match (length authors)
                                      [1 (string-append (first authors) ".") ] 
                                      [2 (string-append (first authors) " and " (second authors) ".")]
                                      [_ (string-join
                                         authors
                                          ", "
                                          #:before-last ", and "
                                          #:after-last ".")]))
                             (venue->long-name venue)
                             (venue->short-name venue)
                             (if (< (length venue) 3)
                                 ""
                                 (format "---~a\\% acceptance" (last venue)))
                             (date-name (pub->date pub))
                             (if (pub->insubmission? pub) "(In submission)" "")
			     (let ([awards (pub->awards pub)])
			       (if (null? awards)
				   " \\vspace{-0.1cm}\\\\\n"
				   (format "\\textbf{~a.} \\\\ \\vspace{-0.1cm}\\\\\n"
					   (first awards)))))))
          pubs-list)
     (void))
   #:exists 'replace))


(print-latex-pubs "journal-pubs.tex"
                  (filter (lambda (pub)
                            (equal? 'journal (first (pub->venue pub))))
                          (sort pubs > #:key pub->date-ord)))

(print-latex-pubs "conference-pubs.tex"
                  (filter (lambda (pub)
                            (equal? 'conf (first (pub->venue pub))))
                          (sort pubs > #:key pub->date-ord)))

(print-latex-pubs "workshop-pubs.tex"
                  (filter (lambda (pub)
                            (equal? 'work (first (pub->venue pub))))
                          (sort pubs > #:key pub->date-ord)))

(print-latex-pubs "thesis-pubs.tex"
                  (filter (lambda (pub)
                            (equal? 'phd (first (pub->venue pub))))
                          (sort pubs > #:key pub->date-ord)))


;; Students
(define (student->name stu)
  (define names (filter (lambda (s) (and (list? s) (equal? 'name (car s)))) stu))
  (if (null? names)
      #f
      (second (first names))))

(define (student->start stu)
  (define years (filter (lambda (s) (and (list? s) (equal? 'start-year (car s)))) stu))
  (second (first years)))

(define (student->end stu)
  (define years (filter (lambda (s) (and (list? s) (equal? 'end-year (car s)))) stu))
  (if (null? years)
      'present
      (second (first years))))

(define (student->website stu)
  (define sites (filter (lambda (s) (and (list? s) (equal? 'website (car s)))) stu))
  (if (null? sites)
      #f
      (second (first sites))))

(define (student->note stu)
  (define notes (filter (lambda (s) (and (list? s) (equal? 'note (car s)))) stu))
  (if (null? notes)
      #f
      (second (first notes))))

(define (student->kind stu)
  (define kinds (filter (lambda (s) (and (list? s) (equal? 'kind (car s)))) stu))
  (if (null? kinds)
      #f
      (second (first kinds))))

(define (student->advisor stu)
  (define adv (filter (lambda (s) (and (list? s) (equal? 'advisor (car s)))) stu))
  (not (null? adv)))

(define (student->committee stu)
  (define adv (filter (lambda (s) (and (list? s) (equal? 'committee (car s)))) stu))
  (not (null? adv)))

(define (student->mentor stu)
  (define adv (filter (lambda (s) (and (list? s) (equal? 'mentor (car s)))) stu))
  (not (null? adv)))

(define (format-student-date start end)
  (match end
         ['present (format "~a--present" start)]
         [end #:when (= start end) (format "~a" start)]
         [_ (format "~a--~a" start end)]))

; Read students.sexpr
(define students (filter student->name (with-input-from-file "students.sexpr" read)))

(define (print-one-student stu)
  (display (format "\\item ~a ~a (~a)~a\n"
                   (format-student-date (student->start stu) (student->end stu))
                   (student->name stu)
		   (student->kind stu)
                   (if (student->note stu)
                       (format "---~a" (student->note stu))
                       ""))))

; Print students.tex
(with-output-to-file
   "students.tex"
  (lambda ()
     (display "\\paragraph{MS/PhD Advisees (As Committee Chair)}\n")
     (display "\\begin{itemize}\n\\vspace{0.15cm}")
     (map (lambda (stu)
	    (when (student->advisor stu)
              (print-one-student stu)))
          (sort students > #:key student->start))
     (display "\\end{itemize}\n")
     
     (display "\\paragraph{MS/PhD Advisees (As Committee Member)}\n")
     (display "\\begin{itemize}\n\\vspace{0.15cm}")
     (map (lambda (stu)
	    (when (and (eq? 'PhD (student->kind stu)) (student->committee stu))
              (print-one-student stu)))
          (sort students > #:key student->start))
     (display "\\end{itemize}\n")

     (display "\\paragraph{Other Mentees}\n")
     (display "\\begin{itemize}\n\\vspace{0.15cm}")
     (map (lambda (stu)
	    (when (student->mentor stu)
              (print-one-student stu)))
          (sort students > #:key student->start))
     (display "\\end{itemize}\n")

     (void))
   #:exists 'replace)


; Read students.sexpr
(define awards (foldl (lambda (pub lst)
                        (foldl (lambda (a lst)
                                 (cons `[(name ,a)
                                         (year ,(date->year (pub->date pub)))]
                                       lst))
                               lst
                               (pub->awards pub)))
                      (with-input-from-file "awards.sexpr" read)
                      pubs))

(define (award->name stu)
  (define names (filter (lambda (s) (and (list? s) (equal? 'name (car s)))) stu))
  (second (first names)))

(define (award->year stu)
  (define years (filter (lambda (s) (and (list? s) (equal? 'year (car s)))) stu))
  (second (first years)))

; Print students.tex
(with-output-to-file
   "awards.tex"
  (lambda ()
     (display "\\begin{itemize}\n")
     (map (lambda (a)
            (display (format "\\item ~a (~a).\n"
                             (award->name a)
                             (award->year a))))
          (sort awards > #:key award->year))
     (display "\\end{itemize}\n")
     (void))
   #:exists 'replace)

; Print contributed-talks.tex
(with-output-to-file
 "contributed-talks.tex"
 (lambda ()
   (display "\\begin{itemize}\n\\itemsep=0.1cm\n")
   (map (lambda (pub)
	  (define talk (pub->talk pub))
	  (when talk
            (display (format "\\item ~a. ~a. ~a ~a.\n"
			     (pub->title pub)
			     talk
			     (venue->short-name (pub->venue pub))
			     (last (pub->date pub))))))
        (sort pubs > #:key pub->date-ord))
   (display "\\end{itemize}\n")
   (void))
 #:exists 'replace)



(system "xelatex gilray-cv")


