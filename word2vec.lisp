;; Ben Kane 02-25-2019
;; Word2Vec implementation in Common Lisp
;; Supports Python calls to Gensim library via server, or rudimentary offline version implemented in Lisp.

(in-package :word2vec)

(defparameter *setup-complete* nil)
(defun setup ()
  (python-over-socket "from gensim.test.utils import *" 'exec)
  (python-over-socket "from gensim.models import Word2Vec" 'exec)
  (setq *setup-complete* t))










;; ;;; Gene Kim 11-24-2018
;; ;;; Functions for generating English text from ULF formulas.

;; (in-package :ulf2english)

;; (setq *debug-ulf2english* nil)

;; ;; Post formats a ULF-to-string mapping.
;; ;; If it is a name (e.g. |John|), the pipes are stripped off.
;; ;; If not a name replace dash and underscores with spaces.
;; ;; Otherwise, the string is made lowercase.
;; (defun post-format-ulf-string (s)
;;   (let ((sstr (if (symbolp s) (util:sym2str s) s)))
;;     (if (ulf:is-strict-name? sstr)
;;       (coerce (subseq (coerce sstr 'list) 1 (1- (length sstr)))
;;               'string)
;;       (cl-strings:replace-all
;;         (cl-strings:replace-all (string-downcase sstr) "-" " ")
;;         "_" " "))))


;; ;; TODO: probably put a version of thi in ulf-lib.
;; ;; Returns t if 'token' is an atomic ULF element that has a corresponding token
;; ;; in the surface string and return nil otherwise.  e.g.,
;; ;;   man.n -> t
;; ;;   that -> t
;; ;;   tht -> nil
;; ;;   k -> nil
;; ;;   to -> t
;; ;;   perf -> t
;; ;;   {man}.n -> nil
;; (defun is-surface-token? (token)
;;   (or (and (ulf:has-suffix? token) (not (ulf:lex-elided? token)))
;;       (ulf:is-strict-name? token)
;;       (member token'(that not and or to))))


;; ;; Maps the ULF suffix to a pure POS symbol for UPPEN MORPH.
;; ;; TODO: complete...
;; (defun suffix-to-pos (suffix)
;;   (case suffix
;;     (adv-a 'adv)
;;     (adv-e 'adv)
;;     (adv-s 'adv)
;;     (adv-f 'adv)
;;     ; Uppen morph calls auxiliaries verbs.
;;     (aux-s 'v)
;;     (aux-v 'v)
;;     ;(aux-s 'aux)
;;     ;(aux-v 'aux)
;;     (otherwise suffix)))


;; (defun pluralize! (ulf)
;; ;``````````````````````
;; ; Converts the given ULF to the plural version of the surface form.
;;     ;; TODO: handle recursive cases... (child-of.n ...)
;;     ;; TODO: handle pluralization of relational nouns [child-of.n -> children-of.n, not
;;     ;;                                                 child-of.n -> child-ofs.n]
;;   (cond
;;     ((null ulf) nil)
;;     ((not (atom ulf)) ulf)
;;     ((ulf:lex-elided? ulf) ulf)
;;     (t
;;      (multiple-value-bind (word suffix) (ulf:split-by-suffix ulf)
;;        (ulf:add-suffix
;;          (cond
;;            ;; Preserve case for strict names.
;;            ((ulf:is-strict-name? word)
;;             (intern (pattern-en-pluralize (string word) :preserve-case t)))
;;            ;; Otherwise ignore case.
;;            (t (intern (pattern-en-pluralize (string word) :preserve-case nil))))
;;          suffix)))))

;; (defun add-tense! (ulf)
;; ;``````````````````````
;; ; Converts the given ULF to the tensed surface form if the input is of the form
;; ; (tense ulf).  Otherwise, it just returns the ulf.
;; ;
;; ; e.g.
;; ;   (past run.v) -> ran.v
;; ;   (pres sleep.v) -> sleep.v
;;   (cond
;;     ;; Simple case where there's tense and a simple verb.
;;     ((and (= 2 (length ulf))
;;           (ulf:lex-tense? (first ulf))
;;           (or (verb? (second ulf))
;;               (aux? (second ulf))))
;;      (let ((tense (first ulf))
;;            (verb (second ulf)))
;;        (multiple-value-bind (word suffix) (ulf:split-by-suffix verb)
;;          (ulf:add-suffix
;;            (intern (pattern-en-conjugate (string word) :tense (ulf2pen-tense tense)))
;;            suffix))))
;;     ;; Ignore all other cases for now.
;;     (t ulf)))

;; (defun dumb-ppart (word)
;; ;```````````````````````
;; ; Takes a word symbol and uses a simple heuristic to make it into the past
;; ; participle form.
;; ;   if it ends in 'e', append 'd'
;; ;   otherwise, append 'ed'
;;   (if (atom word)
;;     (let ((letters (util:split-into-atoms word)))
;;       (if (member (car (last letters)) '(#\e #\E))
;;         (util:fuse-into-atom (append letters '(#\D)))
;;         (util:fuse-into-atom (append letters '(#\E #\D)))))
;;     ;; Just return since it's not an atom.
;;     word))

;; (defun pasv2surface! (ulf)
;; ;`````````````````````
;; ; Converts the given ULF to the pasv form if the input is of the form
;; ; (pasv ulf).  Otherwise, it returns the input directly.
;; ;
;; ; e.g.
;; ;   (pasv hit.v) -> ((past be.v) hit.v)
;; ;   (pasv confuse.v) -> ((past be.v) confused.v)
;; ;   TODO: get the inherited tense
;;   (cond
;;     ((and (= (length ulf))
;;           (eq 'pasv (first ulf))
;;           (verb? (second ulf)))
;;      (list '(past be.v) (verb-to-past-participle! (second ulf))))))

;; (defun verb-to-participle (verb &key (part-type 'PRESENT))
;; ;`````````````````````````````````
;; ; Converts the given verb symbol to participle form of the given type.
;; ; PRESENT
;; ;  run.v -> running.v
;; ;  is.v -> being.v
;; ; PAST
;; ;  run.v -> run.v
;; ;  confuse.v confused.v
;; ;   mention -> mentioned
;;    ;; TODO: enable aliases and tags in pattern-en-conjugate so we can do this
;;    ;; with "ppart".
;;   (assert (member part-type '(PRESENT PAST)))
;;   (if (and (symbolp verb) (verb? verb))
;;     (multiple-value-bind (word suffix) (ulf:split-by-suffix verb)
;;       (ulf:add-suffix
;;         (intern (pattern-en-conjugate (string word) :tense part-type
;;                                       :aspect 'PROGRESSIVE))
;;         suffix))))

;; (defun verb-to-past-participle! (verb)
;;   (verb-to-participle verb :part-type 'PAST))

;; (defun verb-to-present-participle! (verb)
;; ;`````````````````````````````````
;; ; Converts the given verb symbol to present participle form.
;; ;  run.v -> running.v
;; ;  is.v -> being.v
;;   (verb-to-participle verb :part-type 'PRESENT))

;; (defun search-head-verb (vp &key (sub nil))
;; ;``````````````````````
;; ; TODO: move this to ulf-lib
;; ; Searches vp (a ULF VP) for the main verb. If sub is not nil, sub substitutes
;; ; for the main verb.
;; ;
;; ; Returns the following values in a list
;; ;   main verb
;; ;   whether is was found
;; ;   new vp
;; ; TODO: how does this relate to auxiliaries...?
;;   (cond
;;     ;; Simple tensed or not, lexical or passivized verb.
;;     ((ttt:match-expr '(!1 lex-verb? pasv-lex-verb?
;;                           (ulf:lex-tense? (! lex-verb? pasv-lex-verb?)))
;;                      vp)
;;      (values vp t (if sub sub vp)))
;;     ;; Starts with a verb -- recurse into verb.
;;     ((and (listp vp)
;;           (or (verb? (car vp)) (tensed-verb? (car vp))))
;;      (multiple-value-bind (hv found new-carvp) (search-head-verb (car vp) :sub sub)
;;        (values hv found (cons new-carvp (cdr vp)))))
;;     ;; Starts with adv-a or phrasal sentence operator -- recurse into cdr.
;;     ((and (listp vp)
;;           (or (adv-a? (car vp)) (phrasal-sent-op? (car vp))))
;;      (multiple-value-bind (hv found new-cdrvp) (search-head-verb (cdr vp) :sub sub)
;;        (values hv found (cons (car vp) new-cdrvp))))
;;     ;; Otherwise, it's not found.
;;     (t (values nil nil vp))))


;; (defun find-head-verb (vp)
;; ;````````````````````
;; ; Finds the main verb in a ULF VP.
;;   (search-head-verb vp))


;; (defun replace-head-verb (vp sub)
;; ;```````````````````````
;; ; Find the main verb and returns a new VP with the substitute value.
;;   (multiple-value-bind (_ _ newvp) (search-head-verb vp :sub sub)
;;     newvp))


;; (defun search-head-noun (np &key (sub nil))
;; ; TODO: move this to separate file (not TTT phrasal pattern)
;; ; Searches np (a ULF NP) for the head noun. If sub is not nil, sub substitutes
;; ; for the head noun.
;; ;
;; ; Returns the following values in a list
;; ;   head noun
;; ;   whether it was found
;; ;   new np
;; ; This function treats 'plur' as part of the noun.
;;   (cond
;;     ;; Simple lexical or plural case.
;;     ((or (lex-noun? np) (lex-name? np))
;;      (values np t (if sub sub np)))
;;     ;; Basic pluralized case.
;;     ((and (listp np) (= (length np) 2) (equal 'plur (first np))
;;           (or (lex-noun? (second np)) (lex-name-pred? (second np))))
;;      (values np t (if sub sub np)))
;;     ;; Pluralized relational noun case.
;;     ((and (listp np) (= (length np) 2)
;;           (equal 'plur (first np)) (listp (second np)))
;;      (values (list 'plur (first (second np))) t
;;              (if sub (list sub (cdr (second np)))
;;                np)))
;;     ;; Noun post-modification.
;;     ;;   (n+preds ...)
;;     ;;   (n+post ...)
;;     ((ttt:match-expr '((! n+preds n+post) noun? _+) np)
;;      (let ((macro (first np))
;;            (inner-np (second np))
;;            (post (cddr np)))
;;        (multiple-value-bind (hn found new-inner-np) (search-head-noun inner-np :sub sub)
;;          (values hn found (cons macro (cons new-inner-np post))))))
;;     ;; Noun premodification.
;;     ;;  (dog.n monster.n)
;;     ;;  (happy.a fish.n)
;;     ;;  ((mod-n happy.a) fish.n)
;;     ;;  (|Rochester| landscape.n)
;;     ((ttt:match-expr '((! mod-n? noun? adj? term?) noun?) np)
;;      (let ((modifier (first np))
;;            (inner-np (second np)))
;;        (multiple-value-bind (hn found new-inner-np) (search-head-noun inner-np :sub sub)
;;          (values hn found (list modifier new-inner-np)))))
;;     ;; Phrasal sent op.
;;     ;;   (definitely.adv-s table.n)
;;     ;;   (not thing.n)
;;     ((ttt:match-expr '(phrasal-sent-op? noun?) np)
;;      (multiple-value-bind (hn found new-inner-np) (search-head-noun (second np) :sub sub)
;;        (values hn found (list (first np) new-inner-np))))
;;     ;; Otherise, noun followed by other stuff.
;;     ;;   (collapse.n (of.p-arg (the.d empire.n)))
;;     ((and (listp np) (noun? (first np)))
;;      (multiple-value-bind (hn found new-inner-np) (search-head-noun (first np) :sub sub)
;;        (values hn found (cons new-inner-np (cdr np)))))
;;     ;; If none of these, we can't find it.
;;     (t (values nil nil np))))


;; ;; TODO: move this somewhere more generic
;; ;; Returns true if the argument is a plural term.
;; ;;  (the.d (plur *.n))
;; ;;  (the.d (.... (plur *.n)))
;; ;;  they.pro
;; ;;  them.pro
;; ;;  we.pro
;; (defun plural-term? (x)
;;   (cond
;;     ;; If an atom, it's just whether it's one of the selected pronouns.
;;     ((atom x)
;;      ;; TODO: deal with "ours is better"
;;      (member x '(they.pro them.pro we.pro us.pro you.pro these.pro those.pro
;;                           both.pro few.pro many.pro several.pro all.pro any.pro
;;                           most.pro none.pro some.pro ours.pro yours.pro
;;                           theirs.pro)))
;;     ;; For terms from nouns, either the quantifier forces a plural reading, e.g. many,
;;     ;; or we check the head noun for a plural operator.
;;     ((and (listp x) (term? x) (= (length x) 2) (noun? (second x)))
;;      (let ((term-former (first x))
;;            (hn (search-head-noun (second x))))
;;        ;; TODO: deal with examples like "all water is wet"
;;        (or (member term-former '(these.d those.d both.d few.d many.d several.d))
;;            (plur-lex-noun? hn))))
;;     ;; Coordinated nouns or sets of terms are plural.
;;     ((and (listp x) (term? x) (> (length x) 2))
;;      (or (lex-set-of? (first x)) (lex-coord? (second x))))
;;     ;; Otherwise, singular.
;;     (t nil)))


;; (defun plur-lex-noun? (arg)
;; ;````````````````````
;; ; True if arg is of the form (plur <lexical noun>) and false otherwise.
;;   (and (listp arg) (= (length arg) 2)
;;        (eql 'plur (first arg)) (or (lex-noun? (second arg))
;;                                    (lex-name-pred? (second arg)))))

;; (defun pasv-lex-verb? (arg)
;; ;````````````````````
;; ; True if arg is of the form (pasv <lexical verb>) and false otherwise.
;;   (and (listp arg) (= (length arg) 2)
;;        (eql 'pasv (first arg)) (lex-verb? (second arg))))


;; (defun vp-to-participle! (vp &key (part-type nil))
;; ;```````````````````````````````
;; ; Converts a VP so that the main verb is in participle form.
;; ; If no specification is made to the participle form, it assumes that it's
;; ; present participle except in presence of a 'pasv' operator.
;; ; PRESENT PARTICIPLE
;; ;   '(sleep.v (adv-a (in.p (the.d bed.n))))
;; ;   -> '(sleeping.v (adv-a (in.p (the.d bed.n))))
;; ;   '(quickly.adv-a (look.v around.adv-a))
;; ;   -> '(quickly.adv-a (looking.v around.adv-a))
;; ; PAST PARTICIPLE
;; ;   '((pasv sleep.v) (adv-a (in.p (the.d bed.n))))
;; ;   -> '(slept.v (adv-a (in.p (the.d bed.n))))
;; ;   '(carefully.adv-a (pasv write.v))
;; ;   -> '(carefully.adv-a written.v)
;;   (assert (member part-type '(PRESENT PAST nil)))
;;   (cond
;;     ((verb? vp)
;;      (let* ((head-verb (find-head-verb vp))
;;             (participle
;;               (cond
;;                 ;; Present participle condition.
;;                 ((eql part-type 'PRESENT)
;;                  (verb-to-present-participle! head-verb))
;;                 ;; Various past participle conditions.
;;                 ((and (eql part-type 'PAST) (atom head-verb))
;;                  (verb-to-past-participle! head-verb))
;;                 ((and (eql part-type 'PAST) (pasv-lex-verb? head-verb))
;;                  (verb-to-past-participle! (second head-verb)))
;;                 ((eql part-type 'PAST)
;;                  (error "verb ~s is not a form that can become a past participle"
;;                         head-verb))
;;                 ;; Default cases when part-type is nil.
;;                 ((atom head-verb)
;;                  (verb-to-present-participle! head-verb))
;;                 ((pasv-lex-verb? head-verb)
;;                  (verb-to-past-participle! (second head-verb)))
;;                 (t (error "verb ~s is not a form that can become a past participle"
;;                           head-verb)))))
;;        (replace-head-verb vp participle)))
;;     ;; If it isn't a verb phrase, just return.
;;     (t vp)))

;; ;; Convenience functions (also for TTT).
;; (defun vp-to-past-participle! (vp)
;;   (vp-to-participle! vp :part-type 'PAST))
;; (defun vp-to-present-participle! (vp)
;;   (vp-to-participle! vp :part-type 'PRESENT))


;; (defparameter *plur2surface*
;;   '(/ (plur _!)
;;       (pluralize! _!)))
;; (defparameter *tense2surface*
;;   '(/ ((!1 ulf:lex-tense?) _!2)
;;       (add-tense! (!1 _!2))))
;; (defparameter *pasv2surface*
;;   '(/ (pasv _!)
;;       (pasv2surface! (pasv _!))))
;; (defparameter *tense-n-number2surface*
;;   '(/ ((!1 term?) (*1 phrasal-sent-op?) (!2 pred?))
;;       (!1 *1 (conjugate-head-verb! !2 !1))))
;; ;; TODO: complete
;; ;; 1. add phrasal sent ops
;; ;; 2. deal with auxiliaries
;; (defparameter *inv-tense-n-number2surface*
;;   '(/ ((!1 be.v have.v (lex-tense? be.v) (lex-tense? have.v)
;;            aux? (lex-tense? aux?))
;;        (!2 term?) (!3 pred?))
;;       ((conjugate-head-verb! !1 !2) !2 !3)))
;; ;     ((!1 ulf:lex-tense?) _!2)
;; ;      (add-tense! (!1 _!2))))
;; (defparameter *exist-there-tense-n-number2surface*
;;   '(/ (there.pro ((!1 lex-verb? (lex-tense? lex-verb?))
;;                   (*1 phrasal-sent-op?)
;;                   (!2 term?)
;;                   (*2 phrasal-sent-op?)))
;;       (there.pro ((conjugate-head-verb! !1 !2) *1 !2 *2))))
;; (defparameter *inv-exist-there-tense-n-number2surface*
;;   '(/ ((!1 lex-verb? (lex-tense? lex-verb?))
;;        there.pro
;;        (*1 phrasal-sent-op?)
;;        (!2 term?)
;;        (*2 phrasal-sent-op?))
;;       ((conjugate-head-verb! !1 !2) there.pro *1 !2 *2)))


;; (defun conjugate-head-verb! (vp subj)
;; ;``````````````````````````
;; ; Conjugates the main verb of vp according to the tense attached to it and the
;; ; number of the subject.
;; ; Assumes there's no passive operator on the verb, since this should be appled after
;; ; (tense (pasv <verb>)) is expanded to ((tense be.v) (<past part verb> ..))
;;   (let ((num (if (plural-term? subj) 'PL 'SG))
;;         (hv (find-head-verb vp))
;;         tense conjugated lex-verb)
;;     (setq tense (if (tensed-verb? hv) (first hv) nil))
;;     (setq lex-verb (if tense (second hv) hv))
;;     (multiple-value-bind (word suffix) (split-by-suffix lex-verb)
;;       (setq conjugated
;;             (add-suffix
;;               (intern
;;                 (if tense
;;                   (pattern-en-conjugate (string word) :tense (ulf2pen-tense tense) :number num)
;;                   (pattern-en-conjugate (string word) :number num)))
;;               ;suffix))
;;               ; NB: special suffix so we don't recurse...
;;               'specialv))
;;       (replace-head-verb vp conjugated))))


;; (defparameter *participle-for-post-modifying-verbs*
;; ;`````````````````````````````````````````````
;; ; Transform noun and np post-modifying verbs to present-pariciple
;; ; form. E.g. (n+preds man.n walk.v) ->
;;   '(/ ((!1 n+preds np+preds n+post) _!2 _+3)
;;       (!1 _!2 (vp-to-participle! _+3))))

;; (defparameter *participle-for-mod-x*
;;   '(/ ((!1 mod-n mod-a) (!2 verb?))
;;       (!1 (vp-to-participle! !2))))
;; (defparameter *participle-for-implicit-mod-x*
;;   '(/ ((!1 verb?) (!2 noun? adj?))
;;       ((vp-to-participle! !1) !2)))
;; (defparameter *participle-for-adv-a*
;;   '(/ (adv-a (!1 verb?))
;;       (adv-a (vp-to-participle! !1))))
;; (defparameter *pres-part-for-ka*
;;   '(/ (ka (!1 verb?))
;;       (vp-to-present-participle! !1)))

;; ;; Functions to help with writing TTT rules on prog and perf since they have
;; ;; tensed variants.
;; (defun prog2be! (proginst)
;;   (subst 'be.v 'prog proginst))
;; (defun perf2have! (perfinst)
;;   (subst 'have.v 'perf perfinst))

;; ;; All the prog handling rules.
;; (defparameter *prog2surface*
;;   '(/ ((!1 prog (lex-tense? prog)) ; prog, (pres prog), etc.
;;        (*1 phrasal-sent-op?)       ; not, definitely.adv-s, etc.
;;        (!2 verb?))
;;       ((prog2be! !1) *1 (vp-to-present-participle! !2))))
;; (defparameter *inv-prog2surface*
;;   '(/ ((!1 prog (lex-tense? prog)) ; prog, (pres prog), etc.
;;        (*1 phrasal-sent-op?)       ; not, definitely.adv-s , etc.
;;        (!2 term?)                  ; he.pro, |John|, etc.
;;        (*2 phrasal-sent-op?)
;;        (!3 verb?) _*3)             ; verb + rest
;;       ;; Keep everything the same, except prog and the verb.
;;       ((prog2be! !1) *1 !2 *2 (vp-to-present-participle! !3) _*3)))

;; ;; All the perf handling rules.
;; (defparameter *perf2surface*
;;   '(/ ((!1 perf (lex-tense? perf)) ; perf, (past perf), etc.
;;        (*1 phrasal-sent-op?)       ; not, probably.adv-s, etc.
;;        (!2 verb?))
;;       ((perf2have! !1) *1 (vp-to-past-participle! !2))))
;; (defparameter *inv-perf2surface*
;;   '(/ ((!1 perf (lex-tense? perf)) ; perf, (past perf), etc.
;;        (*1 phrasal-sent-op?)       ; not, probably.adv-s, etc.
;;        (!2 term?)                  ; he.pro, |John|, etc.
;;        (*2 phrasal-sent-op?)
;;        (!3 verb?) _*3)
;;       ((perf2have! !1) *1 !2 *2 (vp-to-past-participle! !3) _*3)))


;; (defun add-morphology (ulf)
;;   (ttt:apply-rules
;;     (list
;;       ;; NB: THE ORDER HERE MATTERS
;;       ;; - Many later stages rely on perf and prog having already been
;;       ;;   processed.
;;       ;; - Some rules apply to the same stuff, but the more specific ones
;;       ;;   are done first.

;;       ;; Initial interactive changes.
;;       *prog2surface*
;;       *inv-prog2surface*
;;       *perf2surface*
;;       *inv-perf2surface*
;;       ;; Various participles
;;       *participle-for-post-modifying-verbs*
;;       *participle-for-adv-a*
;;       *participle-for-mod-x*
;;       *participle-for-implicit-mod-x*
;;       *pres-part-for-ka*
;;       ;; Core non-interactive pieces.
;;       *pasv2surface*
;;       *tense-n-number2surface*
;;       *inv-tense-n-number2surface*
;;       *exist-there-tense-n-number2surface*
;;       *inv-exist-there-tense-n-number2surface*
;;       ; NB: comment below when testing tense-n-number2surface, but uncomment during use.
;;       ;*tense2surface* ; default tense if above didn't work.
;;       *plur2surface*)
;;     ulf :max-n 1000
;;     :rule-order :slow-forward))

;; (defun capitalize-first (string)
;;   (if (zerop (length string))
;;       ""
;;       (let ((copy (copy-seq string)))
;;         (setf (char copy 0)
;;               (char-upcase (char copy 0)))
;;         copy)))


;; (defun add-punct-curried (punct)
;;   (lambda (sent)
;;     (cl-strings:join (list sent punct) :separator "")))


;; ;; Function that takes a relational noun in ULF and transforms it into bare
;; ;; noun form. It's meant for TTT mapping, hence the exclamation mark ending.
;; (defun unrel-noun! (ulf)
;;   (multiple-value-bind (word suffix) (ulf:split-by-suffix ulf)
;;     (let ((wchars (util:split-into-atoms word))
;;           (tchars (util:split-into-atoms suffix)))
;;       (util:fuse-into-atom
;;         (append (reverse (nthcdr 3 (reverse wchars)))
;;                 '(\.)
;;                 tchars)))))


;; ;; Converts relational nouns to versions closer to surface form. Implicit
;; ;; referents lead to a deletion of the preposition, e.g.
;; ;;   (on.p ({the}.d (top-of.n *ref)))
;; ;;   -> (on.p ({the}.d top.n))
;; (defun relational-nouns-to-surface (ulf)
;;   (util:unhide-ttt-ops
;;     (ttt:apply-rules '((/ (lex-rel-noun? (! [*S] [*REF]))
;;                           (unrel-noun! lex-rel-noun?)))
;;                      (util:hide-ttt-ops ulf) :max-n 500
;;                      :rule-order :slow-forward)))


;; ;; List of functions for preprocessing ULFs in context (i.e. before any
;; ;; transformation of the ULF).
;; (defparameter *contextual-preprocess-fns*
;;   (list #'relational-nouns-to-surface))
;; ;; Preprocesses the ULF formula according to contextual cues, which are
;; ;; separate from morphological modifications.
;; (defun contextual-preprocess (ulf)
;;   (reduce #'(lambda (acc fn) (funcall fn acc))
;;           *contextual-preprocess-fns*
;;           :initial-value ulf))


;; ;;Extracts and returns the sentence punctuation from the ULF (sentence
;; ;;termination symbol only)
;; ;;Parameters:
;; ;;ulf - the ULF as a list
;; ;;Retval:
;; ;;the sentence termination character as a string
;; ;;---Georgiy
;; (defun extract-punctuation (ulf)
;;   (if (and (> (length ulf) 1)
;; 	   (or (eq (cadr ulf) '?)
;; 	       (eq (cadr ulf) '!)))
;;       (string (cadr ulf))
;;     "."))


;; ;; Input: a list of tokens.
;; ;; Output: tokens with certain determiner-"thing"/"one" combinations merged.
;; ;;
;; ;; Ex. "any" "one" -> "anyone"
;; ;;     "every" "thing" -> "everything"
;; (defun merge-det-thing-combos (tokens)
;;   (cond
;;     ((null tokens) nil)
;;     (t (let* ((rec (merge-det-thing-combos (cdr tokens)))
;;               (cur (car tokens))
;;               (toprec (car rec)))
;;          (if (and (member (string-downcase cur) '("any" "no" "every")
;;                           :test #'equal)
;;                   (member (string-downcase toprec) '("one" "thing")
;;                           :test #'equal))
;;            ;; If the pattern matches, merge the strings.
;;            (cons (cl-strings:join (list cur toprec)) (cdr rec))
;;            ;; Otherwise, just build up the list again.
;;            (cons cur rec))))))


;; (defparameter *ulf2english-stages*
;;   '((contextual-preprocess "Contextual preprocess")
;;     (add-morphology "Adding morphology")
;;     ((lambda (x) (remove-if-not #'is-surface-token? (alexandria:flatten x)))
;;      "Only retaining surface symbols")
;;     ((lambda (x) (mapcar #'util:sym2str x)) "Stringify symbols")
;;     ((lambda (x) (mapcar #'ulf:strip-suffix x)) "Strip suffixes")
;;     ((lambda (x) (mapcar #'post-format-ulf-string x)) "Post-format strings")
;;     (merge-det-thing-combos "Merge special determiner-noun combinations")
;;     ((lambda (x) (cl-strings:join x :separator " ")) "Glue together")))


;; ;; Maps a ULF formula to a corresponding surface string.
;; ;; NB: currently this is incomplete and not fluent.
;; (defun ulf2english (ulf &key (add-punct? t) (capitalize-front? t))
;;   ;; TODO: make more sophisticated version (quotes, ds, etc.).

;;   ;; For now just drop all special operators and just take the suffixed tokens.
;;   ;; The only non-suffixed tokens that we preserve are "that", "not", "and",
;;   ;; "or", "to".
;;   (let* ((idfn #'(lambda (x) x))
;;          (punct (extract-punctuation ulf))
;;          (add-punct-fn (add-punct-curried punct))
;;         staged)
;;     (setq staged (reduce #'(lambda (acc new)
;;                              (let* ((fn (first new))
;;                                     (desc (second new))
;;                                     (res (funcall fn acc)))
;;                                (if *debug-ulf2english*
;;                                  (format t "~a: ~s~%" desc res))
;;                                res))
;;                          *ulf2english-stages* :initial-value ulf))
;;     (funcall (compose
;;                (if add-punct? add-punct-fn idfn)
;;                (if capitalize-front? #'capitalize-first idfn))
;;              staged)))

