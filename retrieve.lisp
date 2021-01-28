(in-package #:wordnet)

(defparameter relationship-common-name
  '((:ALSO-SEE "See also")
    (:ANTONYM "Opposite of")
    (:ATTRIBUTE "Attribute")
    (:CAUSE "Cause")
    (:DERIVATIONALLY-RELATED-FORM "Derived form")
    (:DERIVED-FROM "Derivation")
    (:DOMAIN-OF-SYNSET-REGION "Domain Region")
    (:DOMAIN-OF-SYNSET-TOPIC "Topic domain")
    (:DOMAIN-OF-SYNSET-USAGE "Usage Domain")
    (:ENTAILMENT "Entailment of")
    (:HYPERNYM "More Generally")
    (:HYPONYM "More Specifically")
    (:MEMBER-HOLONYM "Part of")
    (:MEMBER-MERONYM "Member of")
    (:MEMBER-OF-DOMAIN-REGION "Member of Domain Region")
    (:MEMBER-OF-DOMAIN-TOPIC "Member of Domain Topic")
    (:MEMBER-OF-DOMAIN-USAGE "Member Of Domain")
    (:PART-HOLONYM "Part of")
    (:PART-MERONYM "Part of whole")
    (:PARTICIPLE-OF-VERB "Verb Form of")
    (:PERTAINYM "Pertains to")
    (:SIMILAR-TO "Similar to")
    (:SUBSTANCE-HOLONYM "Made from")
    (:SUBSTANCE-HYPERNYM "Classed as")
    (:SUBSTANCE-HYPONYM "Instance of")
    (:SUBSTANCE-MERONYM "Part of substance")
    (:VERB-GROUP "Group")))

(defun get-substance-hyponyms (lemma &optional (synonym lemma))
  (let ((siblings ()))
    (loop for synset in (index-entry-synsets (cached-index-lookup lemma :noun))
          when (find synonym (synset-words synset) :key #'first :test #'string-equal) do
          (loop for pointer in (wordnet-pointers synset) do
                (case (wordnet-pointer-type pointer)
                  (:substance-hyponym
                   (setf siblings (append siblings (list (mapcar #'car (synset-words (wordnet-pointer-to-synset pointer)))))))
                  (:hyponym
                   (dolist (hyponym (mapcar #'car (synset-words (wordnet-pointer-to-synset pointer))))
                     (loop for synset in (index-entry-synsets (cached-index-lookup hyponym :noun)) do
                           (loop for pointer in (wordnet-pointers synset)
                                 when (eq (wordnet-pointer-type pointer) :substance-hyponym) do
                                 (setf siblings (append siblings (list (mapcar #'car (synset-words (wordnet-pointer-to-synset pointer)))))))))))))
    (sort siblings #'string-lessp :key #'first)))

(defun get-hyponyms (lemma &optional (synonym lemma))
  (let ((hyponyms ()))
    (loop for synset in (index-entry-synsets (cached-index-lookup lemma :noun))
          as synonyms = (mapcar #'car (synset-words synset)) do
         (when (find synonym synonyms :test #'string-equal)
            ;;(format t "~s~%" synonyms)
            (loop for pointer in (wordnet-pointers synset)
                  if (eq (wordnet-pointer-type pointer) :hyponym) do
                  (dolist (hyponym (synset-words (wordnet-pointer-to-synset pointer)))
                    (push (first hyponym) hyponyms)))))
    (sort hyponyms #'string-lessp)))

(defun get-countries ()
  (get-substance-hyponyms "country" "state"))

(defun get-state-names ()
  (get-substance-hyponyms "American_state"))

(defun get-minerals ()
  (get-hyponyms "mineral"))

(defun get-word-network (lemma &optional (part-of-speech :noun))
  (setf lemma (substitute #\_ #\Space lemma))
  (let* ((words nil)
         (parts-of-speech (if (listp part-of-speech) part-of-speech '(:noun :verb :adjective :adverb))))
    (flet ((add-relationship (relationship word)
             (unless (string= lemma word)
               (setf word (substitute #\Space #\_ word))
               (let ((related (assoc relationship (cddar words))))
                 (unless (find word (cdr related) :test #'string=)
                   (if related
                       (setf (cdr related) `(,@(cdr related) ,word))
                     (push (list relationship word) (cddar words)))))))

           (get-synset-relationships (synset)
             (let ((relationships ()))
               (dolist (p (wordnet-pointers synset))
                   (let ((type (wordnet-pointer-type p))
                         (from (wordnet-pointer-from-word p))
                         (to (wordnet-pointer-to-word p)))
                     (when (or (typep from 'wordnet-synset-entry)
                               (and (stringp (car from))
                                    (string-equal (car from) lemma)))
                       (if (typep to 'wordnet-synset-entry)
                           (dolist (w (synset-words to))
                             (push (list type (car w)) relationships))
                         (push (list type (car to)) relationships)))))
               ;;(break "get-synset-word-pointers ~s" words)
               relationships)))

    (dolist (pos parts-of-speech)
      (dolist (synset (index-entry-synsets (cached-index-lookup lemma pos)))
        (push (list pos (synset-gloss synset)) words)
        ;;(break "synset ~s" words)
        (dolist (synonym (mapcar #'car (synset-words synset)))
          (add-relationship :synonym synonym))
        (loop for (type word) in (get-synset-relationships synset) do
              (add-relationship type word))))
    (nreverse words))))

(defun wnet (word)
  (let ((wdnet (get-word-network word)))
    (loop for entry in wdnet 
          as (pos gloss) = entry do
          (format t "~&~:a~2%~a~2%" pos gloss)
          (loop for related-words in (cddr entry)
                as relationship = (pop related-words) do
                (format t "~:a:~{ ~a~^,~}~%"
                        (string-capitalize relationship)
                        related-words))
          (format t "~%"))))
