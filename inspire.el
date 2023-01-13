;;; inspire.el --- Emacs interface for inspirehep.net  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Simon Lin

;; Author: Simon Lin <n.sibetz@gmail.com>
;; Keywords: extensions, tex

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'seq)
(require 'map)
(require 'json)
(require 'bibtex)

(defvar inspire-api-url "https://inspirehep.net/api/")

(defvar inspire-query-size 30)

(defvar inspire-entry-list nil)

(defvar inspire-current-entry nil)

(defvar inspire-search-type nil)

(defvar inspire-author-data nil)

(defvar inspire-highlight-overlay nil)

(defvar inspire-use-variable-pitch t)

(defvar inspire-buffer nil)

(defvar inspire-window nil)

(defvar inspire-record-buffer nil)

(defvar inspire-record-window nil)

(defvar inspire-author-buffer nil)

(defvar inspire-author-window nil)

;; faces
(defvar inspire-title-face 'inspire-title-face)
(defface inspire-title-face
  '((t (:inherit font-lock-keyword-face :weight semi-bold)))
  "Face name for article titles in the inspire article list."
  :group 'inspire-fontification)

(defvar inspire-keyword-face 'inspire-keyword-face)
(defface inspire-keyword-face
  '((t (:inherit font-lock-constant-face)))
  "Face name for keywords in the inspire article list."
  :group 'inspire-fontification)

(defvar inspire-link-face 'inspire-link-face)
(defface inspire-link-face
  '((t (:inherit button)))
  "Face name for keywords in the inspire article list."
  :group 'inspire-fontification)

(defvar inspire-author-face 'inspire-author-face)
(defface inspire-author-face
  '((t (:inherit font-lock-type-face)))
  "Face name for authors in the inspire article list."
  :group 'inspire-fontification)

(defvar inspire-date-face 'inspire-date-face)
(defface inspire-date-face
  '((t (:inherit shadow)))
  "Face name for date in the inspire article list."
  :group 'inspire-fontification)

(defvar inspire-abstract-face 'inspire-abstract-face)
(defface inspire-abstract-face
  '((t (:inherit font-lock-string-face)))
  "Face name for abstract in the inspire abstract viewing window."
  :group 'inspire-fontification)

(defvar inspire-subfield-face 'inspire-subfield-face)
(defface inspire-subfield-face
  '((t (:inherit default)))
  "Face name for subfields (comments, subjects, etc.) in the inspire abstract viewing window."
  :group 'inspire-fontification)

(defvar inspire-abstract-math-face 'inspire-abstract-math-face)
(defface inspire-abstract-math-face
  '((t (:inherit font-lock-reference-face :family "Monospace")))
  "Face name for the latex content in abstract in the inspire
abstract viewing window."
  :group 'inspire-fontification)

(defvar inspire-author-heading-face 'inspire-author-heading-face)
(defface inspire-author-heading-face
  '((t (:inherit default :weight semi-bold)))
  "Face name for the author names when searching and displaying author pages."
  :group 'inspire-fontification)

(defvar inspire-author-institute-face 'inspire-author-institute-face)
(defface inspire-author-institute-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face name for the institutes when searching and displaying author pages."
  :group 'inspire-fontification)

(defvar inspire-arxiv-category-face 'inspire-arxiv-category-face)
(defface inspire-arxiv-category-face
  '((t (:inherit font-lock-constant-face)))
  "Face name for arxiv categories in the inspire window."
  :group 'inspire-fontification)


(defvar inspire-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'inspire-prev-entry)
    (define-key map (kbd "k") 'inspire-next-entry)
    (define-key map (kbd "RET") 'inspire-open-current-url)
    (define-key map (kbd "SPC") 'inspire-select-entry)
    (define-key map (kbd "q") 'inspire-exit)
    (define-key map (kbd "b") 'inspire-get-bibtex)
    map))

(define-derived-mode inspire-mode special-mode "inspire"
  :group 'inspire
;;  (setq header-line-format '(:eval (arxiv-headerline-format)))
  (setq inspire-highlight-overlay (make-overlay 1 1))
  (setq inspire-current-entry 0)
  (overlay-put inspire-highlight-overlay 'face '(:inherit highlight :extend t))
  (if inspire-use-variable-pitch
      (variable-pitch-mode 1)
    (variable-pitch-mode -1)))

(define-derived-mode inspire-record-mode special-mode "inspire-record"
  :group 'inspire
  (if inspire-use-variable-pitch
      (variable-pitch-mode 1)
    (variable-pitch-mode -1)))
;;  (setq header-line-format '(:eval (arxiv-headerline-format)))

(define-derived-mode inspire-author-mode special-mode "inspire-author"
  :group 'inspire
  (if inspire-use-variable-pitch
      (variable-pitch-mode 1)
    (variable-pitch-mode -1)))

(defun inspire-literature-search (query-string)
  (interactive "sSearch for literature on inspirehep: ")
  (if (string-match "^ *$" query-string)
      (message "Search condition cannot be blank.")
    (setq inspire-entry-list (inspire-parse-literature (concat inspire-api-url "literature?sort=mostrecent&size=" (format "%s" inspire-query-size) "&q=" query-string)))
    (inspire-populate-page)
    (inspire-populate-record)))

(defun inspire-author-search (query-string)
  (interactive "sSearch for author on inspirehep: ")
  (if (string-match "^ *$" query-string)
      (message "Search condition cannot be blank.")
    (let* ((authors (inspire-parse-author (concat inspire-api-url "authors?sort=bestmatch&size=" (format "%s" inspire-query-size) "&q=" query-string)))
	   (candidates (seq-map 'inspire--author-format-completion-string authors))
	   (selected))
      (setq selected
	    (completing-read "Authors search result: "
			     (lambda (input pred action) ; custom table for ordering
			       (if (eq action 'metadata)
				   `(metadata . ((display-sort-function . ,#'identity)))
				 (complete-with-action action candidates input pred)))
			     nil t))
      (string-match "\t\\([\.A-Za-z0-9]+\\)$" selected)
      (setq inspire-author-data (seq-find
				 (lambda (x) (string-equal
					 (match-string-no-properties 1 selected)
					 (alist-get 'inspire-bai x)))
				 authors))
      (inspire-populate-author-data)
      
      ;; search and display authored literature
      (setq inspire-entry-list
	    (inspire-parse-literature
	     (format "%sliterature?sort=mostrecent&size=%s&q=a %s" inspire-api-url inspire-query-size (alist-get 'inspire-bai inspire-author-data))))
      (setq inspire-current-entry 0)
      (inspire-populate-record)
      (unless (window-live-p inspire-window)
	(setq inspire-window (display-buffer (get-buffer-create "*inspire-search*") '(display-buffer-below-selected . ((window-height . .5))))))
      (select-window inspire-window)
      (inspire-populate-page))))
 

(defun inspire--author-format-completion-string (author-alist)
  (let ((name (alist-get 'name author-alist))
	(key  (alist-get 'inspire-bai author-alist))
	(pos  (alist-get 'current-position author-alist))
	(cat  (alist-get 'arxiv-categories author-alist)))
    (setq name (propertize name 'face 'inspire-author-heading-face))
    (setq key  (propertize key 'face 'shadow))
    (setq pos (if pos
		  (propertize (format "(%s)" pos) 'face 'inspire-author-institute-face)
		""))
    (setq cat (if cat
		  (propertize cat 'face 'inspire-arxiv-category-face)
		""))
    (concat "  "
	    (truncate-string-to-width name 30 nil ?\s) "\t"
	    (truncate-string-to-width pos  70 nil ?\s) "\t"
	    (truncate-string-to-width cat  40 nil ?\s) "\t"
	    key)))

(defun inspire-next-entry (&optional arg)
  "Move to the next inspire entry.
With ARG, move to the next nth entry."
  (interactive "p")
  (setq inspire-current-entry (+ inspire-current-entry arg))
  (let ((len (- (safe-length inspire-entry-list) 1)))
    (when (> inspire-current-entry len)
      (setq inspire-current-entry (- (safe-length inspire-entry-list) 1))
      (message "end of search results"))
  (goto-char (point-min))
  (forward-line (* 4 inspire-current-entry))
  (move-overlay inspire-highlight-overlay
		(point) (progn (beginning-of-line 5) (point)))
  (forward-line (- 4)))
  (when (window-live-p inspire-record-window)
    (inspire-populate-record)))

(defun inspire-prev-entry (&optional arg)
  "Move to the previous inspire entry.
With ARG, move to the previous nth entry."
  (interactive "p")
  (setq inspire-current-entry (- inspire-current-entry arg))
  (when (< inspire-current-entry 0)
    (setq inspire-current-entry 0)
    (message "beginning of search results"))
  (goto-char (point-min))
  (forward-line (* 4 inspire-current-entry))
  (move-overlay inspire-highlight-overlay
		(point) (progn (beginning-of-line 5) (point)))
  (forward-line (- 4))
  (when (window-live-p inspire-record-window)
    (inspire-populate-record)))

(defun inspire-select-entry ()
  "Select the entry to which the cursor is pointing to."
  (interactive)
  (setq inspire-current-entry (/ (line-number-at-pos) 4))
  (goto-char (point-min))
  (forward-line (* 4 inspire-current-entry))
  (move-overlay inspire-highlight-overlay
		(point) (progn (beginning-of-line 5) (point)))
  (forward-line (- 4))
  (when (window-live-p inspire-record-window)
    (inspire-populate-record)))

(defun inspire-open-current-url ()
  (interactive)
  (browse-url (format "https://inspirehep.net/literature/%s" (alist-get 'inspire-id (nth inspire-current-entry inspire-entry-list)))))

(defun inspire-get-bibtex ()
  (interactive)
  (let ((url (alist-get 'bib-link (nth inspire-current-entry inspire-entry-list))))
    (select-window inspire-record-window)
    (pop-to-buffer "*inspire-bibTeX*" '(display-buffer-below-selected))
    (erase-buffer)
    (url-insert (url-retrieve-synchronously url))
    (setq buffer-read-only nil)
    (bibtex-mode)
    (bibtex-set-dialect 'BibTeX t)))

(defun inspire-exit ()
  "Quit inspire-mode and kill all related buffers."
  (interactive)
  (when (window-live-p inspire-author-window)
    (quit-restore-window inspire-window 'kill)
    (quit-restore-window inspire-author-window 'kill)
    (setq inspire-author-window nil))
  (when (window-live-p inspire-record-window)
    (quit-restore-window inspire-record-window 'kill)
    (setq inspire-record-window nil))
  (kill-buffer inspire-buffer)
  (when inspire-record-buffer (kill-buffer inspire-record-buffer))
  (when inspire-author-buffer (kill-buffer inspire-author-buffer))
  (setq inspire-record-buffer nil)
  (setq inspire-author-buffer nil)
  (setq inspire-window nil))

(defun inspire-populate-record ()
  (unless (buffer-live-p inspire-record-buffer)
    (setq inspire-record-buffer (get-buffer-create "*inspire-record*")))
  (unless (window-live-p inspire-record-window)
    (setq inspire-record-window (display-buffer inspire-record-buffer t)))
  (with-current-buffer inspire-record-buffer
    (inspire-record-mode)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (inspire-fill-record (nth inspire-current-entry inspire-entry-list)))))
  
(defun inspire-populate-page ()
  (if inspire-entry-list
      (progn
	(unless (buffer-live-p inspire-buffer)
	  (setq inspire-buffer (get-buffer-create "*inspire-search*")))
	(with-current-buffer inspire-buffer
	  (let ((buffer-read-only nil))
	    (erase-buffer)
	    (inspire-fill-page)
	    (inspire-mode)
	    (goto-char (point-min))
	    (move-overlay inspire-highlight-overlay
			  (point) (progn (beginning-of-line 5) (point)))
	    (goto-char (point-min))))
	(switch-to-buffer inspire-buffer)
	(setq inspire-window (get-buffer-window)))
    (message "No matching search result.")))

(defun inspire-populate-author-data ()
  (unless (buffer-live-p inspire-author-buffer)
    (setq inspire-author-buffer (get-buffer-create "*inspire-author*")))
  (with-current-buffer inspire-author-buffer
    (inspire-author-mode)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (inspire-fill-author-data))
    (goto-char (point-min)))
  (switch-to-buffer inspire-author-buffer)
  (setq inspire-author-window (get-buffer-window)))

(defun inspire-fill-author-data ()
  ;; name and current positions
  (inspire-insert-with-face (format "\n%s" (alist-get 'name inspire-author-data)) '(:inherit inspire-author-heading-face :height 1.2))
  (when-let ((alt-name (alist-get 'native-names inspire-author-data)))
    (inspire-insert-with-face (format " (%s)" alt-name) '(:inherit inspire-author-heading-face :height 1.2)))
  (insert "\n")
  (when-let ((current-pos (alist-get 'current-position inspire-author-data)))
    (inspire-insert-with-face (format "(%s)\n" current-pos) inspire-author-institute-face))
  (insert "\n")

  ;; arxiv categories, author id and advisor
  (inspire-insert-with-face (alist-get 'arxiv-categories inspire-author-data) inspire-arxiv-category-face)
  (inspire-insert-with-face "\n\nAuthor id: " inspire-subfield-face)
  (inspire-insert-url (alist-get 'inspire-bai inspire-author-data)
		      (format "https://inspirehep.net/authors/%s" (alist-get 'control-number inspire-author-data)))
  (when-let ((orcid (alist-get 'orcid inspire-author-data)))
    (inspire-insert-with-face "\nORCID: " inspire-subfield-face)
    (inspire-insert-url orcid (format "https://orcid.org/%s" orcid)))
  (when-let ((advisors (alist-get 'advisors inspire-author-data)))
    (seq-doseq (advisor advisors)
      (insert "\n")
      (pcase (alist-get 'type advisor)
	('"phd" (inspire-insert-with-face "PhD " inspire-subfield-face))
	('"master" (inspire-insert-with-face "Master " inspire-subfield-face))
	('"bachelor" (inspire-insert-with-face "Bachelor " inspire-subfield-face)))
      (inspire-insert-with-face "Advisor: " inspire-subfield-face)
      (inspire-insert-with-face (alist-get 'name advisor) inspire-author-face)))
  (when-let ((email (alist-get 'email inspire-author-data)))
    (inspire-insert-with-face (format "\nemail: %s" email) inspire-subfield-face))
  (insert "\n")

  ;; education and employment history
  (when-let ((positions (alist-get 'positions inspire-author-data)))
    (seq-doseq (pos positions)
      (inspire-insert-with-face (format "\n\t• %s" (alist-get 'date pos)) 'default)
      (insert "\n\t  ")
      (when-let ((rank (alist-get 'rank pos)))
	(inspire-insert-with-face (format "%s, " (alist-get 'rank pos)) '(:inherit default :weight semi-bold)))
      (inspire-insert-with-face (format "%s\n" (alist-get 'institution pos)) 'inspire-author-institute-face)))

  ;; timestamp
  (inspire-insert-with-face (format "\nUpdated: %s" (alist-get 'timestamp inspire-author-data)) 'shadow)
  )

(defun inspire-insert-with-face (str face &rest properties)
  (insert (apply 'propertize `(,str font-lock-face ,face ,@properties))))

(defun inspire-insert-url (str url)
  (insert-button str
		 'action (lambda (x) (browse-url (button-get x 'url)))
		 'face 'inspire-link-face
		 'mouse-face 'highlight
		 'follow-link t
		 'help-echo (format "Link: %s" url)
		 'url url))

(defun inspire-fill-page ()
  (dolist (entry inspire-entry-list)
    (inspire-insert-with-face (format " %s\n " (alist-get 'title entry)) '(:inherit inspire-title-face :height 1.2))
    (if-let (collaborations (alist-get 'collaborations entry))  ; if there is collaborations then don't show authors list
	(inspire-insert-with-face
	 (mapconcat (lambda (coll) (format "%s collaborations" coll)) collaborations ", ") inspire-author-face)
      (let ((names (mapcar (lambda (author) (alist-get 'name author)) (alist-get 'authors entry))))
	(inspire-insert-with-face (mapconcat 'identity (seq-take names 5) ", ") inspire-author-face)
	(when (> (length names) 5)
	  (inspire-insert-with-face " et al." inspire-author-face))))
    (inspire-insert-with-face (format "\n %s \n\n"(alist-get 'date entry)) inspire-date-face)))

(defun inspire-fill-record (entry)
  (inspire-insert-with-face (format "\n%s\n" (alist-get 'title entry)) '(:inherit inspire-title-face :height 1.3))
  (inspire-insert-with-face "\n" inspire-title-face)  
  (if-let (collaborations (alist-get 'collaborations entry))  ; if there is collaborations then don't show authors list
      (inspire-insert-with-face
       (mapconcat (lambda (coll) (format "%s collaborations" coll)) collaborations ", ") inspire-author-face)
    (inspire-insert-with-face (mapconcat (lambda (author) ; author list
					   (concat (format "%s" (alist-get 'name author))
						   (when (alist-get 'affiliation author)
						     (format " (%s)" (mapconcat 'identity (alist-get 'affiliation author) ", ")))))
					 (alist-get 'authors entry) ", ") inspire-author-face))
  (inspire-insert-with-face (format "\n%s\n\n" (alist-get 'date entry)) inspire-date-face)
  
  ;; pages and publication info
  (when-let (pages (alist-get 'number-of-pages entry))
    (inspire-insert-with-face (format "%s Pages\n" pages) inspire-subfield-face))
  (inspire-insert-with-face (format "Type: %s\n" (alist-get 'type entry)) inspire-subfield-face)
  (when-let (journals (alist-get 'journals entry))
    (inspire-insert-with-face (format "Published in: %s\n" (mapconcat 'identity journals ", ")) inspire-subfield-face))
  (when-let (conferences (alist-get 'conferences entry))
    (inspire-insert-with-face (format "Contribution to: %s\n" (mapconcat 'identity conferences ", ")) inspire-subfield-face))
  (when-let (eprint (alist-get 'eprint entry))
    (inspire-insert-with-face "e-print: " inspire-subfield-face)    
    (inspire-insert-url (alist-get 'value eprint) (format "https://arxiv.org/abs/%s" (alist-get 'value eprint)))
    (inspire-insert-with-face (format " [%s]\n" (map-nested-elt eprint '(categories 0))) inspire-subfield-face))
  (when-let (dois (alist-get 'dois entry))    
    (inspire-insert-with-face "DOI: " inspire-subfield-face)
    (seq-doseq (doi dois)
      (inspire-insert-url doi (format "https://doi.org/%s" doi))
      (inspire-insert-with-face ", " inspire-subfield-face))
    (delete-backward-char 2))
  (insert "\n\n")

  ;; abstract & notes
  (when-let (abstract (alist-get 'abstract entry))
    (inspire-insert-with-face (format "Abstract: (%s)\n%s\n\n" (alist-get 'source abstract) (alist-get 'value abstract)) inspire-abstract-face))
  (when-let (notes (alist-get 'note entry))
    (inspire-insert-with-face (format "Note: %s\n\n" notes) inspire-abstract-face))
  
  ;; citations & references
  (inspire-insert-with-face (format "%s References,\t"  (alist-get 'reference-count entry)) inspire-subfield-face)
  (inspire-insert-with-face (format "%s Citations\n"  (alist-get 'citation-count entry)) inspire-subfield-face))

(defun inspire-reorder-name (full-name)
  (if (string-match "^\\(.+\\), \\(.+\\)$" full-name)
      (concat (match-string 2 full-name) " " (match-string 1 full-name))
    full-name))

(defun inspire-parse-literature (url)
  "Call inspire hep to search for literatures. 
Parse the returned results into a list."
  (with-current-buffer (url-retrieve-synchronously url)
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (search-forward "\{")
    (backward-char)
    (let* ((root (alist-get 'hits (json-parse-buffer :object-type 'alist)))
	   (total (alist-get 'total root))
	   (hits (alist-get 'hits root))
	   (alist-entry) (alist-formed '()))    
      (seq-doseq (entry hits)
	(let ((metadata (alist-get 'metadata entry))
	      (title) (authors) (collaborations) (date) (journals) (conferences) (dois) (bib-link) (abstract) (eprint) (citation-count) (reference-count) (number-of-pages) (note) (type) (inspire-id))
	  (setq bib-link (map-nested-elt entry '(links bibtex)))
	  (setq title (map-nested-elt metadata '(titles 0 title)))
	  (setq date (or (map-nested-elt metadata '(earliest_date))
			 (map-nested-elt metadata '(preprint_date))
			 (map-nested-elt metadata '(imprints 0 date))))
	  (setq abstract (map-nested-elt metadata '(abstracts 0)))
	  (setq dois (seq-uniq (seq-map (lambda (elem) (alist-get 'value elem)) (alist-get 'dois metadata))))
	  (setq eprint (map-nested-elt metadata '(arxiv_eprints 0)))
	  (setq citation-count (alist-get 'citation_count metadata))
	  (setq reference-count (seq-length (alist-get 'references metadata)))
	  (setq number-of-pages (alist-get 'number_of_pages metadata))
	  (setq inspire-id (alist-get 'id entry))
	  (setq authors (seq-map (lambda (elem) `((name . ,(inspire-reorder-name (alist-get 'full_name elem)))
					     (affiliation . ,(mapcar (lambda (aff) (alist-get 'value aff)) (alist-get 'affiliations elem)))))
				 (alist-get 'authors metadata)))
	  (setq collaborations (and (alist-get 'collaborations metadata)
				    (seq-map (lambda (elem) (alist-get 'value elem)) (alist-get 'collaborations metadata))))
	  (when-let (pubs (alist-get 'publication_info metadata))
	    (seq-doseq (pub pubs)
	      (cond ((alist-get 'journal_title pub)    ; journal
		     (let ((jtitle (alist-get 'journal_title pub))
			   (jyear (alist-get 'year pub))
			   (jvol (alist-get 'journal_volume pub))
			   (pstart (alist-get 'page_start pub))
			   (pend (alist-get 'page_end pub)))
		       (cond ((eval pend) (setq journals (cons (format "%s %s (%s) %s-%s" jtitle jvol jyear pstart pend) journals)))
			     ((eval pstart) (setq journals (cons (format "%s %s (%s) %s" jtitle jvol jyear pstart) journals)))
			     (t (setq journals (cons (format "%s %s (%s)" jtitle jvol jyear) journals))))))
		    ((alist-get 'cnum pub)    ; conference
		     (setq conferences (cons (format "%s" (alist-get 'cnum pub)) conferences)))
		    )))
	  (setq note (when-let (raw (map-nested-elt metadata '(public_notes 0 value)))
		       (replace-regexp-in-string "\n" "" raw))) ; strip off extra EOLs
	  (setq type (map-nested-elt metadata '(document_type 0)))
	  
	  (setq alist-entry `((title . ,title)
			      (authors . ,authors)
			      (collaborations . ,collaborations)
			      (date . ,date)
			      (journals . ,(nreverse journals))
			      (conferences . ,(nreverse conferences))
			      (dois . ,dois)
			      (bib-link . ,bib-link)
			      (abstract . ,abstract)
			      (note . ,note)
			      (eprint . ,eprint)
			      (citation-count . ,citation-count)
			      (reference-count . ,reference-count)
			      (number-of-pages . ,number-of-pages)
			      (type . ,type)
			      (inspire-id . ,inspire-id)))
	  (setq alist-formed (cons alist-entry alist-formed))))
      (reverse alist-formed))))

(defun inspire-parse-author (url)
  "Call inspire hep to search for authors list.
Parse the returned results into a list."
  (with-current-buffer (url-retrieve-synchronously url)
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (search-forward "\{")
    (backward-char)
    (let* ((root (alist-get 'hits (json-parse-buffer :object-type 'alist)))
	   (total (alist-get 'total root))
	   (hits (alist-get 'hits root))
	   (alist-entry) (alist-formed))
      (seq-doseq (entry hits)
	(let ((metadata (alist-get 'metadata entry))
	      (ids) (inspire-bai) (orcid) (control-number) (name) (native-names) (advisors) (positions) (current-position) (arxiv-categories) (email) (timestamp))
	  (setq ids (alist-get 'ids metadata))
	  (setq inspire-bai (alist-get 'value (seq-find (lambda (x) (equal (alist-get 'schema x) "INSPIRE BAI")) ids)))
	  (setq orcid (alist-get 'value (seq-find (lambda (x) (equal (alist-get 'schema x) "ORCID")) ids)))
	  (setq control-number (alist-get 'control_number metadata))
	  (setq name (or (map-nested-elt metadata '(name preferred_name))
			 (inspire-reorder-name (map-nested-elt metadata '(name value)))))
	  (when-let ((nat (map-nested-elt metadata '(name native_names))))
	    (setq native-names (mapconcat 'identity nat ", ")))
	  (when-let ((adv (seq-filter (lambda (x) (not (eq (alist-get 'hidden x) t))) (alist-get 'advisors metadata))))
	    (setq advisors
		  (seq-map (lambda (x)
			     `((type . ,(alist-get 'degree_type x)) (name . ,(inspire-reorder-name (alist-get 'name x)))))
			   adv)))
	  (when-let ((cat (alist-get 'arxiv_categories metadata)))
	    (setq arxiv-categories (mapconcat (lambda (cat) (format "[%s]" cat)) cat " ")))
	  (setq email (map-nested-elt metadata '(email_addresses 0 value)))
	  (string-match "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" (alist-get 'updated entry))
	  (setq timestamp (match-string 1 (alist-get 'updated entry)))
	  (when-let ((pos-list (alist-get 'positions metadata)))
	    (seq-do (lambda (pos)
		      (let* ((rank    (alist-get 'rank pos))
			     (inst    (alist-get 'institution pos))
			     (start   (or (alist-get 'start_date pos)))
			     (end     (or (alist-get 'end_date pos) ""))			   
			     (current (alist-get 'current pos))
			     (hidden  (alist-get 'hidden pos)))
			(unless (eq hidden t)
			  (if (eq current t)
			      (progn
				(push inst current-position)
				(if start
				    (push `((rank . ,rank) (institution . ,inst) (date . ,(format "%s - present" start))) positions)
				  (push `((rank . ,rank) (institution . ,inst) (date . "present")) positions)))
			    (if start
				(push `((rank . ,rank) (institution . ,inst) (date . ,(format "%s - %s" start end))) positions)
			      (push `((rank . ,rank) (institution . ,inst) (date . ,(format "%s" end))) positions))))))
		    pos-list)
	    (setq positions (nreverse positions))
	    (setq current-position (and current-position (mapconcat 'identity (nreverse current-position) " and "))))
	  
	  (setq alist-entry `((inspire-bai . ,inspire-bai)
			      (orcid . ,orcid)
			      (control-number . ,control-number)
			      (name . ,name)
			      (native-names . ,native-names)
			      (advisors . ,advisors)
			      (arxiv-categories . ,arxiv-categories)
			      (email . ,email)
			      (positions . ,positions)
			      (current-position . ,current-position)
			      (timestamp . ,timestamp)))
	  (setq alist-formed (cons alist-entry alist-formed))))
      (reverse alist-formed))))

  
(provide 'inspire)
;;; inspire.el ends here
