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

(defvar inspire-api-url "https://inspirehep.net/api/")

(defvar inspire-page-limit 10)

(defvar inspire-entry-list nil)

(defvar inspire-current-entry nil)

(defvar inspire-search-type nil)

(defvar inspire-highlight-overlay nil)

(defvar inpsire-use-variable-pitch t)

(defvar inspire-buffer nil)

(defvar inspire-record-buffer nil)

(defvar inspire-record-window nil)

;; faces
(defvar inspire-title-face 'inspire-title-face)
(defface inspire-title-face
  '((t (:inherit font-lock-keyword-face)))
  "Face name for article titles in the inspire article list."
  :group 'inspire-fontification)

(defvar inspire-keyword-face 'inspire-keyword-face)
(defface inspire-keyword-face
  '((t (:inherit font-lock-constant-face)))
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
  '((t (:inherit font-lock-doc-face)))
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


(defvar inspire-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'inspire-prev-entry)
    (define-key map (kbd "n") 'inspire-next-entry)
    (define-key map (kbd "RET") 'inspire-open-current-url)
    (define-key map (kbd "SPC") 'inspire-select-entry)
    (define-key map (kbd "q") 'inspire-exit)
    map))

(define-derived-mode inspire-mode special-mode "inspire"
  :group 'inspire
;;  (setq header-line-format '(:eval (arxiv-headerline-format)))
  (setq inspire-highlight-overlay (make-overlay 1 1))
  (setq inspire-current-entry 0)
  (overlay-put inspire-highlight-overlay 'face '(:inherit highlight :extend t)))
  ;; (if inspire-use-variable-pitch
  ;;     (variable-pitch-mode 1)
  ;;   (variable-pitch-mode -1)))

(defun inspire-search (query-string)
  (interactive "sSearch on inspirehep: ")
  (if (string-match "^ *$" query-string)
      (message "Search condition cannot be blank.")
    (setq inspire-entry-list (inspire-parse-literature (concat inspire-api-url "literature?q=" query-string)))
    (inspire-populate-page)))

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
  (forward-line (- 4))))

(defun inspire-prev-entry (&optional arg)
  "Move to the previous arXiv entry.
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
  (forward-line (- 4)))

(defun inspire-select-entry ()
    "Select the entry to which the cursor is pointing to."
    (interactive)
    (setq inspire-current-entry (/ (line-number-at-pos) 4))
    (goto-char (point-min))
    (forward-line (* 4 inspire-current-entry))
    (move-overlay inspire-highlight-overlay
		  (point) (progn (beginning-of-line 5) (point)))
    (forward-line (- 4)))

(defun inspire-show-record ()
  (interactive)
  (unless (buffer-live-p inspire-record-buffer)
    (setq inspire-record-buffer (get-buffer-create "*inspire-record*")))
  (unless (window-live-p inspire-record-window)
    (setq inspire-record-window (display-buffer "*inspire-record*" t)))
  (inspire-fill-record (nth inspire-current-entry inspire-entry-list)))

(defun inspire-populate-page (&optional inspire-buffer)
  (if inspire-entry-list
      (progn
	(unless inspire-buffer
	  (setq inspire-buffer (get-buffer-create "*inspire-search*")))
	(set-buffer inspire-buffer)
	(let ((buffer-read-only nil))
	  (erase-buffer)
	  (inspire-fill-page)
	  (inspire-mode)
	  (goto-char (point-min))
	  (move-overlay inspire-highlight-overlay
			(point) (progn (beginning-of-line 5) (point)))
	  (goto-char (point-min)))
	(switch-to-buffer-other-window inspire-buffer))
    (message "No matching search result.")))

(defun inspire-insert-with-face (str face &rest properties)
  (insert (apply 'propertize `(,str font-lock-face ,face ,@properties))))

(defun inspire-fill-page ()
  (dolist (entry inspire-entry-list)
    (inspire-insert-with-face (format " %s\n " (alist-get 'title entry)) '(:inherit inspire-title-face :height 1.2))
    (if-let (collaborations (alist-get 'collaborations entry))  ; if there is collaborations then don't show authors list
	(inspire-insert-with-face
	 (mapconcat (lambda (coll) (format "%s collaborations" coll)) collaborations ", ") inspire-author-face)
      (inspire-insert-with-face  ; authors list
       (mapconcat (lambda (author) (alist-get 'name author)) (alist-get 'authors entry) ", ") inspire-author-face))
    (inspire-insert-with-face (format "\n %s \n\n"(alist-get 'date entry)) inspire-date-face)))

(defun inspire-fill-record (entry)
  (with-current-buffer inspire-record-buffer
    (inspire-insert-with-face (format "\n %s\n " (alist-get 'title entry)) '(:inherit inspire-title-face :height 1.5))
    (if-let (collaborations (alist-get 'collaborations entry))  ; if there is collaborations then don't show authors list
	(inspire-insert-with-face
	 (mapconcat (lambda (coll) (format "%s collaborations" coll)) collaborations ", ") (:inherit inspire-author-face :height 1.2))
      (inspire-insert-with-face (mapconcat (lambda (author) ; author list
					     (format "%s (%s)"
						     (alist-get 'name author)
						     (mapconcat 'identity (alist-get 'affiliation author) ", ")))
					   (alist-get 'authors entry) ", ") '(:inherit inspire-author-face :height 1.2)))
    (inspire-insert-with-face (format "\n %s \n\n"(alist-get 'date entry)) '(:inherit inspire-date-face :height 1.2))
    
    (when-let (journals (alist-get 'journals entry))
      (inspire-insert-with-face (format "Published in: %s.\n" (mapconcat 'identity journals ", ")) inspire-subfield-face))
    (when-let (conferences (alist-get 'conferences entry))
      (inspire-insert-with-face (format "Contribution to: %s.\n" (mapconcat 'identity conferences ", ")) inspire-subfield-face))
    (when-let (eprint (alist-get 'eprint entry))
      (inspire-insert-with-face (format "e-print: %s\n" eprint) inspire-subfield-face))
    (insert "\n\n")
    ;; DOI
    ;; abstracts
    ;; citations
  ))

(defun inspire-parse-literature (url)
  "Call inspire hep to search for literatures. 
Parse the returned results into a list."
  (with-current-buffer (url-retrieve-synchronously url)
  (set-buffer-multibyte t)
  (search-forward "\{")
  (backward-char)
  (let* ((root (alist-get 'hits (json-parse-buffer :object-type 'alist)))
	 (total (alist-get 'total root))
	 (hits (alist-get 'hits root))
	 (alist-entry) (alist-formed '()))    
    (seq-doseq (entry hits)
      (let ((metadata (alist-get 'metadata entry))
	    (title) (authors) (collaborations) (date) (journals) (conferences) (dois) (bib-link) (abstract) (eprint) (citation-count) (number-of-pages) (type) (inspire-id))
	(setq bib-link (map-nested-elt entry '(links bibtex)))
	(setq title (map-nested-elt metadata '(titles 0 title)))
	(setq date (map-nested-elt metadata '(preprint_date)))
	(setq abstract (map-nested-elt metadata '(abstracts 0 value)))
	(setq dois (mapcar (lambda (elem) (alist-get 'value elem)) (alist-get 'dois metadata)))
	(setq eprint (map-nested-elt metadata '(arxiv_eprints 0 value)))
	(setq citation-count (alist-get 'citation_count metadata))
	(setq number-of-pages (alist-get 'number_of_pages metadata))
	(setq inspire-id (alist-get 'id entry))
	(setq authors (mapcar (lambda (elem) `((name . ,(let ((str (alist-get 'full_name elem)))
						     (string-match "^\\(.+\\), \\(.+\\)$" str)
						     (concat (match-string 2 str) " " (match-string 1 str))))
					  (affiliation . ,(mapcar (lambda (aff) (alist-get 'value aff)) (alist-get 'affiliations elem)))))
			      (alist-get 'authors metadata)))
	(setq collaborations (and (alist-get 'collaborations metadata)
				  (mapcar (lambda (elem) (alist-get 'value elem)) (alist-get 'collaborations metadata))))
	(when-let (pubs (alist-get 'publication_info metadata))
	  (seq-doseq (pub pubs)
	    (cond ((alist-get 'journal_title pub)    ; journal
		   (let ((jtitle (alist-get 'journal_title pub))
			 (jyear (alist-get 'year pub))
			 (jvol (alist-get 'journal_volume pub))
			 (pstart (alist-get 'page_start pub))
			 (pend (alist-get 'page_end pub)))
		     (setq journals (cons (format "%s %s (%s) %s-%s" jtitle jvol jyear pstart pend) journals))))
		  ((alist-get 'cnum pub)    ; conference
		   (setq conferences (cons (format "%s" (alist-get 'cnum pub)) conferences)))
		  )))
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
			    (eprint . ,eprint)
			    (citation-count . ,citation-count)
			    (number-of-pages . ,number-of-pages)
			    (type . ,type)
			    (inspire-id . ,inspire-id)))
	(setq alist-formed (cons alist-entry alist-formed))))
     (reverse alist-formed))))

(provide 'inspire)
;;; inspire.el ends here
