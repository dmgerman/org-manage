;;; org-manage.el --- Manage org files in a given directory

;; Copyright (C) 2013 Yoshinari Nomura.
;; Copyright (C) 2013 Daniel German
;;
;; version 0.2  <2013-05-29 Wed>
;;
;; Author:  Yoshinari Nomura <nom@quickhack.net>
;; Author:  Daniel German    <dmg@uvic.ca>
;;
;; Based on org-octopress.el by Yoshinari Nomura
;;
;; Licensed under the same terms as Org-mode (http://orgmode.org/).
;; 
;; This module allows easy overview of any org file in a directory
;; (and its subdirectories).  It shows in a table each of the org
;; files, its title and last modification time. The user can easily
;; jump to any of those files from it.
;;
;;
;; Requirements:
;;     You need to install ctable from https://github.com/kiwanami/emacs-ctable
;;
;; Basic settings:
;;
;; (setq org-manage-directory-org       "~/myorgfiles")
;;
;; M-x org-manage
;;

;;; Code:

(require 'org)
(require 'ctable)


;;; Publishing


;;; the default location of the org files
(defvar org-manage-directory-org  "~/org/" 
  "the default directory to scan where  org files are located. It is trimmed from the list of filenames.")

(defvar org-manage-date-format "%y/%m/%d %H:%M" 
  "default date format for last modification of files")

;;; Summary Mode

;; keymap
(defvar org-manage-summary-mode-map  nil
  "Keymap for `org-manage-summary-mode'.")

(defvar org-manage-summary-mode-hook nil)

;; control finding files and recursion

(defvar org-manage-max-recursion 2
   "Stop recursively scannind subdirectories at this depth. < 1for no recursion")

(defvar org-manage-org-files-match "\\.org$"
   "Regular expression for org files. Only these files are displayed.")

(defvar org-manage-org-ignore nil
   "Regular expression for files/directories to ignore during scanning.")

(defvar org-manage-org-max-searched-bytes nil
   "Only search in the first given bytes of the file for the category/title of the file. Nil search in all file")


;; internal variables
(defvar org-manage-table-cp nil 
  "Keeps the table to display")


(defun org-manage-directory-files-recursive (directory match maxdepth ignore)
  "List files in DIRECTORY and in its sub-directories. 
   Return files that match the regular expression MATCH but ignore     
   files and directories that match IGNORE (IGNORE is tested before MATCH. Recurse only 
   to depth MAXDEPTH. If zero or negative, then do not recurse"
  (let* ((files-list '())
         (current-directory-list
          (directory-files directory t)))
    ;; while we are in the current directory
     (while current-directory-list
       (let ((f (car current-directory-list)))
         (cond 
          ((and
	    ignore
	    (string-match ignore f)
	    )
           ; ignore
            nil
           )
          ((and
            (file-regular-p f)
            (file-readable-p f)
            (string-match match f))
	   (setq files-list (cons f files-list))
           )
          ((and
           (file-directory-p f)
           (file-readable-p f)
           (not (string-equal ".." (substring f -2)))
           (not (string-equal "." (substring f -1)))
           (> maxdepth 0))     
           ;; recurse only if necessary
           (setq files-list (append files-list (org-manage-directory-files-recursive f match (- maxdepth -1) ignore)))
           )
          (t)
          )
         )
       (setq current-directory-list (cdr current-directory-list))
       )
       files-list
     )
    )

(defun org-manage-merge-keymap (keymap1 keymap2)
  (append keymap1
          (delq nil
                (mapcar
                 (lambda (x)
                   (if (or (not (consp x))
                           (assoc (car x) keymap1))
                       nil x))
                 keymap2))))

(unless org-manage-summary-mode-map
 (setq org-manage-summary-mode-map (make-sparse-keymap))
; todo: make it update the table when the g key is pressed
 (define-key org-manage-summary-mode-map "g" 'org-manage-update)
 (define-key org-manage-summary-mode-map "q" 'org-manage-quit)
 (setq org-manage-summary-mode-map
       (org-manage-merge-keymap org-manage-summary-mode-map ctbl:table-mode-map)))

;; summary 
(defun org-manage-summary-mode ()
  "Major mode for listing and controlling org-mode based blog articles.
\\{org-manage-summary-mode-map}"
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map org-manage-summary-mode-map)
  (make-local-variable 'org-manage-table-cp) 
  (setq major-mode 'org-manage-summary-mode
        mode-name  "org-manage")
  (setq buffer-undo-list t
        buffer-read-only t)
  (run-hooks 'org-manage-summary-mode-hook))

(defun org-manage-update ()
  "Update the current view"
  (interactive)
  (let (buffer-read-only
        (model (ctbl:cp-get-model org-manage-table-cp)))
    (setf (ctbl:model-data model) (org-manage-scan-files))
    (ctbl:cp-update org-manage-table-cp)))

(defun org-manage-summary-header (&optional title)
  (concat
   (format "%s\n" (or title "My org files"))
   (mapconcat
    'identity
    (org-manage-summary-command-help
     (remove-duplicates
      (mapcar 'cdr (cdr org-manage-summary-mode-map)))
     org-manage-summary-mode-map)
    "\n")
   "\n\n\n"))

(defun org-manage-summary-command-help (symbols &optional keymap)
  (let (symbol keysym keystr docstr summary-list)
    (while (setq symbol (car symbols))
      (setq keysym (where-is-internal symbol (or keymap (current-local-map)) nil)
            keystr (if keysym (mapconcat 'key-description keysym ",") "No keybind")
            docstr (documentation symbol))
      (if docstr
          (setq summary-list (cons (format "%10s ... %s (%s)"
                                           keystr
                                           (car (split-string docstr "\n"))
                                           symbol)
                                   summary-list)))
      (setq symbols (cdr symbols)))
    summary-list))

(defun org-manage-summary-table (contents keymap)
  (let ((param (copy-ctbl:param ctbl:default-rendering-param)))
    (setf (ctbl:param-fixed-header param) t)
    (ctbl:create-table-component-region
     :param param
     :width  nil
     :height nil
     :keymap keymap
     :model
     (make-ctbl:model
      :data contents
      :sort-state '(-1 2)
      :column-model
      (list (make-ctbl:cmodel
             :title "Date"
             :sorter 'ctbl:sort-string-lessp
             :min-width 10
             :align 'left)
            (make-ctbl:cmodel
             :title "Title"
             :align 'left
             :min-width 40
             :max-width 80)
            (make-ctbl:cmodel
             :title "Category"
             :align 'left
             :min-width 20
             :max-width 30)
            (make-ctbl:cmodel
             :title "Filename"
             :align 'left
             :min-width 40
             :max-width 140)
            )))))

(defun org-manage-extract-properties-file (filename)
  (let ((title "")
	(category "")
	(shortname "short")
	(prefix (expand-file-name org-manage-directory-org))
	plist)
    (if filename
        (with-temp-buffer
          (insert-file-contents filename)
	  (if (string= (substring filename 0 (length prefix)) prefix)
	      (setq shortname (substring filename (length prefix)))
	      )
	  (goto-char (point-min))
	  (save-match-data 
	    (if (re-search-forward "#\\+TITLE: *\\(.+\\)$" org-manage-org-max-searched-bytes t)
		(setq title (match-string 1))
	      )
	    (goto-char (point-min))
	    (if (re-search-forward "#\\+CATEGORY: *\\(.+\\)$" org-manage-org-max-searched-bytes t)
		(setq category (match-string 1))
	      )
	    )
          (setq plist (list  ; build list: date;title;category;shortname;filename
		       (format-time-string org-manage-date-format (nth 5 (file-attributes filename 'string)))
		        title category shortname filename)
		)
	  )
      plist)))

(defun org-manage-scan-files ()
  (mapcar
   (lambda (filename)
     (org-manage-extract-properties-file filename))
   (org-manage-directory-files-recursive 
    (expand-file-name org-manage-directory-org) org-manage-org-files-match org-manage-max-recursion org-manage-org-ignore
    )
   ))

;; scan directory recursively

;; startup
(defun org-manage (&optional title)
  "Org-mode and Octopress."
  (interactive)
  (let ((buf (get-buffer-create "org-manage"))
        (cp))
    (switch-to-buffer buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (org-manage-summary-header title))
    (save-excursion
      (setq cp (org-manage-summary-table 
                (org-manage-scan-files) org-manage-summary-mode-map)))
    (ctbl:cp-add-click-hook
     cp
     (lambda ()
       (find-file (nth 4 (ctbl:cp-get-selected-data-row cp)))))
    (org-manage-summary-mode)
    (ctbl:navi-goto-cell
     (ctbl:find-first-cell (ctbl:component-dest cp)))
    (setq org-manage-table-cp cp)
    ))

(defun org-manage-quit ()
  "Simply quit"
  (interactive)
  (kill-buffer (current-buffer))
  )


(provide 'org-manage)

;;; org-manage.el ends here
