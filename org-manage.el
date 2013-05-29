;;; org-manage.el --- Manage org files in a given directory

;; Copyright (C) 2013 Yoshinari Nomura.
;; Copyright (C) 2013 Daniel German
;;
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
(defvar org-manage-directory-org       "~/org")

;;; Summary Mode

;; keymap
(defvar org-manage-summary-mode-map  nil
  "Keymap for `org-manage-summary-mode'.")

(defvar org-manage-summary-mode-hook nil)

(defun org-manage--merge-keymap (keymap1 keymap2)
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
;  (define-key org-manage-summary-mode-map "g" 'org-manage-update)
 (setq org-manage-summary-mode-map
       (org-manage--merge-keymap org-manage-summary-mode-map ctbl:table-mode-map)))

;; summary 
(defun org-manage-summary-mode ()
  "Major mode for listing and controlling org-mode based blog articles.
\\{org-manage-summary-mode-map}"
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map org-manage-summary-mode-map)
  (setq major-mode 'org-manage-summary-mode
        mode-name  "org-manage")
  (setq buffer-undo-list t
        buffer-read-only t)
  (run-hooks 'org-manage-summary-mode-hook))

(defun org-manage--summary-header (&optional title)
  (concat
   (format "%s\n" (or title "My org files"))
   (mapconcat
    'identity
    (org-manage--summary-command-help
     (remove-duplicates
      (mapcar 'cdr (cdr org-manage-summary-mode-map)))
     org-manage-summary-mode-map)
    "\n")
   "\n\n\n"))

(defun org-manage--summary-command-help (symbols &optional keymap)
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

(defun org-manage--summary-table (contents keymap)
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
             :max-width 140)
            (make-ctbl:cmodel
             :title "Filename"
             :align 'left
             :min-width 40
             :max-width 140)
            )))))

(defun org-manage-property (keys &optional filename)
  (let ((plist (org-manage-property-list filename)))
    (mapcar (lambda (key) (org-export-data-with-backend (plist-get plist key) 'html plist))
            keys)))

(defun org-manage-property-list (&optional filename)
  (let ((backend 'html) plist)
    (if filename
        (with-temp-buffer
          (insert-file-contents filename)
          (org-mode)
          (setq plist (org-export-get-environment backend))
          (setq plist (plist-put plist :input-file filename))
	  (setq plist (plist-put plist :last-mod  (format-time-string "%y/%m/%d" (nth 5 (file-attributes filename 'string)))))
	  )
          
      (setq plist (org-export-backend-options backend))
      plist)))


(defun org-manage--scan-file ()
  (mapcar
   (lambda (filename)
     (org-manage-property
      '(:last-mod
	:title
        :input-file
        :input-file
        )
      filename))
   (directory-files
    (expand-file-name
     org-manage-directory-org) t "^.*\\.org$")))

;; startup
(defun org-manage (&optional title)
  "Org-mode and Octopress."
  (interactive)
  (let ((buf (get-buffer-create "org-manage"))
        (cp))
    (switch-to-buffer buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (org-manage--summary-header title))
    (save-excursion
      (setq cp (org-manage--summary-table 
                (org-manage--scan-file) org-manage-summary-mode-map)))
    (ctbl:cp-add-click-hook
     cp
     (lambda ()
       (find-file (nth 2 (ctbl:cp-get-selected-data-row cp)))))
    (org-manage-summary-mode)
    (ctbl:navi-goto-cell
     (ctbl:find-first-cell (ctbl:component-dest cp)))
    ))

;;; Helpers

(defun org-manage--sanitize-title (title)
  (replace-regexp-in-string "[\t ]+" "-" (downcase title)))

(provide 'org-manage)

;;; org-manage.el ends here
