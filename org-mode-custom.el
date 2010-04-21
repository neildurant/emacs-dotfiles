

(require 'org-list)
;; Trigger org-mode for files ending in .org .org_archive and .txt
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(setq org-log-done t)


(setq notesmine-dir (getenv "NOTESMINE_DIR"))

;; Define lists of agenda files for use later.
(setq personal-org-files (file-expand-wildcards (concat personal-org-dir "/*.org")))
(setq notesmine-org-files (file-expand-wildcards (concat notesmine-dir "/*.org")))








(add-to-list 'auto-mode-alist '("\>org$" . org-mode))                           ;; (4)

;; (setq diary-file "~/Documents/journal")
(setq org-agenda-diary-file "~/Documents/personal/journal.org")
(setq org-agenda-include-all-todo t)
;; If an item is SCHEDULED, then don't show the TODO
;; entry until the day it's due.
(setq org-agenda-todo-ignore-scheduled t)

(setq org-agenda-include-all-todo t)    

;; ---  http://doc.norang.ca/org-mode.html#sec-1
; Use IDO for target completion
(setq org-completion-use-ido t)



;; GUI Options ----------------
(tool-bar-mode -1)            ;; No toolbar <evil laugh>



  


(add-to-list 'org-modules 'org-habit)


(require 'org-publish)
;; Zap junk css in exported org files    
(setq org-export-html-style-default "")
(setq org-publish-project-alist
      '(

        ;; Define publishing mode for notesmine!
        ("notesmine-org"
         :base-directory notesmine-dir
         :base-extension "org"
         :publishing-directory (concat notesmine-dir "-html")
         :recursive t
         :publishing-function org-publish-org-to-html
         :style "<link rel=stylesheet href=\"./css/org.css\" type=\"text/css\">"
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         :auto-index t
         :index-filename "sitemap.org"  ; ... call it sitemap.org ...
         :index-title "Notesmine"         ; ... with title 'Sitemap'.
         )
        ;; Define publishing mode for static notesmine stuff.
        ("notesmine-org-static"
         :base-directory notesmine-dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory (concat notesmine-dir "-html")
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("org" :components ("org-notes" "org-static"))
        ))

(setq org-insert-heading-respect-content t)

(setq default-case-fold-search 'foo)

(defun sacha/org-agenda-clock (match)
  ;; Find out when today is
  (let* ((inhibit-read-only t))
    (goto-char (point-max))
    (org-dblock-write:clocktable
     `(:scope agenda
       :maxlevel 4
       :tstart ,(format-time-string "%Y-%m-%d" (calendar-time-from-absolute (1+ org-starting-day) 0))
       :tend ,(format-time-string "%Y-%m-%d" (calendar-time-from-absolute (+ org-starting-day 2) 0))))))

;; Set clockreport orange in agenda list to list things 3 levels and below, MMk?
;; http://www.mail-archive.com/emacs-orgmode@gnu.org/msg14212.html
(setq 
org-agenda-clockreport-parameter-plist '(:link t :maxlevel 99 ))
;; Hide leading stars to make org mode look cleaner
;; http://orgmode.org/manual/Clean-view.html
;;(setq org-hide-leading-stars t)
;;(setq org-odd-levels-only t)
(setq org-log-into-drawer t)

(if (< emacs-major-version 23)
   (defun characterp (obj)
     (and (char-or-string-p obj) (not (stringp obj)))))

;; Don't put blank lines after headings.
(setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))
;; C-M-<return> inserts a new subheading / sub list
(define-key org-mode-map (kbd "C-M-<return>") 'org-insert-subheading)
(define-key org-mode-map (kbd "<kp-enter>") 'org-insert-subheading)

;; Agenda showing closed items -------------
;; Default to only show clock items, not the closed ones.
(setq org-agenda-log-mode-items (quote (clock)))
(setq njn/org-agenda-show-closed 't)

(defun njn/toggle-agenda-log-show-closed()
  "Toggle whether closed clock thingies are shown in the agenda"
  (interactive)
  (if (eq njn/org-agenda-show-closed 't)
      (progn (setq org-agenda-log-mode-items (quote (clock)))
	     (setq njn/org-agenda-show-closed nil)
	     (message "NOT Showing closed clock entries in agenda"))
    (progn (setq org-agenda-log-mode-items (quote (closed clock)))
	   (setq njn/org-agenda-show-closed 't)
	   (message "Showing closed clock entries in agenda"))
    ))


;; Insert immediate timestamp
;; From russell on org-mode mailing list.
(define-key global-map (kbd "<f9>")
 '(lambda () (interactive)
    (when (eq major-mode 'org-mode)
      (insert "\n")
      (org-insert-time-stamp nil t t)
      (insert "\n"))))

(setq org-M-RET-may-split-line nil)
(setq org-insert-heading-always-after-current t)


(setq org-clock-report-include-clocking-task 't)


(require 'org-babel-perl)
(require 'org-babel-python)
;;(require 'org-babel-groovy)


