;; Add org-mode to path
(setq load-path (cons "~/src/3rdparty/elisp/org-mode/lisp" load-path))

(setq load-path (cons "~/.emacs.d/groovy" load-path))

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(setq org-log-done t)
(global-font-lock-mode 1)
(add-to-list 'load-path "~/.emacs.d/remember")
;; Use environment variable $WORKORG to get dir for org-directory
(setq org-directory (getenv "WORKORG"))

(require 'org-install)
;;http://orgmode.org/manual/Setting-up-Remember.html#Setting-up-Remember

(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)
;; http://doc.norang.ca/org-mode.html#sec-1
(global-set-key (kbd "C-M-r") 'org-remember)

;; "GTD" mode for emacs
;; From http://sachachua.com/wp/2007/12/28/emacs-getting-things-done-with-org-basic/
(require 'remember-autoloads)
(setq org-remember-templates
      '(
        ("Programming" ?p "%[~/workdir/org/programming_template.org]" "notes.org" bottom)
        ("Task" ?t "* %?\n\n  %i\n%U" "notes.org")
	("Note" ?n "* %?\n\n%U  %i" "~/Documents/personal/notes.org")
))
(org-remember-insinuate)

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(eval-after-load 'remember
                     '(add-hook 'remember-mode-hook 'org-remember-apply-template))
(global-set-key (kbd "C-c r") 'remember)                                         ;; (3)

(add-to-list 'auto-mode-alist '("\>org$" . org-mode))                            ;; (4)
(global-set-key (kbd "C-c a") 'org-agenda)                                       ;; (5)
(setq org-agenda-include-diary t)                                                ;; (7)
(setq diary-file "~/Documents/personal/journal.org")
(setq org-agenda-include-all-todo t)
;; If an item is SCHEDULED, then don't show the TODO
;; entry until the day it's due.
(setq org-agenda-todo-ignore-scheduled t)

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; ---  http://doc.norang.ca/org-mode.html#sec-1
; Use IDO for target completion
(setq org-completion-use-ido t)

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

; Targets complete in steps so we start with filename, TAB shows the next level of targets etc 
(setq org-outline-path-complete-in-steps t)

(setq org-agenda-custom-commands 
      (quote (("P" "Projects" tags "/!PROJECT" ((org-use-tag-inheritance nil)))
              ("s" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
              ("w" "Tasks waiting on something" tags "WAITING" ((org-use-tag-inheritance nil)))
              ("r" "Refile New Notes and Tasks" tags "REFILE" ((org-agenda-todo-ignore-with-date nil)))
              ;; Overview mode is same as default "a" agenda-mode, except doesn't show TODO
              ;; items that are under another TODO item.
              ("o" "Overview" agenda "" ((org-agenda-todo-list-sublevels nil)))
              ("n" "Notes" tags "NOTES" nil))))

;; GUI Options ----------------
(tool-bar-mode -1)            ;; No toolbar <evil laugh>


;; http://doc.norang.ca/org-mode.html#sec-1 ------------
(setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")

(setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
 ("STARTED" :foreground "blue" :weight bold)
 ("DONE" :foreground "forest green" :weight bold)
 ("WAITING" :foreground "orange" :weight bold)
 ("SOMEDAY" :foreground "magenta" :weight bold)
 ("CANCELLED" :foreground "forest green" :weight bold)
 ("OPEN" :foreground "blue" :weight bold)
 ("PROJECT" :foreground "red" :weight bold))))

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")

;; Change task state w/C-c C-t KEY
(setq org-use-fast-todo-selection t)
(require 'org-publish)
;; Zap junk css in exported org files    
(setq org-export-html-style-default "")
(setq org-publish-project-alist
      '(

;; Define publishing mode for notesmine!
("notesmine-org"
 :base-directory "~/Documents/notesmine-org/"
 :base-extension "org"
 :publishing-directory "~/Documents/notesmine-org-html/"
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
 :base-directory "~/Documents/notesmine-org/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/Documents/notesmine-org-html/"
 :recursive t
 :publishing-function org-publish-attachment
 )

 ("org" :components ("org-notes" "org-static"))
      ))

;; From http://www.mail-archive.com/emacs-orgmode@gnu.org/msg03199.html
;; Allows you to create a new heading after the current line, no matter where your
;; cursor is.
(defun org-new-heading-after-current ()
    "Insert a new heading with same level as current, after current subtree."
    (interactive)
    (org-back-to-heading)
    (org-insert-heading)
    (org-move-subtree-down)
    (end-of-line 1))

(define-key org-mode-map (kbd "M-<return>") 'org-new-heading-after-current)
(setq default-case-fold-search 'foo)


;;in the .emacs
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; fuzzy matching is a must have


(defun sacha/org-agenda-clock (match)
  ;; Find out when today is
  (let* ((inhibit-read-only t))
    (goto-char (point-max))
    (org-dblock-write:clocktable
     `(:scope agenda
       :maxlevel 4
       :tstart ,(format-time-string "%Y-%m-%d" (calendar-time-from-absolute (1+ org-starting-day) 0))
       :tend ,(format-time-string "%Y-%m-%d" (calendar-time-from-absolute (+ org-starting-day 2) 0))))))

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
;; Set clockreport orange in agenda list to list things 3 levels and below, MMk?
;; http://www.mail-archive.com/emacs-orgmode@gnu.org/msg14212.html
(setq 
org-agenda-clockreport-parameter-plist '(:link t :maxlevel 99 ))
;; Hide leading stars to make org mode look cleaner
;; http://orgmode.org/manual/Clean-view.html
(setq org-hide-leading-stars t)
(setq org-odd-levels-only t)


(setq org-log-into-drawer t)