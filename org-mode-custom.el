;; Add remember code to load path
(add-to-list 'load-path "~/.emacs.d/remember")
;; Add org-mode to path
(setq load-path (cons "~/src/3rdparty/org-mode/lisp" load-path))
(setq load-path (cons "~/src/3rdparty/org-mode/contrib/lisp" load-path))

(require 'org-install)
(require 'org-list)
(require 'remember)
(org-remember-insinuate)

;; Trigger org-mode for files ending in .org .org_archive and .txt
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(setq org-log-done t)

;; Use environment variables to set org directories
(setq personal-org-dir (getenv "ORG_DIR"))
(setq notesmine-dir (getenv "NOTESMINE_DIR"))

;; Define lists of agenda files for use later.
(setq personal-org-files (file-expand-wildcards (concat personal-org-dir "/*.org")))
(setq notesmine-org-files (file-expand-wildcards (concat notesmine-dir "/*.org")))
(setq enrollio-org-files (file-expand-wildcards (concat personal-org-dir "/*bworksdb.org")))
(setq main-org-files (file-expand-wildcards (concat personal-org-dir "/nate.org")))

;; Default to main org files for agenda
(setq org-agenda-files main-org-files)

;;http://orgmode.org/manual/Setting-up-Remember.html#Setting-up-Remember
(setq org-default-notes-file (concat personal-org-dir "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)
;; "GTD" mode for emacs
;; From http://sachachua.com/wp/2007/12/28/emacs-getting-things-done-with-org-basic/
(require 'remember-autoloads)
(setq org-remember-templates
      '(
	("Note" ?n "* TODO %?\n\n%U  %i" "~/Documents/personal/notes.org")
        ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/Documents/personal/journal.org" "X" my-check)

))

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(eval-after-load 'remember
                     '(add-hook 'remember-mode-hook 'org-remember-apply-template))

(defun agenda-this-file-only ()
  (interactive)
  (org-agenda-set-restriction-lock 'file)
  (org-agenda-list)
)

;; Keyboard bindings
(global-set-key (kbd "<f5>") 'org-agenda)
(global-set-key (kbd "<f6> l") 'agenda-this-file-only)
(global-set-key (kbd "<f6> f") 'org-agenda-list)
(global-set-key (kbd "<f6> i") 'org-clock-in)
(global-set-key (kbd "<f6> j") 'org-clock-goto)
(global-set-key (kbd "<f6> o") 'org-clock-out)
(global-set-key (kbd "<f6> r") 'org-resolve-clocks)
(global-set-key (kbd "<f6> n") 'org-remember)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cb" 'org-iswitchb)
(add-to-list 'auto-mode-alist '("\>org$" . org-mode))                           ;; (4)

(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))                    ;; (6)
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

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets 
  (quote 
    (
      (personal-org-files :maxlevel . 5) 
      (notesmine-org-files :maxlevel . 5)
    )
  )
)

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

; Targets complete in steps so we start with filename, TAB shows the next level of targets etc 
(setq org-outline-path-complete-in-steps t)

; Custom agenda commands
(setq org-agenda-custom-commands 
      (quote (("P" "Projects" tags "/!PROJECT" ((org-use-tag-inheritance nil)))
              ("S" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
              ("w" "Tasks waiting on something" tags "WAITING" ((org-use-tag-inheritance nil)))
	      ("d" "DELEGATED" tags "DELEGATED" ((org-use-tag-inheritance nil)))
              ("o" "SOMEDAY" tags "SOMEDAY" ((org-use-tag-inheritance nil)))
              ("r" "Refile New Notes and Tasks" tags "REFILE" ((org-agenda-todo-ignore-with-date nil)))
              ("p" "Personal Agenda" agenda ""
               ((org-agenda-files personal-org-files)))
              ("e" "Enrollio Agenda" agenda ""
               ((org-agenda-files enrollio-org-files)))
              ("n" "Notesmine Agenda" agenda ""
               ((org-agenda-files notesmine-org-files)))
              ("j" "Journal" agenda ""
               ((org-agenda-files (file-expand-wildcards (concat personal-org-dir "/journal.org")))))
              ("g" "Geek Agenda" agenda ""
               ((org-agenda-files (file-expand-wildcards (concat personal-org-dir "/*geek.org")))))
              ;; Overview mode is same as default "a" agenda-mode, except doesn't show TODO
              ;; items that are under another TODO (setq org-agenda-custom-commands 
              ("o" "Overview" agenda "" ((org-agenda-todo-list-sublevels nil)))
              ;; Separate menu, with custom searches
              ("f" . "Find in Agenda Files")
              ("fa" "Archive search" search ""
               ((org-agenda-files (file-expand-wildcards (concat personal-org-dir "/*.org_archive")))))
              ("fn" "Notesmine search" search ""
               ((org-agenda-files notesmine-org-files)))
              ("fp" "Personal search" search ""
               ((org-agenda-files personal-org-files)))
              ("fe" "Enrollio search" search ""
               ((org-agenda-files enrollio-org-files)))
)))
;; GUI Options ----------------
(tool-bar-mode -1)            ;; No toolbar <evil laugh>


;; http://doc.norang.ca/org-mode.html#sec-1 ------------
;;(setq org-todo-keywords (quote (
;;(sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
;;(sequence "WAITING(w@/!)" "DELEGATED(e!)" "|" "DEFERRED" "CANCELLED")
;;)))

;; TODO States --
;; I only like the TODO STARTED DONE sequence.  Everything else,
;; I would rather just pick from a list, instead of getting surprised w/more junk
;;             WAITING    means I'm waiting on some other weenie to complete something
;;             DELEGATED  means some other weenie is doing this job :-)
;;             SOMEDAY    means item has been postponed indefinitely, but 
;;                        isn't started and doesn't have a planned start
;;             CANCELLED  means item won't be done (Yay!)
;; Examples: @/! means leave a note and record time when entering.
;;               The ! means to leave a timestamp when exiting, unless the
;;               next state records the time.
;; "D" means mark done/leave note, "d" is just quick "done" w/no note
(setq org-todo-keywords '(
(sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!)")  
(sequence "WAITING(w@/@)" "|" "DELEGATED(e@/@)" "SOMEDAY(o@/@)" "CANCELLED(c@/@)")
))

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

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Clock in-clock out
(defun set-clock-clockenspiel()
(interactive)
(setq org-agenda-log-mode-items (quote (clock)))
  (message "Using %s" org-agenda-log-mode-items))

(defun set-closed-clock-clockenspiel()
(interactive)
(setq org-agenda-log-mode-items (quote (closed clock)))
  (message "Using %s" org-agenda-log-mode-items))

(define-key org-mode-map (kbd "S-<f9>") 'set-clock-clockenspiel)
(define-key org-mode-map (kbd "<f9>") 'set-closed-clock-clockenspiel)

;; org-mode hook
(add-hook 'org-mode-hook
         (lambda ()
           (local-set-key (kbd "\M-\C-n") 'outline-next-visible-heading)
           (local-set-key (kbd "\M-\C-p") 'outline-previous-visible-heading)
           (local-set-key (kbd "\M-\C-u") 'outline-up-heading)))

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("0" . delete-window)
                                      ("1" . delete-other-windows)
                                      ("2" . split-window-vertically)
                                      ("3" . split-window-horizontally)
                                      ("h" . hide-other)
                                      ("k" . org-kill-note-or-show-branches)
                                      ;; Zap the current subtree
                                      ("d" . org-cut-special)
                                      ("r" . org-reveal))))

(easy-menu-define njn-menu org-mode-map "Nate's Org"
  '("Norg"

     ("Clock" ;; submenu
       ["In" org-clock-in]
       ["Out" org-clock-out]
       ["Resolve" org-resolve-clocks]
       ["Goto" org-clock-goto]
       )
     ("Agenda" ;; submenu
       ["Limit to file" agenda-this-file-only]
       ["Regular View" org-agenda-list]
       ["Show Agenda" org-agenda]
       )
     )
  )
;; From norang.org -- when adding a note, clock into it.
(add-hook 'remember-mode-hook 'org-clock-in 'append)
(add-hook 'org-remember-before-finalize-hook 'bh/clock-in-interrupted-task)

(defun bh/clock-in-interrupted-task ()
 "Clock in the interrupted task if there is one"
 (interactive)
 (if (and (not org-clock-resolving-clocks-due-to-idleness)
          (marker-buffer org-clock-marker)
          (marker-buffer org-clock-interrupted-task))
     (org-with-point-at org-clock-interrupted-task
       (org-clock-in nil))
   (org-clock-out)))

(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)

(global-set-key (kbd "<f9> m") 'bh/clock-in-read-mail-and-news-task)
(global-set-key (kbd "<f9> o") 'bh/clock-in-organization-task)
(global-set-key (kbd "<f9> O") 'org-clock-out)

(defun njn/clock-in-task-by-id (id)
 "Clock in a task by id"
 (require 'org-id)
 (save-restriction
   (widen)
   (org-with-point-at (org-id-find id 'marker)
     (org-clock-in nil))))

(defun njn/clock-in-organization-task ()
 (interactive)
 (njn/clock-in-task-by-id "437c2cde-fbf0-491f-92ba-51bae487b338"))

(defun njn/clock-in-read-mail-and-news-task ()
 (interactive)
 (njn/clock-in-task-by-id "85c2e69b-6f37-4236-8896-4f7dd86047c1"))
