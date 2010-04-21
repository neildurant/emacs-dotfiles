

(require 'org-list)
(require 'remember)

(org-remember-insinuate)

;; Trigger org-mode for files ending in .org .org_archive and .txt
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(setq org-log-done t)


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



(require 'remember-autoloads)




(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(eval-after-load 'remember
                     '(add-hook 'remember-mode-hook 'org-remember-apply-template))


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

; Refile targets default to only filez found in personal-org-files directory
(defun njn/set-default-refile-targets() 
  (interactive) 
  (setq org-refile-targets 
	(quote ((personal-org-files :maxlevel . 5))))
)

(njn/set-default-refile-targets)

(defun njn/add-notesmine-to-refile-targets() 
  (interactive)
  (setq org-refile-targets 
	(quote 
	 ((personal-org-files :maxlevel . 5)
          (notesmine-org-files :maxlevel . 5))))
  )

(setq org-refile-allow-creating-parent-nodes 'confirm)

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

; Targets complete in steps so we start with filename, TAB shows the next level of targets etc 
(setq org-outline-path-complete-in-steps t)


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
(add-to-list 'org-modules 'org-habit)
(setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
 ("STARTED" :foreground "blue" :weight bold)
 ("DONE" :foreground "forest green" :weight bold)
 ("WAITING" :foreground "orange" :weight bold)
 ("SOMEDAY" :foreground "magenta" :weight bold)
 ("CANCELLED" :foreground "forest green" :weight bold)
 ("OPEN" :foreground "blue" :weight bold)
 ("PROJECT" :foreground "red" :weight bold))))

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
       ["Remove Limit to file" org-agenda-remove-restriction-lock]
       ["Regular View" org-agenda-list]
       ["Show Agenda" org-agenda]
       )
     )
  )

;; From norang.org -- Change task state to STARTED from TODO when clocking in -------
(defun bh/clock-in-to-started (kw)
  "Switch task from TODO to STARTED when clocking in"
  (if (and (string-equal kw "TODO") ;; Unless we're in a remember buffer
           (not (string-equal (buffer-name) "*Remember*")))
      "STARTED"
    nil))

(setq org-clock-in-switch-to-state (quote bh/clock-in-to-started))


;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK" "CLOCK")))
;; Automatically clock in when adding a note
(add-hook 'remember-mode-hook 'org-clock-in 'append)
;; Save clock data in the CLOCK drawer and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer "CLOCK")
;; Don't clock out when moving task to a done state
(setq org-clock-out-when-done nil)

;; If there's an existing clocked task, then prompt to clock back in.
(add-hook 'org-remember-before-finalize-hook 'njn/clock-in-interrupted-task)

(defun njn/clock-in-interrupted-task ()
 "Clock in the interrupted task if there is one"
 (interactive)
 (if (and (not org-clock-resolving-clocks-due-to-idleness)
          (marker-buffer org-clock-marker)
          (marker-buffer org-clock-interrupted-task)
          (y-or-n-p "Clock back in to prev. task? "))
     (org-with-point-at org-clock-interrupted-task
       (org-clock-in nil))))

;; TODO: Use these after getting UUIDs of often used tasks
;; (global-set-key (kbd "<f9> m") 'bh/clock-in-read-mail-and-news-task)
;; (global-set-key (kbd "<f9> o") 'bh/clock-in-organization-task)
;; (global-set-key (kbd "<f9> O") 'org-clock-out)

(defun njn/clock-in-task-by-id (id)
 "Clock in a task by id"
 (require 'org-id)
 (save-restriction
   (widen)
   (org-with-point-at (org-id-find id 'marker)
     (org-clock-in nil))))




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


