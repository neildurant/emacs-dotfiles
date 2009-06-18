;; Add org-mode to path
(setq load-path (cons "~/src/3rdparty/elisp/org-mode/lisp" load-path))

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(setq org-log-done t)
(global-font-lock-mode 1)
(add-to-list 'load-path "~/.emacs.d/remember")
;; Use environment variable $WORKORG to get dir for org-directory
(setq org-directory (getenv "WORKORG"))
;; Set agenda files = contegix org file by default, meow
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
      '(("Work Tasks" ?t "* TODO %?\n  %i\n  %a" (concat org-directory "/tasks.org"))
        ("Work Notes" ?n "* %?                                        :NOTE:  %u  %a" 
           (concat org-directory "/notes.org") bottom nil)
        ("Personal" ?p "* %U %?\n\n  %i\n  %a" "~/Documents/personal/notes.org")
        ("Tasks" ?T "* TODO %U %?\n\n  %i\n  %a" "~/Documents/personal/tasks.org")
	("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/Documents/personal/journal.org")
        ("Ideas" ?i "* %^{Title}\n  %i\n  %a" "~/Documents/personal/ideas.org" "New Ideas")))

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(eval-after-load 'remember
                     '(add-hook 'remember-mode-hook 'org-remember-apply-template))
(global-set-key (kbd "C-c r") 'remember)                                         ;; (3)

(add-to-list 'auto-mode-alist '("\>org$" . org-mode))                            ;; (4)
(global-set-key (kbd "C-c a") 'org-agenda)                                       ;; (5)
(setq org-agenda-include-diary t)                                                ;; (7)
(setq org-agenda-include-all-todo t)


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
              ("n" "Notes" tags "NOTES" nil))))



;; GUI Options ----------------
(tool-bar-mode -1)            ;; No toolbar <evil laugh>

;; http://doc.norang.ca/org-mode.html#sec-1 ------------
(setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
 (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "PROJECT(P@)" "OPEN(O@)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
 ("STARTED" :foreground "blue" :weight bold)
 ("DONE" :foreground "forest green" :weight bold)
 ("WAITING" :foreground "orange" :weight bold)
 ("SOMEDAY" :foreground "magenta" :weight bold)
 ("CANCELLED" :foreground "forest green" :weight bold)
 ("QUOTATION" :foreground "red" :weight bold)
 ("QUOTED" :foreground "magenta" :weight bold)
 ("APPROVED" :foreground "forest green" :weight bold)
 ("EXPIRED" :foreground "forest green" :weight bold)
 ("REJECTED" :foreground "forest green" :weight bold)
 ("OPENPO" :foreground "blue" :weight bold)
 ("CLOSEDPO" :foreground "forest green" :weight bold)
 ("PROJECT" :foreground "red" :weight bold)
 ("PROJDONE" :foreground "forest green" :weight bold))))

;; Change task state w/C-c C-t KEY
(setq org-use-fast-todo-selection t)
