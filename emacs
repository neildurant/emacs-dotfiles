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
;;(setq org-agenda-files (file-expand-wildcards (concat org-directory "/*.org")))
(setq org-agenda-files (quote (concat org-directory "/contegix.org")))

;;http://orgmode.org/manual/Setting-up-Remember.html#Setting-up-Remember

(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)
;; http://doc.norang.ca/org-mode.html#sec-1
(global-set-key (kbd "C-M-r") 'org-remember)

;; "GTD" mode for emacs
;; From http://sachachua.com/wp/2007/12/28/emacs-getting-things-done-with-org-basic/
(require 'remember-autoloads)
(setq org-remember-templates
      '(("Tasks" ?t "* TODO %?\n  %i\n  %a" org-default-notes-file)))
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(eval-after-load 'remember
                     '(add-hook 'remember-mode-hook 'org-remember-apply-template))
(global-set-key (kbd "C-c r") 'remember)                                         ;; (3)

(add-to-list 'auto-mode-alist '("\>org$" . org-mode))                           ;; (4)
(global-set-key (kbd "C-c a") 'org-agenda)                                       ;; (5)
(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))                    ;; (6)
(setq org-agenda-include-diary t)                                                ;; (7)
(setq org-agenda-include-all-todo t)    
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

; Use IDO for target completion
(setq org-completion-use-ido t)
; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

;; GUI Options ----------------
(tool-bar-mode -1)            ;; No toolbar <evil laugh>