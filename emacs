;; Add org-mode to path
(setq load-path (cons "~/src/3rdparty/elisp/org-mode/lisp" load-path))

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(setq org-log-done t)
(global-font-lock-mode 1)
(add-to-list 'load-path "~/.emacs.d/remember")
;; Use environment variable $WORKORG to get dir for org-directory
(setq org-directory (getenv "WORKORG"))
;; Set agenda files = all files in the org-directory, meow
(require 'org-install)
(setq org-agenda-files (file-expand-wildcards (concat org-directory "/*.org")))
(setq work-notes-file (concat org-directory "/notes.org"))
;;http://orgmode.org/manual/Setting-up-Remember.html#Setting-up-Remember

(define-key global-map "\C-cr" 'org-remember)
;; "GTD" mode for emacs
;; From http://sachachua.com/wp/2007/12/28/emacs-getting-things-done-with-org-basic/
(require 'remember-autoloads)
(setq org-remember-templates
           '(("Personal" ?p "* TODO %?\n  %i\n  %a" "~/Documents/personal/organizer.org")
             ("Work" ?w "* TODO %?\n  %i\n  %a" "notes.org") ;; Automagically defaults to WORKDIR notes.org
             ;;("Work" ?w "* TODO %?\n  %i\n  %a" "/home/nate/Documents/Contegix/org/notes.org")
                     ("Appointments" ?a "* Appointment: %?\n%^T\n%i\n  %a" "~/Documents/personal/organizer.org")))

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

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("/home/nate/Documents/Contegix/org/notes.org"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
