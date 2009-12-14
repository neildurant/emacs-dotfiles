;; Not used as of 2009/12/14
;; Define F8 to toggle between work-org files and home/personal org files
;; http://www.mail-archive.com/emacs-orgmode@gnu.org/msg08209.html
(defun org-my-toggle-agenda-file-set ()
 (interactive)
 (if (equal org-agenda-files "~/Documents/personal/agenda_files_work")
     (setq org-agenda-files "~/Documents/personal/agenda_files_home")
   (setq org-agenda-files "~/Documents/personal/agenda_files_work"))
 (message "Using %s" org-agenda-files))

(define-key org-mode-map (kbd "<f8>") 'org-my-toggle-agenda-file-set)
