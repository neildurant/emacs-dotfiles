;; Show matching parenthesis
(show-paren-mode 1)

(global-font-lock-mode 1)

;; GUI Options ----------------
;; No toolbar <evil laugh>
(tool-bar-mode -1)            

;; Ignore case when searching
(setq case-fold-search t)

;; Use Ido mode, which is auto-completion
;; of files / directories and menu items, etc.
(require 'ido)
(ido-mode t)
;; fuzzy matching is a must have
(setq ido-enable-flex-matching t) 

;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; Bookmark shortcuts
(global-set-key [f7] 'bookmark-bmenu-list)
(global-set-key [(shift f7)] 'bookmark-set)

(if (< emacs-major-version 23)
   (defun characterp (obj)
     (and (char-or-string-p obj) (not (stringp obj)))))

;; Load the custom.el file, to bring in
;; settings that were set using emacs' custom
;; screen
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Highlight the current line
(global-hl-line-mode 1)

;; Use control-up, down, left, right to
;; move cursor between windows
(windmove-default-keybindings 'control)

;; Use f2 to show buffers
(defun show-buffers-and-switch ()
  (interactive)
  (list-buffers)
  (other-window 1)
)

(global-set-key [f2] 'show-buffers-and-switch)

;; IMPORTANT: Use Windoze key for meta key
(setq x-meta-keysym 'super)
(setq x-super-keysym 'meta)

;; Recent files (Open with C-x C-r)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(load "~/.emacs.d/org-mode-custom.el")
