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

;; Platform-specific stuff
(when (eq system-type 'darwin)
       (set-face-font 'default "-apple-monaco-medium-r-normal--14-120-72-72-m-120-iso10646-1")
)


(setq default-major-mode 'org-mode)

;; Used for idiotic Cocoa Emacs that doesn't get the DEL key set correctly.
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/DEL-Does-Not-Delete.html
(normal-erase-is-backspace-mode 1)



;; Used to go through grep/occur findings.
;; Setting these in org-mode only to avoid potential conflicts?
(define-key global-map (kbd "M-p") '(lambda() (interactive) (previous-error)))
(define-key global-map (kbd "M-n") '(lambda() (interactive) (next-error)))


;; Prevent us from having to type entire 'yes' or 'no' when answering
;; "Do you want to save?" questions :-/
;; http://blog.enqueue.eu/emacs-mac-3
(fset 'yes-or-no-p 'y-or-n-p)

;; Load org-mode customizations!  Yay!
(load "/Users/nate/.emacs.d/org-mode-custom.el")


(setq minibuffer-max-depth nil)
