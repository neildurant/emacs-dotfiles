(setenv "PATH" 
        (concat 
          (concat (getenv "HOME") "/src/2ndparty/leiningen/bin")
          ":"
          (getenv "PATH")))
(setq load-path (cons "~/.emacs.d/lib/markdown" load-path))
(autoload 'markdown-mode "markdown-mode.el"
          "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.text" . markdown-mode) auto-mode-alist))
           
;; Show matching parenthesis
(show-paren-mode 1)

;; Load this library to use the find-lisp-find-files function
(load-library "find-lisp")

(global-font-lock-mode 1)

;; GUI Options ----------------
;; No toolbar <evil laugh>
(if window-system 
    (tool-bar-mode -1))

;; Ignore case when searching
(setq case-fold-search t)

;; Use Ido mode, which is auto-completion
;; of files / directories and menu items, etc.
(require 'ido)
(ido-mode t)
;; fuzzy matching is a must have
(setq ido-enable-flex-matching t) 


;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(setq load-path (cons "~/.emacs.d/groovy" load-path))
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

(setq load-path (cons "~/.emacs.d/lib/package/" load-path))
(require 'package)
 (setq package-archives
       '(("original"    . "http://tromey.com/elpa/")
         ("gnu"         . "http://elpa.gnu.org/packages/")
         ("marmalade"   . "http://marmalade-repo.org/packages/")))

;(add-to-list 'package-archives '(
;    ("marmalade" . "http://marmalade-repo.org/packages/")
;    ("gnu" . "http://elpa.gnu.org/packages")
;    ("tromey" . "http://tromey.com/elpa")))
(package-initialize)


(add-to-list 'Info-default-directory-list
             (expand-file-name "~/.emacs.d/src/org/doc/"))
(add-to-list 'Info-default-directory-list
             (expand-file-name "~/.emacs.d/src/org/"))



;; Add color-theme to load-path
(setq load-path (cons "~/.emacs.d/lib/color-theme-6.6.0" load-path))
(require 'color-theme)
(color-theme-initialize)
;;(color-theme-robin-hood)

;; Add nate to load-path
(setq load-path (cons "~/.emacs.d/lib/nate" load-path))

;; Keyboard Shortcuts

;; Bookmark shortcuts
(global-set-key [f7] 'bookmark-bmenu-list)
(global-set-key [(shift f7)] 'bookmark-set)

;; Kill buffer
(global-set-key [(control f4)] '(lambda () (interactive) (kill-buffer)))

(global-set-key [f2] 'ido-switch-buffer)
(if (< emacs-major-version 23)
   (defun characterp (obj)
     (and (char-or-string-p obj) (not (stringp obj)))))

;; Load the custom.el file, to bring in
;; settings that were set using emacs' custom
;; screen
(setq custom-file "~/.emacs.d/custom.el")
(when 
   (file-exists-p custom-file)
   (load custom-file)
)

;; Highlight the current line
(global-hl-line-mode 1)

;; Use control-up, down, left, right to
;; move cursor between windows
(windmove-default-keybindings 'control)
;; Use my keybindings for switching buffers
(global-set-key (kbd "C-<left>") 'previous-buffer)
(global-set-key (kbd "C-<right>") 'next-buffer)
(global-set-key (kbd "s-<right>") 'next-buffer)
(global-set-key (kbd "s-<left>") 'previous-buffer)

;; Use f2 to show buffers
(defun show-buffers-and-switch ()
  (interactive)
  (ibuffer)
)

;; (global-set-key [f2] 'show-buffers-and-switch)
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "s-w") 'delete-window)


;; IMPORTANT: Use Windoze key for meta key
;; (when (eq 'darwin' system-type)
(setq x-meta-keysym 'super)
(setq x-super-keysym 'meta)

;; Recent files (Open with C-x C-r)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Platform-specific stuff
;; (when (eq system-type 'darwin)
(when (eq window-system 'ns)
       (set-face-font 'default "-apple-monaco-medium-r-normal--18-120-72-72-m-120-iso10646-1")
)


(setq default-major-mode 'org-mode)

;; Used for idiotic Cocoa Emacs that doesn't get the DEL key set correctly.
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/DEL-Does-Not-Delete.html
(normal-erase-is-backspace-mode 1)

;; Used to go through grep/occur findings.
;; Setting these in org-mode only to avoid potential conflicts?
(define-key global-map (kbd "M-p") '(lambda() (interactive) (previous-error)))
(define-key global-map (kbd "M-n") '(lambda() (interactive) (next-error)))
(define-key global-map (kbd "M-o") 'occur)

;; Prevent us from having to type entire 'yes' or 'no' when answering
;; "Do you want to save?" questions :-/
;; http://blog.enqueue.eu/emacs-mac-3
(fset 'yes-or-no-p 'y-or-n-p)

;; Add org-mode to path
(setq load-path (cons "~/.emacs.d/src/org/contrib/lisp" load-path))
(setq load-path (cons "~/.emacs.d/src/org/lisp" load-path))
(setq load-path (cons "~/.emacs.d/src/org-tree-slide" load-path))
(setq load-path (cons "~/.emacs.d/src/org-occur-goto" load-path))
(setq load-path (cons "~/.emacs.d/src/emacs-calfw" load-path))
(setq load-path (cons "~/.emacs.d/src/todochiku" load-path))


(require 'org-tree-slide)
(require 'org-capture)
(require 'calfw-org)
;; Disable annoying font-lock message on OSX
(setq font-lock-verbose nil)
;; Disable annoying bell in OSX
(setq ring-bell-function (lambda nil))
;; (require 'org-babel-init)     
;; (require 'org-install)
(org-babel-load-file "~/.emacs.d/org-mode-config.org")
(if (file-exists-p "~/.emacs.d/org-mode-config-local.org")
    (org-babel-load-file "~/.emacs.d/org-mode-config-local.org"))

;; Search stack overflow for 'runmate'
(defun njn/runvim ()
  (interactive)
  (save-buffer)
  (if (eq 'darwin system-type)
    (njn/runeditor "/Users/nate/bin/mvim")
  (njn/runeditor "gvim"))
)

;; http://superuser.com/questions/184340/emacs-how-to-return-to-last-position-after-scroll-page-up-down
(setq scroll-preserve-screen-position 't)

(defun njn/runeditor (editor)
  (let (filename (file-truename buffer-file-name))
    (setq cmd (format "%s %s" editor (file-truename buffer-file-name)))
    (save-window-excursion
      (async-shell-command cmd))))

; http://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs
(defun njn/kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

;; Turn on auto-fill mode by default
;; (setq-default auto-fill-function 'do-auto-fill)

;; Backup directory
(setq backup-directory-alist '(("." . "~/tmp/emacs-backups")))

(setq load-path (cons "~/.emacs.d/src/o-blog" load-path))

;; begin todochiku, enable the growlnotify
(load-file "~/.emacs.d/src/todochiku/todochiku.el")
(setq todochiku-icons-directory "~/Downloads/todochiku-icons")
;; end todochiku

;; begin: orgmode + appt
; For org appointment reminders
;; Get appointments for today
(defun my-org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; Run once, activate and schedule refresh
(my-org-agenda-to-appt)
(appt-activate t)
(run-at-time "24:01" nil 'my-org-agenda-to-appt)

; Update appt each time agenda opened.
(add-hook 'org-finalize-agenda-hook 'my-org-agenda-to-appt)
;; end:   orgmode + appt

(setq appt-display-interval 11)
(setq org-directory "~/Documents/org")
