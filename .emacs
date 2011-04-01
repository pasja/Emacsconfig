;; forbidden commands

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Proper start

(setq initial-scratch-message nil
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      confirm-nonexistent-file-or-buffer nil
      vc-follow-symlinks t ; auto-follow version controlled symlinks
      display-time-day-and-date t
      display-time-24hr-format t)
(setq-default major-mode 'text-mode)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(column-number-mode 1)
(global-linum-mode 1)
(setq linum-format "%d ")
(size-indication-mode t)
(display-time)

(global-set-key (kbd "M-g") 'goto-line)    ; M-g  'goto-line
(global-set-key (kbd "<delete>") 'delete-char)  ; delete == delete
(global-set-key (kbd "M-2") 'hippie-expand)
(require 'windmove) ; windmove
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "C-<left>") 'next-buffer) ; buffer change
(global-set-key (kbd "C-<right>") 'previous-buffer)

(unless (file-exists-p "~/.emacs.d/plugins/") ; create default directories
  (make-directory "~/.emacs.d/plugins/"))
(unless (file-exists-p "~/.emacs.d/cache/")
    (make-directory "~/.emacs.d/cache/"))

;; Boostrap el-get

(url-retrieve
 "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
 (lambda (s)
   (end-of-buffer)
   (eval-print-last-sexp)))

;; Useful aliases

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'perl-mode 'cperl-mode)
(defalias 'eb 'eval-buffer)

;; rectangles

(setq cua-enable-cua-keys nil)
(cua-mode t)

;; ido mode

(require 'ido)
(setq
  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
  ido-ignore-buffers ; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"

     "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
  ido-case-fold  t                 ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)
  ido-enable-flex-matching t       ; try to be too smart :-)
  ido-max-prospects 16              ; don't spam my minibuffer
  ido-confirm-unique-completion t ; wait for RET, even with unique completion
  confirm-nonexistent-file-or-buffer nil ; when using ido, the confirmation is rather annoying...
  ido-everywhere t
  ido-create-new-buffer 'always)

(add-hook 'ido-minibuffer-setup-hook ; increase minibuffer size when ido completion is active
  (function
    (lambda ()
      (make-local-variable 'resize-minibuffer-window-max-height)
      (setq resize-minibuffer-window-max-height 1))))

(ido-mode 1)

;; ibuffer

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; help for minibuffer

(icomplete-mode t)

;; savehist: save some history
(setq savehist-additional-variables    ; also save...
  '(search ring regexp-search-ring)    ; ... my search entries
  savehist-autosave-interval 60        ; save every minute (default: 5 min)
  savehist-file "~/.emacs.d/cache/savehist")   ; keep my home clean
(savehist-mode t)                      ; do customization before activation

;; autokill shell

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; clipboard settings

(setq x-select-enable-primary t  ; killing/yanking interacting with primary X11 selection
      x-select-enable-clipboard t  ; killing/yanking interact with clipboard X11 selection
      yank-pop-change-selection t)  ; rotating the kill ring change the X11 clipboard.

;; zone-mode for .hu

(add-to-list 'auto-mode-alist '("\\.hu$" . zone-mode))

;; save hooks

(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p) ; auto chmod scripts
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; remove trailing whitespace

;; configure cperl

(eval-after-load 'cperl-mode
  '(progn
     (define-key cperl-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (cperl-set-style "K&R")
     (setq cperl-invalid-face nil
	   cperl-indent-parens-as-block t
	   cperl-tab-always-indent nil
	   cperl-highlight-variables-indiscriminately t)
     ))

;; smart shell start

(defun sh (name)
     "Smart shell start"
     (interactive "sShell name: ")
     (shell name)
     (set-process-query-on-exit-flag (get-process "shell") nil))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; set scheme

(add-hook 'inferior-scheme-mode-hook
	  '(lambda ((set-process-query-on-exit-flag (get-process "scheme") nil))

;; External libraries

(add-to-list 'load-path "~/.emacs.d/plugins")
(when (file-exists-p "~/.emacs.d/plugins/cperl-mode.el")
  (load "cperl-mode.el"))

;; External libraries (with el-get)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(setq el-get-sources
 '(el-get
   icomplete+
   yasnippet
   (:name color-theme
	  :after (lambda()
		    (color-theme-taming-mr-arneson)
		    ))
   (:name git-emacs
	  :after (lambda ()
		   (require 'git-status)
		   ))
   (:name dired+
	  :after (lambda ()
		   (require 'dired+)
		   (toggle-dired-find-file-reuse-dir 1) ; reuse existing dired buffer
		   (setq dired-recursive-copies 'always ; recursive copy/delete
			 dired-recursive-deletes 'top
			 dired-dwim-target t)
		   (define-key dired-mode-map (kbd "^")
		     (lambda ()
		       (interactive) (find-alternate-file "..")))
		   ))
   (:name autopair
	  :after (lambda ()
		   (require 'autopair)
		   (autopair-global-mode 1)
		   ))
   (:name mode-compile
	  :after (lambda()
		   (autoload 'mode-compile "mode-compile"
		     "Command to compile current buffer file based on the major mode" t)
		   (global-set-key (kbd "C-c c") 'mode-compile)
		   (autoload 'mode-compile-kill "mode-compile"
		     "Command to kill a compilation launched by `mode-compile'" t)
		   (global-set-key (kbd "C-c k") 'mode-compile-kill)
		   ))
   ))

(el-get 'sync)
