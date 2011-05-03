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
(windmove-default-keybindings 'meta)
(global-set-key (kbd "C-<left>") 'next-buffer) ; buffer move
(global-set-key (kbd "C-<right>") 'previous-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(unless (file-exists-p "~/.emacs.d/cache/") ; create default cache directory
    (make-directory "~/.emacs.d/cache/"))

(defalias 'yes-or-no-p 'y-or-n-p) ; Useful aliases
(defalias 'perl-mode 'cperl-mode)
(defalias 'eb 'eval-buffer)

;; scrolling

(setq
  scroll-margin 0                        ; do smooth scrolling, ...
  scroll-conservatively 100000           ; ... the defaults ...
  scroll-up-aggressively 0               ; ... are very ...
  scroll-down-aggressively 0             ; ... annoying
  scroll-preserve-screen-position t)     ; preserve screen pos with C-v/M-v

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
  ido-max-prospects 16             ; don't spam my minibuffer
  ido-confirm-unique-completion t  ; wait for RET, even with unique completion
  confirm-nonexistent-file-or-buffer nil ; when using ido, the confirmation is rather annoying...
  ido-everywhere t
  ido-create-new-buffer 'always)

(add-hook 'ido-minibuffer-setup-hook ; increase minibuffer size when ido completion is active
  (function
    (lambda ()
      (make-local-variable 'resize-minibuffer-window-max-height)
      (setq resize-minibuffer-window-max-height 1))))

;; super-supercharge ido

(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.

    Set it to nil using let in around-advice for functions where the
    original completing-read is required.  For example, if a function
    foo absolutely must use the original completing-read, define some
    advice like this:

    (defadvice foo (around original-completing-read-only activate)
      (let (ido-enable-replace-completing-read) ad-do-it))")

(defadvice completing-read ; Replace completing-read wherever possible, unless directed otherwise
  (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
	  (and (boundp 'ido-cur-list)
	       ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
	  (setq ad-return-value
		(ido-completing-read prompt
				     allcomp
				     nil require-match initial-input hist def))
	ad-do-it))))

(add-hook 'dired-mode-hook            ; bugfix for dired
	  '(lambda () (setq ido-enable-replace-completing-read nil)))

;; (ido-mode 1)
(add-hook 'term-setup-hook 'ido-mode) ; TRAMP bugfixing

;; savehist: save some history
(setq savehist-additional-variables   ; also save...
  '(search ring regexp-search-ring)   ; ... my search entries
  savehist-autosave-interval 60       ; save every minute (default: 5 min)
  savehist-file "~/.emacs.d/cache/savehist")   ; keep my home clean
(savehist-mode t)                     ; do customization before activation

;; autokill attached processess

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; clipboard settings

(setq x-select-enable-primary t     ; killing/yanking interacting with primary X11 selection
      x-select-enable-clipboard t   ; killing/yanking interact with clipboard X11 selection
      yank-pop-change-selection t)  ; rotating the kill ring change the X11 clipboard.

;; zone-mode for .hu

(add-to-list 'auto-mode-alist '("\\.hu$" . zone-mode))

;; save hooks

(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)   ; auto chmod scripts
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; remove trailing whitespace

;; Highlight some words in text mode

(add-hook 'text-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX\\)" 1 font-lock-warning-face t)))))

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
     (shell name))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Configure tramp

(setq shell-prompt-pattern "^[^a-zA-Z].*[~#$%>] *" ; we need a bit more funky pattern, as tramp will start $SHELL (sudo -s), ie., zsh for root user
      tramp-default-method "ssh"
      tramp-persistency-file-name "~/.emacs.d/cache/tramp")

;; Copy/Paste one line without selecting it

(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

;; External libraries

(add-to-list 'load-path "~/.emacs.d/plugins")
  (unless (file-exists-p "~/.emacs.d/plugins/cperl-mode.elc")  ; auto byte-compile all of them
    (byte-recompile-directory "~/.emacs.d/plugins/" 0))
  (load "~/.emacs.d/plugins/cperl-mode")

;; Boostrap el-get

(url-retrieve
 "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
 (lambda (s)
   (end-of-buffer)
   (eval-print-last-sexp)))

;; External libraries (with el-get)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(setq el-get-sources
 '(el-get
   (:name smex
   	  :after (lambda ()
   		   (global-set-key (kbd "M-x") 'smex)
   		   (global-set-key (kbd "M-X") 'smex-major-mode-commands)
   		   (setq smex-save-file "~/.emacs.d/cache/smex-items")
   		   ))
   yasnippet
   (:name color-theme
	  :after (lambda ()
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
			 dired-dwim-target t
			 dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
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
	  :after (lambda ()
		   (autoload 'mode-compile "mode-compile"
		     "Command to compile current buffer file based on the major mode" t)
		   (global-set-key (kbd "C-c c") 'mode-compile)
		   (autoload 'mode-compile-kill "mode-compile"
		     "Command to kill a compilation launched by `mode-compile'" t)
		   (global-set-key (kbd "C-c k") 'mode-compile-kill)
		   ))
   ))

(el-get 'sync)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cperl-nonoverridable-face ((t (:foreground "LightGoldenrod2"))))
 '(font-lock-warning-face ((t (:background "dim gray" :foreground "yellow" :weight extra-bold)))))
