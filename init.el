;; forbidden commands

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Proper start

(setq initial-scratch-message nil
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      confirm-nonexistent-file-or-buffer nil
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

(global-set-key (kbd "M-g") 'goto-line)                    ; M-g  'goto-line
(global-set-key (kbd "<delete>") 'delete-char)             ; delete == delete
(global-set-key (kbd "M-2") 'hippie-expand)

(require 'windmove) ; windmove
(windmove-default-keybindings 'meta)
(global-set-key (kbd "C-<left>") 'next-buffer)             ; buffer move
(global-set-key (kbd "C-<right>") 'previous-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c E")                              ; open ~/.emacs.d/init.el
  (lambda()(interactive)(find-file "~/.emacs.d/init.el")))

(unless (file-exists-p "~/.emacs.d/cache/")                ; create default cache directory
  (make-directory "~/.emacs.d/cache/"))

(defalias 'yes-or-no-p 'y-or-n-p)                          ; Useful aliases
(defalias 'perl-mode 'cperl-mode)
(defalias 'eb 'eval-buffer)

(setq reb-re-syntax 'string)                               ; set re-builder style

;; scrolling

(setq
 scroll-margin 0                        ; do smooth scrolling, ...
 scroll-conservatively 100000           ; ... the defaults ...
 scroll-up-aggressively 0               ; ... are very ...
 scroll-down-aggressively 0             ; ... annoying
 scroll-preserve-screen-position t)     ; preserve screen pos with C-v/M-v

;; some UTF-8 goodies

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;

;; spelling 

(setq ispell-program-name "aspell"
      ispell-dictionary "hungarian")

;; (setq ispell-program-name "ispell"
;;       ispell-dictionary "magyar")

;; (if (file-exists-p "/usr/bin/hunspell")                                         
;;     (progn
;;       (setq ispell-program-name "hunspell")
;;       (eval-after-load "ispell"
;;         '(progn (defun ispell-get-coding-system () 'utf-8)))))

;; (setq ispell-program-name "hunspell"   ; export DICTIONARY=hu_HU && export DICPATH=/usr/share/hunspell
;;       ispell-local-dictionary "hu_HU"
;;       ispell-skip-html t
;;       ispell-local-dictionary-alist
;;       '(("hu_HU" "\[\[:alpha:\]\]" "[^[:alpha:]]" "[']" nil ("-d" "hu_HU") nil utf-8))
;;       ispell-really-hunspell t)
;; (setq ispell-dictionary "hu_HU")

;; rectangles

(setq cua-enable-cua-keys nil)
(cua-mode t)

;; ido mode

(require 'ido)
(setq
 ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
 ido-ignore-buffers                  ; ignore these guys
 '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"

   "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
 ido-case-fold  t                    ; be case-insensitive
 ido-enable-last-directory-history t ; remember last used dirs
 ido-max-work-directory-list 30      ; should be enough
 ido-max-work-file-list      50      ; remember many
 ido-use-filename-at-point nil       ; don't use filename at point (annoying)
 ido-use-url-at-point nil            ; don't use url at point (annoying)
 ido-enable-flex-matching t          ; try to be too smart :-)
 ido-max-prospects 16                ; don't spam my minibuffer
 ido-confirm-unique-completion t     ; wait for RET, even with unique completion
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

(defadvice completing-read                         ; Replace completing-read wherever possible, unless directed otherwise
  (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
	  (and (boundp 'ido-cur-list)
	       ido-cur-list))                      ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
	  (setq ad-return-value
		(ido-completing-read prompt allcomp nil require-match initial-input hist def))
	ad-do-it))))

;; (add-hook 'dired-mode-hook            ; bugfix for dired (not good, does not set back after we quit dired)
;; 	  '(lambda () (setq ido-enable-replace-completing-read nil)))

;; (ido-mode 1)
(add-hook 'term-setup-hook 'ido-mode)              ; TRAMP bugfixing

;; savehist: save some history
(setq savehist-additional-variables                ; also save...
      '(search ring regexp-search-ring)            ; ... my search entries
      savehist-autosave-interval 60                ; save every minute (default: 5 min)
      savehist-file "~/.emacs.d/cache/savehist")   ; keep my home clean
(savehist-mode t)                                  ; do customization before activation

;; autokill attached processess

(setq kill-buffer-query-functions                  ; on buffers ...
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))
(defadvice save-buffers-kill-emacs
  (around no-query-kill-emacs activate)            ; ... and on quit
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; clipboard settings

(setq x-select-enable-clipboard t        ; copy-paste should work ...
  interprogram-paste-function            ; ...with...
  'x-cut-buffer-or-selection-value)      ; ...other X clients

;; zone-mode for .hu

(add-to-list 'auto-mode-alist '("\\.hu$" . zone-mode))

;; save hooks

(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)   ; auto chmod scripts

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
  (interactive (if mark-active (list (region-beginning) (region-end))
		 (message "Copied line")
		 (list (line-beginning-position) 
		       (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

;; configure vc

(setq vc-follow-symlinks t ) ; auto-follow version controlled symlinks
      
(add-hook 'log-edit-mode 
	  (lambda ()
	    (flyspell-mode -1)))

;; configure ediff

(setq ediff-split-window-function 'split-window-horizontally)

;; Boostrap el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; External libraries (with el-get)

(require 'el-get)
(setq el-get-sources
      '(el-get
	(:name smex
	       :after (lambda ()
			(global-set-key (kbd "M-x") 'smex)
			(global-set-key (kbd "M-X") 'smex-major-mode-commands)
			(setq smex-save-file "~/.emacs.d/cache/smex-items")
			))
	(:name yasnippet
	       :after (lambda ()
			(setq yas/root-directory
			      '("~/.emacs.d/yas/")) ; my own snippets
			(mapc 'yas/load-directory yas/root-directory)
			(setq yas/wrap-around-region t)
			(setq yas/prompt-functions
			      '(yas/x-prompt yas/ido-prompt))
			(yas/global-mode 1)         ; make it global
			(add-to-list 'auto-mode-alist '("yas/.*" . snippet-mode))
			(yas/reload-all)
			))
	(:name auto-complete
	       :after (lambda ()
			(setq ac-comphist-file "~/.emacs.d/cache/ac.cache"
			      ac-dwim t)
			(setq-default ac-sources '(ac-source-words-in-same-mode-buffers ac-source-words-in-buffer ac-source-yasnippet))
			(ac-linum-workaround)
			(define-key ac-mode-map (kbd "s-<tab>") 'auto-complete)
			))
	(:name color-theme
	       :after (lambda ()
			(color-theme-taming-mr-arneson)
			))
	(:name git-emacs
	       :after (lambda ()
			(require 'git-status)
			(setq git-state-modeline-decoration 'git-state-decoration-colored-letter
			      git--log-flyspell-mode nil)
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
	(:name mode-compile
	       :after (lambda ()
			(autoload 'mode-compile "mode-compile"
			  "Command to compile current buffer file based on the major mode" t)
			(global-set-key (kbd "C-c c") 'mode-compile)
			(autoload 'mode-compile-kill "mode-compile"
			  "Command to kill a compilation launched by `mode-compile'" t)
			(global-set-key (kbd "C-c k") 'mode-compile-kill)
			))
	(:name anything
	       :after (lambda ()
			(require 'anything)
			(require 'anything-match-plugin)
			))
	))

(el-get 'sync)

;; External libraries

(add-to-list 'load-path "~/.emacs.d/plugins")
(byte-recompile-directory "~/.emacs.d/plugins/" 0) ; auto byte-compile all of them

(require 'perl-completion)                         ; (https://github.com/imakado/perl-completion)
(add-hook  'cperl-mode-hook                        ; configure perl-completion 
           (lambda ()
	     (setq ac-sources '(ac-source-perl-completion ac-source-words-in-same-mode-buffers ac-source-words-in-buffer ac-source-yasnippet)
		   plcmp-method-inspecter 'class-inspector
		   plcmp-use-keymap nil)
	     (perl-completion-mode t)
	     (define-key cperl-mode-map (kbd "C-<tab>") 'plcmp-cmd-smart-complete)
	     ))

(load "~/.emacs.d/plugins/cperl-mode")  ; newer cperl mode (https://github.com/jrockway/cperl-mode/tree/mx-declare)
(eval-after-load 'cperl-mode            ; configure cperl
  '(progn
     (define-key cperl-mode-map (kbd "C-j") 'reindent-then-newline-and-indent)
     (cperl-set-style "K&R")
     (setq cperl-invalid-face nil
	   cperl-indent-parens-as-block t
	   cperl-tab-always-indent nil
	   cperl-highlight-variables-indiscriminately t)
     ))

(require 'fixme-mode)    ; fixme mode (http://www.emacswiki.org/emacs/FixmeMode)
(fixme-mode 1)

(require 'autopair)      ; autopair mode (http://code.google.com/p/autopair/source/browse/trunk/autopair.el r42) 
(autopair-global-mode 1) ; home-brew fix for auto-complete (comment out RET and return keybinding)


;; wanderlust

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;; custom

(custom-set-faces
 '(cperl-nonoverridable-face ((t (:foreground "LightGoldenrod2")))))

(custom-set-variables
)
