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
      display-time-24hr-format t
      redisplay-dont-pause t                 ; faster scrolling
      sentence-end-double-space nil)         ; period single space ends sentence
(setq-default major-mode 'text-mode)

(unless (string= window-system "x")
    (progn (tool-bar-mode -1)
	   (scroll-bar-mode -1)))
(menu-bar-mode -1)
(show-paren-mode 1)
(column-number-mode 1)
(global-linum-mode 1)
(setq linum-format "%d ")
(size-indication-mode t)
(display-time)

(global-set-key (kbd "M-g") 'goto-line)                    ; M-g  'goto-line
(global-set-key (kbd "<delete>") 'delete-char)             ; delete == delete
(global-set-key (kbd "M-2") 'hippie-expand)

(require 'windmove)                                        ; windmove
(windmove-default-keybindings 'super)
(global-set-key (kbd "C-<left>") 'next-buffer)             ; buffer move
(global-set-key (kbd "C-<right>") 'previous-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "<f5>")                               ; open ~/.emacs.d/init.el
		(lambda ()
		  (interactive)(find-file "~/.emacs.d/init.el")))

(unless (file-exists-p "~/.emacs.d/cache/")                ; create default cache directory
  (make-directory "~/.emacs.d/cache/"))

(defalias 'yes-or-no-p 'y-or-n-p)                          ; Useful aliases
(defalias 'perl-mode 'cperl-mode)
(defalias 'eb 'eval-buffer)

;; scrolling

(setq
 scroll-margin 0                         ; do smooth scrolling, ...
 scroll-conservatively 100000            ; ... the defaults ...
 scroll-up-aggressively 0                ; ... are very ...
 scroll-down-aggressively 0              ; ... annoying
 scroll-preserve-screen-position t)      ; preserve screen pos with C-v/M-v

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

;; savehist: save some history

(setq savehist-additional-variables                      ; also save...
      '(search ring regexp-search-ring                   ; ... my search entries ...
	       icicle-previous-raw-file-name-inputs      ; ... and my icicles history
	       icicle-previous-raw-non-file-name-inputs)
      savehist-autosave-interval 60                      ; save every minute (default: 5 min)
      savehist-file "~/.emacs.d/cache/savehist")         ; keep my home clean
(savehist-mode t)                                        ; do customization before activation

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
  (shell name)
  (delete-other-windows))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; configure tramp

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

;; configure SQL

(setq sql-sqlite-program "sqlite3")

;; configure org-mode

(setq org-link-abbrev-alist
      '(("RT" . "https://rt.info.ppke.hu/Ticket/Display.html?id=%s"))
      org-return-follows-link t
      org-CUA-compatible t)

;; configure dired

(setq dired-listing-switches "-alh")                   ; display size in human readable form
(setq image-dired-dir "~/.emacs.d/cache/image-dired/")
(eval-after-load "dired-aux"                           ; support .zip uncompress
   '(add-to-list 'dired-compress-file-suffixes 
                 '("\\.zip\\'" ".zip" "unzip")))

;; configure woman
(require 'woman)
(setq woman-fontify t
      woman-fill-frame t
      woman-manpath '("/usr/share/man" "/usr/local/share/man" ("/bin" . "/usr/share/man") ("/usr/bin" . "/usr/share/man") ("/sbin" . "/usr/share/man") ("/usr/sbin" . "/usr/share/man") ("/usr/local/bin" . "/usr/local/man") ("/usr/local/bin" . "/usr/local/share/man") ("/usr/local/sbin" . "/usr/local/man") ("/usr/local/sbin" . "/usr/local/share/man") ("/usr/X11R6/bin" . "/usr/X11R6/man") ("/usr/bin/X11" . "/usr/X11R6/man") ("/usr/games" . "/usr/share/man") ("/opt/bin" . "/opt/man") ("/opt/sbin" . "/opt/man")))
(set-face-attribute 'woman-bold nil
		    :inherit 'bold
		    :slant 'italic
		    :foreground "green")

;; configure help 

(setq help-window-select t)

;; Boostrap el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; External libraries (with el-get)

(setq el-get-sources
      '(el-get

	(:name yasnippet
	       :type svn
	       :url "http://yasnippet.googlecode.com/svn/tags/REL_0_6_1c/"
	       :features yasnippet
	       :compile nil
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
	       :type git
	       :url "https://github.com/m2ym/auto-complete.git"
	       :features auto-complete
	       :post-init (lambda ()
		    (add-to-list 'ac-dictionary-directories (expand-file-name "dict" pdir))
		    (require 'auto-complete-config)
		    (ac-config-default))
	       :after (lambda ()
			(setq ac-comphist-file "~/.emacs.d/cache/ac.cache"
			      ac-dwim t)
			(setq-default ac-sources '(ac-source-words-in-same-mode-buffers ac-source-words-in-buffer ac-source-yasnippet))
			(ac-linum-workaround)
			(define-key ac-mode-map (kbd "C-<tab>") 'auto-complete)
			))

	(:name color-theme
	       :after (lambda ()
			(color-theme-taming-mr-arneson)
			))

	(:name git-emacs
	       :description "Yet another git emacs mode for newbies"
	       :type git
	       :url "https://github.com/tsgates/git-emacs.git"
	       :features git-emacs
	       :after (lambda ()
			(require 'git-status)
			(setq git-state-modeline-decoration 'git-state-decoration-colored-letter
			      git--log-flyspell-mode nil
			      git--use-ido nil)
			))

	(:name dired+
	       :after (lambda ()
			(require 'dired+)
			(toggle-diredp-find-file-reuse-dir 1) ; reuse existing dired buffer
			(setq dired-recursive-copies 'always  ; recursive copy/delete
			      dired-recursive-deletes 'top
			      dired-dwim-target t
			      dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
			(define-key dired-mode-map (kbd "^")
			  (lambda ()
			    (interactive) (find-alternate-file "..")))
			(add-hook 'dired-mode-hook
				  (lambda ()
				    (setq dired-omit-files-p t)))
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

	(:name dired-tar
	       :type "http"
	       :url "http://www-ftp.lip6.fr/pub/emacs/elisp-archive/incoming/dired-tar.el.gz"
	       :build ("gunzip -c dired-tar.el.gz > dired-tar.el")
	       :compile "dired-tar.el"
	       :features "dired-tar")

	(:name info+
	       :type emacswiki
	       :after (lambda () 
			(eval-after-load "info" '(require 'info+))))

	(:name replace+
	       :type emacswiki
	       :after (lambda () 
			(eval-after-load "replace" '(require 'replace+))))

	(:name grep+
	       :type emacswiki
	       :features "grep+")

	(:name ffap-
	       :type emacswiki
	       :features "ffap-")

	(:name lacarte
	       :type emacswiki
	       :features "lacarte")
	       
	(:name cperl-mode   ; newer cperl mode (https://github.com/jrockway/cperl-mode/tree/mx-declare)
	       :type "http"
	       :url "https://raw.github.com/jrockway/cperl-mode/mx-declare/cperl-mode.el"
	       :before (lambda ()
			  (set-face-foreground 'cperl-nonoverridable-face "LightGoldenrod2"))
	       :features "cperl-mode"
	       :after (lambda()
			(define-key cperl-mode-map (kbd "C-j") 'reindent-then-newline-and-indent)
			(cperl-set-style "K&R")
			(setq cperl-invalid-face nil
			      cperl-indent-parens-as-block t
			      cperl-tab-always-indent nil
			      cperl-highlight-variables-indiscriminately t)))

	(:name perl-completion
	       :type "git"
	       :url "https://github.com/imakado/perl-completion.git"
	       :depends anything
	       :website "https://github.com/imakado/perl-completion"
	       :before (lambda ()
			 (setq plcmp-method-inspecter 'class-inspector       
			       plcmp-use-keymap nil))
	       :features "perl-completion"
	       :after (lambda ()
			(add-hook  'cperl-mode-hook                        
				   (lambda ()
				     (setq ac-sources '(ac-source-perl-completion ac-source-words-in-buffer ac-source-yasnippet))
				     (perl-completion-mode t)))))

	(:name fixme-mode
	       :type emacswiki
	       :before (lambda ()
			 (setq fixme-highlighted-words '("FIXME" "TODO" "BUG" "KLUDGE" "XXX")
			       fixme-modes '(erlang-mode java-mode c-mode emacs-lisp-mode jde-mode scheme-mode python-mode ruby-mode cperl-mode slime-mode common-lisp-mode c++-mode d-mode js2-mode haskell-mode tuareg-mode lua-mode pascal-mode fortran-mode prolog-mode asm-mode csharp-mode sml-mode conf-mode conf-ppd-mode conf-unix-mode conf-colon-mode conf-space-mode conf-windows-mode conf-javaprop-mode conf-xdefaults-mode)))
	       :features "fixme-mode"
	       :after (lambda ()
			(fixme-mode 1)))

	(:name icicles
	       :type "http-tar"
	       :url "https://users.itk.ppke.hu/~pasja/icicles.tar.gz"
	       :options ("xzf")
	       :autoloads nil
	       :features "icicles"
	       :after (lambda ()
			(defun my-c-return ()
			  "When in minibuffer use `icicle-candidate-action', otherwise use `cua-set-rectangle-mark'."
			  (interactive)
			  (if (window-minibuffer-p (selected-window))
			      (call-interactively 'icicle-candidate-action)
			    (call-interactively 'cua-set-rectangle-mark)))
			(setq icicle-mark-position-in-candidate 'input-end
			      icicle-point-position-in-candidate 'input-end)
			(icy-mode 1)
			))

	))

(setq my-packages
      (append 
       '(el-get yasnippet auto-complete color-theme git-emacs
		dired+ mode-compile anything dired-tar info+
		replace+ grep+ ffap- lacarte cperl-mode perl-completion
		fixme-mode icicles apache-mode nxhtml nyan-mode yaml-mode)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

;; External libraries

(add-to-list 'load-path "~/.emacs.d/plugins")
(byte-recompile-directory "~/.emacs.d/plugins/" 0) ; auto byte-compile all of them

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

;; Customize
 
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
