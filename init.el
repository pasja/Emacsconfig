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
      sentence-end-double-space nil          ; period single space ends sentence
      load-prefer-new t)
(setq-default major-mode 'text-mode
	      indent-tabs-mode nil)

(unless (string= window-system "x")
    (progn (tool-bar-mode -1)
	   (scroll-bar-mode -1)))
(menu-bar-mode -1)
(set-fringe-mode  '(0 . 0))
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
(windmove-default-keybindings 'shift)
(global-set-key (kbd "C-<left>") 'next-buffer)             ; buffer move
(global-set-key (kbd "C-<right>") 'previous-buffer)

(global-set-key (kbd "<f5>")                               ; open ~/.emacs.d/init.el
		(lambda ()
		  (interactive)(find-file "~/.emacs.d/init.el")))

(global-set-key (kbd "<f9>") 'save-buffers-kill-emacs)     ; hotkey for quit

(unless (file-exists-p "~/.emacs.d/cache/")                ; create default cache directory
  (make-directory "~/.emacs.d/cache/"))

(defalias 'yes-or-no-p 'y-or-n-p)                          ; Useful aliases
(defalias 'perl-mode 'cperl-mode)
(defalias 'eb 'eval-buffer)
(defalias 'list-buffers 'ibuffer)

;; scrolling

(setq
 scroll-margin 0                         ; do smooth scrolling, ...
 scroll-conservatively 100000            ; ... the defaults ...
 scroll-up-aggressively 0.0              ; ... are very ...
 scroll-down-aggressively 0.0            ; ... annoying
 scroll-preserve-screen-position t)      ; preserve screen pos with C-v/M-v

;; some UTF-8 goodies

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;

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

(defun insert-from-primary-clipboard ()
  "Insert the text from the current x-selection."
  (interactive)
  (when select-active-regions
    (let (select-active-regions)
      (deactivate-mark)))
    (let ((primary
	 (cond
	  ((eq system-type 'windows-nt)
	   ;; MS-Windows emulates PRIMARY in x-get-selection, but not
	   ;; in x-get-selection-value (the latter only accesses the
	   ;; clipboard).  So try PRIMARY first, in case they selected
	   ;; something with the mouse in the current Emacs session.
	   (or (x-get-selection 'PRIMARY)
	       (x-get-selection-value)))
	  ((fboundp 'x-get-selection-value) ; MS-DOS and X.
	   ;; On X, x-get-selection-value supports more formats and
	   ;; encodings, so use it in preference to x-get-selection.
	   (or (x-get-selection-value)
	       (x-get-selection 'PRIMARY)))
	  ;; FIXME: What about xterm-mouse-mode etc.?
	  (t
	   (x-get-selection 'PRIMARY)))))
    (unless primary
      (error "No selection is available"))
    (push-mark (point))
    (insert primary)))

(global-set-key (kbd "S-<insert>") 'insert-from-primary-clipboard)

(setq mouse-yank-at-point t)

;; zone-mode for .hu

(add-to-list 'auto-mode-alist '("\\.hu$" . zone-mode))

;; save hooks

(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)   ; auto chmod scripts

;; configure autosave

(unless (file-exists-p "~/.autosaves")
  (make-directory "~/.autosaves"))

(setq
   backup-by-copying t           ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.autosaves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)            ; use versioned backups

(defun force-backup-of-buffer ()
    (setq buffer-backed-up nil))

(add-hook 'before-save-hook 'force-backup-of-buffer)

;; smart shell start

(defun sh (name)
  "Smart shell start"
  (interactive "sShell name: ")
  (shell name)
  (delete-other-windows))

;; colorize shell and comint

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; configure ibuffer

(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("Default"
                   ("Dired" (mode . dired-mode))
                   ("Perl" (mode . cperl-mode))
		   ("Org" (or (mode . org-mode)
			      (name . "^\\*Calendar\\*$")))
		   ("Magit" (name . "^\\*magit*"))
		   ("Documentation" (or
				     (name . "^\\*WoMan*")
				     (mode . Info-mode)
				     (name . "^\\*Man*")
				     (name . "^\\*Help\\*$")))
		   ("Elisp" (or
			     (mode . emacs-lisp-mode)))
		   ("ERC" (or
			     (mode . erc-mode)))
		   ("System" (or
                             (name . "^\\*scratch\\*$")
                             (name . "^\\*Messages\\*$")
			     (name . "^\\*Completions\\*$")
			     (mode . compilation-mode)
			     (name . "^\\*Shell*")
			     (mode . Custom-mode)))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "Default")))

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

;; configure undo

(setq undo-limit 800000
      undo-strong-limit 1200000
      undo-outer-limit 120000000)

;; configure vc

(setq vc-follow-symlinks t ) ; auto-follow version controlled symlinks
      
(add-hook 'log-edit-mode 
	  (lambda ()
	    (flyspell-mode -1)))

;; configure diff

(setq diff-switches "-u")

;; configure ediff

(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)
(setq-default ediff-auto-refine 'on)

;; configure SQL

(setq sql-sqlite-program "sqlite3")

;; insert today

(global-set-key (kbd "M-3")                             ; easy timestamp for rovancs.org
		(lambda ()
		  (interactive)
		  (insert(format-time-string "%Y-%m-%d"))))

;; configure dired

(require 'dired)
(setq dired-listing-switches "-alhX")                   ; display size in human readable form
(setq image-dired-dir "~/.emacs.d/cache/image-dired/")
(eval-after-load "dired-aux"                            ; support .zip uncompress
   '(add-to-list 'dired-compress-file-suffixes 
                 '("\\.zip\\'" ".zip" "unzip")))

(setq dired-guess-shell-alist-user
      '(("\\.avi\\|\\.flv\\|\\.mp4\\|\\.wmv\\|.mov" "mplayer" "vlc")))

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 2))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

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

;; configure prog-mode
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)

;; configure winner-mode

(winner-mode 1)

;; configure calendar

(setq calendar-week-start-day 1)

;; Boostrap el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; configure elpa

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; External libraries (with el-get)

(setq el-get-sources
      '(el-get

	(:name yasnippet
	       :type github
	       :website "https://github.com/capitaomorte/yasnippet.git"
	       :description "YASnippet is a template system for Emacs."
	       :pkgname "capitaomorte/yasnippet"
	       :features yasnippet
	       :compile "yasnippet.el"
	       :after (progn
			(setq yas-snippet-dirs
			      '("~/.emacs.d/yas/") ; my own snippets
			      yas/wrap-around-region t
			      yas-use-menu nil)
			(add-to-list 'auto-mode-alist '("yas/.*" . snippet-mode))
			(yas/global-mode 1)))         ; make it global

	(:name rainbow-delimiters
	       :type github
	       :pkgname "Fanael/rainbow-delimiters"
	       :features rainbow-delimiters
	       :after (progn
			(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
			(add-hook 'erc-mode-hook '(lambda ()
			     (rainbow-delimiters-mode -1)))
			(add-hook 'org-mode-hook '(lambda ()
			     (rainbow-delimiters-mode -1)))))

	(:name magit
	       :features "magit"
	       :after (progn
			(setq magit-git-standard-options '("--no-pager" "-c" "core.quotepath=false")
                              magit-restore-window-configuration t
                              magit-status-buffer-switch-function
                              (lambda (buffer) ; there might already be an Emacs function which does this
                                (pop-to-buffer buffer)
                                (delete-other-windows)))
			(global-set-key (kbd "C-x g")
                                        'magit-status)))

	(:name dired+
	       :features "dired+"
	       :before (progn (setq diredp-hide-details-initially-flag nil))
	       :after (progn
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
	       :after (progn
			(autoload 'mode-compile "mode-compile"
			  "Command to compile current buffer file based on the major mode" t)
			(global-set-key (kbd "C-c c") 'mode-compile)
			(autoload 'mode-compile-kill "mode-compile"
			  "Command to kill a compilation launched by `mode-compile'" t)
			(global-set-key (kbd "C-c k") 'mode-compile-kill)
			))

	(:name dired-tar
	       :type "http"
	       :url "http://www-ftp.lip6.fr/pub/emacs/elisp-archive/incoming/dired-tar.el.gz"
	       :build ("gunzip -c dired-tar.el.gz > dired-tar.el")
	       :compile "dired-tar.el"
	       :features "dired-tar")

	(:name info+
	       :type emacswiki
	       :after (progn
			(eval-after-load "info" '(require 'info+))))

	(:name replace+
	       :type emacswiki
	       :after (progn
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
	       
	(:name cperl-mode
	       :type github
	       :pkgname "jrockway/cperl-mode"
	       :features "cperl-mode"
	       :after (progn
			(define-key cperl-mode-map (kbd "C-j") 'reindent-then-newline-and-indent)
			(cperl-set-style "K&R")
			(setq cperl-invalid-face nil
			      cperl-indent-parens-as-block t
			      cperl-tab-always-indent nil
			      cperl-highlight-variables-indiscriminately t)
			(define-key cperl-mode-map "{" 'nil)))

	(:name fixme-mode
	       :type emacswiki
	       :before (progn
			 (setq fixme-highlighted-words '("FIXME" "TODO" "BUG" "KLUDGE" "XXX")
			       fixme-modes '(erlang-mode java-mode c-mode emacs-lisp-mode jde-mode scheme-mode python-mode ruby-mode cperl-mode slime-mode common-lisp-mode c++-mode d-mode js2-mode haskell-mode tuareg-mode lua-mode pascal-mode fortran-mode prolog-mode asm-mode csharp-mode sml-mode conf-mode conf-ppd-mode conf-unix-mode conf-colon-mode conf-space-mode conf-windows-mode conf-javaprop-mode conf-xdefaults-mode)))
	       :features "fixme-mode"
	       :after (progn
			(fixme-mode 1)))

	(:name bookmark+
	       :type "http-tar"
	       :url "https://users.itk.ppke.hu/~pasja/bookmarkplus.tar.gz"
	       :options ("xzf")
	       :compile nil	       
	       :autoloads nil
	       :features "bookmark+"
	       :after (progn
			(setq-default bookmark-default-file "~/.emacs.d/cache/.emacs.bmk"
			      bmkp-bmenu-commands-file "~/.emacs.d/cache/.emacs-bmk-bmenu-commands.el"
			      bmkp-bmenu-state-file "~/.emacs.d/cache/.emacs-bmk-bmenu-state.el"
			      bmkp-last-as-first-bookmark-file nil)))

	(:name dired-sort-menu
	       :type emacswiki
	       :features "dired-sort-menu"
	       :after (progn
			(add-hook 'dired-load-hook
				  (lambda () (require 'dired-sort-menu)))))

	(:name dired-sort-menu+
	       :type emacswiki
	       :features "dired-sort-menu+")

	(:name undo-tree
	       :features "undo-tree"
	       :after (progn
			(global-undo-tree-mode 1)))

	(:name smartparens
	       :after (progn
			(require 'smartparens-config)
			(smartparens-global-mode 1)))

	(:name company-mode
	       :after (progn
			(global-company-mode)))

	(:name icicles
	       :type "http-tar"
	       :url "https://users.itk.ppke.hu/~pasja/icicles.tar.gz"
	       :options ("xzf")
	       :compile nil
	       :autoloads nil
	       :features "icicles"
	       :after (progn
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

	(:name solarized-theme
	       :type github
	       :pkgname "sellout/emacs-color-theme-solarized"
	       :autoloads nil
	       :description "Solarized themes for Emacs"
	       :prepare (add-to-list 'custom-theme-load-path default-directory)
	       :after (progn
			(add-hook 'after-make-frame-functions
				  (lambda (frame)
				    (set-frame-parameter frame
							 'background-mode
							 'dark)))
			(load-theme 'solarized t)))

	(:name circe
	       :website "https://github.com/jorgenschaefer/circe/wiki"
	       :description "Circe is yet another client for IRC in Emacs. It provides most features one would expect from an IRC client, with sane defaults to start from."
	       :type github
	       :pkgname "jorgenschaefer/circe"
	       :load-path ("lisp"))

	(:name csv-mode
	       :website "http://www.emacswiki.org/emacs/CsvMode"
	       :description "Major mode for editing CSV (comma separated value) files."
	       :type http
	       :url "http://elpa.gnu.org/packages/csv-mode-1.2.el")

	(:name haskell-mode
	       :description "A Haskell editing mode"
	       :type github
	       :pkgname "haskell/haskell-mode"
	       :build ("make all")
	       :post-init (progn
			    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
			    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
			    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
			    (setq haskell-process-type 'cabal-repl)))

	(:name org-mode
	       :website "http://orgmode.org/"
	       :description "Org-mode is for keeping notes, maintaining ToDo lists, doing project planning, and authoring with a fast and effective plain-text system."
	       :type git
	       :url "git://orgmode.org/org-mode.git"
	       :info "doc"
	       :build/berkeley-unix `,(mapcar
				       (lambda (target)
					 (list "gmake" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
				       '("oldorg"))
	       :build `,(mapcar
			 (lambda (target)
			   (list "make" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
			 '("oldorg"))
	       :load-path ("." "lisp" "contrib/lisp")
	       :features "org"
	       :after (progn
			(setq org-link-abbrev-alist
			      '(("RT" . "https://rt.info.ppke.hu/Ticket/Display.html?id=%s"))
			      org-return-follows-link t
			      org-CUA-compatible t)

			(add-hook 'org-shiftup-final-hook 'windmove-up)         ; Make windmove work in org-mode
			(add-hook 'org-shiftleft-final-hook 'windmove-left)
			(add-hook 'org-shiftdown-final-hook 'windmove-down)
			(add-hook 'org-shiftright-final-hook 'windmove-right)

			(global-set-key (kbd "<f6>")
					(lambda ()
					  (interactive)
					  (find-file "~/org/rovancs.org")))

			(defun myorg-update-parent-cookie ()
			  (when (equal major-mode 'org-mode)
			    (save-excursion
			      (ignore-errors
				(org-back-to-heading)
				(org-update-parent-todo-statistics)))))

			(defadvice org-kill-line (after fix-cookies activate)
			  (myorg-update-parent-cookie))

			(defadvice kill-whole-line (after fix-cookies activate)
			  (myorg-update-parent-cookie))
			(eval-after-load 'org '(progn
						 (setq org-default-notes-file (concat org-directory "/notes.org"))
						 (define-key global-map (kbd "<f8>") 'org-capture)))
			;; plantuml

			;; active Org-babel languages
			(org-babel-do-load-languages
			 'org-babel-load-languages
			 '(;; other Babel languages
			   (plantuml . t)))

			(setq org-plantuml-jar-path
			      (expand-file-name "~/bin/plantuml.jar"))

			(defun my-org-confirm-babel-evaluate (lang body)
			  (not (string= lang "plantuml")))  ; don't ask for plantuml
			(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)))

	(:name emms
	       :description "The Emacs Multimedia System"
	       :type git
	       :url "git://git.sv.gnu.org/emms.git"
	       :info "doc"
	       :load-path ("./lisp")
	       :features emms-setup
	       :build `(("mkdir" "-p" ,(expand-file-name (format "%s/emms" user-emacs-directory)))
			("make" ,(format "EMACS=%s" el-get-emacs)
			 ,(format "SITEFLAG=\\\"--no-site-file -L %s/emacs-w3m/ \\\""
				  el-get-dir)
			 "autoloads" "lisp" "docs"))
	       :build/berkeley-unix `(("mkdir" "-p" ,(expand-file-name (format "%s/emms" user-emacs-directory)))
				      ("gmake" ,(format "EMACS=%s" el-get-emacs)
				       ,(format "SITEFLAG=\\\"--no-site-file -L %s/emacs-w3m/ \\\""
						el-get-dir)
				       "autoloads" "lisp" "docs"))
	       :after (progn
			(emms-all)
			(setq emms-cache-file "~/.emacs.d/cache/emms-cache"
			      emms-info-auto-update nil
			      emms-playlist-buffer-name "EMMS Playlist")
			(if (file-readable-p "~/.emacs.d/cache/emms-cache")
			      (emms-cache-restore))
			(require 'emms-player-mpd)
			(setq emms-player-mpd-music-directory "~/Zene/"
			      emms-player-mpd-server-name "localhost"
			      emms-player-mpd-server-port "6600")
			(setq emms-info-functions 'emms-info-mpd)
			(add-to-list 'emms-player-list 'emms-player-mpd)
			(emms-player-mpd-connect)))

	))

(setq my-packages
      (append 
       '(el-get yasnippet magit undo-tree smartparens company-mode s web-mode
		dired+ mode-compile dired-tar info+ bookmark+ dired-sort-menu
		replace+ grep+ ffap- lacarte cperl-mode dired-sort-menu+
		fixme-mode icicles apache-mode nyan-mode yaml-mode haskell-mode
		rainbow-delimiters csv-mode popup solarized-theme circe org-mode)
       (eval-after-load "el-get"
	 '(mapcar 'el-get-source-name el-get-sources))))

(el-get 'sync my-packages)

;; External libraries

(add-to-list 'load-path "~/.emacs.d/plugins")
(byte-recompile-directory "~/.emacs.d/plugins/" 0) ; auto byte-compile all of them
(mapc 'load-file
      (directory-files "~/Emacsconfig/plugins" t ".elc$")) ; load them all!

;; spelling

(load-file "/usr/share/emacs/24.4/lisp/textmodes/ispell.elc")
(setq-default ispell-program-name "hunspell")
(ispell-change-dictionary "hu_HU" t)

;; circe

(setq circe-reduce-lurker-spam t
      circe-active-users-timeout 43200
      circe-color-nicks-everywhere t
      circe-highlight-nick-type 'occurence
      circe-server-max-reconnect-attempts nil
      circe-format-server-topic "*** Topic change by {origin}: {topic-diff}"
      circe-format-self-say "<{nick}> {body}"
      circe-new-buffer-behavior 'ignore)

(require 'circe-color-nicks)
(enable-circe-color-nicks)

(add-hook 'circe-chat-mode-hook 'my-circe-prompt)
(defun my-circe-prompt ()
  (lui-set-prompt
   (concat (propertize (concat (buffer-name) "")
                       'face 'circe-prompt-face)
           " ")))

(require 'circe-lagmon)
(circe-lagmon-mode)

(require 'lui-autopaste)
(add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

(require 'lui-logging)
(setq lui-logging-directory "~/irclog"
      lui-logging-file-format "{buffer}@{network}"
      lui-logging-format "[%Y-%m-%d %T] {text}")
(add-hook 'circe-chat-mode-hook 'enable-lui-logging)

(add-hook 'circe-chat-mode-hook '(lambda ()(linum-mode -1)))

(setq
 lui-time-stamp-position 'left
 lui-time-stamp-format "%H:%M "
 lui-fill-type nil)

(when (string= (chomp (hostname-to-string)) "midgard") ; autojoin
  (progn (add-to-list 'load-path "~/.emacs.d/secrets/")
	 (require 'server)
	 (functionp 'server-running-p)
	 (if (and (not (server-running-p "irc"))
		  (string= server-name "irc"))
	     (require 'ercidentities))))

;; Customize
 
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
