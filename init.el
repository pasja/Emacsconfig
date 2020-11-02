;; necessary to keep emacs 25.1.1 happy
; (package-initialize)

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
      load-prefer-newer t
      frame-resize-pixelwise t
      x-underline-at-descent-line t
      x-stretch-cursor t
      calc-multiplication-has-precedence nil)

(setq-default major-mode 'text-mode
	      indent-tabs-mode nil)

(with-eval-after-load 'tool-bar
  (tool-bar-mode -1))
(with-eval-after-load 'scroll-bar
  (scroll-bar-mode -1))
(with-eval-after-load 'fringe
  (set-fringe-mode  '(0 . 0)))
(menu-bar-mode -1)
(show-paren-mode 1)
(column-number-mode 1)
(setq linum-format "%d ")
(size-indication-mode t)
(display-time)

(global-set-key (kbd "M-g") 'goto-line)                    ; M-g  'goto-line
(global-set-key (kbd "<delete>") 'delete-forward-char)     ; delete == delete
(global-set-key (kbd "M-2") 'hippie-expand)

(require 'windmove)                                        ; windmove
(windmove-default-keybindings 'shift)
(global-set-key (kbd "C-<left>") 'next-buffer)             ; buffer move
(global-set-key (kbd "C-<right>") 'previous-buffer)

(global-set-key (kbd "<f5>")                               ; open ~/.emacs.d/init.el
		(lambda ()
		  (interactive)
                  (find-file "~/.emacs.d/init.el")))

(global-set-key (kbd "<f6>")
                (lambda ()
                  (interactive)
                  (find-file "~/org/rovancs.org")))

(global-set-key (kbd "<f9>") 'save-buffers-kill-emacs)     ; hotkey for quit

(global-set-key (kbd "M-3")                                ; easy timestamp for rovancs.org
		(lambda ()
		  (interactive)
		  (insert(format-time-string "%Y-%m-%d"))))

(unless (file-exists-p "~/.emacs.d/cache/")                ; create default cache directory
  (make-directory "~/.emacs.d/cache/"))

(defalias 'yes-or-no-p 'y-or-n-p)                          ; Useful aliases
(defalias 'perl-mode 'cperl-mode)
(defalias 'eb 'eval-buffer)
(defalias 'list-buffers 'ibuffer)

;; scrolling

(setq scroll-margin 0                         ; do smooth scrolling, ...
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
      '(search ring regexp-search-ring)                  ; ... my search entries ...
      savehist-autosave-interval 60                      ; save every minute (default: 5 min)
      savehist-file "~/.emacs.d/cache/savehist")         ; keep my home clean
(savehist-mode t)                                        ; do customization before activation

;; autokill attached processess

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions)
      confirm-kill-processes nil)

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

(defun my-do-not-backup-pass-predicate (name)
  (let ((case-fold-search nil))
    (not
     (string-match-p "/dev/shm/.*" name))))

(defun my-backup-predicate (name)
  (and (normal-backup-enable-predicate name)
       (my-do-not-backup-pass-predicate name)))

(setq backup-by-copying t          ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.autosaves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t            ; use versioned backups
      backup-enable-predicate #'my-backup-predicate)

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
      tramp-default-method "sshx"
      tramp-persistency-file-name "~/.emacs.d/cache/tramp")

;; Copy/Paste one line without selecting it

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
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

(setq vc-follow-symlinks t) ; auto-follow version controlled symlinks

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

;; configure dired

(require 'dired)
(setq dired-listing-switches "-alhX")                   ; display size in human readable form
(setq image-dired-dir "~/.emacs.d/cache/image-dired/")
(eval-after-load "dired-aux"                            ; support .zip uncompress
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(setq dired-guess-shell-alist-user
      '(("\\.avi\\|\\.flv\\|\\.mp4\\|\\.wmv\\|.mov" "mplayer" "vlc")))

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

;; spelling

(setq-default ispell-program-name "hunspell")
(ispell-change-dictionary "en_US" t)

;; turn off auto revert messages

(setq auto-revert-verbose nil)

;; recentf

(setq recentf-save-file "~/.emacs.d/cache/recentf.el")
(recentf-mode)

;; BB spec stuff

;; browse kayako from BOSS folder
(defun browse-url-kayako ()
  (interactive)
  (let* ((baseurl "https://support.balabit.com/staff/index.php?/Tickets/Ticket/View/")
         (url (concat baseurl (thing-at-point 'filename t))))
    (browse-url url)))

(global-set-key (kbd "<f8>") 'browse-url-kayako)

;; find unknown logs in bundles
(defun pasja--dired-find-unknown ()
  (interactive)
  (find-name-dired "." "*unknown*"))

(define-key dired-mode-map (kbd "C-M-<up>") 'pasja--dired-find-unknown)

;; small utility functions which are needed for el-get initialization

(defun my--hostname-to-string ()
  "insert the contents of /etc/hostname to a string"
  (when (file-readable-p "/etc/hostname")
    (with-temp-buffer
      (insert-file-contents "/etc/hostname")
      (buffer-string))))

(require 'subr-x)

;; Boostrap el-get
;; preinstall the following debian packages:
;; apt install hunspell hunspell-hu texinfo build-essential
;; apt install global python-pygments silversearcher-ag install-info

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

(el-get-bundle el-get)

(el-get-bundle fixme
  :type github
  :pkgname "lewang/fic-mode"
  :description "Show FIXME/TODO/BUG(...) in special face only in comments and strings"
  (add-hook 'prog-mode-hook 'fic-mode))

(el-get-bundle org-mode
  (with-eval-after-load 'org
    (setq org-link-abbrev-alist
          '(("RT" . "https://rt.info.ppke.hu/Ticket/Display.html?id=%s"))
          org-return-follows-link t
          org-CUA-compatible t
          org-fontify-done-headline t
          org-highlight-latex-and-related '(latex)
          org-M-RET-may-split-line '((default . nil))
          org-agenda-start-on-weekday 1
          org-enforce-todo-checkbox-dependencies t)

    (add-hook 'org-shiftup-final-hook 'windmove-up)         ; Make windmove work in org-mode
    (add-hook 'org-shiftleft-final-hook 'windmove-left)
    (add-hook 'org-shiftdown-final-hook 'windmove-down)
    (add-hook 'org-shiftright-final-hook 'windmove-right)

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
                             (setq org-default-notes-file (concat org-directory "/notes.org"))))

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

(el-get-bundle emms
  (with-eval-after-load 'emms
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

(el-get-bundle mode-compile
  (progn (autoload 'mode-compile "mode-compile"
           "Command to compile current buffer file based on the major mode" t)
         (global-set-key (kbd "C-c c") 'mode-compile)
         (autoload 'mode-compile-kill "mode-compile"
           "Command to kill a compilation launched by `mode-compile'" t)
         (global-set-key (kbd "C-c k") 'mode-compile-kill)))

(el-get-bundle cperl-mode
  (with-eval-after-load 'cperl-mode
    (cperl-set-style "BSD")
    (setq cperl-invalid-face nil
          cperl-indent-parens-as-block t
          cperl-tab-always-indent nil
          cperl-highlight-variables-indiscriminately t
          cperl-merge-trailing-else nil)
    (define-key cperl-mode-map "{" 'nil))) ; smartparens fixup

(el-get-bundle haskell-mode
  (progn (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
         (setq haskell-process-type 'cabal-repl)))

(el-get-bundle info+
    (eval-after-load "info" '(require 'info+)))

(el-get-bundle solarized-emacs
  (load-theme 'solarized-dark t))

(el-get-bundle dired+
  :before (setq diredp-hide-details-initially-flag nil)
  (progn (diredp-toggle-find-file-reuse-dir 1) ; reuse existing dired buffer
         (setq dired-recursive-copies 'always  ; recursive copy/delete
               dired-recursive-deletes 'top
               dired-dwim-target t
               dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

         (define-key dired-mode-map (kbd "^") 'pasja-goto-up-in-dired)

         (defun pasja-goto-up-in-dired ()
           (interactive)
           (let ((pasja-prev-dir-name (file-truename default-directory)))
             (find-alternate-file "..")
             (dired-goto-file pasja-prev-dir-name)))

         (add-hook 'dired-mode-hook
                   (lambda ()
                     (setq dired-omit-mode t)))))

(el-get-bundle rainbow-delimiters
  (outline-minor-mode t) ; TODO: https://github.com/sellout/emacs-color-theme-solarized/issues/165
  (outline-minor-mode nil)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(el-get-bundle company-mode
  (progn (global-company-mode)
         (with-eval-after-load 'cperl-mode
           (add-to-list 'company-dabbrev-code-modes 'cperl-mode))))

(el-get-bundle magit
  (progn (global-set-key (kbd "C-x g") 'magit-status)
    (with-eval-after-load 'magit
      (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))))

(el-get-bundle smartparens
  (progn (require 'smartparens-config)
         (smartparens-global-mode 1)))

(el-get-bundle replace+
  (with-eval-after-load "replace"
    '(require 'replace+)))

(el-get-bundle dired-sort-menu
  (with-eval-after-load "dired"
    '(require 'dired-sort-menu)))

(el-get-bundle dired-sort-menu+)

(el-get-bundle undo-tree
  (global-undo-tree-mode 1))

(el-get-bundle swiper
  (progn (ivy-mode 1)
         (global-set-key (kbd "C-c C-r") 'ivy-resume)
         (global-set-key (kbd "M-x") 'counsel-M-x)
         (global-set-key (kbd "C-x C-f") 'counsel-find-file)
         (global-set-key (kbd "C-c g") 'counsel-git)
         (global-set-key (kbd "C-c j") 'counsel-git-grep)
         (global-set-key (kbd "C-c k") 'counsel-ag)
         (global-set-key (kbd "C-x 8") 'counsel-unicode-char)
         (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
         (setq ivy-use-virtual-buffers t)))

(el-get-bundle ggtags
  (progn
    (add-hook 'prog-mode-hook #'ggtags-mode)))

(el-get-bundle beginend
  :type github
  :pkgname "DamienCassou/beginend"
  :description "Redefine M-< and M-> (or any key bound to beginning-of-buffer or end-of-buffer) for some modes so that point moves to meaningful locations."
  (beginend-global-mode))

(el-get-bundle ox-jira
  :type github
  :pkgname "stig/ox-jira.el"
  :description "JIRA Backend for Org Export Engine")

(el-get-bundle markdown-mode)

(el-get-bundle php-mode)

(el-get-bundle s)

(el-get-bundle web-mode)

(el-get-bundle csv-mode)

(el-get-bundle apache-mode)

(el-get-bundle yaml-mode)

(el-get-bundle auctex)

(el-get-bundle alchemist)

(el-get-bundle dockerfile-mode)

(el-get-bundle grep+)

(el-get-bundle ffap-)

(el-get-bundle restclient)

(el-get-bundle image+
  (with-eval-after-load 'image
    '(require 'image+)))

(unless (string= (string-trim-right (my--hostname-to-string)) "hel")
  (el-get-bundle! pdf-tools
    :build (("make" "autobuild"))
    (pdf-tools-install)))

(el-get-bundle x509-mode
  :type github
  :pkgname "jobbflykt/x509-mode"
  :description "View certificates and CRLs using OpenSSL in Emacs")

(el-get-bundle erlang-mode)

(el-get-bundle lua-mode)

(el-get-bundle orgaggregate
  :type github
  :pkgname "tbanel/orgaggregate"
  :description "Aggregating a table is creating a new table by computing sums, averages, and so on, out of material from the first table.")

(el-get-bundle! circe
  (with-eval-after-load 'circe
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
    (setq lui-logging-directory "~/irclog/"
          lui-logging-file-format "{buffer}@{network}"
          lui-logging-format "[%Y-%m-%d %T] {text}")
    (add-hook 'circe-chat-mode-hook 'enable-lui-logging)

    (setq lui-time-stamp-position 'left
          lui-time-stamp-format "%H:%M "
          lui-fill-type nil)

    (when (string= (chomp (hostname-to-string)) "midgard") ; autojoin
      (progn (add-to-list 'load-path "~/.emacs.d/secrets/")
             (require 'server)
             (functionp 'server-running-p)
             (if (and (not (server-running-p "irc"))
                      (string= server-name "irc"))
                 (require 'ercidentities))))))

;; External libraries

(add-to-list 'load-path "~/.emacs.d/plugins")
(byte-recompile-directory "~/.emacs.d/plugins/" 0) ; auto byte-compile all of them
(mapc 'load-file
      (directory-files "~/Emacsconfig/plugins" t ".elc$")) ; load them all!

;; Customize
 
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
