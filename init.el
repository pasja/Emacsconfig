;; -*- lexical-binding: t -*-

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
      sentence-end-double-space nil          ; period single space ends sentence
      load-prefer-newer t
      frame-resize-pixelwise t
      x-underline-at-descent-line t
      x-stretch-cursor t
      use-system-tooltips nil
      calc-multiplication-has-precedence nil
      calc-kill-line-numbering nil
      isearch-lazy-count t
      display-raw-bytes-as-hex t
      help-window-keep-selected t)

(setq-default major-mode 'text-mode
              indent-tabs-mode nil)

(with-eval-after-load 'tool-bar
  (tool-bar-mode -1))
(with-eval-after-load 'scroll-bar
  (scroll-bar-mode -1))
(with-eval-after-load 'fringe
  (set-fringe-mode '(0 . 0)))
(menu-bar-mode -1)
(show-paren-mode 1)
(column-number-mode 1)
(setq linum-format "%d ")
(size-indication-mode t)
(display-time)

(global-set-key (kbd "M-g") 'goto-line)                    ; M-g  'goto-line
(global-set-key (kbd "M-2") 'hippie-expand)

(windmove-default-keybindings 'shift)                      ; windmove
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

(setopt use-short-answers t)

(defalias 'perl-mode 'cperl-mode)                          ; Useful aliases
(defalias 'eb 'eval-buffer)
(defalias 'list-buffers 'ibuffer)

;; scrolling

(setq scroll-margin 0                         ; do smooth scrolling, ...
      scroll-conservatively 100000            ; ... the defaults ...
      scroll-up-aggressively 0.0              ; ... are very ...
      scroll-down-aggressively 0.0            ; ... annoying
      scroll-preserve-screen-position t)      ; preserve screen pos with C-v/M-v

(pixel-scroll-precision-mode)

;; some UTF-8 goodies

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;

;; rectangles

(global-set-key (kbd "C-M-<return>") #'cua-rectangle-mark-mode)

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
           (or (gui-get-selection 'PRIMARY)
               (gui-get-primary-selection)))
          ((fboundp 'gui-get-primary-selection) ; MS-DOS and X.
           ;; On X, x-get-selection-value supports more formats and
           ;; encodings, so use it in preference to x-get-selection.
           (or (gui-get-primary-selection)
               (gui-get-selection 'PRIMARY)))
          ;; FIXME: What about xterm-mouse-mode etc.?
          (t
           (gui-get-selection 'PRIMARY)))))
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

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)

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

(require 'tramp)

(setq shell-prompt-pattern "^[^a-zA-Z].*[~#$%>] *" ; we need a bit more funky pattern, as tramp will start $SHELL (sudo -s), ie., zsh for root user
      tramp-default-method "sshx"
      tramp-persistency-file-name "~/.emacs.d/cache/tramp")

;; yadm support

(add-to-list 'tramp-methods
             '("yadm"
               (tramp-login-program "yadm")
               (tramp-login-args (("enter")))
               (tramp-login-env (("SHELL") ("/bin/sh")))
               (tramp-remote-shell "/bin/sh")
               (tramp-remote-shell-args ("-c"))))

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
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-keep-variants nil)
(setq-default ediff-auto-refine 'on)

(defun ediff-current-windows ()
  "Run ediff on the buffers displayed in the current frame's two windows."
  (interactive)
  (let ((windows (window-list)))
    (if (= (length windows) 2)
       (let ((buf1 (window-buffer (car windows)))
             (buf2 (window-buffer (cadr windows))))
         (ediff-buffers buf1 buf2))
     (error "This function requires exactly 2 windows"))))

;; configure SQL

(setq sql-sqlite-program "sqlite3")

;; configure dired

(require 'dired)
(setq dired-listing-switches "-alhX --time-style=long-iso")                   ; display size in human readable form
(setq image-dired-dir "~/.emacs.d/cache/image-dired/")

(setq dired-recursive-copies  'always
      dired-recursive-deletes 'always
      wdired-use-dired-vertical-movement t
      wdired-allow-to-change-permissions t
      dired-vc-rename-file t)

(setq dired-guess-shell-alist-user
      '(("\\.avi\\|\\.flv\\|\\.mp4\\|\\.wmv\\|.mov" "mpv" "vlc")))

(require 'dired-aux)
(declare-function w32-shell-execute "w32fns.c")
(declare-function shell-command-guess "dired-aux" (files))
(defvar shell-command-guess-open "open")

(defun dired-do-open (&optional arg)
  "Open the marked files or a file at click/point externally.
If files are marked, run the command from `shell-command-guess-open'
on each of marked files.  Otherwise, run it on the file where
the mouse is clicked, or on the file at point."
  (interactive "P" dired-mode)
  (let ((files (if (mouse-event-p last-nonmenu-event)
                   (save-excursion
                     (mouse-set-point last-nonmenu-event)
                     (dired-get-marked-files nil arg))
                 (dired-get-marked-files nil arg)))
        (command shell-command-guess-open))
    (when (and (memq system-type '(windows-nt))
               (equal command "start"))
      (setq command "open"))
    (when command
      (dolist (file files)
        (cond
         ((memq system-type '(gnu/linux))
          (call-process command nil 0 nil file))
         ((memq system-type '(ms-dos))
          (shell-command (concat command " " (shell-quote-argument file))))
         ((memq system-type '(windows-nt))
          (w32-shell-execute command (convert-standard-filename file)))
         ((memq system-type '(cygwin))
          (call-process command nil nil nil file))
         ((memq system-type '(darwin))
          (start-process (concat command " " file) nil command file))
         (t
          (error "Open not supported on this system")))))))

;; configure woman

(require 'woman)
(setq woman-fontify t
      woman-fill-frame t)
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

(defun view-text-file-as-info-manual ()
  (interactive)
  (require 'ox-texinfo)
  (let ((org-export-with-broken-links 'mark))
    (pcase (file-name-extension (buffer-file-name))
      (`"info"
       (info (buffer-file-name)))
      (`"texi"
       (info (org-texinfo-compile (buffer-file-name))))
      (`"org"
       (info (org-texinfo-export-to-info)))
      (`"md"
       (let ((org-file-name (concat (file-name-sans-extension (buffer-file-name)) ".org")))
         (apply #'call-process "pandoc" nil standard-output nil
                `("-f" "markdown"
                  "-t" "org"
                  "-o" , org-file-name
                  , (buffer-file-name)))
         (with-current-buffer (find-file-noselect org-file-name)
           (info (org-texinfo-export-to-info)))))
      (_ (user-error "Don't know how to convert `%s' to an `info' file"
                     (file-name-extension (buffer-file-name)))))))

(global-set-key (kbd "C-x x v") 'view-text-file-as-info-manual)

(when (eq system-type 'darwin)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (setq ns-command-modifier 'control
        ns-control-modifier 'meta
        ns-right-option-modifier 'none)
  (global-set-key [home] 'beginning-of-line-text)
  (global-set-key [end] 'move-end-of-line)
  (global-set-key (kbd "<M-right>") #'right-word)
  (global-set-key (kbd "<M-left>") #'left-word)
  (global-set-key (kbd "<M-delete>") #'kill-word))

;; Boostrap el-get
;; preinstall the following debian packages:
;; apt install hunspell hunspell-hu texinfo build-essential texlive
;; apt install global python3-pygments ripgrep install-info
;; apt install autoconf automake gcc libpng-dev libpoppler-dev
;; apt install libpoppler-glib-dev libz-dev make pkg-config zip

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; configure elpa

(require 'package)

;; External libraries (with el-get)

(el-get-bundle el-get)

(el-get-bundle fixme
  :type github
  :pkgname "lewang/fic-mode"
  :description "Show FIXME/TODO/BUG(...) in special face only in comments and strings"
  (add-hook 'prog-mode-hook 'fic-mode))

(el-get-bundle org-mode
  :before (setq org-CUA-compatible t)
  (with-eval-after-load 'org
    (setq org-link-abbrev-alist
          '(("RT" . "https://rt.info.ppke.hu/Ticket/Display.html?id=%s"))
          org-return-follows-link t
          org-fontify-done-headline t
          org-highlight-latex-and-related '(latex)
          org-M-RET-may-split-line '((default . nil))
          org-agenda-start-on-weekday 1
          org-enforce-todo-checkbox-dependencies t
          org-agenda-files '("~/Notes/Master.org")
          org-capture-templates
          '(("n" "New" entry (file+headline "~/Notes/Master.org" "Incoming")
             "* NEW %?\n  %i\n" :empty-lines 1))
          org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil         ; Refile in a single go
          org-enforce-todo-dependencies t
          org-log-into-drawer t
          org-log-states-order-reversed t
          org-startup-folded nil
          org-todo-keywords
          '((sequence "TODO(t!)" "NEW(n)" "INPROGRESS(i!)" "BLOCKED(b@/!)" "|" "DONE(d!)" "CANCELLED(c@/!)")))

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

(when (string= (system-name) "asgard")
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
      (emms-player-mpd-connect))))

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

         (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

         (define-key dired-mode-map (kbd "j") #'dired-do-open)))


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
         (global-set-key (kbd "<f7>") (lambda ()
                                        (interactive)
                                        (magit-status "/yadm::")))
    (with-eval-after-load 'magit
      (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
            magit-diff-refine-hunk t
            magit-bury-buffer-function #'magit-restore-window-configuration))))

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
  :before (setq undo-tree-history-directory-alist '(("." . "/tmp")))
  (global-undo-tree-mode 1))

(el-get-bundle swiper
    :type github
    :pkgname "pasja/swiper"
    (progn (ivy-mode 1)
           (global-set-key (kbd "C-c C-r") 'ivy-resume)
           (global-set-key (kbd "M-x") 'counsel-M-x)
           (global-set-key (kbd "C-x C-f") 'counsel-find-file)
           (global-set-key (kbd "C-c g") 'counsel-git)
           (global-set-key (kbd "C-c j") 'counsel-git-grep)
           (global-set-key (kbd "C-c r") 'counsel-rg)
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

(el-get-bundle yasnippet
  (yas-global-mode 1))

(el-get-bundle image+
  (with-eval-after-load 'image
    '(require 'image+)))

(when (string= (system-name) "asgard")
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

(el-get-bundle posframe)

(el-get-bundle ivy-posframe
  :type github
  :pkgname "tumashu/ivy-posframe"
  :description "ivy-posframe is a ivy extension, which let ivy use posframe to show its candidate menu."
  (progn
    (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
    (defface ivy-posframe-cursor
      '((t (:inherit ivy-cursor)))
      "Face used by the ivy-posframe's fake cursor."
      :group 'ivy-posframe)
    (ivy-posframe-mode 1)))

(el-get-bundle prescient
  :type github
  :pkgname "radian-software/prescient.el"
  :description "prescient.el is a library which sorts and filters lists of candidates, such as appear when you use a package like Ivy or Company."
  (progn
    (ivy-prescient-mode)
    (company-prescient-mode)
    (prescient-persist-mode)))

(el-get-bundle dired-rsync
  :type github
  :pkgname "stsquad/dired-rsync"
  :description "This package adds a single command dired-rsync which allows the user to copy marked files in a dired buffer via rsync."
  (define-key dired-mode-map (kbd "C-c C-r") 'dired-rsync))

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

    (when (string= (system-name) "midgard") ; autojoin
      (when (string= (daemonp) "irc")
        (add-to-list 'load-path "~/.emacs.d/secrets/")
        (require 'ercidentities)
        (enable-circe-new-day-notifier)
        (add-to-list 'circe-format-not-tracked 'circe-new-day-notifier-format-message)))))

(el-get-bundle exec-path-from-shell
  (exec-path-from-shell-initialize))

(el-get-bundle git-link
  :type github
  :pkgname "sshaw/git-link"
  :description "Interactive Emacs functions that create URLs for files and commits in GitHub/Bitbucket/GitLab/... repositories.")

(el-get 'sync)

;; External libraries

(add-to-list 'load-path "~/.emacs.d/plugins")
(byte-recompile-directory "~/.emacs.d/plugins/" 0) ; auto byte-compile all of them
(mapc 'load-file
      (directory-files "~/.emacs.d/plugins/" t ".elc$")) ; load them all!

;; Customize

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

