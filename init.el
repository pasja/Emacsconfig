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

(defun pasja--goto-up-in-dired ()
  (interactive)
  (let ((pasja-prev-dir-name (file-truename default-directory)))
    (find-alternate-file "..")
    (dired-goto-file pasja-prev-dir-name)))

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

;; Boostrap elpaca
;; preinstall the following debian packages:
;; apt install hunspell hunspell-hu texinfo build-essential texlive
;; apt install global python3-pygments ripgrep install-info
;; apt install autoconf automake gcc libpng-dev libpoppler-dev
;; apt install libpoppler-glib-dev libz-dev make pkg-config zip

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(setq use-package-always-ensure t)

(use-package compat)

(use-package transient)

(use-package magit
  :bind (("C-x g" . magit-status)
         ("<f7>" . (lambda () (interactive
                          (magit-status "/yadm::")))))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-diff-refine-hunk t
        magit-bury-buffer-function #'magit-restore-window-configuration))

(use-package vertico
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package prescient
  :config
    (prescient-persist-mode))

(use-package posframe)

(use-package vertico-posframe
  :config
  (vertico-posframe-mode 1))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t) ; only need to install it, embark loads it after consult if found

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)                    ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.

    :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  :config

  ;; Enable auto completion, configure delay, trigger and quitting
  (setq corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-trigger "." ;; Custom trigger characters
        corfu-quit-no-match 'separator) ;; or t

  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-mouse-mode)
  ;; (corfu-popupinfo-mode)
)

;; Add extensions
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
)

(use-package solarized-theme
  :ensure (:wait t)
  :config (load-theme 'solarized-dark t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (outline-minor-mode t) ; TODO: https://github.com/sellout/emacs-color-theme-solarized/issues/165
  (outline-minor-mode nil))

(use-package smartparens
  :ensure (:wait t)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

(use-package beginend
  :config
  (beginend-global-mode))

(use-package markdown-mode)

(use-package php-mode)

(use-package s)

(use-package web-mode)

(use-package csv-mode)

(use-package apache-mode)

(use-package yaml-mode)

(use-package dockerfile-mode)

(use-package restclient)

(use-package yasnippet
  :ensure (:wait t)
  :config
  (yas-global-mode 1))

(use-package x509-mode)

(use-package lua-mode)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package git-link)

(use-package dired+
  :ensure (:host github :repo "emacsmirror/dired-plus" :wait t)
  :demand
  :init
  (setq diredp-hide-details-initially-flag nil)
   :bind (:map dired-mode-map
               ("j" . dired-do-open)
               ("^" . pasja--goto-up-in-dired))
   :hook (dired-mode . dired-omit-mode)
   :config
   (diredp-toggle-find-file-reuse-dir 1) ; reuse existing dired buffer
   (setq dired-recursive-copies 'always  ; recursive copy/delete
         dired-recursive-deletes 'top
         dired-dwim-target t
         dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package orgtbl-aggregate)

;; External libraries

(add-to-list 'load-path "~/.emacs.d/plugins")
(byte-recompile-directory "~/.emacs.d/plugins/" 0) ; auto byte-compile all of them
(mapc 'load-file
      (directory-files "~/.emacs.d/plugins/" t ".elc$")) ; load them all!

;; Customize

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
