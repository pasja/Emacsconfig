;; forbidden commands

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Proper start

(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq-default major-mode 'text-mode)
(setq confirm-nonexistent-file-or-buffer nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(global-visual-line-mode 1)
(column-number-mode 1)
(global-linum-mode 1)
(setq linum-format "%d ")
(size-indication-mode t)
(setq vc-follow-symlinks t) ; auto-follow version controlled symlinks
(global-set-key (kbd "M-g") 'goto-line)    ; M-g  'goto-line
(global-set-key (kbd "<delete>") 'delete-char)  ; delete == delete

;; Useful aliases

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'perl-mode 'cperl-mode)
(defalias 'eb 'eval-buffer)

;; rectangles

(setq cua-enable-cua-keys nil) 
(cua-mode t)
(global-set-key (kbd "C-c m") 'cua-set-rectangle-mark)

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
  ido-max-prospects 8              ; don't spam my minibuffer
  ido-confirm-unique-completion t ; wait for RET, even with unique completion
  confirm-nonexistent-file-or-buffer nil ; when using ido, the confirmation is rather annoying...
  ido-everywhere t
  ido-create-new-buffer 'always)

;; increase minibuffer size when ido completion is active

(add-hook 'ido-minibuffer-setup-hook
  (function
    (lambda ()
      (make-local-variable 'resize-minibuffer-window-max-height)
      (setq resize-minibuffer-window-max-height 1))))

(ido-mode 1)

;; ibuffer

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; help for minibuffer

(icomplete-mode t)

;; autokill shell

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; windmove

(require 'windmove)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)

;; buffer change

(global-set-key (kbd "C-<left>") 'next-buffer)
(global-set-key (kbd "C-<right>") 'previous-buffer)

;; clipboard settings

(setq x-select-enable-primary nil  ; stops killing/yanking interacting with primary X11 selection
  x-select-enable-clipboard t  ; makes killing/yanking interact with clipboard X11 selection
  yank-pop-change-selection t)  ; makes rotating the kill ring change the X11 clipboard.

;; zone-mode for .hu

(add-to-list 'auto-mode-alist '("\\.hu$" . zone-mode))

;; auto chmod scripts

(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

;; configure cperl

(eval-after-load 'cperl-mode
  '(progn
     (define-key cperl-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (cperl-set-style "K&R")
     (setq cperl-invalid-face nil 
	   cperl-indent-parens-as-block t
	   cperl-tab-always-indent nil)
     )
  )
    
;; usual programming

(global-set-key (kbd "M-2") 'hippie-expand)
(global-set-key (kbd "M-3") 'comment-dwim)

;; smart shell start

(defun sh (name)
     "Smart shell start"
     (interactive "sShell name: ")
     (shell name)
     (set-process-query-on-exit-flag (get-process "shell") nil)
     )

;; set scheme

(add-hook 'inferior-scheme-mode-hook 'scheme-exit-hook)
(defun scheme-exit-hook ()
  (set-process-query-on-exit-flag (get-process "scheme") nil)
)

;;Configure dired

(add-hook 'dired-mode-hook ;same buffer for all directories
 (lambda ()
  (define-key dired-mode-map (kbd "<return>")
    'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
 ))

(setq dired-recursive-copies 'always ; recursive copy/delete
      dired-recursive-deletes 'top
      dired-dwim-target t)

;; External libraries 

(add-to-list 'load-path "~/.emacs.d/plugins")

;; External libraries (with el-get)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(setq
 el-get-sources
 '(el-get
   autopair
   git-emacs
   dired+
  )
)
(el-get 'sync)

;; git-emacs

(require 'git-status)

;; Autopair

(require 'autopair)
(autopair-global-mode 1)

;; add yasnippet
