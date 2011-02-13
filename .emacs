
;; forbidden commands

(put 'narrow-to-region 'disabled nil)

(put 'set-goal-column 'disabled nil)

(put 'dired-find-alternate-file 'disabled nil)

;; Proper start

(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq show-paren t)
(visual-line-mode 1)

;; Useful aliases

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'perl-mode 'cperl-mode)
(defalias 'eb 'eval-buffer)

;; only for rectangles

(setq cua-enable-cua-keys nil) 
(cua-mode t)
(global-set-key (kbd "C-c m") 'cua-set-rectangle-mark)

;; ido mode 

(require 'ido) 
(ido-mode 'both) ; for buffers and files
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

 ido-enable-flex-matching nil     ; don't try to be too smart
 ido-max-prospects 8              ; don't spam my minibuffer
 ido-confirm-unique-completion t) ; wait for RET, even with unique completion

 (setq confirm-nonexistent-file-or-buffer nil) ; when using ido, the confirmation is rather annoying...

;; ibuffer

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; autocomplete for minibuffer

(icomplete-mode t)

;; windmove

(require 'windmove)
(global-set-key (kbd "A-<left>") 'windmove-left)
(global-set-key (kbd "A-<right>") 'windmove-right)
(global-set-key (kbd "A-<up>") 'windmove-up)
(global-set-key (kbd "A-<down>") 'windmove-down)

;; buffer-change

(global-set-key (kbd "C-<left>") 'next-buffer)
(global-set-key (kbd "C-<right>") 'previous-buffer)

;; easy C-x b

;; (iswitchb-mode 1)
;; (add-to-list 'iswitchb-buffer-ignore "^\*")

;; (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
;; (defun iswitchb-local-keys ()
;;   (define-key iswitchb-mode-map (kbd "<left>") 'iswitchb-next-match)
;;   (define-key iswitchb-mode-map (kbd "<right>") 'iswitchb-prev-match)
;;   )

;; line and column numbers

(column-number-mode 1)
(global-linum-mode 1)
(setq linum-format "%d ")

;; zone-mode for .hu

(add-to-list 'auto-mode-alist '("\\.hu$" . zone-mode))

;; smart shell start

(defun sh (name)
     "Smart shell start"
     (interactive "sShell name: ")
     (shell name)
     )

;; configure cperl

(eval-after-load 'cperl-mode
  '(progn
     (define-key cperl-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (cperl-set-style "K&R"))
  )
    
(setq cperl-invalid-face nil) 
(setq cperl-indent-parens-as-block t)
(setq cperl-tab-always-indent nil)

;; usual programming

(global-set-key (kbd "M-2") 'hippie-expand)
(global-set-key (kbd "M-3") 'comment-dwim)


;; External libraries (with el-get)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(setq
 el-get-sources
 '(el-get
   autopair
   )
)
(el-get 'sync)

;; add yasnippet

;;(add-to-list 'load-path "~/.emacs.d/plugins")
;;(require 'yasnippet-bundle)

;; configure autopair

(require 'autopair)
(autopair-global-mode 1)

