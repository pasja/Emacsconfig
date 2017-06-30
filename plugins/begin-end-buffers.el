(defmacro my-special-beginning-of-buffer (mode &rest forms)
  "Define a special version of `beginning-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-min' and then FORMS are evaluated.  If the point did
not change because of the evaluation of FORMS, jump
unconditionally to `point-min'.  This way repeated invocations
toggle between real beginning and logical beginning of the
buffer."
  (declare (indent 1))
  (let ((fname (intern (concat "my-" (symbol-name mode) "-beginning-of-buffer")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-min))
           ,@forms
           (when (= p (point))
             (goto-char (point-min)))))
       (add-hook ',mode-hook
                 (lambda ()
                   (define-key ,mode-map
                     [remap beginning-of-buffer] ',fname))))))

(defmacro my-special-end-of-buffer (mode &rest forms)
  "Define a special version of `end-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-max' and then FORMS are evaluated.  If the point did
not change because of the evaluation of FORMS, jump
unconditionally to `point-max'.  This way repeated invocations
toggle between real end and logical end of the buffer."
  (declare (indent 1))
  (let ((fname (intern (concat "my-" (symbol-name mode) "-end-of-buffer")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-max))
           ,@forms
           (when (= p (point))
             (goto-char (point-max)))))
       (add-hook ',mode-hook
                 (lambda ()
                   (define-key ,mode-map
                     [remap end-of-buffer] ',fname))))))

(my-special-beginning-of-buffer dired
  (while (not (ignore-errors (dired-get-filename)))
    (dired-next-line 1)))
(my-special-end-of-buffer dired
  (dired-previous-line 1))

(my-special-beginning-of-buffer occur
  (occur-next 1))
(my-special-end-of-buffer occur
  (occur-prev 1))

(my-special-beginning-of-buffer ibuffer
  (ibuffer-forward-line 1))
(my-special-end-of-buffer ibuffer
  (ibuffer-backward-line 1))

(my-special-beginning-of-buffer recentf-dialog
  (when (re-search-forward "^  \\[" nil t)
    (goto-char (match-beginning 0))))
(my-special-end-of-buffer recentf-dialog
  (re-search-backward "^  \\[" nil t))

(my-special-beginning-of-buffer org-agenda
  (org-agenda-next-item 1))
(my-special-end-of-buffer org-agenda
  (org-agenda-previous-item 1))

(my-special-beginning-of-buffer ag
  (compilation-next-error 1))
(my-special-end-of-buffer ag
  (compilation-previous-error 1))
