(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(icicle-top-level-key-bindings (quote ((cua-set-rectangle-mark my-c-return t) ([pause] icicle-switch-to/from-minibuffer t) ("`" icicle-search-generic t) ("$" icicle-search-word t) ("^" icicle-search-keywords t) ("'" icicle-occur t) ("=" icicle-imenu t) ("\"" icicle-search-text-property t) ("/" icicle-complete-thesaurus-entry t) ([24 134217829] icicle-execute-named-keyboard-macro t) (" " icicle-command-abbrev t) ("5o" icicle-select-frame t) ("" icicle-describe-option-of-type t) ([S-f4] icicle-kmacro t) (abort-recursive-edit icicle-abort-recursive-edit t) (bookmark-jump icicle-bookmark t) (bookmark-jump-other-window icicle-bookmark-other-window t) (bookmark-set icicle-bookmark-cmd t) (minibuffer-keyboard-quit icicle-abort-recursive-edit (fboundp (quote minibuffer-keyboard-quit))) (delete-window icicle-delete-window t) (delete-windows-for icicle-delete-window t) (dired icicle-dired t) (dired-other-window icicle-dired-other-window t) (exchange-point-and-mark icicle-exchange-point-and-mark t) (execute-extended-command icicle-execute-extended-command t) (find-file icicle-file t) (find-file-other-window icicle-file-other-window t) (find-file-read-only icicle-find-file-read-only t) (find-file-read-only-other-window icicle-find-file-read-only-other-window t) (insert-buffer icicle-insert-buffer t) (kill-buffer icicle-kill-buffer t) (kill-buffer-and-its-windows icicle-kill-buffer t) (other-window icicle-other-window-or-frame t) (other-window-or-frame icicle-other-window-or-frame t) (pop-global-mark icicle-goto-global-marker-or-pop-global-mark t) (set-mark-command icicle-goto-marker-or-set-mark-command t) (switch-to-buffer icicle-buffer t) (switch-to-buffer-other-window icicle-buffer-other-window t) (where-is icicle-where-is t) (yank icicle-yank-maybe-completing t) (bmkp-tag-a-file icicle-tag-a-file (fboundp (quote bmkp-tag-a-file))) (bmkp-untag-a-file icicle-untag-a-file (fboundp (quote bmkp-untag-a-file))) (bmkp-find-file-all-tags icicle-find-file-all-tags (fboundp (quote bmkp-find-file-all-tags))) (bmkp-find-file-all-tags-other-window icicle-find-file-all-tags-other-window (fboundp (quote bmkp-find-file-all-tags))) (bmkp-find-file-all-tags-regexp icicle-find-file-all-tags-regexp (fboundp (quote bmkp-find-file-all-tags-regexp))) (bmkp-find-file-all-tags-regexp-other-window icicle-find-file-all-tags-regexp-other-window (fboundp (quote bmkp-find-file-all-tags-regexp-other-window))) (bmkp-find-file-some-tags icicle-find-file-some-tags (fboundp (quote bmkp-find-file-some-tags))) (bmkp-find-file-some-tags-other-window icicle-find-file-some-tags-other-window (fboundp (quote bmkp-find-file-some-tags-other-window))) (bmkp-find-file-some-tags-regexp icicle-find-file-some-tags-regexp (fboundp (quote bmkp-find-file-some-tags-regexp))) (bmkp-find-file-some-tags-regexp-other-window icicle-find-file-some-tags-regexp-other-window (fboundp (quote bmkp-find-file-some-tags-regexp-other-window))) (bmkp-bookmark-list-jump icicle-bookmark-bookmark-list (fboundp (quote bmkp-bookmark-list-jump))) (bmkp-desktop-jump icicle-bookmark-desktop (fboundp (quote bmkp-desktop-jump))) (bmkp-dired-jump icicle-bookmark-dired (fboundp (quote bmkp-dired-jump))) (bmkp-dired-jump-other-window icicle-bookmark-dired-other-window (fboundp (quote bmkp-dired-jump))) (bmkp-file-jump icicle-bookmark-file (fboundp (quote bmkp-file-jump))) (bmkp-file-jump-other-window icicle-bookmark-file-other-window (fboundp (quote bmkp-file-jump))) (bmkp-file-this-dir-jump icicle-bookmark-file-this-dir (fboundp (quote bmkp-file-this-dir-jump))) (bmkp-file-this-dir-jump-other-window icicle-bookmark-file-this-dir-other-window (fboundp (quote bmkp-file-this-dir-jump))) (bmkp-gnus-jump icicle-bookmark-gnus (fboundp (quote bmkp-gnus-jump))) (bmkp-gnus-jump-other-window icicle-bookmark-gnus-other-window (fboundp (quote bmkp-gnus-jump))) (bmkp-info-jump icicle-bookmark-info (fboundp (quote bmkp-info-jump))) (bmkp-info-jump-other-window icicle-bookmark-info-other-window (fboundp (quote bmkp-info-jump))) (bmkp-local-file-jump icicle-bookmark-local-file (fboundp (quote bmkp-local-file-jump))) (bmkp-local-file-jump-other-window icicle-bookmark-local-file-other-window (fboundp (quote bmkp-local-file-jump))) (bmkp-man-jump icicle-bookmark-man (fboundp (quote bmkp-man-jump))) (bmkp-man-jump-other-window icicle-bookmark-man-other-window (fboundp (quote bmkp-man-jump))) (bmkp-non-file-jump icicle-bookmark-non-file (fboundp (quote bmkp-non-file-jump))) (bmkp-non-file-jump-other-window icicle-bookmark-non-file-other-window (fboundp (quote bmkp-non-file-jump))) (bmkp-region-jump icicle-bookmark-region (fboundp (quote bmkp-region-jump))) (bmkp-region-jump-other-window icicle-bookmark-region-other-window (fboundp (quote bmkp-region-jump))) (bmkp-remote-file-jump icicle-bookmark-remote-file (fboundp (quote bmkp-remote-file-jump))) (bmkp-remote-file-jump-other-window icicle-bookmark-remote-file-other-window (fboundp (quote bmkp-remote-file-jump))) (bmkp-specific-buffers-jump icicle-bookmark-specific-buffers (fboundp (quote bmkp-specific-buffers-jump))) (bmkp-specific-buffers-jump-other-window icicle-bookmark-specific-buffers-other-window (fboundp (quote bmkp-specific-buffers-jump))) (bmkp-specific-files-jump icicle-bookmark-specific-files (fboundp (quote bmkp-specific-files-jump))) (bmkp-specific-files-jump-other-window icicle-bookmark-specific-files-other-window (fboundp (quote bmkp-specific-files-jump))) (bmkp-this-buffer-jump icicle-bookmark-this-buffer (fboundp (quote bmkp-this-buffer-jump))) (bmkp-this-buffer-jump-other-window icicle-bookmark-this-buffer-other-window (fboundp (quote bmkp-this-buffer-jump))) (bmkp-all-tags-jump icicle-bookmark-all-tags (fboundp (quote bmkp-all-tags-jump))) (bmkp-all-tags-jump-other-window icicle-bookmark-all-tags-other-window (fboundp (quote bmkp-all-tags-jump))) (bmkp-all-tags-regexp-jump icicle-bookmark-all-tags-regexp (fboundp (quote bmkp-all-tags-regexp-jump))) (bmkp-all-tags-regexp-jump-other-window icicle-bookmark-all-tags-regexp-other-window (fboundp (quote bmkp-all-tags-regexp-jump))) (bmkp-some-tags-jump icicle-bookmark-some-tags (fboundp (quote bmkp-some-tags-jump))) (bmkp-some-tags-jump-other-window icicle-bookmark-some-tags-other-window (fboundp (quote bmkp-some-tags-jump))) (bmkp-some-tags-regexp-jump icicle-bookmark-some-tags-regexp (fboundp (quote bmkp-some-tags-regexp-jump))) (bmkp-some-tags-regexp-jump-other-window icicle-bookmark-some-tags-regexp-other-window (fboundp (quote bmkp-some-tags-regexp-jump))) (bmkp-file-all-tags-jump icicle-bookmark-file-all-tags (fboundp (quote bmkp-file-all-tags-jump))) (bmkp-file-all-tags-jump-other-window icicle-bookmark-file-all-tags-other-window (fboundp (quote bmkp-file-all-tags-jump))) (bmkp-file-all-tags-regexp-jump icicle-bookmark-file-all-tags-regexp (fboundp (quote bmkp-file-all-tags-regexp-jump))) (bmkp-file-all-tags-regexp-jump-other-window icicle-bookmark-file-all-tags-regexp-other-window (fboundp (quote bmkp-file-all-tags-regexp-jump))) (bmkp-file-some-tags-jump icicle-bookmark-file-some-tags (fboundp (quote bmkp-file-some-tags-jump))) (bmkp-file-some-tags-jump-other-window icicle-bookmark-file-some-tags-other-window (fboundp (quote bmkp-file-some-tags-jump))) (bmkp-file-some-tags-regexp-jump icicle-bookmark-file-some-tags-regexp (fboundp (quote bmkp-file-some-tags-regexp-jump))) (bmkp-file-some-tags-regexp-jump-other-window icicle-bookmark-file-some-tags-regexp-other-window (fboundp (quote bmkp-file-some-tags-regexp-jump))) (bmkp-file-this-dir-all-tags-jump icicle-bookmark-file-this-dir-all-tags (fboundp (quote bmkp-file-this-dir-all-tags-jump))) (bmkp-file-this-dir-all-tags-jump-other-window icicle-bookmark-file-this-dir-all-tags-other-window (fboundp (quote bmkp-file-this-dir-all-tags-jump))) (bmkp-file-this-dir-all-tags-regexp-jump icicle-bookmark-file-this-dir-all-tags-regexp (fboundp (quote bmkp-file-this-dir-all-tags-regexp-jump))) (bmkp-file-this-dir-all-tags-regexp-jump-other-window icicle-bookmark-file-this-dir-all-tags-regexp-other-window (fboundp (quote bmkp-file-this-dir-all-tags-regexp-jump))) (bmkp-file-this-dir-some-tags-jump icicle-bookmark-file-this-dir-some-tags (fboundp (quote bmkp-file-this-dir-some-tags-jump))) (bmkp-file-this-dir-some-tags-jump-other-window icicle-bookmark-file-this-dir-some-tags-other-window (fboundp (quote bmkp-file-this-dir-some-tags-jump))) (bmkp-file-this-dir-some-tags-regexp-jump icicle-bookmark-file-this-dir-some-tags-regexp (fboundp (quote bmkp-file-this-dir-some-tags-regexp-jump))) (bmkp-file-this-dir-some-tags-regexp-jump-other-window icicle-bookmark-file-this-dir-some-tags-regexp-other-window (fboundp (quote bmkp-file-this-dir-some-tags-regexp-jump))) (bmkp-url-jump icicle-bookmark-url (fboundp (quote bmkp-url-jump))) (bmkp-url-jump-other-window icicle-bookmark-url-other-window (fboundp (quote bmkp-url-jump))) (bmkp-w3m-jump icicle-bookmark-w3m (fboundp (quote bmkp-w3m-jump))) (bmkp-w3m-jump-other-window icicle-bookmark-w3m-other-window (fboundp (quote bmkp-w3m-jump))) (find-tag icicle-find-tag (fboundp (quote command-remapping))) (find-tag-other-window icicle-find-first-tag-other-window t) (pop-tag-mark icicle-pop-tag-mark (fboundp (quote command-remapping))) (eval-expression icicle-pp-eval-expression (fboundp (quote command-remapping))) (pp-eval-expression icicle-pp-eval-expression (fboundp (quote command-remapping))) ([27 134217848] lacarte-execute-command (fboundp (quote lacarte-execute-command))) ([134217824] lacarte-execute-menu-command (fboundp (quote lacarte-execute-menu-command))) ([f10] lacarte-execute-menu-command (fboundp (quote lacarte-execute-menu-command)))))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
