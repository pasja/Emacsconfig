(defun hostname-to-string ()
  "insert the contents of /etc/hostname to a string"
  (when (file-readable-p "/etc/hostname") 
    (with-temp-buffer 
      (insert-file-contents "/etc/hostname")
      (buffer-string))))
