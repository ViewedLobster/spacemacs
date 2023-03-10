
(defun shite-get-os ()
  (let ((os (substring (shell-command-to-string "uname") 0 -1)))
    (if (equal os "Darwin")
        'shite-os-macos
      (when (equal os "Linux")
          'shite-os-linux))))

(defun shite-open-with-default (path)
  (let* ((os (shite-get-os))
         (cmd (cl-case os
                ('shite-os-macos "open")
                ('shite-os-linux "xdg-open"))))
    (shite-open-with cmd path)))

(defun shite-open-with (cmd path)
  (when cmd
    (shell-command (format "%s %s &"
                           cmd
                           (shell-quote-argument (expand-file-name path))))))

(provide 'shite)
