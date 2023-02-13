;; whitespace hell

(defcustom whitespace-hell-max-column nil
  "Maximum whitespace fill column for whitespace hell")

(defun wsh-get-max-column ()
  "get maximum whitespace hell column"
  (or whitespace-hell-max-column fill-column))

(defun wsh-is-whitespace (char)
  "is the char whitespace?"
  (eq (char-syntax char) ?\s)
  )

(defun wsh-ws-fill-line-random ()
  "fill line if the line is not already hellified"
  (save-excursion
    (end-of-line)
    (let ((line-width (current-column)))
      (if (and (< line-width (wsh-get-max-column))
               (or (eq line-width 0)
                   (not (wsh-is-whitespace (char-before)))))
          (wsh-append-whitespace
           (max 1 (random (- (wsh-get-max-column) line-width)))))
      )
    )
  )

(defun wsh-hellify-line ()
  "Hellify line"
  (wsh-ws-fill-line-random)
  )

(defun wsh-append-whitespace (num)
  (save-excursion
    (end-of-line)
    (insert (make-string num ?\s))))

(defun wsh-whitespace-hell ()
  "make whitespace hell"
  (with-current-buffer (current-buffer)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (wsh-hellify-line)
        (forward-line)))))


(define-minor-mode whitespace-hell-mode
  "Enable whitespace hell mode, or how I learned to love the ioopm psychos.

Make sure to remove all hooks that removes trailing whitespace on save.

For full enjoyment, enable highlighting of trailing whitespace."
  :global nil
  :init-value nil
  :lighter " ws-hell"
  (if whitespace-hell-mode
      (add-hook 'write-contents-functions 'wsh-whitespace-hell nil t)
    (remove-hook 'write-contents-functions 'wsh-whitespace-hell t))
  )

(provide 'whitespace-hell)
