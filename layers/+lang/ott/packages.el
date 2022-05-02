;; ott layer

(defconst ott-packages
  '((ott-mode :location local)))

(defun spacemacs-ott/ott-check ()
  "Check current file for ott errors"
  (interactive)
  (shell-command (format
                  "ott -colour false -i %s"
                  (shell-quote-argument (buffer-file-name))))
  )

(defun spacemacs-ott/ott-preview ()
  "Preview ott pdf"
  (interactive)
  (let* ((current (buffer-file-name))
         (sans (file-name-sans-extension current))
         (in  current)
         (tex (concat sans ".tex"))
         (out (concat sans ".pdf")))
    (when (eq
           0
           (shell-command (format
                           "ott -colour false -i %s -o %s"
                           (shell-quote-argument in)
                           (shell-quote-argument tex))))
      (when (eq 0 (shell-command (format
                                  "pdflatex %s"
                                  (shell-quote-argument tex))))
        (spacemacs-ott/ott-open-pdf out)))
    )
  )

(defun spacemacs-ott/ott-open-pdf (file)
  (async-shell-command (format "open %s" (shell-quote-argument file)))
  )

(defun ott/init-ott-mode ()
  (use-package ott-mode
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'ott-mode
        "c" 'spacemacs-ott/ott-check
        "p" 'spacemacs-ott/ott-preview))
    )
  )
