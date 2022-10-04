(defcustom orgnotes-base-dir "~/Documents/orgnotes/"
  "Base directory for orgnotes")

(defun orgnotes-ensure-dir (dir)
  "week directory"
  (if (file-exists-p dir)
      (if (eq (file-attribute-type (file-attributes dir)) t)
          t
        nil)
    (progn
      (make-directory dir t)
      t)))

(defun orgnotes-open-note (kind &optional name)
  "Open org note. kind can be any of the following:
'nowly : open new note with timestamp under notesdir/<weekstart>/<today>/.

'daily: open daily note for current date

'weekly: open weekly note for current week"
  (interactive "xNote scope: ")
  (let ((week (substring (shell-command-to-string
                          "date -v -Mon \"+%Y-%m-%d\"")
                         0 -1))
        (day (substring (shell-command-to-string
                          "date \"+%Y-%m-%d--%a\"")
                         0 -1)))
    (let ((dir (if (eq kind 'weekly)
                   (format "%s/%s" orgnotes-base-dir week)
                 (format "%s/%s/%s" orgnotes-base-dir week day))))
      (when (orgnotes-ensure-dir dir)
        (cond ((eq kind 'daily) (let ((filename (or name "daily")))
                                  (find-file (format "%s/%s.org" dir filename))))
              ((eq kind 'weekly) (let ((filename (or name "weekly")))
                                   (find-file (format "%s/%s.org" dir filename))))
              ((eq kind 'nowly)
               (let ((extra (if name
                                (format "-%s" name)
                              ""))
                     (timestamp (substring (shell-command-to-string
                                            "date \"+%Y-%m-%d--%H:%M:%S\"")
                                           0 -1)))
                 (find-file (format "%s/%s%s.org" dir timestamp extra))))))))
)

(defun orgnotes-open-daily ()
  "Open today's note"
  (interactive)
  (orgnotes-open-note 'daily)
  )

(defun orgnotes-open-weekly ()
  "Open this week's note"
  (interactive)
  (orgnotes-open-note 'weekly)
  )

(defun orgnotes-open-nowly ()
  "Open timestamped note"
  (interactive)
  (orgnotes-open-note 'nowly)
  )

(defun orgnotes-open-named (name)
  "Open named note in todays directory"
  (interactive "sName: ")
  (orgnotes-open-note 'daily name))

(defun orgnotes-find ()
  "Run helm find in orgnotes directory"
  (interactive)
  (progn (require 'helm-find)
         (helm-find-1 orgnotes-base-dir)))

(provide 'orgnotes)
