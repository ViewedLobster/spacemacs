
(defconst notes-packages
  '((orgnotes :location local)))

(defun notes/init-orgnotes ()
  (use-package orgnotes
    :config
    (progn
      (spacemacs/set-leader-keys "od" 'orgnotes-open-daily)
      (spacemacs/set-leader-keys "ow" 'orgnotes-open-weekly)
      (spacemacs/set-leader-keys "ot" 'orgnotes-open-nowly)
      (spacemacs/set-leader-keys "on" 'orgnotes-open-named)
      (spacemacs/set-leader-keys "of" 'orgnotes-find))
    )
  )
