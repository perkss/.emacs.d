;;; Imenu
;;; Shows the code structure through menu items

;; Will list the code structure
(use-package imenu-list
  :ensure t
  :bind (("M-7" . imenu-list)
         ("M-C-g" . counsel-imenu))
  :config
  (setq imenu-list-focus-after-activation t))

;; Navigate to any part of document across buffers
(use-package imenu-anywhere
  :ensure t
  :config
  (global-set-key (kbd "M-C-n") #'imenu-anywhere)
  )


(provide 'init-imenu)
