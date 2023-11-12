;;;; Projectile

;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package projectile
  :bind
  (:map projectile-command-map
        ("." . counsel-projectile-find-file-dwim))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (progn
    (setq projectile-enable-caching t)
    ))


 (eval-after-load "projectile"
    '(progn
       (add-to-list 'projectile-globally-ignored-directories "*node_modules")
       (add-to-list 'projectile-globally-ignored-directories "*DS_Store")
       (add-to-list 'projectile-globally-ignored-directories "*bower_components")
       (add-to-list 'projectile-globally-ignored-directories "*.class")
       (add-to-list 'projectile-globally-ignored-directories "*.~")
       (add-to-list 'projectile-globally-ignored-directories "*build")
       (add-to-list 'projectile-globally-ignored-directories "*cmake-build")))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t
  :bind
  (:map global-map
        ("C-c e" . #'treemacs)
        ("C-c E" . #'treemacs-projectile)))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(provide 'init-projectile)
