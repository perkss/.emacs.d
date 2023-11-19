;;;; Projectile

(use-package projectile
  :bind
  (:map projectile-command-map
        ("." . counsel-projectile-find-file-dwim))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (progn
    (setq projectile-enable-caching t)))


 (eval-after-load "projectile"
    '(progn
       (add-to-list 'projectile-globally-ignored-directories "*node_modules")
       (add-to-list 'projectile-globally-ignored-directories "*DS_Store")
       (add-to-list 'projectile-globally-ignored-directories "*bower_components")
       (add-to-list 'projectile-globally-ignored-directories "*.class")
       (add-to-list 'projectile-globally-ignored-directories "*.~")
       (add-to-list 'projectile-globally-ignored-directories "*build")
       (add-to-list 'projectile-globally-ignored-directories "*.cache")
       (add-to-list 'projectile-globally-ignored-directories "*cmake-build")))

(provide 'init-projectile)
