;;;; Treemacs

(use-package treemacs
  :ensure t
  :defer t
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

    (use-package treemacs-magit
    :after magit
    :autoload treemacs-magit--schedule-update
    :hook ((magit-post-commit
            git-commit-post-finish
            magit-post-stage
            magit-post-unstage)
           . treemacs-magit--schedule-update))

(provide 'init-treemacs)
