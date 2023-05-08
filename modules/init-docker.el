;;;; Configuration of Docker related features

(use-package docker
  :bind ("C-c D" . docker))


(use-package dockerfile-mode
  :mode
  ("Dockerfile\\'" . dockerfile-mode)
  :config
  (setq-default docker-use-sudo nil))

(provide 'init-docker)
