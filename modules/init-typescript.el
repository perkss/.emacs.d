;;; init-typescript --- Configuration for Typescript

;;; Commentary:

;;; Code:
(use-package typescript-mode
  :ensure t
  :after lsp-mode
  :mode ("\.ts$")
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (setq tab-width 2)
              (setq typescript-indent-level tab-width)
              )))

(use-package tide
  :init
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))


(provide 'init-typescript)
;;; init-typescript.el ends here
