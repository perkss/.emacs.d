;;;;; Configuration for Rust

;;(use-package cl-lib :ensure nil)
;;(require 'init-lib)

;;  Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (setq company-tooltip-align-annotations t))


;; (use-package rustic
;;   :mode "\\.rs\\'"
;;   :ensure
;;   :bind (:map rustic-mode-map
;;               ("M-j" . lsp-ui-imenu)
;;               ("M-?" . lsp-find-references)
;;               ("C-c C-c l" . flycheck-list-errors)
;;               ("C-c C-c a" . lsp-execute-code-action)
;;               ("C-c C-c r" . lsp-rename)
;;               ("C-c C-c q" . lsp-workspace-restart)
;;               ("C-c C-c Q" . lsp-workspace-shutdown)
;;               ;;("C-c C-c s" . lsp-rust-analyzer-status)
;;               )
;;   :config
;;   (add-hook 'rust-mode-hook 'racer-mode)
;;   (add-hook 'racer-mode-hook 'eldoc-mode)
;;   (add-hook 'rust-mode-hook 'company-mode)

;;   ;; uncomment for less flashiness
;;   ;; (setq lsp-eldoc-hook nil)
;;   ;; (setq lsp-enable-symbol-highlighting nil)
;;   ;; (setq lsp-signature-auto-activate nil)

;;   ;; comment to disable rustfmt on save
;;   (setq rustic-format-on-save t))


(provide 'init-rust)
