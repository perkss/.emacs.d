;;; init-kotlin --- Configuration for Kotlin

;;; Commentary:

;;; Code:
(use-package kotlin-mode
  :after (lsp-mode dap-mode)
  :config
  (require 'dap-kotlin)
  ;; should probably have been in dap-kotlin instead of lsp-kotlin
  (setq lsp-kotlin-debug-adapter-path (or (executable-find "kotlin-debug-adapter") ""))
  :mode
  ("\\.kts\\'" . kotlin-mode)
  :hook
  (kotlin-mode . lsp))


(use-package flycheck-kotlin
 :after (kotlin-mode flycheck)
 :commands
 (flycheck-kotlin-setup)
 :config
 (flycheck-kotlin-setup))


(use-package gradle-mode
  :hook ((java-mode kotlin-mode) . gradle-mode))

(use-package javadoc-lookup)

;;; LSP Config

(setq lsp-ui-sideline-show-code-actions t)

(provide 'init-kotlin)
;;; init-kotlin.el ends here
