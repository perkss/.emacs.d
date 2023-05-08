;;;;;;;; Configuration for Java

(use-package lsp-java
  :ensure t
  :after lsp
  :config
(setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx3G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-Djava.awt.headless=true"
         )

;;        lsp-java-java-path "/usr/lib/jvm/java-11-openjdk/bin/java"

        ;; Don't organise imports on save
  ;;      lsp-java-save-action-organize-imports nil

        ;; Don't format my source code (I use Maven for enforcing my
        ;; coding style)
        lsp-java-format-enabled nil)

  (add-hook 'java-mode-hook 'lsp)
(add-hook 'java-mode-hook 'flycheck-mode)
(add-hook 'java-mode-hook 'company-mode)
)


;; (use-package lsp-java-boot
;;   :ensure t
;;   :after lsp
;;   :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-java
  :ensure nil
  :after (lsp-java)
  :config
  (global-set-key (kbd "<f7>") 'dap-step-in)
  (global-set-key (kbd "<f8>") 'dap-next)
  (global-set-key (kbd "<f9>") 'dap-continue)
  )

(use-package kotlin-mode
  :hook
  (kotlin-mode . lsp))


(provide 'init-java)
