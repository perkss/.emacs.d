;;; init-lsp --- Initialize LSP mode for Exordium

;;; Commentary:

;;; Code:
(require 'init-prefs)

;;; LSP Performance
(setq gc-cons-threshold 99999999)
  ;; process buffer for the LSP server needs to be larger
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package flycheck
  :commands flycheck-mode
  :init (global-flycheck-mode)
  :config
  (add-hook 'typescript-mode-hook 'flycheck-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :defines flycheck-pos-tip-timeout
  :hook (global-flycheck-mode . flycheck-pos-tip-mode)
  :config (setq flycheck-pos-tip-timeout 15))

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix exordium-lsp-keymap-prefix)


(use-package lsp-mode
  :if exordium-lsp-mode-enable
  :hook ((c-mode-common  . lsp)
         (c-ts-mode-common  . lsp)
         (typescript-mode . lsp)
         (typescript-ts-mode . lsp)
         (c++-mode  . lsp)
         (c++-ts-mode  . lsp)
         (c-mode  . lsp)
         (c-ts-mode  . lsp)
         (c-or-c++-mode  . lsp)
         (c-or-c++-ts-mode  . lsp)
         (java-mode . lsp)
         (java-ts-mode . lsp)
         (js-mode . lsp)
         (js-ts-mode . lsp)
         (js-jsx-mode . lsp)
         (python-mode . lsp)
         (python-ts-mode . lsp)
         (web-mode . lsp)
         (haskell-mode . lsp)
         )
  :init
  (setq-default lsp-clients-clangd-executable
                (seq-find #'executable-find exordium-lsp-clangd-executable))
  :commands (lsp lsp-deferred)

  :config
  (if exordium-enable-which-key
      (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
  (setq lsp-clients-clangd-args exordium-lsp-clangd-args)
  (setq lsp-diagnostic-package :flycheck)
  (setq lsp-flycheck-live-reporting t)
  ;; company mode configuration for lsp-mode
  (setq lsp-completion-provider :capf)

  ;; semantic hilite via lsp server
  (setq lsp-enable-semantic-highlighting t)

  (setq lsp-idle-delay 0.2) ;; clangd is fast increase this if
  (global-set-key (kbd "<M-return>") 'lsp-execute-code-action)
  (setq treemacs-space-between-root-nodes nil)
  (setq lsp-completion-enable t lsp-enable-on-type-formatting t)
  (setq lsp-log-io nil)
  (setq lsp-signature-render-document nil)
  (setq lsp-pyls-plugins-flake8-enabled t)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)

     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  ;; :custom
)

(use-package lsp-ui
  :after lsp-mode
  :init
  (setq lsp-ui-doc-enable exordium-lsp-ui-doc-enable
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-position exordium-lsp-ui-doc-position
        lsp-ui-doc-include-signature t

        lsp-ui-sideline-enable exordium-lsp-ui-sideline-enable
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover nil

        lsp-ui-flycheck-enable exordium-lsp-ui-flycheck-enable
        lsp-ui-flycheck-list-position exordium-lsp-ui-flycheck-list-position

        lsp-ui-peek-enable exordium-lsp-ui-peek-enable

        lsp-lens-enable t)
  :commands lsp-ui-mode)

(with-eval-after-load 'lsp-mode
  (dolist (dir '("[/\\\\]\\.ccls-cache\\'"
                 "[/\\\\]\\.mypy_cache\\'"
                 "[/\\\\]\\.pytest_cache\\'"
                 "[/\\\\]\\.cache\\'"
                 "[/\\\\]\\.clwb\\'"
                 "[/\\\\]__pycache__\\'"
                 "[/\\\\]cmake-build\\'"
                 "[/\\\\]bazel-bin\\'"
                 "[/\\\\]bazel-code\\'"
                 "[/\\\\]bazel-genfiles\\'"
                 "[/\\\\]bazel-out\\'"
                 "[/\\\\]bazel-testlogs\\'"
                 "[/\\\\]third_party\\'"
                 "[/\\\\]third-party\\'"
                 "[/\\\\]buildtools\\'"
                 "[/\\\\]out\\'"
                 "[/\\\\]build\\'"
                 ))
    (push dir lsp-file-watch-ignored-directories))

  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\.my-files\\'"))


(use-package lsp-treemacs
  :after lsp-mode
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :requires (dap-lldb dap-cpptools dap-gdb-lldb)
  :init
  (setq lsp-enable-dap-auto-configure t)
  :config
  (require 'dap-cpptools)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (dap-gdb-lld-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Config"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
         :gdbpath "rust-lldb"
         :target nil
         :cwd nil))
  :commands dap-mode)

(use-package which-key
  :if exordium-enable-which-key
  :config
  (which-key-mode))

(use-package flycheck-haskell
  :ensure t)

(with-eval-after-load 'lsp-mode
  (require 'dap-cpptools))

(provide 'init-lsp)
;;; init-lsp.el ends here
