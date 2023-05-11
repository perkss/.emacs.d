;;;; Let's give Light Table a run for its money
;;;;
;;;; Installed packages (and dependencies):
;;;; - cider
;;;; - rainbow-delimiters
;;;; - paredit
;;;; And you need 'lein' from leiningen.org in your path.
;;;; Note that clojure.el is in subdir 'extensions'.
;;;;
;;;; Usage:
;;;;   M-x cider-jack-in
;;;;   (Ignore the error when it starts). Use M-C-x to evaluate Clojure forms.
;;;;   C-c C-q to quit in the REPL (or use teh CIDER menu)
(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package cljsbuild-mode
  :ensure t)

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

;;; Env PATH
(defun set-exec-path-for-lein ()
  (let ((path-from-shell
         (shell-command-to-string "$SHELL -i -c -n 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-for-lein)

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

(provide 'init-clojure)
