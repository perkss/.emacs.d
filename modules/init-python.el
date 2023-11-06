;;;; Configuration for Python

(setq lsp-pyls-plugins-flake8-enabled t)

;; (setq lsp-pylsp-server-command "~./local/bin/pyslp")

(use-package python
  :mode ("\\.py\\'" . python-mode)
  ("\\.wsgi$" . python-mode)
  :interpreter ("python3" . python-mode)
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (setq python-indent-offset 4)
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'python-mode-hook 'color-identifiers-mode))

;; Python
;; (use-package jedi
;;   :ensure t
;;   :init
;;   (add-to-list 'company-backends 'company-jedi)
;;   :config
;;   (use-package company-jedi
;;     :ensure t
;;     :init
;;     (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
;;     (setq company-jedi-python-bin "python3"))
;;   )

(use-package pytest
  :ensure t)

(use-package anaconda-mode
  :ensure t
  :init (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :config (use-package company-anaconda
            :ensure t
            :init (add-hook 'python-mode-hook 'anaconda-mode)
            (eval-after-load "company"
              '(add-to-list 'company-backends '(company-anaconda :with company-capf)))))



;; (use-package elpy
;;   :ensure t
;;   :commands elpy-enable
;;   :init (with-eval-after-load 'python (elpy-enable))

;;   :config
;;   (electric-indent-local-mode -1)
;;   (delete 'elpy-module-highlight-indentation elpy-modules)
;;   (delete 'elpy-module-flymake elpy-modules)

;;   (defun ha/elpy-goto-definition ()
;;     (interactive)
;;     (condition-case err
;;         (elpy-goto-definition)
;;       ('error (xref-find-definitions (symbol-name (symbol-at-point))))))

;;   :bind (:map elpy-mode-map ([remap elpy-goto-definition] .
;;                              ha/elpy-goto-definition)))


;; (defun elpy-goto-definition-or-rgrep ()
;;   "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
;;     (interactive)
;;     (ring-insert find-tag-marker-ring (point-marker))
;;     (condition-case nil (elpy-goto-definition)
;;         (error (elpy-rgrep-symbol
;;                    (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))

;; (define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition-or-rgrep)

(provide 'init-python)
