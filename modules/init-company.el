;;;; company-mode


;;; got to work out modes
;;; https://emacs.stackexchange.com/questions/27502/stop-company-mode-from-completing-dictionary-words

(use-package company
  :diminish "CA"
  :after (rtags)
  :defer
  :config
  (setq rtags-completions-enabled nil)
  ;; Turn on company mode everywhere
  (global-company-mode)
  (add-to-list 'company-backends '(company-capf company-dabbrev-code :with company-yasnippet :separate))
  (setq company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.05)))
  (setq company-minium-prefix-length 2)
  (setq company-echo-delay 0.2)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-downcase t)
  (setq company-dabbrev-minimum-length 4)
  (setq company-files-exclusions '(".git/" "cmake-build" "build" ".clangd" ".DS_Store"))
  (setq company-transformers '(delete-consecutive-dups company-sort-by-occurrence))
  :bind
  (:map company-active-map
        ;; Key to force trigger company-complete
        :map global-map
        ("C-." . #'company-complete)
        ("C-c y" . #'company-yasnippet)))

(provide 'init-company)
