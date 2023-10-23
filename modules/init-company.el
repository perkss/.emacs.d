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
  (add-to-list 'company-backends '(company-capf company-dabbrev-code company-clang :with company-yasnippet))
  (add-to-list 'company-backends '(
                                   company-keywords
                                   company-cmake
                                   company-jedi
                                   ;;                        company-semantic
                                   company-files
                                   :with
                                   company-yasnippet
                                   company-dabbrev-code
                                   company-dabbrev
                                   ;;                        company-ispell
                                   )) ;;company-dabbrev https://company-mode.github.io/manual/Backends.html
  (setq company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.2)))
  (setq company-minium-prefix-length 2)
  (setq company-echo-delay 0.5)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-downcase t)
  (setq company-dabbrev-minimum-length 4)
  (setq company-files-exclusions '(".git/" "build" ".clangd" ".DS_Store"))
  (setq company-transformers '(delete-consecutive-dups company-sort-by-occurrence))
  :bind
  (:map company-active-map
        ;; Use ESC to escape company-complete (in addition to C-g)
        ("<escape>" . #'company-abort)
        ;; Key to force trigger company-complete
        :map global-map
        ("C-." . #'company-complete)
        ("C-c y" . #'company-yasnippet)))

(use-package company-jedi
  :ensure t)

(use-package company-statistics
  :ensure t
  :after company
  :init
  (company-statistics-mode))

(provide 'init-company)
