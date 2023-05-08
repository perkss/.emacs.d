;;;; company-mode


;;; got to work out modes
;;; https://emacs.stackexchange.com/questions/27502/stop-company-mode-from-completing-dictionary-words

(use-package company
  :diminish "CA"
  :after (rtags)
  :defer
  :config
  (setq rtags-completions-enabled t)
  ;; Turn on company mode everywhere
  (global-company-mode)
  (add-to-list 'company-backends '(company-capf
                                  company-keywords
                        company-semantic
                        company-files
                        company-etags
                        company-elisp
                        company-clang
                        company-jedi
                        company-cmake
                        company-ispell
                        company-yasnippet
                         company-dabbrev
                                   )) ;;company-dabbrev https://company-mode.github.io/manual/Backends.html
  (setq company-idle-delay nil)
  :bind
  (:map company-active-map
        ;; Use ESC to escape company-complete (in addition to C-g)
        ("<escape>" . #'company-abort)
        ;; Key to force trigger company-complete
        :map global-map
        ("C-." . #'company-complete)))


(setq company-dabbrev-downcase nil)
;;        company-show-numbers f
;;        company-dabbrev-downcase nil
(use-package company-jedi
  :ensure t)


(use-package company-statistics
  :ensure t
  :after company
  :init
  (company-statistics-mode))

(provide 'init-company)
