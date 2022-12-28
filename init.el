;;;  ___      __   __   __

;;; EMACS Config based on EXORDIUM: https://github.com/emacs-exordium/exordium

;; Reduce the frequency of garbage collection by making it happen on
;; each 100MB of allocated data (the default is on every 0.76MB). This reduces
;; the startup time.

(setq dotfiles-lisp-dir
      (file-name-as-directory
       (concat (file-name-directory
                (or (buffer-file-name) load-file-name))
               "lisp")))
(add-to-list 'load-path dotfiles-lisp-dir)

;; Set the cmd as meta key
(setq ns-command-modifier (quote meta))

(setq gc-cons-threshold 99999999)

(let ((min-version "27.1"))
  (when (version< emacs-version min-version)
    (error "This config requires at least Emacs %s, but you're running %s"
           min-version emacs-version)))

(setq initial-scratch-message
      "EXORDIUM DID NOT LOAD CORRECTLY.
Check the warnings and messages buffers, or restart with --debug-init")

(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

(if (file-exists-p "/usr/bin/hunspell")
    (progn
      (setq ispell-program-name "hunspell")
      (eval-after-load "ispell"
        '(progn (defun ispell-get-coding-system () 'utf-8)))))

(defconst exordium-before-init "before-init.el"
  "name of the before init file")

(defconst exordium-prefs "prefs.el"
  "name of the prefs file")

(defconst exordium-after-init "after-init.el"
  "name of the after init file")

(defconst exordium-custom "emacs-custom.el"
  "name of the customization file")

;; Use this file for HTTP proxy settings if needed for packages.  Also add
;; additional packages to exordium-extra-packages for packages to be
;; automatically pulled from the elpa archives

(defconst exordium-before-init-file (locate-user-emacs-file exordium-before-init)
  "location of the master before init file")

(defconst exordium-modules-dir (locate-user-emacs-file "modules")
  "location of the modules directory")
(defconst exordium-themes-dir (locate-user-emacs-file "themes")
  "location of the themes directory")
(defconst exordium-extensions-dir (locate-user-emacs-file "extensions")
  "location of the extensions directory")
(defconst exordium-local-dir (locate-user-emacs-file "local")
  "location of the local directory")

(defconst exordium-prefs-file (locate-user-emacs-file exordium-prefs)
  "location of the master prefs file")

(defconst exordium-after-init-file (locate-user-emacs-file exordium-after-init)
  "location of the master after init file")

(defconst exordium-custom-file (locate-user-emacs-file exordium-custom)
  "location of the customization file")

;; Save any custom set variable in exordium-custom-file rather than at the end of init.el:
(setq custom-file exordium-custom-file)

(defcustom exordium-extra-packages ()
  "Additional packages to auto load from elpa repositories"
    :group 'exordium
    :type  'list)

(defcustom exordium-extra-pinned ()
  "Additional packages locations to pin to"
  :group 'exordium
  :type  'list)

;; Taps definition of before and after files. These are loaded
;; after master 'before', 'after', and 'prefs' files

(defconst exordium-taps-root (locate-user-emacs-file "taps")
  "location of the tapped directories")

(defconst exordium-tapped-before-init-files ()
  "all tapped before init files, including master")

(defconst exordium-tapped-prefs-files ()
  "all tapped prefs files, including master")

(defconst exordium-tapped-after-init-files ()
  "all tapped after init files, including master")

(defconst exordium-melpa-package-repo "http://melpa.org/packages/"
  "URL for packages repository")

(defconst exordium-pinned-melpa-package-repo "http://melpa.org/packages/"
  "URL for pinned default packages. Set to stable melpa.org if you want stable")

(defconst exordium-gnu-package-repo "http://elpa.gnu.org/packages/"
  "URL for the GNU package repository")

(when (file-accessible-directory-p exordium-taps-root)
  (dolist (tap (nreverse (directory-files exordium-taps-root t "^[^\.][^\.]?*+")))
    (when (file-accessible-directory-p tap)
      (let ((tapped (concat (file-name-as-directory tap) exordium-before-init)))
        (when (file-readable-p tapped)
          (add-to-list 'exordium-tapped-before-init-files tapped))
        (setq tapped (concat (file-name-as-directory tap) exordium-prefs))
        (when (file-readable-p tapped)
          (add-to-list 'exordium-tapped-prefs-files tapped))
        (setq tapped (concat (file-name-as-directory tap) exordium-after-init))
        (when (file-readable-p tapped)
          (add-to-list 'exordium-tapped-after-init-files tapped))))))

(when (file-readable-p exordium-before-init-file)
  (add-to-list 'exordium-tapped-before-init-files exordium-before-init-file))

(when (file-readable-p exordium-prefs-file)
  (add-to-list 'exordium-tapped-prefs-files exordium-prefs-file))

(when (file-readable-p exordium-after-init-file)
  (add-to-list 'exordium-tapped-after-init-files exordium-after-init-file))

(when (file-readable-p exordium-custom-file)
  (add-to-list 'exordium-tapped-after-init-files exordium-custom-file))

;; Load before init files
(dolist (tapped-file exordium-tapped-before-init-files)
  (load tapped-file))


;;; Packages from Melpa
;; Use M-x `package-refresh-contents' to update the cache.
;; Use M-x `package-list-package' to load and display the list of packages,
;; then press I to mark for installation and X to execute (it's like dired).

;; Initialize the package system
(require 'package)

(add-to-list 'package-archives
             (cons "melpa" exordium-melpa-package-repo) t)
(add-to-list 'package-archives
             (cons "melpa-pinned" exordium-pinned-melpa-package-repo) t)
(add-to-list 'package-archives
             (cons "gnu" exordium-gnu-package-repo) t)

(setq package-user-dir (concat "~/.emacs.d/elpa-" emacs-version))

(package-initialize)

;; Load the packages we need if they are not installed already
(let ((package-pinned-packages (append
                                '((use-package             . "melpa-pinned")
                                  (diminish                . "melpa-pinned")
                                  (bind-key                . "melpa-pinned"))
                                exordium-extra-pinned))
      (has-refreshed nil))

  (defun update-package (p  has-refreshed)
    (unless (package-installed-p p)
      (unless has-refreshed
        (message "Refreshing package database...")
        (package-refresh-contents)
        (setq has-refreshed t)
        (message "Done."))
      (package-install p)))

  (dolist (pkg package-pinned-packages)
    (let ((p (car pkg)))
      (update-package p has-refreshed)))

  (dolist (pkg exordium-extra-packages)
    (update-package pkg has-refreshed)))


;;; Path for "require"

(add-to-list 'load-path exordium-modules-dir)

(defun add-directory-tree-to-load-path (dir &optional ignore-if-absent)
  "Add DIR and all its subdirs to the load path."
  (cond ((file-directory-p dir)
         (add-to-list 'load-path dir)
         (let ((default-directory dir))
           (normal-top-level-add-subdirs-to-load-path)))
        ((not ignore-if-absent)
         (warn "Missing directory: %s" dir))))

(add-directory-tree-to-load-path exordium-extensions-dir)
(add-directory-tree-to-load-path exordium-themes-dir)
(add-directory-tree-to-load-path exordium-local-dir t)

(setq custom-theme-directory exordium-themes-dir)


;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

;;; Load Modules
(use-package bytecomp :ensure nil)
(defun recompile-modules ()
  "Recompile modules for which the .elc is older than the .el, if
the .elc exists. Also discard .elc without corresponding .el"
  (interactive)
  (dolist (dir (list exordium-modules-dir
                     exordium-themes-dir
                     exordium-extensions-dir
                     exordium-local-dir))
    (when (file-directory-p dir)
      ;; Recompile
      (dolist (el (directory-files dir t "\\.el$"))
        (let ((elc (byte-compile-dest-file el)))
          (when (and (file-exists-p elc)
                     (file-newer-than-file-p el elc))
            (byte-compile-file el))))
      ;; Discard .elc singletons
      (dolist (elc (directory-files dir t "\\.elc$"))
        (let ((el (concat (concat (file-name-sans-extension elc) ".el"))))
          (unless (file-exists-p el)
            (warn "Removing singleton .elc file: %s" elc)
            (delete-file elc)))))))
(recompile-modules)

(use-package init-lib :ensure nil)         ; utility functions - load this first
(use-package init-environment :ensure nil) ; environment variables

;;; Local preferences (fonts, frame size etc.)
(use-package init-prefs :ensure nil)       ; defines variables that prefs.el can override
(dolist (tapped-file exordium-tapped-prefs-files)
  (load tapped-file))

;;; Themes
;;; Note: use "export TERM=xterm-256color" for emacs -nw
(use-package init-progress-bar :ensure nil)
(when exordium-nw
  (set-face-background 'highlight nil))
(use-package init-themes :ensure nil :if exordium-theme)

;;; Desktop
(when exordium-desktop
  (use-package init-desktop :ensure nil))

;;; Look and feel
(use-package init-look-and-feel :ensure nil)   ; fonts, UI, keybindings, saving files etc.
(use-package init-font-lock :ensure nil)       ; enables/disables font-lock globally.
(use-package init-linum :ensure nil)           ; line numbers
(use-package init-smooth-scroll
  :ensure nil
  :if exordium-smooth-scroll
  :config (smooth-scroll-mode 1)) ; smooth
                                                                                                       ; scroll

(update-progress-bar)

;;; Usability
(use-package init-window-manager :ensure nil)  ; navigate between windows
(use-package init-util :ensure nil)            ; utilities like match paren, bookmarks...
(use-package init-ido :ensure nil)             ; supercharged completion engine
(use-package init-highlight :ensure nil)       ; highlighting current line, symbol under point
(use-package init-autocomplete :ensure nil
  :if (eq exordium-complete-mode :auto-complete))
(use-package init-company :ensure nil
  :if (eq exordium-complete-mode :company))

(use-package init-helm-projectile :ensure nil
  :if exordium-helm-projectile)
(use-package init-helm :ensure nil)            ; setup helm

(use-package init-help :ensure nil
  :if exordium-help-extensions)

(update-progress-bar)

(use-package init-dired :ensure nil)           ; enable dired+ and wdired permission editing
(use-package init-git :ensure nil)             ; Magit and git gutter
(use-package init-git-visit-diffs :ensure nil) ; visit diffs in successive narrowed buffers
(use-package init-forge :ensure nil)           ; Forge
(use-package init-flb-mode :ensure nil)        ; frame-local buffers

(update-progress-bar)

;;; Prog mode
(use-package init-prog-mode :ensure nil)

;;; Shell mode
(use-package init-shell :ensure nil)

;;; Major modes
(use-package init-markdown :ensure nil)
(use-package init-org :ensure nil)
(use-package init-xml :ensure nil)

;;; OS-specific things
(use-package init-osx :ensure nil :if exordium-osx)

;;; C++
(use-package init-cpp :ensure nil)
(use-package init-bde-style :ensure nil)
(use-package init-yasnippet :ensure nil :if exordium-yasnippet)
(use-package init-gdb :ensure nil)


;;; RTags
(use-package init-rtags :ensure nil)
(when (and (eq exordium-complete-mode :auto-complete)
       exordium-rtags-auto-complete)
  (rtags-auto-complete))
(use-package init-rtags-helm :ensure nil)
(use-package init-rtags-cmake :ensure nil)
(use-package init-rtags-cdb :ensure nil)

;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1
(defun setup-flycheck-rtags ()
  (interactive)
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))


(use-package rtags
  :ensure t
  ;; defer loading after 1 second to speed startup
  :defer 1
  :hook (c++-mode . rtags-start-process-unless-running)
  :config
  (message "Loaded rtags")
  (setq rtags-autostart-diagnostics t)
  ;; use standard C-c r <key> keybindings
  (rtags-enable-standard-keybindings))

(setq rtags-display-result-backend 'helm)



(use-package company-rtags
  :ensure t
  ;; load immediately after rtags
  :after rtags
  :init
  (setq rtags-completions-enabled t)
  :config
  ;; add to company backends only after package is loaded
  (add-to-list 'company-backends 'company-rtags))

(use-package ivy-rtags
  :ensure t
  ;; package is loaded automatically by rtags, no need to load manually
  :defer t
  :init
  ;; use ivy autocompletion interface for displaying rtags suggestions
  (setq rtags-display-result-backend 'ivy))
(update-progress-bar)

;;; JS
(use-package init-javascript :ensure nil)

;;; Python
(use-package init-python :ensure nil)

;;; Ruby
(use-package init-ruby :ensure nil)

;;; Lisp
(use-package init-elisp :ensure nil)

;;; Clojure
(use-package init-clojure :ensure nil :if exordium-clojure)

;;; Groovy
(use-package init-groovy :ensure nil)

;;; include-what-you-use
(use-package init-iwyu :ensure nil)

(update-progress-bar)

;;; Local extensions
(dolist (tapped-file exordium-tapped-after-init-files)
  (load tapped-file))

(use-package init-powerline :ensure nil
  :if (and exordium-theme exordium-enable-powerline))

;; Docker
(use-package init-docker :ensure nil)

;;; LSP
(use-package init-lsp :ensure nil :if exordium-lsp-mode-enable)

(update-progress-bar)

;;; Greetings
(setq initial-scratch-message
      (let ((current-user (split-string (user-full-name) " ")))
        (format ";; Happy hacking %s!

" (if current-user (car current-user) exordium-current-user))))

;;; End of file

;;; Perkss Changes
(defconst savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq ring-bell-function 'ignore)

(setq large-file-warning-threshold 100000000)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)


;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
;; highlight the current line
(global-hl-line-mode +1)

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))



(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))


(use-package json-mode
  :ensure t)

(use-package ag
  :ensure t)

(use-package kibit-helper
  :ensure t)

(use-package cider-eval-sexp-fu
  :ensure t)

(use-package css-mode
  :mode "\\.css\\'")

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))


(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package diff-mode
  :commands diff-mode)

(use-package diffview
  :commands (diffview-current diffview-region diffview-message))


(use-package imenu-anywhere
  :ensure t
  :config
  (global-set-key (kbd "C-.") #'imenu-anywhere))

;;Cucumber
(use-package feature-mode
  :ensure t
  :config
  (setq feature-step-search-path "features/**/*steps.clj")
  (setq feature-step-search-gems-path "gems/ruby/*/gems/*/**/*steps.rb"))

(use-package ielm
  :ensure t
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char))
  :config
  (setq avy-background t))


(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode +1))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))


(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)


  (global-set-key (kbd "C-z") 'undo)
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-S-z") 'redo)
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (setq
   undo-tree-visualizer-diff t
   undo-tree-visualizer-timestamps t))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))


    ;; web-mode: An autonomous emacs major-mode for editing web templates.
;; http://web-mode.org/
(use-package web-mode
  :defer t
  :init
  (setq
   web-mode-code-indent-offset 2
   web-mode-comment-style 2
   web-mode-css-indent-offset 2
   web-mode-enable-current-element-highlight t
   web-mode-enable-current-column-highlight t
   web-mode-markup-indent-offset 2)
  :mode
  ("\\.erb\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  ("\\.tpl\\'" . web-mode))

(use-package swiper-helm
  :ensure t)



;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

(use-package flycheck-haskell
  :ensure t)


(use-package hardcore-mode
  :ensure t
  :init
  (setq too-hardcore-backspace t)
  (setq too-hardcore-return t)
  :config
  (global-hardcore-mode))


;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))


;; don't use tabs for indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)))


(use-package paren
  :config
  (show-paren-mode +1))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))


(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))


(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))








(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))


(use-package counsel-dash
  :bind ("C-c C-h" . counsel-dash))

(use-package counsel-gtags

  :disabled t
  :after counsel)


(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode)
  (define-key projectile-mode-map [remap projectile-ag]
    #'counsel-projectile-rg))

(use-package counsel-tramp
  :commands counsel-tramp)

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

;; Side project viewer
(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle))

(use-package highlight-symbol
  :ensure t
  :config
  (highlight-symbol-mode t)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))


;; Yaml mode support
(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'")

(use-package plantuml-mode
  :ensure t
  :mode "\\.plu\\'"
  :custom
  (plantuml-jar-path "~/plantuml/plantuml.jar")
  (plantuml-default-exec-mode 'jar))

(use-package flycheck-plantuml
  :ensure t
  :commands (flycheck-plantuml-setup)
  :init
  (with-eval-after-load 'flycheck
    (flycheck-plantuml-setup)))

;; Ruby
(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(use-package ruby-mode
  :config
  (add-hook 'ruby-mode-hook #'subword-mode))

(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))


(use-package rjsx-mode
  :ensure t)

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (setq company-tooltip-align-annotations t))

(use-package sql-indent
  :ensure t)

(use-package github-browse-file
  :ensure t)

;; Python
(use-package jedi
  :ensure t)

(use-package pytest
  :ensure t)

(use-package company-jedi
  :ensure t)


(update-progress-bar)
(show-paren-mode)

;; Send backups and auto saves to direcotey
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

 (setq backup-directory-alist
       `(("." . ,(concat user-emacs-directory "backups"))))


; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(setq cljr-inject-dependencies-at-jack-in nil)
;;; Typescript
(use-package typescript-mode
  :ensure t
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (setq tab-width 2)
              (setq typescript-indent-level tab-width)
              (setq evil-shift-width typescript-indent-level))))
(use-package spacemacs-common
  :ensure spacemacs-theme
  :init
  :config (load-theme 'spacemacs-dark t))

(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
            (lambda ()
              (when (locate-dominating-file "." ".clang-format")
                (clang-format-buffer))
              ;; Continue to save.
              nil)
            nil
            ;; Buffer local hook.
            t))

(update-progress-bar)

;; Run this for each mode you want to use the hook.
(add-hook 'c-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'c++-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'glsl-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))

(use-package cmake-project)

(defun maybe-cmake-project-mode ()
  (if (or (file-exists-p "CMakeLists.txt")
          (file-exists-p
             (expand-file-name "CMakeLists.txt" (car (project-roots (project-current))))))
      (cmake-project-mode)))

(add-hook 'c-mode-hook 'maybe-cmake-project-mode)
(add-hook 'c++-mode-hook 'maybe-cmake-project-mode)
