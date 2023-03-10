;;;; Projectile - see http://batsov.com/projectile/
;;;
;;; ----------------- -------------------------------------------------------
;;; Key               Definition
;;; ----------------- -------------------------------------------------------
;;; C-c p p           [Opt remap] Select project and open file with helm.
;;; C-c p f           [Opt remap] Open file with helm-projectile (current project).
;;; C-c p s g         Grep in project.
;;; ... and many more under C-c p
;;; C-c h             Open file with helm-projectile (current project).
;;; C-c H             Same but first select the project.
;;; or C-c M-h
;;; C-c e             treemacs: toggle the directory tree.
;;; C-c E             Open treemacs with a projectile project.
;;; C-S-a             Search with Ag: in current projectile project. (see also`init-helm.el')
;;; C-S-r             Search with ripgrep: in current projectile project. (see also`init-helm.el')

(require 'init-prefs)

(use-package helm-rg)

(use-package projectile
  :bind
  (:map projectile-command-map
        ("." . helm-projectile-find-file-dwim))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode))

(use-package helm-projectile
  :init
  (defun exordium-helm-projectile--exit-helm-and-do-ag ()
    "Exit helm and run ag on first selected candidate."
    (interactive)
    (if-let ((project (car (helm-marked-candidates))))
        (helm-run-after-exit #'helm-do-ag
                             project)
      (error "No candidates selected")))

  (defun exordium-helm-projectile--switch-project-and-do-rg (project)
    "Switch projct to PROJECT and run ripgrep there."
    (interactive)
    (let ((projectile-switch-project-action #'helm-projectile-rg))
      (projectile-switch-project-by-name project)))

  (defun exordium-helm-projectile--exit-helm-and-do-rg ()
    "Exit helm and switch project to first selected candidate and run rg there."
    (interactive)
    (if-let ((project (car (helm-marked-candidates))))
        (helm-run-after-exit #'exordium-helm-projectile--switch-project-and-do-rg
                             project)
      (error "No candidates selected")))

  :bind
  (:map global-map
        ("C-c h"   . #'helm-projectile)
        ("C-c H"   . #'helm-projectile-switch-project)
        ("C-c M-h" . #'helm-projectile-switch-project)
        ("C-S-a"   . #'helm-projectile-ag)
        ("C-S-r"   . #'helm-projectile-rg)
   :map helm-projectile-projects-map
        ("C-S-a" . #'exordium-helm-projectile--exit-helm-and-do-ag)
        ("C-S-r" . #'exordium-helm-projectile--exit-helm-and-do-rg))

  :config
  (helm-add-action-to-source "Silver Searcher (ag) in project `C-S-a'"
                             #'helm-do-ag
                             helm-source-projectile-projects)

  (helm-add-action-to-source "ripgrep (arg) in project `C-S-r'"
                             #'exordium-helm-projectile--switch-project-and-do-rg
                             helm-source-projectile-projects)
  (when exordium-helm-everywhere
    (helm-projectile-on)))

(use-package treemacs
  :ensure t
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

    (use-package treemacs-magit
    :after magit
    :autoload treemacs-magit--schedule-update
    :hook ((magit-post-commit
            git-commit-post-finish
            magit-post-stage
            magit-post-unstage)
           . treemacs-magit--schedule-update))

(use-package treemacs-projectile
  :bind
  (:map global-map
        ("C-c e" . #'treemacs)
        ("C-c E" . #'treemacs-projectile)))

;; Prevent Projectile from indexing the build directory.
(when exordium-rtags-cmake-build-dir
  (let ((top-level (car (split-string exordium-rtags-cmake-build-dir "/"))))
    ;; By default, top-level = "cmake.bld" (excluding the "<arch>")
    (when top-level
      (setq projectile-globally-ignored-directories
            (cons top-level projectile-globally-ignored-directories)))))

(provide 'init-helm-projectile)
