;;; package --- Summary
;;; Commentary:
;;; Code:

;; dir to store all extra extensions
(setq dotfiles-dir (file-name-directory
                (or (buffer-file-name) load-file-name)))
(setq tmp-dir (file-name-as-directory (concat dotfiles-dir "tmp")))
(make-directory tmp-dir t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package is installed and managed by straight.el
(straight-use-package 'use-package)

;; Use built-in project.el instead of straight's version (fixes Emacs 30+ conflict)
(straight-use-package '(project :type built-in))

(eval-when-compile
  (require 'use-package))

(setq url-http-attempt-keepalives nil)

;(setq debug-on-error t)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(when (memq window-system '(mac ns))
  (x-focus-frame nil)
  ;; (exec-path-from-shell-initialize)
  )

(defun load-system-specific-configs (postfix)
  "Load system specific/user specific files if around."
  (setq system-specific-config (concat dotfiles-dir "user/" (system-name) postfix ".el")
        user-specific-config (concat dotfiles-dir "user/" user-login-name postfix ".el")
        user-specific-dir (concat dotfiles-dir "user/" user-login-name postfix))
  (add-to-list 'load-path user-specific-dir)

  (if (file-exists-p system-specific-config) (load system-specific-config))
  (if (file-exists-p user-specific-config) (load user-specific-config))
  (if (file-exists-p user-specific-dir)
      (mapc #'load (directory-files user-specific-dir nil ".*el$"))))

(load-system-specific-configs "")

(setq vendor-dir (concat dotfiles-dir "/vendor"))
(add-to-list 'load-path vendor-dir)

;; (setq ispell-program-name "aspell")
(menu-bar-mode -1)

(setq inhibit-splash-screen t)
(switch-to-buffer "*scratch*")
(delete-other-windows)

(set-face-attribute 'default nil
                  :family "Inconsolata"
                  :height 160)

;; show line numbers
;;(global-linum-mode 1)
(global-display-line-numbers-mode)
(setq linum-format "%4d \u2502 ")

(show-paren-mode 1)
(electric-pair-mode 1)

;; tabs are 2 spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; color theme
(add-to-list 'custom-theme-load-path (concat dotfiles-dir "themes"))

;; In your own user.el file you can (setq user-specific-color-theme
;; 'other-theme-name) to load your own theme.
(if (boundp 'user-specific-color-theme)
  (load-theme user-specific-color-theme t)
  (load-theme 'zenburn t))

;; KEYBINDINGS
;;--------------------------------------------------

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key [f5] 'call-last-kbd-macro)

;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(use-package exec-path-from-shell
  :straight (:host github :repo "purcell/exec-path-from-shell" :files ("*.el"))
  :config
  (exec-path-from-shell-initialize))

(use-package multiple-cursors
  :straight (:host github :repo "magnars/multiple-cursors.el" :files ("*.el"))
  :init
  (global-set-key (kbd "H-@") 'mc/edit-lines)
  (global-set-key (kbd "H->") 'mc/mark-next-like-this)
  (global-set-key (kbd "H-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "s-@") 'mc/mark-all-like-this-dwim))

(use-package company
  :straight (:host github :repo "company-mode/company-mode" :files ("*.el"))
  :init
  (global-company-mode)
  :config
  (global-set-key (kbd "<M-tab>") 'company-complete)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2))

(use-package editorconfig
  :straight (:host github :repo "editorconfig/editorconfig-emacs" :files ("*.el"))
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package clojure-mode
  :straight (:host github :repo "clojure-emacs/clojure-mode" :files ("*.el"))
  ;; :pin melpa-stable

  :init
  (setq clojure-indent-style 'always-align)

  :after flycheck-clj-kondo

  :bind
  (("H-t H-f" . 'clojure-thread-first-all)
   ("H-t H-l" . 'clojure-thread-last-all)
   ("H-t H-u" . 'clojure-unwind-all))

  :config
  (require 'flycheck-clj-kondo)
  (defun ~/clojure/scratch ()
    "Create/retrieve a Clojure scratch buffer and switch to it"
    (interactive)
    (let ((buf (get-buffer-create "*clj-scratch*")))
      (switch-to-buffer buf)
      (clojure-mode)))

  (defun ~/clojure/string-name (s)
    (substring s 1 -1))

  (defun ~/clojure/keyword-name (s)
    (substring s 1))

  (defun ~/clojure/delete-and-extract-sexp ()
    (let* ((begin (point)))
      (forward-sexp)
      (let* ((result (buffer-substring-no-properties begin (point))))
        (delete-region begin (point))
        result)))

  (defun ~/clojure/toggle-keyword-string ()
    (interactive)
    (save-excursion
      (if (equal 1 (point))
          nil
        (cond
         ((equal "\"" (char-at-point))
          (insert ":" (~/clojure/string-name
                       (~/clojure/delete-and-extract-sexp))))
         ((equal ":" (char-at-point))
          (insert "\"" (~/clojure/keyword-name
                        (~/clojure/delete-and-extract-sexp)) "\""))
         (t (progn
              (backward-char)
              (~/clojure/toggle-keyword-string)))))))

  (add-hook 'clojure-mode-hook
            (lambda ()
              (paredit-mode +1)
              (turn-on-eldoc-mode)
              (put-clojure-indent 'fact 'defun)
              (put-clojure-indent 'facts 'defun)
              (put-clojure-indent 'future-fact 'defun)
              (put-clojure-indent 'future-facts 'defun)))

  (define-key clojure-mode-map
              (kbd "s-:")
              '~/clojure/toggle-keyword-string)

  (global-set-key (kbd "C-s-x") '~/clojure/scratch))

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil
      ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )


(use-package align-cljlet
  :straight (:host github :repo "gstamp/align-cljlet" :files ("*.el")))

(use-package cider
  :straight (:host github :repo "clojure-emacs/cider" :files ("lisp/*.el"))
  :init
  (setq cider-repl-history-size 10000)
  (setq cider-repl-history-file "~/.cider/history")
  (setq cider-prompt-for-symbol nil)

  (setq cider-repl-use-clojure-font-lock t)
  (setq nrepl-hide-special-buffers t)
  (setq cider-popup-stacktraces nil)
  (setq cider-repl-tab-command #'indent-for-tab-command)
  (setq cider-repl-result-prefix ";; => ")
  (setq cider-font-lock-dynamically '(macro core function var))
  (setq cider-test-show-report-on-success t)
  (setq cider-repl-popup-stacktraces t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-wrap-history t)
  (setq cider-test-default-exclude-selectors '("integration" "disabled"))

  ;; specify the print length to be 100 to stop infinite sequences
  ;; killing things.
  (setq cider-repl-print-length 100)

  :config
  (define-key cider-repl-mode-map (kbd "s-<up>") 'cider-repl-backward-input)
  (define-key cider-repl-mode-map (kbd "s-<down>") 'cider-repl-forward-input)

  (define-key cider-mode-map
    (kbd "C-c C-j") 'cider-find-dwim)

  (add-hook 'cider-mode-hook (lambda ()
                               (company-mode)
                               (eldoc-mode)
                               (paredit-mode +1)
                               (fix-paredit-repl)
                               (local-set-key (kbd "C-c k") 'cider-refresh)))
  (add-hook 'cider-repl-mode-hook (lambda ()
                                    (eldoc-mode)
                                    (paredit-mode +1)
                                    (company-mode)))
  (add-hook 'cider-interaction-mode-hook 'eldoc-mode)
  :catch (lambda (keyword err)
           (message (error-message-string err))))

(use-package direnv
  :straight (:host github :repo "wbolster/emacs-direnv" :files ("*.el"))
  :config
  (direnv-mode))

(use-package python
  :straight (:host gitlab :repo "python-mode-devs/python-mode")
  :hook (inferior-python-mode . fix-python-password-entry)
  :custom
  (python-shell-interpreter "jupyter-console")
  (python-shell-interpreter-args "--simple-prompt")
  (python-shell-prompt-detect-failure-warning nil)
  (python-shell-completion-native-disabled-interpreters
   '("pypy" "ipython" "jupyter" "jupyter-console"))
  (python-indent-offset 4)
  :config
  (require 'py-isort)
  (add-hook 'before-save-hook 'py-isort-before-save)
  (defun fix-python-password-entry ()
    (push
     'comint-watch-for-password-prompt comint-output-filter-functions))

  (defun my-setup-python (orig-fun &rest args)
    "Use corresponding kernel for current Pyenv version"
    (let* ((curr-python (car (split-string (shim-version) ":")))
           (python-shell-buffer-name (concat "Python-" curr-python))
           (python-shell-interpreter-args (if (bound-and-true-p djangonaut-mode)
                                              "shell_plus -- --simple-prompt"
                                            (concat "--simple-prompt --kernel=pyenv_" curr-python)))
           (python-shell-interpreter (if (bound-and-true-p djangonaut-mode)
                                         "django-admin"
                                       python-shell-interpreter)))
      (apply orig-fun args)))

  (advice-add 'python-shell-get-process-name :around #'my-setup-python)
  (advice-add 'python-shell-calculate-command :around #'my-setup-python))

;; Highlight and reformat docstrings in python
(use-package python-docstring
  :straight (:host github :repo "glyph/python-docstring-mode" :files ("*.el"))
  :hook (python-mode . python-docstring-mode))

(use-package poetry
  :straight (:host github :repo "cybniv/poetry.el" :files ("*.el")))

(use-package py-isort
  :straight (:host github :repo "paetzke/py-isort.el" :files ("*.el")))

(use-package python-black
  :straight (:host github :repo "wbolster/emacs-python-black" :files ("*.el"))
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package python-pytest
  :straight (:host github :repo "wbolster/emacs-python-pytest" :files ("*.el")))

(use-package python-coverage
  :straight (:host github :repo "wbolster/emacs-python-coverage" :files ("*.el"))
  :after python-pytest)

(use-package shim
  :straight (:host github :repo "twlz0ne/shim.el")
  :demand t
  :hook
  ((python-mode)
   . shim-mode)
  :config
  (shim-init-python)
  (shim-register-mode 'python 'python-ts-mode))

(use-package yasnippet
  :straight (:host github :repo "joaotavora/yasnippet" :files ("*.el"))
  :config (yas-global-mode 1))

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                         :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                         :build (:not compile))
  :init
  (global-lsp-bridge-mode))

(use-package lsp-pyright
  :straight (:host github :repo "emacs-lsp/lsp-pyright" :files ("*.el"))
  :ensure t
  :custom (lsp-pyright-langserver-command "basedpyright") ;; or pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(use-package org
  :straight (:host github :repo "emacsmirror/org")
  ;; :pin gnu
  :mode (("\\.org$" . org-mode))
  ;;:ensure org-plus-contrib
  :config
  (setq org-export-with-sub-superscripts nil)
  (progn
    ;; config stuff
    ))

(use-package simple-httpd
  :straight (:host github :repo "skeeto/emacs-web-server" :files ("*.el")))

(use-package websocket
  :straight (:host github :repo "ahyatt/emacs-websocket" :files ("*.el")))

(use-package jupyter
  :straight (:host github :repo "emacs-jupyter/jupyter" :files ("*.el"))
  :ensure t
  :bind (("C-c J R" . jupyter-run-repl)) ; Custom keybinding for running a REPL
  :config
  (setq jupyter-default-kernel "python3")
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (jupyter . t))))      ; Add jupyter to org-babel-load-languages
  ;; Other configurations as needed
  )

(use-package yaml-mode
  :straight (:host github :repo "yoshiki/yaml-mode" :files ("*.el"))
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package lsp-mode
  :straight (:host github :repo "emacs-lsp/lsp-mode")
  :ensure t
  :custom
  (lsp-pylsp-plugins-jedi-use-pyenv-environment t)
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode
          yaml-mode)
         . lsp)
  :config
  (setq lsp-enable-file-watchers nil)
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat
                  "/opt/homebrew/bin" path-separator
                  "/usr/local/bin" path-separator
                  (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  ;; Optional: In case `clojure-lsp` is not in your $PATH
                                        ;(setq lsp-clojure-server-command '("/path/to/clojure-lsp"))
  )

(use-package lsp-ui
  :straight (:host github :repo "emacs-lsp/lsp-ui" :files ("*.el"))
  :ensure t
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-enable nil))

(use-package org-modern
  :straight (:host github :repo "minad/org-modern" :files ("*.el"))
  :after org
  :init
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-agenda-tags-column 0)

  (setq org-ellipsis "…")
  ;;(set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  ;;(with-eval-after-load 'org (global-org-modern-mode))
  )

(use-package olivetti
  :straight (:host github :repo "rnkn/olivetti" :files ("*.el"))
  :init (add-hook 'org-mode-hook 'olivetti-mode)
  (setq-default olivetti-body-width 144))

(use-package plantuml-mode
  :straight (:host github :repo "skuro/plantuml-mode" :files ("*.el"))
  :after org
  :init
  ;;(setq plantuml-default-exec-mode 'jar)
  ;;(setq plantuml-jar-path "/Users/marioaqu/Downloads/plantuml-lgpl-1.2024.4.jar")
  (setq org-plantuml-jar-path "/Users/marioaqu/Downloads/plantuml-asl-1.2024.4.jar")
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)
                                                           (emacs-lisp . t)
                                                           (http . t))))

(use-package ws-butler
  :straight (:host github :repo "lewang/ws-butler")
  :ensure t
  :hook (prog-mode . ws-butler-mode))

(use-package magit
  :straight (:host github :repo "magit/magit")
  :init
  (global-set-key (kbd "C-x g")   'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(use-package aggressive-indent
  :straight (:host github :repo "Malabarba/aggressive-indent-mode" :files ("*.el")))

(use-package undo-tree
  :straight (:host gitlab :repo "tsc25/undo-tree" :files ("*.el"))
  :config
  (global-undo-tree-mode))

(use-package jsonian
  :straight (:host github :repo "iwahbe/jsonian" :files ("*.el"))
  :ensure nil
  :after so-long
  :config
  (jsonian-no-so-long-mode))

(use-package markdown-mode
  :straight (:host github :repo "jrblevin/markdown-mode")
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package highlight
  :straight (:host github :repo "emacsmirror/highlight" :files ("*.el")))

(use-package highlight-symbol
  :straight (:host github :repo "nschum/highlight-symbol.el" :files ("*.el"))
  :config
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))

(use-package idle-highlight-mode
  :straight (:host codeberg :repo "ideasman42/emacs-idle-highlight-mode" :files ("*.el"))
  :config
  (set-face-foreground 'region "white")
  (set-face-background 'region "blue")
  :hook prog-mode)

(use-package cider-eval-sexp-fu
  :straight (:host github :repo "clojure-emacs/cider-eval-sexp-fu" :files ("*.el"))
  :after cider)

(use-package projectile
  :straight (:host github :repo "bbatsov/projectile" :files ("*.el"))
  :ensure t
  :init
  (setq projectile-project-root-files
        (quote
         ("rebar.config" "project.clj" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" ".git" ".projectile_root" "pyproject.toml")))
  (setq projectile-project-root-files-bottom-up (quote (".projectile" ".hg" ".fslckout" ".bzr" "_darcs")))
  (setq projectile-file-exists-remote-cache-expire (* 10 60))
  (setq projectile-globally-ignored-file-suffixes '(".bak" ".tmp"))
  (setq projectile-globally-ignored-directories '("-/tmp"))
  :config
  (projectile-mode)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map) ; Binds C-c p to the Projectile command map
              ("s-p" . projectile-command-map))) ; Binds s-p (Super-p) to the Projectile command map

(use-package popup
  :straight (:host github :repo "auto-complete/popup-el" :files ("*.el")))

(use-package typescript-mode
  :straight (:host github :repo "emacs-typescript/typescript.el" :files ("*.el")))

(use-package helm
  :straight (:host github :repo "emacs-helm/helm" :files ("*.el")))

(use-package helm-projectile
  :straight (:host github :repo "bbatsov/helm-projectile" :files ("*.el"))
  :after (helm projectile popup)
  :config
  (global-set-key (kbd "C-c p h") 'helm-projectile))

(use-package helm-ag
  :straight (:host github :repo "emacsattic/helm-ag" :files ("*.el"))
  :after (helm projectile popup)
  :init
  (setq helm-ag-use-agignore t)
  :config
  (global-set-key (kbd "C-c a g") 'helm-projectile-ag))

;(use-package helm-cider)

(use-package smex
  :straight (:host github :repo "nonsequitur/smex" :files ("*.el"))
  :config
  (require 'smex)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(use-package flycheck
  :straight (:host github :repo "flycheck/flycheck" :files ("*.el"))
  :init
  (global-flycheck-mode)
  :config
  (set-face-attribute 'flycheck-error nil :underline '(:color "red2" :style wave)))

(use-package flycheck-color-mode-line
  :straight (:host github :repo "flycheck/flycheck-color-mode-line" :files ("*.el"))
  :requires flycheck
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package flycheck-clj-kondo
  :straight (:host github :repo "borkdude/flycheck-clj-kondo" :files ("*.el")))
;; (use-package typescript)
;; (use-package tide
;;   :ensure t
;;   :after (typescript-mode company flycheck)
;;   :config (tide-setup)
;;   :hook ((typescript-mode . tide-hl-identifier-mode)
;;          (before-save . tide-format-before-save)))

(use-package paredit
  :straight (:host github :repo "emacsmirror/paredit" :files ("*.el"))
  :config
  (defun ~/paredit/wrap-quote ()
    "Wrap the following sexp in double quotes."
    (interactive)
    (save-excursion
      (insert "\"")
      (forward-sexp)
      (insert "\"")))

  (defun ~/paredit/forward-transpose-sexps ()
    (interactive)
    (paredit-forward)
    (transpose-sexps 1)
    (paredit-backward))

  (defun ~/paredit/backward-transpose-sexps ()
    (interactive)
    (transpose-sexps 1)
    (paredit-backward)
    (paredit-backward))

  (defun ~/paredit/forward-kill-and-insert ()
    (interactive)
    (paredit-kill)
    ;; (evil-insert-state)
    )

  (defun ~/paredit-mode ()
    (paredit-mode t)
    ;; (~/paredit/define-evil-keys)
    )

  (defun paredit-wrap-round-from-behind ()
    (interactive)
    (save-excursion  (forward-sexp -1)
                     (paredit-wrap-round)))

  (defun paredit-wrap-square-from-behind ()
    (interactive)
    (save-excursion (forward-sexp -1)
                    (paredit-wrap-square)))

  (defun paredit-wrap-curly-from-behind ()
    (interactive)
    (save-excursion (forward-sexp -1)
                    (paredit-wrap-curly)))

  (define-key paredit-mode-map (kbd "s-s")       'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "s-b")       'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-<right>") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-<left>")  'paredit-forward-barf-sexp)

  (define-key paredit-mode-map (kbd "M-(")       'paredit-wrap-round)
  (define-key paredit-mode-map (kbd "M-)")       'paredit-wrap-round-from-behind)
  (define-key paredit-mode-map (kbd "s-[")       'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "s-]")       'paredit-wrap-square-from-behind)
  (define-key paredit-mode-map (kbd "s-{")       'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "s-}")       'paredit-wrap-curly-from-behind)

  (global-set-key (kbd "H-p")  'paredit-mode)

  (defun turn-on-paredit () (~/paredit-mode))

  (add-hook 'emacs-lisp-mode-hook       'turn-on-paredit)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-paredit)
  (add-hook 'lisp-mode-hook             'turn-on-paredit)
  (add-hook 'slime-repl-mode-hook       'turn-on-paredit)
  (add-hook 'clojure-mode-hook          'turn-on-paredit)
  (add-hook 'cider-repl-mode-hook       'turn-on-paredit)

  ;; Stop SLIME's REPL from grabbing DEL,
  ;; which is annoying when backspacing over a '('
  (defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))

  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit))

(add-hook 'slime-repl-mode-hook (lambda ()
                                  (paredit-mode +1)
                                  (fix-paredit-repl)))

;; (use-package copilot
;;   :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-M-o" . 'copilot-accept-completion-by-word)
;;               ("C-M-l" . 'copilot-accept-completion-by-line)
;;               ("C-M-p" . 'copilot-accept-completion-by-paragraph)
;;               ("C-n" . 'copilot-next-completion)
;;               ("C-p" . 'copilot-previous-completion))

;;   :config
;;   (add-to-list 'copilot-indentation-alist '(prog-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(org-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(text-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(closure-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(clojure-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))


(add-to-list 'load-path "~/projects/emacs-libvterm/")
(require 'vterm)

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(use-package rustic
  :straight (:host github :repo "emacs-rustic/rustic" :files ("*.el"))
  :init (setq lsp-rust-analyzer-cargo-cfgs [])
  :config
  (setq lsp-document-sync-method 'full)
  (setq lsp-enable-on-type-formatting nil)
  :ensure t)

;; (setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; markdown
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(global-auto-revert-mode 1)

;; Setting to prevent prompting before loading clj files
(setq enable-local-variables :safe)

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files."))


;; rename file and buffer
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))

(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory))

(defun scratch-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

(load-system-specific-configs "-after")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Scrolling!!!!
(global-set-key (kbd "<wheel-down>") 'scroll-up-line)
(global-set-key (kbd "<wheel-up>") 'scroll-down-line)

;; Friendly scrolling in the terminal
(xterm-mouse-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Backup autosave files to /tmp
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))


(put 'upcase-region 'disabled nil)

;; Allow Emacs to use more system memory to avoid more frequent GC runs
(setq gc-cons-threshold 20000000)

(with-eval-after-load 'dired
  (require 'dired-x)
  ;; Set dired-x global variables here.  For example:
  ;; (setq dired-guess-shell-gnutar "gtar")
  ;; (setq dired-x-hands-off-my-keys nil)

  (setq dired-omit-files
        (concat dired-omit-files "^\\..+\\.\\~undo\\-tree\\~$"))
  )
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (dired-omit-mode 1)
            ))



;; slime and paredit
(defun fix-paredit-repl ()
  (interactive)
  (local-set-key "{" 'paredit-open-curly)
  (local-set-key "}" 'paredit-close-curly)
  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){")
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")["))


;;
;; change orientation of split (toggle)
;;
(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((current (current-buffer))
        (split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer current)))

;;
;; swap split buffers and switch focus to opposite (which is now in current pane)
;;
(defun reverse-arrangement ()
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only reverse a frame split in two"))
  (let ((current (current-buffer))
        (opposite (window-buffer (next-window))))
    (set-window-buffer (selected-window) opposite)
    (set-window-buffer (next-window) current)
    (switch-to-buffer opposite)))

;; (global-set-key (kbd "s-r") 'toggle-frame-split)
;; (global-set-key (kbd "s-R") 'reverse-arrangement)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages '((monet :url "https://github.com/stevemolitor/monet"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'init)
;;; init.el ends here
