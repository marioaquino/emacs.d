;;; package --- Summary
;;; Commentary:
;;; Code:

;; dir to store all extra extensions
(setq dotfiles-dir (file-name-directory
                (or (buffer-file-name) load-file-name)))
(setq tmp-dir (file-name-as-directory (concat dotfiles-dir "tmp")))
(make-directory tmp-dir t)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;;("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;;("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ;;("marmalade" . "https://marmalade-repo.org/packages/")
                         ))

(package-initialize)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(inf-clojure . "melpa-stable") t)

(setq url-http-attempt-keepalives nil)

;(setq debug-on-error t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(when (not package-archive-contents)
  (package-refresh-contents))

(when (memq window-system '(mac ns))
  (x-focus-frame nil)
  (exec-path-from-shell-initialize))

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
(global-linum-mode t)
(setq linum-format "%4d \u2502 ")

(show-paren-mode 1)

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
  :config
  (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package)

(use-package multiple-cursors
  :init
  (global-set-key (kbd "H-@") 'mc/edit-lines)
  (global-set-key (kbd "H->") 'mc/mark-next-like-this)
  (global-set-key (kbd "H-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "s-@") 'mc/mark-all-like-this-dwim))

(use-package company
  :init
  (global-company-mode)
  :config
  (global-set-key (kbd "<M-tab>") 'company-complete)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2))

;; (use-package distinguished-theme)

(use-package clojure-mode
  :pin melpa-stable

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

(use-package align-cljlet)

(use-package cider
  :pin melpa-stable
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

  ;; specify the print length to be 100 to stop infinite sequences
  ;; killing things.
  (setq cider-repl-print-length 100)

  :config
  (define-key cider-repl-mode-map (kbd "s-<up>") 'cider-repl-backward-input)
  (define-key cider-repl-mode-map (kbd "s-<down>") 'cider-repl-forward-input)

  (define-key cider-mode-map
    (kbd "C-c C-j") 'cider-find-dwim)

;  (evil-define-key
;    'normal cider-popup-buffer-mode-map
;    (kbd "q") 'quit-window)

;  (evil-define-key
;    'normal cider-docview-mode-map
;    (kbd "q") 'quit-window)

;  (evil-define-key
;    'normal cider-stacktrace-mode-map
;    (kbd "q") 'quit-window)

;  (evil-define-key
;    'normal cider-mode-map
;    (kbd ",e") '~/clojure/cider-eval-expression-at-point-in-repl)

;  (evil-define-key
;    'normal cider-mode-map
;    (kbd ",l") 'cider-load-file)

;  (evil-define-key
;    'normal cider-mode-map
;    (kbd ",d") 'cider-doc)

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

;; (use-package clj-refactor
;;   :after clojure-mode
;;   :config
;;   (add-hook 'clojure-mode-hook (lambda ()
;;                                  (clj-refactor-mode 1)
;;                                  (yas-minor-mode 1)
;;                                  ;; insert keybinding setup here
;;                                  (cljr-add-keybindings-with-prefix "C-c C-m")
;;                                  (dolist (mapping '(("async" . "clojure.core.async")
;;                                                     ("gen"   . "clojure.spec.gen.alpha")
;;                                                     ("json"  . "cheshire.core")
;;                                                     ("log"   . "clojure.tools.logging")
;;                                                     ("prop"  . "clojure.test.check.properties")
;;                                                     ("s"     . "clojure.spec.alpha")
;;                                                     ("stest" . "clojure.spec.test.alpha")
;;                                                     ("t"     . "clojure.test")
;;                                                     ("tc"    . "clojure.test.check.clojure-test")))
;;                                    (add-to-list 'cljr-magic-require-namespaces mapping t))))
;;   :catch (lambda (keyword err)
;;            (message (error-message-string err))))

;; (use-package evil
;;   :config

;;   ;; Enable evil mode but start Emacs in Emacs mode and allow change to evil w <Ctrl + z>
;;   (evil-mode t)
;;   (setq evil-default-state 'emacs)

;;   (defun ~/evil/backward-char-crosslines ()
;;     (interactive)
;;     (evil-backward-char 1 t))

;;   (defun ~/evil/forward-char-crosslines ()
;;     (interactive)
;;     (evil-forward-char 1 t))

;;   (define-key evil-motion-state-map
;;     (kbd "<left>") '~/evil/backward-char-crosslines)
;;   (define-key evil-motion-state-map
;;     (kbd "<right>") '~/evil/forward-char-crosslines)
;;   (define-key evil-motion-state-map
;;     (kbd "C-y") 'yank)
;;   (define-key evil-insert-state-map
;;     (kbd "C-k") 'kill-line)
;;   (define-key evil-insert-state-map
;;     (kbd "C-M-k") 'kill-word)
;;   (define-key evil-insert-state-map
;;     (kbd "C-y") 'yank))

;; (use-package evil-paredit)

;; (use-package evil-mc
;;   :config
;;   (global-set-key (kbd "H--") 'evil-mc-mode)
;;   (define-key evil-mc-key-map (kbd "C-g") 'evil-mc-undo-all-cursors))

(use-package ws-butler
  :init (ws-butler-global-mode 1))

(use-package magit
  :init
  (global-set-key (kbd "C-x g")   'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(use-package aggressive-indent)

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package json-mode)

(use-package markdown-mode)

(use-package yaml-mode)

(use-package highlight)

(use-package highlight-symbol
  :config
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))

(use-package idle-highlight-mode
  :config
  (set-face-foreground 'region "white")
  (set-face-background 'region "blue")
  :hook prog-mode)

(use-package neotree
  :config
  (global-set-key [f8] 'neotree-toggle))

(use-package cider-eval-sexp-fu
  :after cider)

(use-package projectile
  :init
  (setq projectile-project-root-files
        (quote
         ("rebar.config" "project.clj" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" ".git" ".projectile_root")))
  (setq projectile-project-root-files-bottom-up (quote (".projectile" ".hg" ".fslckout" ".bzr" "_darcs")))
  (setq projectile-file-exists-remote-cache-expire (* 10 60))
  :config
  (projectile-mode))

(use-package popup)

(use-package typescript-mode)

(use-package helm)

(use-package helm-projectile
  :after (helm projectile popup)
  :config
  (global-set-key (kbd "C-c p h") 'helm-projectile))

(use-package helm-ag
  :after (helm projectile popup)
  :init
  (setq helm-ag-use-agignore t)
  :config
  (global-set-key (kbd "C-c a g") 'helm-projectile-ag))

;(use-package helm-cider)

(use-package smex
  :config
  (require 'smex)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (set-face-attribute 'flycheck-error nil :underline '(:color "red2" :style wave)))

(use-package flycheck-color-mode-line
  :requires flycheck
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package flycheck-clj-kondo)
;; (use-package typescript)
;; (use-package tide
;;   :ensure t
;;   :after (typescript-mode company flycheck)
;;   :config (tide-setup)
;;   :hook ((typescript-mode . tide-hl-identifier-mode)
;;          (before-save . tide-format-before-save)))

(use-package paredit
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

  ;; (defun ~/paredit/define-evil-keys ()
  ;;   ;; Normal state
  ;;   (define-key evil-normal-state-local-map
  ;;     "W(" 'paredit-wrap-round)
  ;;   (define-key evil-normal-state-local-map
  ;;     "W[" 'paredit-wrap-square)
  ;;   (define-key evil-normal-state-local-map
  ;;     "W{" 'paredit-wrap-curly)
  ;;   (define-key evil-normal-state-local-map
  ;;     "W\"" '~/paredit/wrap-quote)
  ;;   (define-key evil-normal-state-local-map
  ;;     "(" 'paredit-backward-slurp-sexp)
  ;;   (define-key evil-normal-state-local-map
  ;;     ")" 'paredit-backward-barf-sexp)
  ;;   (define-key evil-normal-state-local-map
  ;;     "{" 'paredit-forward-barf-sexp)
  ;;   (define-key evil-normal-state-local-map
  ;;     "}" 'paredit-forward-slurp-sexp)
  ;;   (define-key evil-normal-state-local-map
  ;;     (kbd "C-S-r") 'paredit-raise-sexp)
  ;;   (define-key evil-normal-state-local-map
  ;;     "S" 'paredit-splice-sexp)
  ;;   (define-key evil-normal-state-local-map
  ;;     "s" 'paredit-split-sexp)
  ;;   (define-key evil-normal-state-local-map
  ;;     "T" '~/paredit/backward-transpose-sexps)
  ;;   (define-key evil-normal-state-local-map
  ;;     "t" '~/paredit/forward-transpose-sexps)
  ;;   (define-key evil-normal-state-local-map
  ;;     "Y" 'paredit-copy-as-kill)
  ;;   (define-key evil-normal-state-local-map
  ;;     "C" '~/paredit/forward-kill-and-insert)
  ;;   (define-key evil-normal-state-local-map
  ;;     "D" 'paredit-kill)
  ;;   ;; Insert state
  ;;   (define-key evil-insert-state-local-map
  ;;     (kbd "C-(") 'paredit-backward-slurp-sexp)
  ;;   (define-key evil-insert-state-local-map
  ;;     (kbd "C-)") 'paredit-backward-barf-sexp)

  ;;   (define-key evil-insert-state-local-map
  ;;     (kbd "C-k") 'paredit-kill)
  ;;   (define-key evil-insert-state-local-map
  ;;     (kbd "C-M-k") 'kill-sexp)
  ;;   (define-key evil-insert-state-local-map
  ;;     (kbd "C-y") 'yank))

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

;;
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

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
(global-set-key [mouse-4] '(lambda ()
                            (interactive)
                            (scroll-down 1)))
(global-set-key [mouse-5] '(lambda ()
                            (interactive)
                            (scroll-up 1)))

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


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'init)
;;; init.el ends here
