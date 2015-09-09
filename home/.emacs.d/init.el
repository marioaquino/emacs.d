;; PACKAGES
;;--------------------------------------------------

;; dir to store all extra extensions
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq tmp-dir (file-name-as-directory (concat dotfiles-dir "tmp")))
(make-directory tmp-dir t)

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))

(package-initialize)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(inf-clojure . "melpa-stable") t)

(when (not package-archive-contents)
  (package-refresh-contents))

(setq url-http-attempt-keepalives nil)

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-ruby
                      starter-kit-js
                      starter-kit-eshell
                      highlight
                      dockerfile-mode
                      clojure-mode
                      inf-clojure
                      company
                      clj-refactor
                      coffee-mode
                      markdown-mode
                      highlight-symbol
                      cider
                      exec-path-from-shell
                      yaml-mode
                      ace-jump-mode
                      popup
                      fuzzy
                      flx-ido
                      projectile
                      git-messenger
                      restclient
                      jinja2-mode)
  "A list of packages to ensure are installed at launch.")

(defun install-package (package)
  (when (not (package-installed-p package))
    (package-install package)))

(dolist (p my-packages)
  (install-package p))

;; ENVIRONMENT
;;--------------------------------------------------

(when (memq window-system '(mac ns))
  (x-focus-frame nil)
  (exec-path-from-shell-initialize))

(defun load-system-specific-configs (postfix)
  "Load system specific/user specific files if around"
  (setq system-specific-config (concat dotfiles-dir "user/" system-name postfix ".el")
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

(setq ispell-program-name "aspell")

;; CODING STYLES
;;--------------------------------------------------

;; smooth-scrolling stops that annoying jump when moving around
(require 'smooth-scrolling)

;; makes sexps flash when you eval them!
(require 'highlight)
(require 'eval-sexp-fu)
(require 'nrepl-eval-sexp-fu)
(setq nrepl-eval-sexp-fu-flash-duration 0.5)

;; use inconsolata
(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 160)

;; show line numbers
(global-linum-mode t)

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


(set-face-foreground 'region "white")
(set-face-background 'region "blue")

;; PROJECtile settings

(projectile-global-mode)
(setq projectile-project-root-files
      (quote
       ("rebar.config" "project.clj" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" ".git" ".projectile_root")))
(setq projectile-project-root-files-bottom-up (quote (".projectile" ".hg" ".fslckout" ".bzr" "_darcs")))
(setq projectile-file-exists-remote-cache-expire (* 10 60))

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)


;; KEYBINDINGS
;;--------------------------------------------------


;; steve yegges's suggested keybindings
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key [f5] 'call-last-kbd-macro)

;; find file in project
(require 'find-file-in-project)
(add-to-list 'ffip-patterns "*\.cljs")
(add-to-list 'ffip-patterns "*\.coffee")

;; slime and paredit
(defun fix-paredit-repl ()
  (interactive)
  (local-set-key "{" 'paredit-open-curly)
  (local-set-key "}" 'paredit-close-curly)
  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){")
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")["))

(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1) (fix-paredit-repl)))

(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))

(require 'hl-sexp)
(add-hook 'clojure-mode-hook (lambda () (hl-sexp-mode +1)))

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (yas/minor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-x")))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)


;; cider
(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook (lambda ()
                             (cider-turn-on-eldoc-mode)
                             (paredit-mode +1)
                             (fix-paredit-repl)
                             (local-set-key (kbd "C-c k") 'cider-refresh)))
(add-hook 'cider-mode-hook 'company-mode)

(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'company-mode)

(setq cider-repl-popup-stacktraces t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-wrap-history t)

;; specify the print length to be 100 to stop infinite sequences
;; killing things.
(setq cider-repl-print-length 100)

;; Company mode all over.
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "<M-tab>") 'company-complete)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 2)

;;
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; markdown
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; ace jump mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(global-auto-revert-mode 1)

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files."))

;; kibit
;; Teach compile the syntax of the kibit output
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist 'kibit)

;; A convenient command to run "lein kibit" in the project to which
;; the current emacs buffer belongs to.
(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))


(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

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

;; python jedi

;; need to have epc and jedi installed
;; (setq jedi:setup-keys t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'python-mode-hook 'auto-complete-mode)
;; (put 'upcase-region 'disabled nil)


;; tern
;; (add-to-list 'load-path "~/src/github/tern/emacs/")
;; (autoload 'tern-mode "tern.el" nil t)
;; (add-hook 'js-mode-hook (lambda () (tern-mode t)))



;; git-messenger
(require 'git-messenger)

(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory))

(defun scratch-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

(load-system-specific-configs "-after")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(custom-enabled-themes (quote (distinguished)))
 '(custom-safe-themes
   (quote
    ("14225e826195202fbc17dcf333b94d91deb6e6f5ca3f5a75357009754666822a" default)))
 '(fci-rule-color "#383838")
 '(package-selected-packages
   (quote
    (cider cider-decompile discover-clj-refactor ack-and-a-half slamhound clj-refactor align-cljlet jinja2-mode restclient git-messenger projectile flx-ido fuzzy popup ace-jump-mode yaml-mode exec-path-from-shell highlight-symbol markdown-mode coffee-mode company inf-clojure clojure-mode dockerfile-mode highlight starter-kit-eshell starter-kit-js starter-kit-ruby starter-kit-bindings starter-kit-lisp starter-kit)))
 '(vc-annotate-background "#2b2b2b")
 '(vc-annotate-color-map
   (quote
    ((20 . "#bc8383")
     (40 . "#cc9393")
     (60 . "#dfaf8f")
     (80 . "#d0bf8f")
     (100 . "#e0cf9f")
     (120 . "#f0dfaf")
     (140 . "#5f7f5f")
     (160 . "#7f9f7f")
     (180 . "#8fb28f")
     (200 . "#9fc59f")
     (220 . "#afd8af")
     (240 . "#bfebbf")
     (260 . "#93e0e3")
     (280 . "#6ca0a3")
     (300 . "#7cb8bb")
     (320 . "#8cd0d3")
     (340 . "#94bff3")
     (360 . "#dc8cc3"))))
 '(vc-annotate-very-old-color "#dc8cc3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Scrolling!!!!
(global-set-key [mouse-4] '(lambda ()
                             (interactive)
                             (scroll-down 1)))
(global-set-key [mouse-5] '(lambda ()
                             (interactive)
                             (scroll-up 1)))

;; Friendly scrolling in the terminal
(xterm-mouse-mode)
(setq magit-last-seen-setup-instructions "1.4.0")
