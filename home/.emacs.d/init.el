;; PACKAGES
;;--------------------------------------------------

;; dir to store all extra extensions
(setq dotfiles-dir (file-name-directory
                (or (buffer-file-name) load-file-name)))
(setq tmp-dir (file-name-as-directory (concat dotfiles-dir "tmp")))
(make-directory tmp-dir t)

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                      ;;("marmalade" . "http://marmalade-repo.org/packages/")
                      ;;("melpa" . "http://melpa.milkbox.net/packages/")
                      ("melpa" . "https://melpa.org/packages/")
                      ;;("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                      ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(inf-clojure . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(web-mode . "melpa-stable") t)

(when (not package-archive-contents)
(package-refresh-contents))

(setq url-http-attempt-keepalives nil)

;; (setq debug-on-error t)

(defvar my-packages '(ace-jump-mode
                  bundler
                  cider
                  clj-refactor
                  clojure-mode
                  company
                  distinguished-theme
                  enh-ruby-mode
                  exec-path-from-shell
                  flycheck
                  flycheck-color-mode-line
                  flycheck-joker
                  fuzzy
                  helm
                  helm-ag
                  helm-projectile
                  highlight
                  highlight-symbol
                  idle-highlight-mode
                  inf-clojure
                  inf-ruby
                  jinja2-mode
                  json-mode
                  magit
                  markdown-mode
                  neotree
                  paredit
                  popup
                  projectile
                  projectile-rails
                  restclient
                  robe
                  rspec-mode
                  rvm
                  smex
                  solarized-theme
                  yaml-mode)
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
(menu-bar-mode -1)

(setq inhibit-splash-screen t)
(switch-to-buffer "*scratch*")
(delete-other-windows)

;; CODING STYLES
;;--------------------------------------------------

;; smooth-scrolling stops that annoying jump when moving around
(require 'smooth-scrolling)

;; makes sexps flash when you eval them!
(require 'highlight)

(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(require 'eval-sexp-fu)
(require 'nrepl-eval-sexp-fu)
(setq nrepl-eval-sexp-fu-flash-duration 0.5)

;; use inconsolata
(set-face-attribute 'default nil
                  :family "Inconsolata"
                  :height 160)

;; show line numbers
(global-linum-mode t)

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


(set-face-foreground 'region "white")
(set-face-background 'region "blue")

;; PROJECtile settings

(projectile-global-mode)
(setq projectile-project-root-files
    (quote
      ("rebar.config" "project.clj" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" ".git" ".projectile_root")))
(setq projectile-project-root-files-bottom-up (quote (".projectile" ".hg" ".fslckout" ".bzr" "_darcs")))
(setq projectile-file-exists-remote-cache-expire (* 10 60))

;; KEYBINDINGS
;;--------------------------------------------------

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key [f8] 'neotree-toggle)

(global-set-key (kbd "C-c p h") 'helm-projectile)
(global-set-key (kbd "C-c a g") 'helm-projectile-ag)

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
(add-hook 'cider-interaction-mode-hook 'eldoc-mode)
(add-hook 'cider-mode-hook (lambda ()
                            (eldoc-mode)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(cider-repl-use-pretty-printing t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (distinguished)))
 '(custom-safe-themes
   (quote
    ("0d456bc74e0ffa4bf5b69b0b54dac5104512c324199e96fc9f3a1db10dfa31f3" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "52b5da0a421b020e2d3429f1d4929089d18a56e8e43fe7470af2cea5a6c96443" "aae95fc700f9f7ff70efbc294fc7367376aa9456356ae36ec234751040ed9168" "14225e826195202fbc17dcf333b94d91deb6e6f5ca3f5a75357009754666822a" default)))
 '(fci-rule-color "#383838")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(magit-diff-use-overlays nil)
 '(neo-window-fixed-size nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (json-navigator go-mode evil-visual-mark-mode tox cider solarized-theme evil ag robe bundler projectile-rails fiplr distinguished-theme cider-decompile discover-clj-refactor ack-and-a-half slamhound clj-refactor align-cljlet jinja2-mode restclient git-messenger projectile flx-ido fuzzy popup ace-jump-mode yaml-mode exec-path-from-shell highlight-symbol markdown-mode coffee-mode company inf-clojure clojure-mode dockerfile-mode highlight starter-kit-eshell starter-kit-js starter-kit-ruby starter-kit-bindings starter-kit-lisp starter-kit)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(python-shell-interpreter "python3")
 '(rspec-use-spring-when-possible nil)
 '(ruby-align-chained-calls t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background "#2b2b2b")
 '(vc-annotate-background-mode nil)
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
 '(vc-annotate-very-old-color "#dc8cc3")
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
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

(rvm-use-default)

(add-hook 'projectile-mode-hook 'projectile-rails-on)

(projectile-rails-global-mode)

(require 'bundler)
(require 'rspec-mode)
(setq rspec-use-rvm t)

(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
(add-hook 'ruby-mode-hook 'robe-mode)

(add-hook 'enh-ruby-mode-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; (add-to-list 'auto-mode-alist
;;              '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-mode))

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

(eval-after-load 'company
'(push 'company-robe company-backends))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'prog-mode-hook
            (lambda()
              (idle-highlight-mode 1)))

;; Backup autosave files to /tmp
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (put-clojure-indent 'fact 'defun)
            (put-clojure-indent 'facts 'defun)
            (put-clojure-indent 'future-fact 'defun)
            (put-clojure-indent 'future-facts 'defun)))
(put 'upcase-region 'disabled nil)

;; Allow Emacs to use more system memory to avoid more frequent GC runs
(setq gc-cons-threshold 20000000)

;;(setq cider-default-cljs-repl 'shadow)
;; ClojureScript cider setup
;;(setq cider-cljs-lein-repl
;;      "(do (user/go)
;;           (user/cljs-repl))")
