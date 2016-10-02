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
(add-to-list 'package-pinned-packages '(web-mode . "melpa-stable") t)

(when (not package-archive-contents)
  (package-refresh-contents))

(setq url-http-attempt-keepalives nil)

(defvar my-packages '(ace-jump-mode
                      ag
                      bundler
                      cider
                      clj-refactor
                      clojure-mode
                      coffee-mode
                      company
                      exec-path-from-shell
                      enh-ruby-mode
                      fiplr
                      flx-ido
                      fuzzy
                      haml-mode
                      highlight
                      highlight-symbol
                      inf-clojure
                      inf-ruby
                      jinja2-mode
                      json-mode
                      markdown-mode
                      neotree
                      popup
                      projectile
                      projectile-rails
                      restclient
                      robe
                      rspec-mode
                      rvm
                      sass-mode
                      scss-mode
                      starter-kit
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-js
                      starter-kit-lisp
                      starter-kit-ruby
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
(global-set-key [f8] 'neotree-toggle)

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
    ("774c80b518fbf8613d1a281c7624c021186c8cf24fc842b5c65461604ece9cfc" default)))
 '(fci-rule-color "#383838")
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
 '(vc-annotate-very-old-color "#dc8cc3")
 '(xterm-mouse-mode t))
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

(add-hook 'projectile-mode-hook 'projectile-rails-on)

(setq fiplr-ignored-globs '((directories (".git" ".svn" "tmp"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))

(global-set-key (kbd "C-x f") 'fiplr-find-file)

(rvm-use-default)

(require 'bundler)
(require 'rspec-mode)

(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode 'electric-pair-mode)

(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

(eval-after-load 'company
  '(push 'company-robe company-backends))


;; Set C-f to find-file
;; Emacs binding
(global-set-key (kbd "C-f") 'fiplr-find-file)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Backup autosave files to /tmp
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))
