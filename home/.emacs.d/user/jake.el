;; -*- lexical-binding: t -*-


(install-package 'projectile)
(install-package 'flx-ido)
(projectile-global-mode)
(setq projectile-project-root-files
      (quote
       ("rebar.config" "project.clj" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" ".git")))
(setq projectile-project-root-files-bottom-up (quote (".projectile" ".hg" ".fslckout" ".bzr" "_darcs")))
(setq projectile-file-exists-remote-cache-expire (* 10 60))

(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

(install-package 'color-theme-sanityinc-tomorrow)
(setq user-specific-color-theme 'sanityinc-tomorrow-eighties)

(defun j/test-refresh ()
  (interactive)
  (find-file "~/src/jakemcc/lein-test-refresh/test-refresh/project.clj"))

(defun open-file-fn (file)
  (lambda ()
    (interactive)
    (find-file file)))

(defun create-project-shortcuts (prefix base)
  (dolist (elt (directory-files base))
    (let ((project (concat base "/" elt "/project.clj")))
      (when (file-exists-p project)
        (fset (intern (concat prefix elt)) (open-file-fn project))))))

(defun refresh-project-shortcuts ()
  (interactive)
  (create-project-shortcuts "j/" "~/src/jakemcc"))

(refresh-project-shortcuts)

(defun o/scratch ()
  (interactive)
  (find-file "~/Copy/outpace/ideas.org"))

(defun o/log ()
  (interactive)
  (find-file "~/Copy/outpace/log.org"))

(defun b/insert-clojure-code (arg)
  (interactive "p")
  (insert "``` clojure
```")
  (beginning-of-line)
  (open-line arg))

(defun b/insert-console-code (arg)
  (interactive "p")
  (insert "``` console
```")
  (beginning-of-line)
  (open-line arg))
