;;; distinguished-theme.el --- A dark and elegant theme for emacs.

;; Copyright © 2014 Kim Silkebækken

;; Author: Kim Silkebækken <kim.silkebaekken@gmail.com>
;; URL: https://github.com/Lokaltog/distinguished-theme
;; Package-Version: 20151216.2015
;; Package-Commit: 9b1d25ac59465a5016d187ea84b7614c95a29b3b
;; Version: 0.0.1

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; A modified port of the distinguished theme for vim.

;;; Code:
(deftheme distinguished "Distinguished color theme")

(let ((dst-fg          "#ffffff")
      (dst-bg          "#080808")
      (dst-bg+0        "#1c1c1c")
      (dst-bg+1        "#303030")
      (dst-bg+2        "#444444")
      (dst-gray-2      "#585858")
      (dst-gray-1      "#6c6c6c")
      (dst-gray        "#767676")
      (dst-gray+1      "#bcbcbc")
      (dst-gray+2      "#d0d0d0")
      (dst-steel       "#8a8a8a")
      (dst-steel+1     "#9e9e9e")
      (dst-steel+2     "#c6c6c6")
      (dst-blue        "#d7875f")
      (dst-blue+1      "#5f8787" )
      (dst-blue+2      "#5f87ff" )
      (dst-green-2     "#005f00" )
      (dst-green-1     "#008700" )
      (dst-green       "#afaf5f")
      (dst-green+1     "#5f87af" )
      (dst-red-3       "#af0000")
      (dst-red-2       "#ff0000")
      (dst-red-1       "#d70000")
      (dst-red         "#d7af5f")
      (dst-red+1       "#d7875f")
      (dst-red+2       "#ff8700")
      (dst-yellow-2    "#8700ff")
      (dst-yellow-1    "#ffd700")
      (dst-yellow      "#ffff00")
      (dst-yellow+1    "#af875f")
      (dst-yellow+2    "#ffff87")
      (dst-intense-red "#ff5f00"))
  (custom-theme-set-faces
   'distinguished
   `(default ((t (:foreground ,dst-fg :background ,dst-bg))))
   `(cursor ((t (:foreground ,dst-bg :background ,dst-fg))))
   `(hl-line ((t (:background ,dst-bg+0))))
   `(minibuffer-prompt ((t (:foreground ,dst-green :weight bold))))
   `(region ((t (:background ,dst-bg+2))))
   `(secondary-selection ((t (:inherit hl-line))))
   `(fringe ((t (:foreground ,dst-gray :background ,dst-bg+0))))
   `(vertical-border ((t (:foreground ,dst-bg+2))))
   `(mode-line ((t (:foreground ,dst-gray+2 :background ,dst-bg+2 :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id ((t (:foreground ,dst-green :weight bold))))
   `(mode-line-inactive ((t (:foreground ,dst-gray :background ,dst-bg+1 :box (:line-width -1 :style released-button)))))
   `(button ((t (:foreground ,dst-blue+1 :underline t))))

   ;; font lock
   `(font-lock-builtin-face ((t (:foreground ,dst-yellow+1 :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,dst-gray, :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,dst-bg+2))))
   `(font-lock-doc-face ((t (:foreground ,dst-gray))))
   `(font-lock-constant-face ((t (:foreground ,dst-yellow+1 :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,dst-red :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,dst-blue :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,dst-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,dst-steel+1 :weight bold :slant italic))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,dst-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,dst-red :weight bold))))
   `(font-lock-string-face ((t (:foreground ,dst-green))))
   `(font-lock-type-face ((t (:foreground ,dst-green+1 :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,dst-blue+1 :weight normal :slant italic))))
   `(font-lock-warning-face ((t (:foreground ,dst-intense-red :weight bold))))

   ;; python-mode
   `(py-pseudo-keyword-face ((t (:foreground ,dst-blue+1 :weight bold))))
   `(py-object-reference-face ((t (:foreground ,dst-blue :weight bold :slant italic))))
   `(py-variable-name-face ((t (:foreground ,dst-blue+1 :weight normal :slant italic))))
   `(py-number-face ((t (:foreground ,dst-yellow-1 :weight normal :slant italic))))
   `(py-import-from-face ((t (:foreground ,dst-red))))
   `(py-def-class-face ((t (:foreground ,dst-blue :weight bold))))
   `(py-decorators-face ((t (:foreground ,dst-yellow :slant italic))))
   `(py-class-name-face ((t (:foreground ,dst-green+1 :weight bold))))
   `(py-exception-name-face ((t (:foreground ,dst-red :weight bold))))

   ;; js2-mode
   `(js2-external-variable ((t (:foreground ,dst-yellow+1 :weight normal :slant italic))))
   `(js2-function-param ((t (:foreground ,dst-red+1 :weight normal :slant italic))))

   ;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,dst-yellow-1 :weight normal :slant italic))))

   ;; basic whitespace-mode (tabs/newlines)
   `(whitespace-tab ((t (:foreground ,dst-bg+1 :background nil :weight normal))))
   `(whitespace-newline ((t (:foreground ,dst-red-3 :background nil :weight normal))))

   ;; show parens
   `(show-paren-mismatch ((t (:foreground ,dst-fg :background ,dst-red-1 :weight bold))))
   `(show-paren-match ((t (:foreground ,dst-fg :background ,dst-green-2 :weight bold))))

   ;; search highlight
   `(isearch ((t (:foreground ,dst-fg :background ,dst-green-2 :weight bold :slant normal :box (:line-width -1 :style released-button)))))
   `(isearch-fail ((t (:foreground ,dst-fg :background ,dst-red-1 :weight bold :slant normal :box (:line-width -1 :style released-button)))))
   `(lazy-highlight ((t (:foreground ,dst-bg :background ,dst-yellow+1 :weight bold :slant normal :box (:line-width -1 :style released-button)))))

   ;; rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,dst-yellow+2))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,dst-green))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,dst-red+1))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,dst-blue+1))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,dst-yellow+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,dst-green))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,dst-red+1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,dst-blue+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,dst-yellow+2))))

   ;; git-gutter
   `(git-gutter:added ((t (:foreground ,dst-green-1 :weight bold))))
   `(git-gutter:deleted ((t (:foreground ,dst-red-1 :weight bold))))
   `(git-gutter:modified ((t (:foreground ,dst-blue :weight bold))))
   `(git-gutter:unchanged ((t (:foreground ,dst-fg :weight bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,dst-green-1  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,dst-red-1 :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,dst-blue :weight bold))))

   ;; flx
   `(flx-highlight-face ((t (:foreground ,dst-red :weight bold :underline ,dst-red-2))))

   ;; flycheck
   `(flycheck-error
     ((((supports :underline (:style line)))
       (:underline (:style line :color ,dst-red-1) :inherit unspecified))
      (t (:foreground ,dst-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style line)))
       (:underline (:style line :color ,dst-yellow) :inherit unspecified))
      (t (:foreground ,dst-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style line)))
       (:underline (:style line :color ,dst-blue) :inherit unspecified))
      (t (:foreground ,dst-blue :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,dst-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,dst-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,dst-blue :weight bold))))

   ;; ace-jump
   `(ace-jump-face-background ((t (:foreground ,dst-gray-2 :background nil :weight normal :inverse-video nil))))
   `(ace-jump-face-foreground ((t (:foreground ,dst-bg :background ,dst-intense-red :weight bold :slant normal :inverse-video nil :box (:line-width -1 :style released-button)))))

   ;; auto-complete
   `(ac-candidate-face ((t (:foreground ,dst-gray :background ,dst-bg+1))))
   `(ac-completion-face ((t (:foreground ,dst-gray-1 :background nil :weight normal :slant normal :underline ,dst-bg+1))))
   `(ac-selection-face ((t (:foreground ,dst-fg :background ,dst-blue :weight bold))))
   `(popup-tip-face ((t (:foreground ,dst-bg :background ,dst-yellow))))
   `(popup-scroll-bar-foreground-face ((t (:background ,dst-gray+2))))
   `(popup-scroll-bar-background-face ((t (:background ,dst-bg+2))))
   `(popup-isearch-match ((t (:foreground ,dst-fg :background ,dst-bg))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,dst-fg :background ,dst-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,dst-gray :background ,dst-bg+1 :slant italic))))
   `(company-tooltip-selection ((t (:foreground ,dst-fg :background ,dst-blue :weight bold))))
   `(company-tooltip-mouse ((t (:background ,dst-bg+2))))
   `(company-tooltip-common ((t (:foreground ,dst-gray :background ,dst-bg+1 :weight bold))))
   `(company-tooltip-common-selection ((t (:foreground ,dst-blue+2 :background ,dst-blue :weight bold))))
   `(company-scrollbar-fg ((t (:background ,dst-gray))))
   `(company-scrollbar-bg ((t (:background ,dst-bg+2))))
   `(company-preview ((t (:background ,dst-blue))))
   `(company-preview-common ((t (:foreground ,dst-fg :background ,dst-yellow :weight bold))))

   ;; auto-dim-other-buffers
   `(auto-dim-other-buffers-face ((t (:foreground ,dst-gray+1 :background ,dst-bg+0))))

   ;; diff
   `(diff-added ((t (:foreground ,dst-green :background ,dst-green-2))))
   `(diff-changed ((t (:foreground ,dst-yellow-1 :background ,dst-yellow-2))))
   `(diff-file-header ((t (:background ,dst-green+1 :weight bold))))
   `(diff-function ((t (:background ,dst-gray-1))))
   `(diff-header ((t (:background ,dst-gray-1))))
   `(diff-hunk-header ((t (:background ,dst-gray-1))))
   `(diff-index ((t (:background ,dst-blue+1))))
   `(diff-indicator-added ((t (:foreground ,dst-green-1 :weight bold))))
   `(diff-indicator-changed ((t (:foreground ,dst-yellow-1 :weight bold))))
   `(diff-indicator-removed ((t (:foreground ,dst-red-1 :weight bold))))
   `(diff-refine-added ((t (:foreground ,dst-green :background
                                        ,dst-green-1 :weight bold))))
   `(diff-refine-change ((t (:foreground ,dst-yellow-1 :background
                                         ,dst-red+2 :weight bold))))
   `(diff-refine-removed ((t (:foreground ,dst-red-1 :background ,dst-red+1))))
   `(diff-removed ((t (:foreground ,dst-red-1))))
  ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'distinguished)
;;; distinguished-theme.el ends here
