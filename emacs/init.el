;;======================================================================
;; Configuration file to Emacs (>=25.2.2) by Vinicius Riffel.
;;
;; This file is hosted at https://github.com/vriffel/emacs.
;;
;; Almost all the content available here was obtained/inspired by
;; Walmes Zeviani's Emacs settings. (https://github.com/walmes). Please,
;; send questions, problems and/or suggestions as an issue on GitHub
;; project of this file.
;;======================================================================

;; Load Fernando Mayer's functions (see https://github.com/fernadomayer)
(load "~/.emacs.d/lisp/funcs.el")

(require 'package)
;; Any add to list for package-archives (to add marmalade or melpa) goes here
(add-to-list 'package-archives 
    '("MELPA" .
      "http://melpa.milkbox.net/packages/"))
(package-initialize)

(setq byte-compile-warnings '(cl-functions))

;; Fix bug see https://emacs.stackexchange.com/questions/52171/trying-to-install-auctex-via-package-install
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Intialize fullscreen
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(tool-bar-mode 0)                   ;; Remove tool bar
(menu-bar-mode 0)                   ;; Remove menu bar
(scroll-bar-mode -1)                ;; Remove scroll bar
(global-hl-line-mode 1)             ;; Highlight the cursor line.
(visual-line-mode 1)                ;; Screen lines, not logical lines.
(show-paren-mode 1)                 ;; Highlight matching pairs.
(delete-selection-mode 1)           ;; Allows delete region.
(setq column-number-mode t)         ;; Show cursor position.
(setq auto-save-default nil)        ;; Turn off #autosave#.
(setq make-backup-files nil)        ;; Turn off backup~.
(setq comment-empty-lines t)        ;; Comment even in empty lines.
(setq x-select-enable-clipboard t)  ;; Allow shared transfer area.
(setq-default indent-tabs-mode nil) ;; Spaces to indent.
(setq-default fill-column 72)       ;; Column width.

;; Highlight whitespace.
(setq whitespace-line-column fill-column)
(setq whitespace-style
      '(face lines-tail trailing tabs empty))
(global-whitespace-mode +1)

;; Adds font
;; (cond
;;  ((find-font (font-spec :name "Inconsolata"))
;;   (set-default-font "Inconsolata-14"))
;;  ((find-font (font-spec :name "Noto Sans Mono"))
;;   (set-default-font "Noto Sans Mono-14"))
;;  (t
;;   (set-default-font "Ubuntu Mono-14")))

;; (set-default-font "Noto Sans Mono")
;; (set-default-font "Inconsolata")
;; (set-default-font "Ubuntu Mono-14")
(set-frame-font "Inconsolata 14" nil t)


;; Adds IDO and smex
(ido-mode t)
;(global-set-key (kbd "M-x") 'smex)

;; C-page down e C-page up to move along buffers.
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)


;; C-z to 'undo, the default is C-/.
(global-unset-key "\C-z")
(global-set-key "\C-z" 'undo)

;; M-. to (un)comment paragraph.
(global-set-key [?\M-.] (kbd "M-h M-; M-}"))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t))

(package-initialize)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
;; (package-initialize)

;; Use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-verbose t)
(setq use-package-always-ensure nil)

(require 'use-package)

;; Set some configs to company
(setq company-idle-delay             0.2
      company-minimum-prefix-length  2)

;; Load themes
;; (require 'color-theme-sanityinc-tomorrow)
(use-package ample-theme
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t)
               (enable-theme 'ample-flat))
  :defer t
  :ensure t)

;;----------------------------------------------------------------------
;; Markdown extensions.
;; (IT MUST BE BEFORE LATEX EXTENSIONS.)

;; (when (not (package-installed-p 'markdown-mode))
;;   (package-install 'markdown-mode))

(use-package imenu-list
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil))

(use-package markdown-mode
  :ensure nil
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (progn
    (add-hook 'markdown-mode-hook 'turn-on-orgstruct)
    (add-hook 'markdown-mode-hook 'turn-on-orgstruct++)
    (add-hook 'markdown-mode-hook 'imenu-add-menubar-index)
    (setq imenu-auto-rescan t)
    (require 'imenu-list)
    (setq imenu-list-focus-after-activation t
          imenu-list-auto-resize nil)
    (add-hook 'markdown-mode-hook
              '(lambda()
                 (global-set-key (kbd "<f10>")
                                 'imenu-list-smart-toggle)))
    )
  )

;;----------------------------------------------------------------------
;; R+MarkDown extensions (emacs >= 24.3.1).
;; (IT MUST BE BEFORE LATEX EXTENSIONS.)

;; (when (not (package-installed-p 'polymode))
;;   (package-install 'polymode))

;; Based on:
;; https://github.com/SteveLane/dot-emacs/blob/master/packages-polymode.el

(use-package polymode
  ;; :ensure markdown-mode
  ;; :ensure poly-R
  ;; :ensure poly-noweb
  ;; :bind
  ;; (("S-<f7>" . polymode-next-chunk-same-type)
  ;;  ("S-<f8>" . polymode-previous-chunk-same-type))
  :mode
  (("\\.Rnw\\'" . poly-noweb+r-mode)
   ("\\.Rmd\\'" . poly-markdown+r-mode))
  )

(use-package poly-markdown
  ;; :ensure polymode
  :defer t)

(use-package poly-R
  ;; :ensure polymode
  ;; :ensure poly-markdown
  ;; :ensure poly-noweb
  :defer t)

(use-package yaml-mode
  :ensure nil
  :mode (("\\.ya?ml\\'" . yaml-mode)
         ("\\.toml\\'"  . yaml-mode)))

;;--------------------------------------------------------------------
;; Emacs Speaks Statistics
(use-package ess
  :ensure nil
  ;; :ensure ess-site
  ;; :ensure ess-view
  :init
  (progn
    (require 'ess-site)
    (require 'ess-view))
  :bind
  (("C-S-<f5>" . ess-eval-chunk)
   ("C-S-<f6>" . ess-eval-chunk-and-step)
   ("C-S-<f7>" . ess-noweb-next-code-chunk)
   ("C-S-<f8>" . ess-noweb-previous-code-chunk)
   ("C-S-<f9>" . ess-noweb-goto-chunk))
  ;; ;; Movement across chunks in Rnw files.
  ;; (global-set-key (kbd "C-S-<f5>") 'ess-eval-chunk)
  ;; (global-set-key (kbd "C-S-<f6>") 'ess-eval-chunk-and-step)
  ;; (global-set-key (kbd "C-S-<f7>") 'ess-noweb-next-code-chunk)
  ;; (global-set-key (kbd "C-S-<f8>") 'ess-noweb-previous-code-chunk)
  ;; (global-set-key (kbd "C-S-<f9>") 'ess-noweb-goto-chunk)
  :config
  (setq-default ess-dialect "R")
  (setq ess-indent-with-fancy-comments nil)
  (setq-default inferior-R-args "--no-restore-history --no-save ")
  (setq ess-view--spreadsheet-program "gnumeric")
  ;; Script and console font lock highlight.
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)))
  (setq inferior-R-font-lock-keywords
        '((ess-S-fl-keyword:prompt . t)
          (ess-R-fl-keyword:messages . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:matrix-labels . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)))
  (add-hook
   'ess-mode-hook
   '(lambda()
      ;;-------------------------------------
      (ess-toggle-underscore nil)
      (define-key ess-mode-map [?\M--]
        'ess-cycle-assign) ;; `Alt + -'  to cycle `<- | <<- | = ...'.
      ;;-------------------------------------
      (company-mode 1)                               ;; (company-mode -1)
       (ess-smart-equals-mode -1)
      ;;-------------------------------------
      (define-key ess-mode-map [f5] 'company-R-args) ;; F5 do show ARGS.
      (setq-local comment-add 1)                     ;; Double ## as default.
      (setq ess-smart-operators t)                   ;; Smart comma.
      ;; (setq ess-indent-with-fancy-comments nil)      ;; No indent levels.
      ;; (setq ess-smart-equals-no-space t)
      (setq comint-scroll-to-bottom-on-input t)
      (setq comint-scroll-to-bottom-on-output t)
      (setq comint-move-point-for-output t)
      )
   )
  ;;-----------------------------------------
  (defadvice ess-eval-buffer (before really-eval-buffer compile activate)
    "Prevent call ess-eval-buffer by accident, frequently by
     hitting C-c C-b instead of C-c C-n."
    (if (yes-or-no-p
         (format "Are you sure you want to evaluate the %s buffer?"
                 buffer-file-name))
        (message "ess-eval-buffer started.")
      (error "ess-eval-buffer canceled!")))
  )

;; Fix ess problem see https://github.com/emacs-ess/ESS/issues/725
;; (ess-r-package-mode -1)
;; (setq ess-r-set-evaluation-env nil)

;; Smart equals
;; (use-package ess-smart-equals
;;   :init   (setq ess-smart-equals-extra-ops '(brace paren percent))
;;   :after  (:any ess-r-mode inferior-ess-r-mode ess-r-transcript-mode)
;;   :config (ess-smart-equals-activate))

;; Smart Parens
;; (use-package smartparens
;;   :ensure nil
;;   :diminish smartparens-mode
;;   :config
;;   (progn
;;     (require 'smartparens-config)
;;     (smartparens-global-mode 1)
;;     (sp-pair "\"" nil :unless '(sp-point-after-word-p))
;;     (sp-pair "'" nil :unless '(sp-point-after-word-p))
;;     )
;;   )

;;----------------------------------------------------------------------
;; Latex extensions.

(use-package auctex
  :ensure nil
  :defer nil
  :mode
  (("\\.pgf\\'" . latex-mode)
   ("\\.pgs\\'" . latex-mode))
  :config
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#000000"))
 '(custom-enabled-themes (quote (ample-flat)))
 '(custom-safe-themes
   (quote
    ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(fci-rule-color "#2a2a2a")
 '(package-selected-packages
   (quote
    (ample-zen-theme yaml-mode poly-R poly-markdown polymode markdown-mode imenu-list smex color-theme-sanityinc-tomorrow auctex smartparens company ess-view ess-smart-underscore ess-smart-equals ess)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;----------------------------------------------------------------------
;; TMB

(load "~/.emacs.d/lisp/tmb.el")

(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 73) ; chars
              (height . 50) ; lines
              (left . 50)
              (top . 50)))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 73)
              (height . 50)
              (left . 50)
              (top . 50))))
  (progn
    (setq initial-frame-alist '( (tool-bar-lines . 0)))
    (setq default-frame-alist '( (tool-bar-lines . 0)))))
