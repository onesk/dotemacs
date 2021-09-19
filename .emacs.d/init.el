;; ELPA / MELPA setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; use-package
(setq package-archive-priorities
      '(("melpa" . 20)
	("gnu" . 10)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

;; binding & modeline
(use-package diminish)
(use-package bind-key)

;; no visual clutter
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; org-mode by default
(setq-default major-mode 'org-mode)

;; shorter predicate
(fset 'yes-or-no-p 'y-or-n-p)

;; redirect backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; winner mode
(global-set-key (kbd "C-c p") 'winner-undo)
(global-set-key (kbd "C-c n") 'winner-redo)

(winner-mode 1)

;; color theming
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; colunm numbers
(setq column-number-mode t)

;; default indentation
(setq-default default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)

;; Steve Yegge effective-emacs
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

;; Remap backspace to C-h
(global-set-key "\C-h" 'backward-delete-char-untabify)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key [(super h)] 'help-command)

;; ace-jump-mode
(use-package ace-jump-mode
  :bind
  (("C-c <SPC>" . ace-jump-mode)
   ("C-c M-<SPC>" . ace-jump-mode-pop-mark)))

;; Magit-status
(global-set-key (kbd "C-x g s") 'magit-status)

;; ivy/counsel/swiper
(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x C-g" . counsel-rg))
  :config (counsel-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package ivy
  :defer 0.1
  :diminish
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

;; company
(require 'company)
(setq company-idle-delay nil)

(defun complete-or-indent ()
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-according-to-mode)))

(global-set-key (kbd "C-M-/") 'complete-or-indent)

;;

(set-default 'truncate-lines t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; cc-mode indentation & layout
(defun my-c-mode-hook () (local-set-key (kbd "C-c o") 'ff-find-other-file))

(defun my-c++-mode-hook () (setq c-basic-offset 4
                                 c-default-style "linux"
                                 indent-tabs-mode nil
                                 tab-width 4))

(add-hook 'c-mode-hook 'my-c-mode-hook)

(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; Markdown mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; toml-mode
(require 'toml-mode)
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

;; Rust
(use-package flycheck)
(use-package rustic)

(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

(setq rustic-lsp-client 'eglot)
(setq rustic-lsp-server 'rust-analyzer)

(setq lsp-rust-analyzer-display-chaining-hints t)
(setq lsp-rust-analyzer-display-parameter-hints nil)
(setq lsp-rust-analyzer-macro-expansion-method 'rustic-analyzer-macro-expand)
(setq lsp-rust-analyzer-server-command '("~/.local/bin/rust-analyzer"))
(setq lsp-rust-analyzer-server-display-inlay-hints nil)
(setq lsp-rust-full-docs t)
(setq lsp-rust-server 'rust-analyzer)
(setq lsp-ui-doc-alignment 'window)
(setq lsp-ui-doc-position 'top)
(setq lsp-ui-sideline-enable nil)

;; ace-window
(require 'ace-window)
(setq aw-keys '(?a ?e ?u ?d ?h ?t ?s))
(setq aw-dispatch-always t)

;; Key chords
(defun sachachua/key-chord-define (keymap keys command)
  (if (/= 2 (length keys))
      (error "Key-chord keys must have two elements"))
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (define-key keymap (vector 'key-chord key1 key2) command)))
(fset 'key-chord-define 'sachachua/key-chord-define)

(use-package key-chord
  :init
  (progn
    (fset 'key-chord-define 'sachachua/key-chord-define)
    (setq key-chord-one-key-delay 0.2)
    (key-chord-mode 1)
    (key-chord-define-global "uu" 'undo)
    (key-chord-define-global "yy" 'ace-window)
    (key-chord-define-global "zz" 'my/quadrants)))

;; Electric pairs always on
(electric-pair-mode t)

;; use Hack as default font
(set-frame-font "Hack")

;; Org mode
(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c c") 'org-capture)

(setq-default major-mode 'org-mode)

;; Dockerfile mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Allow disabling ivy completion on C-x C-f with universal argument
;; (for TRAMP use, for example)

(defun find-file-disable-ivy (uniarg)
  (interactive "P")
  (let ((completing-read-function (if uniarg
                                      'completing-read-default
                                       completing-read-function)))
    (call-interactively 'find-file)))

(global-set-key (kbd "C-x C-f") 'find-file-disable-ivy)

;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("14f0fbf6f7851bfa60bf1f30347003e2348bf7a1005570fd758133c87dafe08f" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default))
 '(lsp-log-io t)
 '(org-agenda-files '("~/orgfiles/life.org" "~/orgfiles/audi.org"))
 '(package-selected-packages
   '(winner web-mode tide go-mode counsel swiper ivy eglot lsp-ui flycheck rustic with-emacs lsp-mode typescript-mode rjsx-mode solidity-mode nginx-mode yaml-mode dockerfile-mode org corral toml-mode avy ace-window ace-jump-mode key-chord typing markdown-mode which-key discover-my-major f company-racer racer cargo company geben-helm-projectile magit rust-mode csharp-mode zenburn-theme color-theme-solarized ##))
 '(safe-local-variable-values
   '((org-todo-keyword-faces
      ("TODO" . "red")
      ("INPROGRESS" . "orange")
      ("DONE" . "green")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
