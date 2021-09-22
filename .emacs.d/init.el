;; ELPA / MELPA setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; use-package
(setq package-archive-priorities
      '(("melpa" . 20)
	("gnu" . 10)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(setq-default
 load-prefer-newer t
 inhibit-startup-message t

 ;; separate customization file
 custom-file "~/.emacs.d/custom-file.el"

 create-lockfiles nil

 ;; indentation
 indent-tabs-mode nil
 default-tab-width 4

 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))

 auto-save-default nil

 enable-recursive-minibuffers t

 ring-bell-function 'ignore)

;; Load `custom-file` manually as we have modified the default path.
(load-file custom-file)

;; Change all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; OS X path fix
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x)))
  (exec-path-from-shell-initialize))

;; binding & modeline
(use-package delight)
(use-package diminish)
(use-package bind-key)

;; no visual clutter
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; winner mode
(global-set-key (kbd "C-c p") 'winner-undo)
(global-set-key (kbd "C-c n") 'winner-redo)

(winner-mode 1)

;; color theming
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; column numbers
(setq column-number-mode t)

;; default indentation
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

;; magit
(use-package magit
  :bind ("C-x g s" . magit-status))

;; ivy/counsel/swiper
(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x C-g" . counsel-rg)))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package ivy
  :config
  (setq ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil)
  (ivy-mode t))

;; yasnippet
(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode 1))

;; company
(use-package company
  :bind (:map global-map
         ("M-/" . hippie-expand)
         ("M-l" . company-complete)
         :map company-active-map
         ("TAB" . company-complete-common-or-cycle)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :custom
  (company-show-numbers t)
  (company-idle-delay nil)

  (hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))

  (company-quick-access-keys '(";" "," "y" "a" "o" "e" "u" "q" "j" "k"))

  :config
  (global-company-mode t))

;; trailing ws delete
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
(use-package markdown-mode)

;; toml-mode
(use-package toml-mode)

;; flycheck
(use-package flycheck
  :custom
  (flycheck-highlighting-style nil))

;; lsp
(use-package lsp-mode
  :bind (:map lsp-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-?" . lsp-describe-thing-at-point)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "check")
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 1.0)
  (lsp-enable-symbol-highlighting nil)
  (lsp-signature-auto-activate nil)
  (lsp-lens-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  (lsp-modeline-code-actions-enable nil))

;; Rust
(use-package rustic
  :config
  (add-hook 'rustic-mode-hook 'rustic-mode-hook))

(defun rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;; Typescript
(defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode)
    (eldoc-mode)
    (lsp))

(use-package tide
  :config
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))

  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-hook 'web-mode-hook #'setup-tide-mode))

;; ace-window
(use-package ace-window
  :config
  (setq aw-keys '(?a ?e ?u ?d ?h ?t ?s))
  (setq aw-dispatch-always t))

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
    (key-chord-define-global "yy" 'ace-window)))

;; Electric pairs always on
(electric-pair-mode t)

;; use Hack as default font
(set-frame-font "Hack")

;; Org mode
(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c c") 'org-capture)

;; Dockerfile mode
(use-package dockerfile-mode)

;; Allow disabling ivy completion on C-x C-f with universal argument
;; (for TRAMP use, for example)

(defun find-file-disable-ivy (uniarg)
  (interactive "P")
  (let ((completing-read-function (if uniarg
                                      'completing-read-default
                                       completing-read-function)))
    (call-interactively 'find-file)))

(global-set-key (kbd "C-x C-f") 'find-file-disable-ivy)
