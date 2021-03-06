;; inherit bashrc defaults

;; (defun import-shell-var (name set-exec)
;;   (let ((value-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string (concat "$SHELL --login -i -c 'echo $" name "'")))))
;;     (setenv name value-from-shell)
;;     (when set-exec
;;       (setq exec-path (split-string value-from-shell path-separator)))))

;; (import-shell-var "PATH" t)
;; (import-shell-var "RUST_SRC_PATH" nil)

;; org-mode by default
(setq-default major-mode 'org-mode)

;; ELPA / MELPA setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(scroll-bar-mode -1)
(tool-bar-mode -1)

;; shorter predicate
(fset 'yes-or-no-p 'y-or-n-p)

;; redirect backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; winner mode
(use-package winner
  :bind
  (("C-c p" . winner-undo)
   ("C-c n" . winner-redo)
   ))

(winner-mode 1)

;; color theming
(load-theme 'zenburn t)

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

;; Kill buffer closes pane
(defun close-buffer-and-kill-window ()
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(global-set-key (kbd "C-x M-k") 'close-buffer-and-kill-window)

;; ace-jump-mode

(use-package ace-jump-mode
  :bind
  (("C-c <SPC>" . ace-jump-mode)
   ("C-c M-<SPC>" . ace-jump-mode-pop-mark)))

;; discover-my-major
(define-key 'help-command "\C-m" 'discover-my-major)
(define-key 'help-command "\M-m" 'discover-my-mode)

;; which-key
(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-right)
(setq which-key-idle-delay 3.0)

;; Magit-status
(global-set-key (kbd "C-x g s") 'magit-status)

;; ivy/counsel/swiper
(ivy-mode 1)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

(global-set-key (kbd "M-x") 'counsel-M-x)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)

(global-set-key (kbd "C-x C-g") 'counsel-rg)

(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

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

;; python-mode for SCons by default
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("Swigscript" . python-mode))
(add-to-list 'auto-mode-alist '("Linkscript" . python-mode))

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

;; Quadrants

(require 'windmove)

(defun my/quadrants ()
  (interactive)
  (progn
    (split-window-right)
    (windmove-right)
    (split-window-below)
    (windmove-left)
    (split-window-below)))

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

;; corral - an alternative to smartparens/paredit

(require 'corral)

(global-set-key (kbd "C-(") 'corral-parentheses-backward)
(global-set-key (kbd "C-)") 'corral-parentheses-forward)
(global-set-key (kbd "C-{") 'corral-braces-backward)
(global-set-key (kbd "C-}") 'corral-braces-forward)
(global-set-key (kbd "M-[") 'corral-brackets-backward)
(global-set-key (kbd "M-]") 'corral-brackets-forward)
(global-set-key (kbd "M-'") 'corral-double-quotes-backward)

;; use Hack as default font
(set-frame-font "Hack")

;; Custom input method for Russian
(load-file "~/.emacs.d/russian-dvp.el")
(setq default-input-method "russian-dvp")

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
   '(counsel swiper ivy eglot lsp-ui flycheck rustic with-emacs lsp-mode typescript-mode rjsx-mode solidity-mode nginx-mode yaml-mode dockerfile-mode org corral toml-mode avy ace-window ace-jump-mode key-chord typing markdown-mode which-key discover-my-major f company-racer racer cargo company geben-helm-projectile magit rust-mode csharp-mode zenburn-theme color-theme-solarized ##))
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
