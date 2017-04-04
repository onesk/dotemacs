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

;; Magit-status

(global-set-key (kbd "C-x g s") 'magit-status)

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

;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("14f0fbf6f7851bfa60bf1f30347003e2348bf7a1005570fd758133c87dafe08f" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(package-selected-packages
   (quote
    (magit rust-mode csharp-mode zenburn-theme color-theme-solarized ##))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'set-goal-column 'disabled nil)
