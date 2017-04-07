;; inherit bashrc defaults

(defun import-shell-var (name set-exec)
  (let ((value-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string (concat "$SHELL --login -i -c 'echo $" name "'")))))
    (setenv name value-from-shell)
    (when set-exec
      (setq exec-path (split-string value-from-shell path-separator)))))

(import-shell-var "PATH" t)
(import-shell-var "RUST_SRC_PATH" nil)

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

(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; projectile (w/counsel)
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(require 'projectile)

(projectile-mode)
(counsel-projectile-on)

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

;; Rust development environment
(require 'racer)

(add-hook 'rust-mode-hook
          (lambda () (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

(setq racer-cmd (concat (getenv "HOME") "/.cargo/bin/racer"))
(setq racer-rust-src-path (expand-file-name (getenv "RUST_SRC_PATH")))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; TODO RECONSIDER
;; (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
;; (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

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
    (markdown-mode which-key discover-my-major f company-racer racer cargo company geben-helm-projectile counsel-projectile counsel ivy magit rust-mode csharp-mode zenburn-theme color-theme-solarized ##))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'set-goal-column 'disabled nil)
