;; -*- lexical-binding: t -*-

;; Basic package system setup
(require 'package)
(setq package-archives nil)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (melpa-url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" melpa-url) t))
(when (< emacs-major-version 24)
  ;; Ensure compatibility libraries on very old Emacsen
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; UI / basic behavior
(setq inhibit-startup-message t)   ; no startup message
(tool-bar-mode -1)                ; no tool bar
(show-paren-mode 1)               ; show matching parens
(setq-default cursor-type 'box)

;; Keybindings (safe top-level declarations; functions may not yet be loaded)
(global-set-key (kbd "S-SPC") #'company-complete)
(global-set-key (kbd "<f1>") #'save-buffer)
(global-set-key (kbd "<f2>") #'slime-eval-defun)
(global-set-key (kbd "<f3>") #'octave-send-line)
(global-set-key (kbd "<f4>") #'magit-status)
(global-set-key (kbd "s-.") #'slime-eval-defun)

;; Ido configuration
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode +1)

;; Try to enable ido-hacks if available
(when (require 'ido-hacks nil t)
  (ido-hacks-mode))

;; Disable VC (speed up file opening if you don't use VC)
(with-eval-after-load "vc"
  (remove-hook 'find-file-hooks 'vc-find-file-hook))
(setq vc-handled-backends nil)

;; Paredit: Autoload and enable for Lisp modes
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)

;; Associate .ctl files with scheme-mode
(push '("\\.ctl\\'" . scheme-mode) auto-mode-alist)

;; Eval-in-repl: if installed, enable Python evaluation binding
(when (and (require 'eval-in-repl nil t)
           (require 'eval-in-repl-python nil t))
  (add-hook 'python-mode-hook
            (lambda ()
              "Provide a convenient binding to eval code in Python REPL."
              (local-set-key (kbd "<C-return>") 'eir-eval-in-python))))

;; SLIME setup (try to be robust if quicklisp helper is missing)
(when (or (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
          (require 'slime nil t))
  ;; Load quicklisp slime helper if present (silently ignore errors)
  (ignore-errors
    (load (expand-file-name "~/quicklisp/slime-helper.el")))

  ;; Require slime autoloads if available; otherwise try to require slime
  (require 'slime-autoloads nil t)
  (when (require 'slime nil t)
    ;; Use slime-fancy by default
    (slime-setup '(slime-fancy))

    ;; Default Lisp implementation
    (setq inferior-lisp-program "sbcl")

    ;; Utility to start slime if not connected
    (defun cliki/start-slime ()
      "Start SLIME if not already connected."
      (interactive)
      (unless (slime-connected-p)
        (save-excursion (slime))))

    (add-hook 'slime-mode-hook #'cliki/start-slime)

    ;; Better TAB behaviour in slime: indent and complete symbol
    (defun slime-tab ()
      "SLIME TAB: indent and/or complete symbol."
      (interactive)
      (slime-indent-and-complete-symbol))

    (defun my-slime-mode-hook ()
      "Custom keybindings for slime-mode."
      (define-key slime-mode-map (kbd "<tab>") #'slime-tab))

    (add-hook 'slime-mode-hook #'my-slime-mode-hook)))

;; Miscellaneous small helpers

;; Ensure magit (if installed) is available on F4 key; attempt to autoload
(when (require 'magit nil t)
  ;; magit-status is autoloaded by magit; nothing further needed
  (message "Magit loaded."))

;; Try to load ido-hacks earlier if not already loaded above
(unless (featurep 'ido-hacks)
  (require 'ido-hacks nil t))

;; Provide some GPTEL defaults if package is installed
(when (require 'gptel nil t)
  (setq gptel-model 'gpt-5-mini
        gptel-backend (gptel-make-gh-copilot "Copilot")))

;; Clean up leftover legacy comments and keep customization at the end.
;; custom-set-faces and custom-set-variables are managed by Emacs' customize UI.
(custom-set-faces
 '(default ((t (:family "fixed" :foundry "misc" :slant normal :weight normal :height 98 :width semi-condensed)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 '(package-selected-packages
   '(eval-in-repl gptel gptel-magit ido-hacks magit markdown-mode)))
