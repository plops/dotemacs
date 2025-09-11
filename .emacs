;; -*- lexical-binding: t -*-

;;;; --- 1. Package Management ---
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

;;;; --- 2. Bootstrap use-package ---
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;;; --- 3. Basic Emacs UI and Behavior ---
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(show-paren-mode 1)
(setq-default cursor-type 'box)
(setq vc-handled-backends nil)
(setq create-lockfiles nil) ; More friendly for build systems

;;;; --- 4. Modern Completion Framework: Vertico ---
(use-package vertico
  :init (vertico-mode))
(use-package savehist
  :init (savehist-mode 1))
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

;;;; --- 5. C++ Development Environment ---
;; This section contains all the packages and settings for a modern C++ workflow.

(use-package eglot
  :defer t
  :hook ((c++-mode . eglot-ensure)
         (c-mode   . eglot-ensure))
  ;; The :config block runs *after* the package has been loaded.
  ;; This is the correct place to modify variables defined by eglot.
  :config
  ;; Add support for cmake-language-server in cmake-mode.
  (add-to-list 'eglot-server-programs '(cmake-mode . ("cmake-language-server")))
  (message "Eglot is configured for C/C++ and CMake."))

(use-package company
  :defer t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-backends '(company-capf)))

(use-package cmake-mode
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; Custom function for code formatting using clang-format
(defun my-cpp-format-buffer ()
  "Format the current C/C++ buffer using clang-format."
  (interactive)
  (when (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode))
    (let ((buf (current-buffer)))
      (shell-command-on-region (point-min) (point-max) "clang-format" buf t)
      (message "Formatted buffer with clang-format."))))

;; Hook to format the code automatically right before you save a file.
(defun my-cpp-auto-format-on-save ()
  "Enable auto-formatting on save for C/C++ modes."
  (add-hook 'before-save-hook #'my-cpp-format-buffer nil 'local))

(add-hook 'c++-mode-hook #'my-cpp-auto-format-on-save)
(add-hook 'c-mode-hook #'my-cpp-auto-format-on-save)


;;;; --- 6. Other Language and Tooling Configuration ---
(use-package magit
  :defer t
  :bind ("<f4>" . magit-status))

(use-package paredit
  :defer t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode       . enable-paredit-mode)
         (scheme-mode     . enable-paredit-mode)))


;;;; --- 7. Custom Variables and Faces ---
(custom-set-faces
 '(default ((t (:family "fixed" :foundry "misc" :slant normal :weight normal :height 98 :width semi-condensed)))))

(custom-set-variables
 '(package-selected-packages
   '(cmake-mode company eglot magit orderless paredit savehist use-package vertico)))

;;;; --- End of Emacs Configuration ---
