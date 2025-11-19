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

;;;; --- 3. Performance Tuning ---
;; These settings are designed to make Emacs feel faster and more responsive,
;; especially during intensive tasks.

;; Tune the garbage collector to reduce stuttering. The default is 800 kilobytes.
;; We set it to a much higher value (100 MB) so it runs far less often. This
;; uses more RAM but provides a smoother experience.
(setq gc-cons-threshold (* 100 1024 1024))


;;;; --- 4. Basic Emacs UI and Behavior ---
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(show-paren-mode 1)
(setq-default cursor-type 'box)
(setq vc-handled-backends nil)  ; Disable version control integration for speed
(setq create-lockfiles nil)     ; More friendly for build systems

;;;; --- 5. Modern Completion Framework: Vertico ---
(use-package vertico
  :init (vertico-mode))
(use-package savehist
  :init (savehist-mode 1))
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

;;;; --- 6. C++ Development Environment ---
(use-package eglot
  :defer t
  :hook ((c++-mode . eglot-ensure)
         (c-mode   . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(cmake-mode . ("cmake-language-server")))
  (message "Eglot is configured for C/C++ and CMake."))

;; (use-package company
;;   :defer t
;;   :hook (after-init . global-company-mode)
;;   :config
;;   (setq company-idle-delay 0.2)
;;   (setq company-minimum-prefix-length 2)
;;   (setq company-backends '(company-capf)))

(use-package cmake-mode
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; Hook to format C/C++ code automatically right before you save a file.
(defun my-cpp-auto-format-on-save ()
  "Enable auto-formatting on save for C/C++ modes."
  (add-hook 'before-save-hook #'my-cpp-format-buffer nil 'local))

(defun my-cpp-format-buffer ()
  "Format the current C/C++ buffer using clang-format."
  (interactive)
  (when (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode))
    (let ((buf (current-buffer)))
      (shell-command-on-region (point-min) (point-max) "clang-format" buf t)
      (message "Formatted buffer with clang-format."))))

(add-hook 'c++-mode-hook #'my-cpp-auto-format-on-save)
(add-hook 'c-mode-hook #'my-cpp-auto-format-on-save)


;;;; --- 7. Other Language and Tooling Configuration ---
(use-package magit
  :defer t
  :bind ("<f4>" . magit-status))

(use-package paredit
  :defer t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode       . enable-paredit-mode)
         (scheme-mode     . enable-paredit-mode)))

(use-package slime
  :defer t
  :commands (slime slime-connect)
  :init
  (defun my-slime-tab-and-complete ()
    "Custom function for TAB in SLIME. Indents or completes."
    (interactive)
    (slime-indent-and-complete-symbol))
  :config
  ;(load (expand-file-name "~/quicklisp/slime-helper.el") 'noerror 'nomessage)
  (slime-setup '(slime-fancy))
  (setq inferior-lisp-program "sbcl")
  :bind (("<f2>" . slime-eval-defun)
         ("s-." . slime-eval-defun)
         (:map slime-mode-map
               ("<tab>" . my-slime-tab-and-complete))))

(use-package markdown-mode
  :defer t
  :mode ("\\.md\\'" . markdown-mode))

;; (use-package gptel
;;   :defer t
;;   :commands (gptel gptel-send)
;;   :config
;;   (setq gptel-model "gpt-5-mini" 
;;         gptel-backend (gptel-make-gh-copilot "Copilot")))


(use-package copilot
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-<tab>" . copilot-accept-completion-by-word)))

;; 1.  **The "Tab" Conflict:** You are using `company` and `copilot` together. Both want the `<tab>` key. The configuration above gives Copilot priority when its ghost text is visible. It often looks messy when both the Company popup and the Copilot ghost text appear simultaneously. Deal with it or disable `company-mode` if you prefer Copilot.

;; 2.  **Activation:**
;;     After reloading your config (`M-x eval-buffer` or restart), you must run these two commands interactively:
;;     *   `M-x copilot-install-server` (Downloads the language server).
;;     *   `M-x copilot-login` (Authenticates with GitHub).

;; 3.  **Node Version:**
;;     As far as I can tell, the most common failure point is an outdated Node.js version. Verify it in your terminal with `node -v` showing >22 before complaining that it doesn't work. sudo emerge -av net-libs/nodejs ; needs +npm use flag

;;;; --- 8. Miscellaneous Customizations ---
;; Associate .ctl files with scheme-mode
(add-to-list 'auto-mode-alist '("\\.ctl\\'" . scheme-mode))

;;;; --- 9. Custom Variables and Faces (Managed by Emacs) ---
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "fixed" :foundry "misc" :slant normal :weight normal :height 98 :width semi-condensed)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cmake-mode company gptel magit markdown-mode orderless paredit slime
		vertico yaml-mode)))

;;;; --- End of Emacs Configuration ---
