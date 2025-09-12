;; -*- lexical-binding: t -*-

;;;; --- 1. Package Management with straight.el ---
;; This section bootstraps straight.el, a modern, git-based package manager.
;; It replaces the traditional package.el setup. Make sure Git is installed.

;; Prevent package.el from loading its packages at startup (for Emacs >= 27).
(setq package-enable-at-startup nil)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         (or (bound-and-true-p straight-base-dir)
                             user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; --- 2. Configure use-package to work with straight.el ---
;; Ensure use-package itself is installed via straight.el
(straight-use-package 'use-package)

;; Make all use-package declarations use straight.el by default.
(setq use-package-always-ensure t) ; For use-package < 2.4.0
(setq straight-use-package-by-default t) ; For use-package >= 2.4.0

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

;;;; --- 6. C++ Development Environment with lspce ---
;; NOTE: lspce requires 'f.el' and 'yasnippet'. We ensure they are installed.
(use-package f)
(use-package yasnippet)

;; lspce is an LSP client implemented as a dynamic module in Rust.
;; It requires `cargo` to build its shared library on first install.
(use-package lspce
  ;; This special :straight recipe handles cloning and building the Rust module.
  :straight
  `(lspce :type git :host github :repo "zbelial/lspce"
          :files (:defaults ,(pcase system-type
                               ('gnu/linux "lspce-module.so")
                               ('darwin "lspce-module.dylib")))
          :pre-build ,(pcase system-type
                        ('gnu/linux '(("cargo" "build" "--release")
                                      ("cp" "./target/release/liblspce_module.so" "./lspce-module.so")))
                        ('darwin '(("cargo" "build" "--release")
                                   ("cp" "./target/release/liblspce_module.dylib" "./lspce-module.dylib")))))
  :hook ((c++-mode . lspce-mode)
         (c-mode   . lspce-mode)
         (cmake-mode . lspce-mode))
  :config
  (progn
    (message "Configuring lspce for C/C++/CMake.")
    ;; Set a log file for debugging purposes.
    (lspce-set-log-file "/tmp/lspce.log")
    (lspce-enable-logging)

    (add-hook 'c++-mode-hook 'lspce-mode)
    
    ;; Explicitly define how to get the language type from the major mode.
    ;; This function is crucial for lspce to know which server to start.
    (setq lspce-lsp-type-function
          (lambda ()
            (cond
             ((derived-mode-p 'c++-mode) "C++")
             ((derived-mode-p 'c-mode) "C")
             ((derived-mode-p 'cmake-mode) "CMake")
             (t nil))))

    ;; Configure LSP servers. The keys ("C++", "C", "CMake") must now match
    ;; the strings returned by the function above.
    (setq lspce-server-programs
          `(("C++" "clangd" "--all-scopes-completion --clang-tidy --enable-config --header-insertion-decorators=0")
            ("C" "clangd" "--all-scopes-completion --clang-tidy --enable-config --header-insertion-decorators=0")
            ("CMake" "cmake-language-server" "")))

    ;; --- COMPLETION SETUP ---
    ;; Integrate lspce with Emacs's built-in completion system (used by Vertico).
    (add-to-list 'completion-at-point-functions #'lspce-completion-at-point)))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; Hook to format C/C++ code automatically right before you save a file.
;; This functionality is independent of the LSP client and remains unchanged.
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
  :bind ("<f4>" . magit-status))

(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode       . enable-paredit-mode)
         (scheme-mode     . enable-paredit-mode)))

(use-package slime
  :commands (slime slime-connect)
  :init
  (defun my-slime-tab-and-complete ()
    "Custom function for TAB in SLIME. Indents or completes."
    (interactive)
    (slime-indent-and-complete-symbol))
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el") 'noerror 'nomessage)
  (slime-setup '(slime-fancy))
  (setq inferior-lisp-program "sbcl")
  :bind (("<f2>" . slime-eval-defun)
         ("s-." . slime-eval-defun)
         (:map slime-mode-map
               ("<tab>" . my-slime-tab-and-complete))))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package gptel
  :commands (gptel gptel-send)
  :config
  (setq gptel-model "gpt-5-mini"
        gptel-backend (gptel-make-gh-copilot "Copilot")))

;;;; --- 8. Miscellaneous Customizations ---
;; Associate .ctl files with scheme-mode
(add-to-list 'auto-mode-alist '("\\.ctl\\'" . scheme-mode))

;;;; --- 9. Custom Variables and Faces (Managed by Emacs) ---
;; NOTE: The 'package-selected-packages' variable is managed by package.el
;; and is no longer needed with straight.el. It has been removed.
(custom-set-faces
 '(default ((t (:family "fixed" :foundry "misc" :slant normal :weight normal :height 98 :width semi-condensed)))))

;;;; --- End of Emacs Configuration ---
