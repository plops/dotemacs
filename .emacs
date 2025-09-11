;; -*- lexical-binding: t -*-

;;;; --- 1. Package Management ---
;; This section sets up Emacs's package manager. We define the sources
;; (archives) from which to download packages, with MELPA being the largest.
;; This code needs to run early, as the rest of the configuration depends on it.

(require 'package)

;; Define package archives
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)

;;;; --- 2. Bootstrap use-package ---
;; `use-package` is a macro that dramatically simplifies package configuration.
;; It allows us to group settings by package, automatically install missing
;; packages, and, crucially, lazy-load them to keep startup times fast.
;; We first ensure `use-package` itself is installed.

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; This line makes `use-package` automatically install any packages that are
;; not yet present on your system.
(setq use-package-always-ensure t)


;;;; --- 3. Basic Emacs UI and Behavior ---
;; These are fundamental tweaks to the Emacs user interface and default
;; behavior. They are not tied to any specific package.

(setq inhibit-startup-message t)   ; Disable the default startup splash screen
(tool-bar-mode -1)                ; Disable the graphical toolbar
(show-paren-mode 1)               ; Highlight matching parentheses
(setq-default cursor-type 'box)   ; Use a block cursor instead of a thin line
(setq vc-handled-backends nil)     ; Disable version control integration for faster file opening


;;;; --- 4. Modern Completion Framework: Vertico ---
;; This section replaces the older `ido-mode` with `vertico`, a modern,
;; lightweight, and highly extensible completion UI. We also add several
;; companion packages for a complete experience.

(use-package vertico
  :init
  ;; The :init block runs *before* the package is loaded. We enable vertico-mode
  ;; globally here. This ensures it's active for all completion commands.
  (vertico-mode)
  :config
  ;; You can further customize vertico's appearance or behavior here if needed.
  ;; For example, to make it cycle through candidates at the end:
  ;; (setq vertico-cycle t)
  )

(use-package savehist
  :init
  ;; This ensures that the history of commands you use in the minibuffer
  ;; is saved between Emacs sessions.
  (savehist-mode 1))

(use-package orderless
  :init
  ;; `orderless` provides a more powerful way of filtering completion candidates.
  ;; You can type space-separated components in any order to match.
  ;; For example, "file find" will match "find-file".
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

;;;; --- 5. Language and Tooling Configuration ---
;; Each of the following blocks configures a specific package for a language
;; or tool. Thanks to `use-package`, each block is self-contained.

(use-package magit
  :defer t  ; Don't load magit until it's called
  :bind ("<f4>" . magit-status)
  :config
  (message "Magit is configured."))

(use-package paredit
  :defer t
  ;; The :hook keyword is a clean way to enable a mode for specific major modes.
  ;; This enables paredit automatically when you open a Lisp or Scheme file.
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode       . enable-paredit-mode)
         (scheme-mode     . enable-paredit-mode)))

(use-package slime
  :defer t
  ;; The :commands keyword makes sure SLIME is loaded when you first
  ;; try to run one of its interactive commands, like `slime`.
  :commands (slime slime-connect)
  :init
  ;; This custom function will be defined before SLIME loads.
  (defun my-slime-tab-and-complete ()
    "Custom function for TAB in SLIME. Indents the line, or completes a symbol at point."
    (interactive)
    (slime-indent-and-complete-symbol))
  :config
  ;; This code runs after SLIME has been loaded.
  (load (expand-file-name "~/quicklisp/slime-helper.el") 'noerror 'nomessage)
  (slime-setup '(slime-fancy))
  (setq inferior-lisp-program "sbcl")
  :bind (;; Global keybindings for SLIME functions
         ("<f2>" . slime-eval-defun)
         ("s-." . slime-eval-defun) ; Super (Windows/Command) + period
         ;; A keybinding specific to slime-mode buffers
         (:map slime-mode-map
               ("<tab>" . my-slime-tab-and-complete))))

(use-package markdown-mode
  :defer t
  ;; Load markdown-mode when you open a file with a matching extension.
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (message "Markdown-mode is configured."))

(use-package gptel
  :defer t
  ;; The package will be loaded when you first call one of these commands.
  :commands (gptel gptel-send)
  :config
  ;; Set your preferred model and backend.
  ;; Ensure the model name is current and valid.
  (setq gptel-model "gpt-4"
        gptel-backend (gptel-make-gh-copilot "Copilot")))


;;;; --- 6. Custom File Types and Hooks ---
;; This section is for miscellaneous configurations that don't fit into a
;; specific package block above.

;; Associate .ctl files with scheme-mode
(add-to-list 'auto-mode-alist '("\\.ctl\\'" . scheme-mode))

;; Python REPL evaluation (using eval-in-repl package)
(use-package eval-in-repl
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'eval-in-repl-python) ; Ensure python backend is loaded
                         (local-set-key (kbd "<C-return>") 'eir-eval-in-python))))


;;;; --- 7. Custom Variables and Faces ---
;; This section is managed automatically by Emacs's "Customize" interface.
;; It's best practice to keep it at the very end of your init file.
;; For a cleaner setup, you could move these into a separate file (`custom.el`)
;; by adding `(setq custom-file (locate-user-emacs-file "custom.el"))`
;; at the top of your init file and then loading it with `(load custom-file 'noerror)`.

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
 '(package-selected-packages nil))

;;;; --- End of Emacs Configuration ---
