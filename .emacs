;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

;(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)


(global-set-key (kbd "<f1>") 'save-buffer)
(global-set-key (kbd "<f2>") 'slime-eval-defun)

(global-set-key (kbd "<f4>") 'magit-status)

(require 'ido-hacks)

(ido-hacks-mode)

;; (load "/home/martin/.emacs.d/ats-mode")

;; (load "/home/martin/src/ESS/lisp/ess-site")
;; ;;(setq inferior-julia-program-name "/home/martin/julia-4851ee9847/bin/julia")
;; ;;(setq inferior-julia-program-name "/home/martin/julia-0.5.0-2016-01-02/bin/julia")
;; (setq inferior-julia-program-name "/home/martin/julia-0.4.2/bin/julia")

;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; ;; Enable company globally for all mode
;; ;(global-company-mode)

;; ;; Reduce the time after which the company auto completion popup opens
;; (setq company-idle-delay 0.2)

;; ;; Reduce the number of characters before company kicks in
;; (setq company-minimum-prefix-length 1)

;; ;; Set path to racer binary
;; (setq racer-cmd "/usr/local/bin/racer")

;; ;; Set path to rust src directory
;; (setq racer-rust-src-path "/home/martin/src/rust/src/")

;; ;; Load rust-mode when you open `.rs` files
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; ;; Setting up configurations when you load rust-mode
;; (add-hook 'rust-mode-hook
;; 	  '(lambda ()
;; 	     ;; Enable racer
;; 	     (racer-activate)
;; 	     ;; Hook in racer with eldoc to provide documentation
;; 	     (racer-turn-on-eldoc)
;; 	     ;; Use flycheck-rust in rust-mode
;; 	     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;; 	     ;; Use company-racer in rust mode
;; 	     (set (make-local-variable 'company-backends) '(company-racer))
;; 	     ;; Key binding to jump to method definition
;; 	     (local-set-key (kbd "M-.") #'racer-find-definition)
;; 	     ;; Key binding to auto complete and indent
;; 	     (local-set-key (kbd "TAB") #'racer-complete-or-indent)))
;; (require 'semantic)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (ggtags-mode 1)
;; 	      (semantic-mode 1)
;; 	      (global-semanticdb-minor-mode 1)
;; 	      (global-semantic-idle-summary-mode 1) ;; doesn't work in c++
;; 	      (global-semantic-idle-scheduler-mode 1))))


;(add-to-list 'package-archives '("melpa-unstable" . "http://melpa.milkbox.net/packages/"))
(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
(setq vc-handled-backends nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (magit-filenotify magit egg paredit helm-projectile auctex)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode nil))

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode +1)
(tool-bar-mode -1)
(show-paren-mode 1)
(global-set-key (kbd "s-.") 'slime-eval-defun)
;(declare-function edmacro-subseq "edmacro" (seq start &optional end))
					;(load (expand-file-name "~/quicklisp/slime-helper.el"))
(add-to-list 'load-path (expand-file-name "~/quicklisp/local-projects/slime"))
(require 'slime-autoloads)
(slime-setup '(slime-fancy))
;;(slime-setup '())
(setq inferior-lisp-program "sbcl --dynamic-space-size 700")
;(setq inferior-lisp-program "sbcl --dynamic-space-size 700")
					;(setq inferior-lisp-program "/mnt/usr/local/bin/sbcl --dynamic-space-size 720 --core /home/martin/sbcl.core-for-slime")

;(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(defun cliki:start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

(add-hook 'slime-mode-hook 'cliki:start-slime)

;(require 'slime-autoloads)
;(add-to-list 'slime-contribs 'slime-fancy)
;(setq inferior-lisp-program "/mnt/usr/local/bin/sbcl --dynamic-space-size 930")
;;(setq inferior-lisp-program "/usr/local/bin/ecl")
;;(setq inferior-lisp-program "/home/martin/Downloads/lx86cl64 --image-name /home/martin/Downloads/lx86cl64.image")

; (add-hook 'lisp-mode-hook 'enable-paredit-mode)
;(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(defun slime-tab ()
  "slime-mode tab dwim, either indent, complete symbol or yas/expand"
  (interactive)
  (slime-indent-and-complete-symbol))

(defun my-slime-mode-hook ()
  (interactive)
  (define-key slime-mode-map (kbd "<tab>")
    'slime-tab))

(add-hook 'slime-mode-hook 'my-slime-mode-hook)

;; (require 'redshank-loader
;; 	 "/home/martin/.emacs.d/elpa/redshank-20120510.1230/redshank-loader")

;; (eval-after-load "redshank-loader"
;;   `(redshank-setup '(lisp-mode-hook
;; 		     slime-repl-mode-hook) t))

;; (load "/home/martin/quicklisp/local-projects/c-mera/util/c-mera-lisp-indent-hack.el")

;(ac-config-default)


;(require 'ac-slime)
;(add-hook 'slime-mode-hook 'set-up-slime-ac)
;(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'slime-repl-mode))


;(custom-set-faces '(default ((t (:family "lucidatypewriter" :foundry "b&h" :slant normal :weight medium :height 220 :width normal)))))
;; (put 'upcase-region 'disabled nil)

;(add-to-list 'load-path "/home/martin/.emacs.d")
;(require 'taskjuggler-mode)(custom-set-faces '(default ((t (:family "fixed" :foundry "misc" :slant normal :weight normal :height 98 :width semi-condensed)))))



;; (use-package parinfer
;;   :ensure t
;;   :bind
;;   (("C-," . parinfer-toggle-mode))
;;   :init
;;   (progn
;;     (setq parinfer-extensions
;;           '(defaults       ; should be included.
;;             pretty-parens  ; different paren styles for different modes.
;;             evil           ; If you use Evil.
;;             lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
;;             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;;             smart-yank))   ; Yank behavior depend on mode.
;;     (add-hook 'clojure-mode-hook #'parinfer-mode)
    
;;     (add-hook 'lisp-mode-hook #'parinfer-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "fixed" :foundry "misc" :slant normal :weight normal :height 98 :width semi-condensed)))))

