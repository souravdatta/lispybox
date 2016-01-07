(setq inhibit-startup-message t)

(load (expand-file-name (concat (getenv "BASENAME") "/quicklisp/slime-helper.el")))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program (concat (getenv "BASENAME") "/sbcl/run-sbcl.sh"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(kill-buffer "*scratch*")
;;(shell)
(slime)
(add-hook 'window-setup-hook 'delete-other-windows)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
