
;;;; lisp

(defun my-lisp-mode-hook ()

  ;; Swap () and []
  (local-set-key (kbd "(") (lambda () (interactive) (insert "[")))
  (local-set-key (kbd ")") (lambda () (interactive) (insert "]")))
  (local-set-key (kbd "[") (lambda () (interactive) (insert "(")))
  (local-set-key (kbd "]") (lambda () (interactive) (insert ")")))

  ;; copilot
;  (copilot-mode 1)
  )

(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'slime-repl-mode-hook 'my-lisp-mode-hook)




;;;; SBCL specific
;; create ~/.sbclinit

;;;; SBCL - SETUP QUICKLISP
;;  (load "~/quicklisp/setup.lisp")              ; init
;;  (ql:add-to-init-file)                        ; run init every time

;; (ql:update-client)                            ; update the client
;; (ql:update-dist "quicklisp")                  ; update index
;; (ql:system-apropos "search-pattern"           ; find what's available


;; (ql:quickload system-name)                    ; load package
;; (ql:uninstall system-name)                    ; uninstall

;; (ql:who-depends-on system-name)

;;;; slime
;; (ql:quickload "quicklisp-slime-helper")        ; installing

(load (expand-file-name "~/cl/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(global-set-key (kbd "C-c l")
                (lambda ()
                  (interactive)
                  (switch-to-buffer "*slime-repl sbcl*")))
