





;; inbox
;; * https://crawshaw.io/blog/programming-with-llms
;; * https://benhouston3d.com/blog/crafting-readmes-for-ai












;;;; copilot

;; (shell-command-to-string "node --version")

;; brew install npm
;; brew install nvm
;; nvm install 22
;; nvm use 22
;; nvm alias default 22


;; (executable-find "node")
;; (shell-command-to-string "node --version")  ; should show v22


;; M-x package-install editorconfig
;; M-x package-install json-rpc
;; M-x package-install dash
;; M-x package-install s
;; M-x package-install quelpa

(require 'editorconfig)
(require 'jsonrpc)
(require 'quelpa)

;; (quelpa '(copilot :fetcher github :repo "copilot-emacs/copilot.el" :files ("*.el")))


;; (setenv "NODE_DEBUG" "*")
;; (setenv "NODE_DEBUG" "http2,tls,net,stream")


(require 'copilot)

;; M-x copilot-install-server
;; M-x copilot-login


(add-hook 'prog-mode-hook 'copilot-mode)

(add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
(add-to-list 'copilot-indentation-alist '(lisp-mode 2))
(add-to-list 'copilot-indentation-alist '(markdown-mode 2))
(add-to-list 'copilot-indentation-alist '(org-mode 2))
(add-to-list 'copilot-indentation-alist '(prog-mode 2))
(add-to-list 'copilot-indentation-alist '(text-mode 2))

(setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs
(setq-default tab-width 2)          ;; Set default tab width to 4

(with-eval-after-load 'copilot
  (define-key copilot-completion-map (kbd "C-c n") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "C-c p") 'copilot-previous-completion)
  (define-key copilot-completion-map (kbd "C-c DEL") 'copilot-clear-overlay)
  (define-key copilot-completion-map (kbd "C-c w") 'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "C-c l") 'copilot-accept-completion-by-line)
  (define-key copilot-completion-map (kbd "C-c TAB") 'copilot-accept-completion)
;;  (define-key copilot-completion-map (kbd "C-c RET") 'copilot-accept-completion)
)
























;;;; gptel - https://github.com/karthink/gptel?tab=readme-ov-file#usage
;; M-x package-install gptel

(require 'gptel)

(setq gptel-model 'gemini-2.5-flash
      gptel-backend (gptel-make-gemini "Gemini"
                      :key (getenv "GEMINI_API_KEY")
                      :stream t)

;; default setup (openai)

;; (setq gptel-model 'gpt-5
;;      gptel-api-key "....")
;; alt setup (amthropic)
;; (gptel-make-anthropic "Claude" :stream t :key "...."




;; bindings

(global-set-key (kbd "C-c g") 'gptel)
;; (global-set-key (kbd "C-c C-g a") 'gptel-add)
;; (global-set-key (kbd "C-c C-g f") 'gptel-add-file)
;; (global-set-key (kbd "C-c C-g r") 'gptel-rewrite)
;; (global-set-key (kbd "C-c C-g m") 'gptel-menu)
;; (global-set-key (kbd "C-c C-g x") 'gptel-context-remove-all)
;; (global-set-key (kbd "C-c C-g X") 'gptel-reset)



;; (defun my-gptel-mode-keys ()
;;  (local-set-key (kbd "C-RET") 'gptel-send))

(add-hook 'gptel-mode-hook 'my-gptel-mode-keys)












;;;; aider
;;
;;   % brew update
;;   % brew install python
;;   % open -e ~/.zprofile
;;
;; Add the following line to your .zprofile
;;
;;     export PATH="$(brew --prefix python)/libexec/bin:$PATH"
;;
;; test
;;
;;    % which python3
;;
;; then:
;;
;;    % brew install aider
;;    % cd ~/to/your/project
;;
;;    % aider --list-models gemini/
;;    % aider --model gemini-2.5-pro --api-key......
;;
;;    % aider --model gpt-4.1 --api-key......
;;    % aider --model sonnet --api-key......
;;


;; https://aider.chat/docs/usage.html
;; https://emacs.stackexchange.com/questions/169/how-do-i-reload-a-file-in-a-buffer
;; https://github.com/magit/transient
;; https://github.com/MatthewZMD/aidermacs?tab=readme-ov-file



;;;; aidermacs

;; M-x package-install aidermacs
;; required updating 'seq
(setq package-install-upgrade-built-in t)
(progn (unload-feature 'seq t)
       (require 'seq))

(require 'aidermacs)

;; https://aider.chat/docs/llms.html

;; M-x aidermacs-change-model

;;;;; now set in .zprofile

;; (setenv "ANTHROPIC_API_KEY" ... )
;; (setenv "GEMINI_API_KEY" ... )
;; (setenv "OPENAI_API_KEY" ... )



;; (setq aidermacs-default-model "gemini/gemini-2.5-pro-exp-03-25")

;; (setq aidermacs-use-architect-mode t)
;; (setq aidermacs-architect-model "gemini/gemini-2.5-pro-exp-03-25")

;; (setq aidermacs-editor-model "gpt-4.1")
;; ;; (setq aidermacs-editor-model "gpt-4.1-mini")       ; switch in for cheap mode

;; (setq aidermacs-weak-model "gemini/gemini-2.5-flash-preview-04-17")
;; ;; (setq aidermacs-weak-model 'gemini-2.5-flash-preview-04-17)


;; try after rate limit

(setq aidermacs-default-model "gpt-4.1")
(setq aidermacs-use-architect-mode nil)
(setq aidermacs-weak-model "gpt-4.1")


(global-set-key (kbd "C-c a")
                'aidermacs-transient-menu)
















;;;; claude code
;; npm install -g @anthropic-ai/claude-code
;; claude

;; https://docs.anthropic.com/en/docs/agents-and-tools/claude-code/tutorials
;; https://github.com/stevemolitor/claude-code.el

