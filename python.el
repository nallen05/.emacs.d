




;; python




;;;;;;;;;;;;;;;; INSTALL ;;;;;;;;;;;;;;;;

;;
;; % brew install pyenv
;;
;; Add this to your ~/.zshrc or ~/.bash_profile
;;
;;    export PYENV_ROOT="$HOME/.pyenv"
;;    [[ -d "$PYENV_ROOT/bin" ]] && export PATH="$PYENV_ROOT/bin:$PATH"
;;    eval "$(pyenv init -)"
;;
;; Restart your terminal or run source ~/.zshrc to apply the changes.
;;
;;     % pyenv install --list | grep " 3.13"
;;
;; Install the latest 3.13 version (e.g., 3.13.0)
;;
;;     % pyenv install 3.13.0
;;
;; Set your global default Python version. This tells pyenv which version to use anytime you are not inside a specific project.
;;
;;     % pyenv global 3.13.0
;;
;; Verify the setup.
;;
;;    % source ~/.zshrc
;;    % python --version
;;    % which python
;;
;; Expected output: /Users/your_username/.pyenv/shims/python
;;
;; pyright:
;;
;;    % brew install node
;;    % npm install -g pyright
;;    % which pyright-langserver
;;
;; setup project
;;
;;    % cd my_project
;;    % python -m venv venv
;;    % source venv/bin/activate
;;
;; start git project so elglot sees it
;;
;;    % git init
;;
;; install needed packages
;;
;;    % pip install "ruff" "pyright" "pytest" "typer[all]"
;;


;;;;;;;;;;;;;;;; WORKFLOW ;;;;;;;;;;;;;;;


;; Check for linting errors
;;    % ruff check .
;;
;; Check for formatting issues (will pass if you've been saving in Emacs)
;;    % ruff format --check .
;;;
;; Check for type errors
;;    % pyright .
;;


;; sometimes needed, reset eglot:
;;   M-x eglot


;;;;;;;;;;;;;;;; INSTALL EMACS STUFF ;;;;;;;;;;;;


;; M-x package-install RET eglot RET
;; M-x package-install RET ruff-format RET
;; M-x package-install RET pytest RET

(require 'project)
(require 'eglot)



;; Tell eglot which language server to start for Python files.
(add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
;; Automatically start eglot whenever you open a Python file.
(add-hook 'python-mode-hook #'eglot-ensure)


;; Automatically enable format-on-save using ruff.
(require 'ruff-format)
(add-hook 'python-mode-hook #'ruff-format-on-save-mode)

;; pytest
(require 'pytest)

;; This ensures the keybindings are set only after the pytest package is loaded.
;; (with-eval-after-load 'pytest
;;   (define-key python-mode-map (kbd "C-c C-t") 'pytest-one)      ; Run test at point
;;   (define-key python-mode-map (kbd "C-c C-f") 'pytest-file)     ; Run tests in file
;;   (define-key python-mode-map (kbd "C-c C-p") 'pytest-project)) ; Run all tests



;; This line tells Emacs to apply the ANSI color filter to any output
;; that appears in a compilation buffer (like the one pytest uses).
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)




