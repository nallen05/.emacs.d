



;; M-x necromancer-mode     turn on/off mode to show variables in mode line


;; C-c M             edit model
;; C-c g             edit goal
;; C-c r             set role
;; C-c j             set job/mode
;; C-c n             set role+job combo
;; C-c c             edit context
;; C-c i             set input manner (-1=region/task, 0=+goal/context, 1=+preamble, 2=+postamble)
;; C-c o             set output manner (-1=above, 0=overwrite, 1=below, 2=eof, 3=sidebar, DEL=no_op)
;; C-c RET           run necromancer (it will first launch edit task)
;; C-c s             open sidebar buffer (this window)
;; C-c S             change sidebar buffer
;;
;; C-c s <var>       within this buffer, shadow <var>
;; C-c S <var>       kill the buffer local shadow; global will be used instead


;;;;;;;;;;;;; WITHIN EDIT STRING MENU ;;;;;;;;;;;;;;;;;;
;;
;; C-c DEL           abort
;; C-c RET           submit
;; C-c f             insert file reference
;; C-c b             insert buffer reference


;; SCRATCH
;;  * gptel-abort



;; filesystem

(defvar necromancer--project-dir "~/"
  "used for expanding references inside context")

(defvar necromancer--template-dir "~/.emacs.d/templates/"
  "for finding templates needed to build prompts")


;; configuring gptel backends

(defvar necromancer--model nil)

(defvar necromancer--model-configs
  '((:name "gemini-2.5-pro"        :nickname "pro"         :provider :gemini      :api-key-env "GEMINI_API_KEY")
    (:name "gemini-2.5-flash"      :nickname "flash"       :provider :gemini      :api-key-env "GEMINI_API_KEY")
    (:name "gemini-2.5-flash-lite" :nickname "flash-light" :provider :gemini      :api-key-env "GEMINI_API_KEY")
    (:name "claude-opus-4-1"         :nickname "opus-4.1"    :provider :anthropic   :api-key-env "ANTHROPIC_API_KEY")
    (:name "claude-opus-4"           :nickname "opus-4"      :provider :anthropic   :api-key-env "ANTHROPIC_API_KEY")
    (:name "claude-sonnet-4-5"       :nickname "sonnet-4.5"  :provider :anthropic   :api-key-env "ANTHROPIC_API_KEY")
    (:name "claude-sonnet-4"         :nickname "sonnet-4"    :provider :anthropic   :api-key-env "ANTHROPIC_API_KEY")
    (:name "claude-3-7-sonnet-latest" :nickname "sonnet-3.7"  :provider :anthropic   :api-key-env "ANTHROPIC_API_KEY")
    (:name "claude-3-5-haiku-latest"  :nickname "haiku-3.5"   :provider :anthropic   :api-key-env "ANTHROPIC_API_KEY")))
    
(defvar necromancer--gemini-backend nil)
(defvar necromancer--anthropic-backend nil)

;;; Helper functions:

(defun necromancer--get-config-by (key value)
  "Find a model configuration from NECROMANCER--MODEL-CONFIGS."
  (catch 'found
    (dolist (config necromancer--model-configs nil)
      (when (equal (plist-get config key) value)
        (throw 'found config)))))

(defun necromancer--hydrate-model (nickname)
  "Given a NICKNAME, return the full model name string."
  (let ((config (necromancer--get-config-by :nickname nickname)))
    (when config
      (plist-get config :name))))

(defun necromancer--dehydrate-model (name)
  "Given a full MODEL-NAME, return its nickname string."
  (let ((config (necromancer--get-config-by :name name)))
    (when config
      (plist-get config :nickname))))

(defun necromancer--get-available-model-nicknames ()
  "Return a list of all model nicknames for completion."
  (mapcar (lambda (config) (plist-get config :nickname))
          necromancer--model-configs))

(defun necromancer--set-model ()
  "Set `necromancer--model` interactively with completion."
  (interactive)
  (let* ((nicknames (necromancer--get-available-model-nicknames))
         (current-nickname (necromancer--dehydrate-model necromancer--model))
         (choice (completing-read (concat "Select model (current: "
                                          (propertize current-nickname 'face 'bold)
                                          "): ")
                                  nicknames
                                  nil ; predicate
                                  t   ; require-match
                                  nil ; initial-input
                                  )))
    (when (and choice (not (string-empty-p choice)))
      (let ((full-name (necromancer--hydrate-model choice)))
        (if full-name
            (progn
              (setq necromancer--model full-name)
              (message "Model set to: %s" choice))
          (message "Unknown model nickname: %s" choice))))))

(defun necromancer--initialize-gptel-backends ()
  "Initialize GPTel backend instances for known providers if they are not already set.
API keys are retrieved from their respective environment variables.
Warnings are logged if an API key is missing for a provider."
  
  ;; Initialize Gemini backend
  (let ((api-key (getenv "GEMINI_API_KEY")))
    (if api-key
        (setq necromancer--gemini-backend
              (gptel-make-gemini "Gemini" :key api-key :stream t))
      (error "MISSING GEMINI_API_KEY")))

  ;; Initialize Anthropic backend
  (let ((api-key (getenv "ANTHROPIC_API_KEY")))
    (if api-key
        (setq necromancer--anthropic-backend
              (gptel-make-anthropic "Anthropic" :key api-key :stream t))
      (error "MISSING ANTHROPIC_API_KEY"))))

(defun necromancer--configure-gptel-backend-and-model ()
  "Set `gptel-backend` and `gptel-model` based on `necromancer--model`."
  (unless necromancer--model
    (error "NECROMANCER--MODEL not set!"))

  (let* ((config (necromancer--get-config-by :name necromancer--model))
         (provider (plist-get config :provider)))
    (cond
      ((eq provider :gemini)     (setq gptel-backend necromancer--gemini-backend))
      ((eq provider :anthropic) (setq gptel-backend necromancer--anthropic-backend))
      (t (error "Unknown provider '%s' for model '%s'" provider necromancer--model)))

    (setq gptel-model necromancer--model)))

;; Run initialization at load time.
(necromancer--initialize-gptel-backends)



;; (gptel-make-gemini "Gemini"
;;   :key 'gptel-api-key  ; or specify your Gemini API key
;;   :stream t)

;; (setq gptel-backend
;;       (gptel-make-gemini "Gemini"
;;         :key (getenv "GEMINI_API_KEY")
;;         :stream t))

;; (setq gptel-model "gemini-2.5-flash-lite")





;; buffers

(defvar necromancer--sidebar-buffer "*necromancer-sidebar*"
  "Responses can get routed here to avoid modifying the source buffer.")

(defvar necromancer--edit-string-buffer "*necromancer-edit-string*"
  "Ephemeral buffer for editing multi-line strings.")

(defvar necromancer--system-prompt-buffer "*necromancer-system-prompt*"
  "User system gets built here before sending. Exposed for troubleshooting.")

(defvar necromancer--user-prompt-buffer "*necromancer-user-prompt*"
  "User prompt gets built here before sending. Exposed for troubleshooting.")

(defvar necromancer--thinking-buffer "*necromancer-thinking*"
  "Reasoning model outputs get redirected here. Cleared upon each new request.")


;; setting variables to be buffer-local instead of global

(defun necromander--make-variable-local (variable)
  "Make VARIABLE buffer-local in the current buffer only."
  (interactive
   (list (intern
          (completing-read "Make buffer-local: "
                          obarray
                          #'boundp
                          t))))
  (make-local-variable variable)
  (message "Made '%s' buffer-local in current buffer" variable))

(defun necromancer--kill-local-variable (variable)
  "Kill the buffer-local binding of VARIABLE in the current buffer."
  (interactive
   (list (intern
          (completing-read "Kill local variable: "
                          obarray
                          #'boundp
                          t))))
  (kill-local-variable variable)
  (message "Killed buffer-local binding of '%s'" variable))


(global-set-key (kbd "C-c s") 'necromancer--make-variable-local)
(global-set-key (kbd "C-c S") 'necromancer--kill-local-variable)


;; input: model

(defvar necromancer--model "gemini-2.5-flash-lite")

(defvar necromancer--known-models
  '("gemini-2.5-pro"
    "gemini-2.5-flash"
    "gemini-2.5-flash-lite"))

(defun necromancer--set-model ()
  (interactive)
  (let ((choice (completing-read (concat "Select model (current: "
			                                   (propertize necromancer--model 'face 'bold)
			                                   ") :")
                                 necromancer--known-models
                                 nil                      ;; predicate (not needed here)
                                 t                        ;; require match
                                 nil)))                   ;; no initial value
    (setq necromancer--model
          (cond
           ((member choice necromancer--known-models) choice)
           ((member necromancer--model necromancer--known-models) necromancer--model)
           (t (first necromancer--known-models))))
    (message "Model set to: %s" necromancer--model)))

(global-set-key (kbd "C-c M") 'necromancer--set-model)


;; inputs: goal & context

(defun necromancer--read-multiline-string (prompt &optional initial-value)
  (let ((buffer-name necromancer--edit-string-buffer)
	(edit-window nil)
	(result nil))
   (unwind-protect
       (progn
	 (with-current-buffer (get-buffer-create buffer-name)
	   (erase-buffer)
	   (when initial-value
	     (insert initial-value))
	   (text-mode)

	   (use-local-map (copy-keymap text-mode-map))

	   ;; Prevent recentering and blank space scrolling
           (setq-local scroll-conservatively 101)  ; Never recenter
           (setq-local scroll-margin 0)            ; No margin around point
           (setq-local maximum-scroll-margin 0)    ; Disable margin
           (setq-local scroll-step 1)              ; Scroll one line at a time

	    
	    ;; keybindings
	    (local-set-key (kbd "C-c <return>")
			   (lambda ()
			     (interactive)
			     (exit-recursive-edit)))
	    (local-set-key (kbd "C-c DEL")
			   (lambda ()
			     (interactive)
			     (abort-recursive-edit)))
	    (local-set-key (kbd "C-c f")
			   'necromancer--insert-file-to-context)
	    (local-set-key (kbd "C-c b")
			   'necromancer--insert-buffer-to-context)
	    (message "%s C-c RET to finish; C-c DEL to cancel" prompt)
	    
	    ;; Display with initial reasonable size
	    (setq edit-window
                  (display-buffer-at-bottom
                   (current-buffer)
                   '((window-height . 10))))
            (select-window edit-window)
	    
	    ;; Add hook to resize after each change
            (add-hook 'post-command-hook
                      (lambda ()
			(when (and (eq (current-buffer) (get-buffer buffer-name))
				   (get-buffer-window (current-buffer)))
                          (fit-window-to-buffer 
                           (get-buffer-window (current-buffer))
                           15   ; max 15 lines
                           5))) ; min 5 lines
                      nil t)  ; Buffer-local hook
	    
	    (select-window edit-window)

	   
	    ;; Handle abort
	    (condition-case nil
		(progn
		  (recursive-edit)
		  ;; ensure we're still in the right buffer
		  (when (buffer-live-p (get-buffer buffer-name))
		    (with-current-buffer buffer-name
		      (setq result 
			    (buffer-substring-no-properties (point-min) (point-max))))))
	      (quit (setq result nil)))))

     ;; Cleanup: window first, then buffer
     (when (and edit-window (window-live-p edit-window))
       (delete-window edit-window))
     (when (get-buffer buffer-name)
       (kill-buffer buffer-name)))
   
   result))

(defvar necromancer--goal nil)

(defun necromancer--edit-goal ()
  (interactive)
  (let ((new-value (necromancer--read-multiline-string 
                    "Edit goal: " 
                    necromancer--goal)))
    (when new-value  ; Only update if not aborted
      (setq necromancer--goal new-value)
      (message "Goal updated"))))

(global-set-key (kbd "C-c g") 'necromancer--edit-goal)

(defvar necromancer--context nil)

(defun necromancer--edit-context ()
  (interactive)
  (let ((new-value (necromancer--read-multiline-string 
                    "Edit context: " 
                    necromancer--context)))
    (when new-value  ; Only update if not aborted
      (setq necromancer--context new-value)
      (message "Context updated"))))

(global-set-key (kbd "C-c c") 'necromancer--edit-context)



;; inserting files & buffers into the context

(defun necromancer--insert-file-reference ()
  (interactive)
  (let ((file (read-file-name "Select a file to insert: "
			      (expand-file-name necromancer--project-dir))))
    (when file
      (insert (format "{{%s}}" file)))))

;; (global-set-key (kbd "C-c f") 'necromancer--insert-file-to-context)

(defun necromancer--insert-buffer-reference ()
  (interactive)
  (let ((buffer-name (read-buffer "Select a buffer to insert: "
                                  nil  ; no default buffer
                                  t))) ; require match to existing buffer
    (when buffer-name
      (insert (format "{{%s}}" buffer-name)))))

;; (global-set-key (kbd "C-c b") 'necromancer--insert-buffer-to-context)


(defvar necromancer--reference-prefix-template
  "#==============================\n# %s: %s\n#==============================\n")

(defun necromancer--expand-references (input-string)
  "Replace {{xxx}} patterns with buffer or file contents.
   * First checks for a buffer, then a file
   * NECROMANCER--REFERENCE-PREFIX-TEMPLATE is used to annote the insertion "
  (let ((result input-string)
        (pos 0))
    ;; Repeatedly search for {{...}} patterns
    (while (string-match "{{\\([^}]+\\)}}" result pos)
      (let* ((full-match (match-string 0 result))
             (name (match-string 1 result))
             (start (match-beginning 0))
             (end (match-end 0))
             (buffer (get-buffer name)))
        (cond
         ;; First check for buffer
         (buffer
          (let* ((contents (with-current-buffer buffer
                            (buffer-string)))
                 (header (format necromancer--reference-prefix-template
				 "Buffer"
				 name))
                 (replacement (concat header contents)))
            (setq result (concat (substring result 0 start)
                               replacement
                               (substring result end)))
            ;; Move past the replacement to avoid re-matching
            (setq pos (+ start (length replacement)))))
         
         ;; Then check for file
         (t
          (let ((filepath (expand-file-name name necromancer--project-dir)))
            (if (file-exists-p filepath)
                ;; Replace with header + contents
                (let* ((contents (with-temp-buffer
                                  (insert-file-contents filepath)
                                  (buffer-string)))
                       (header (format necromancer--reference-prefix-template
				       "File"
                                       name))
                       (replacement (concat header contents)))
                  (setq result (concat (substring result 0 start)
                                     replacement
                                     (substring result end)))
                  ;; Move past the replacement to avoid re-matching
                  (setq pos (+ start (length replacement))))
              ;; Neither buffer nor file exists, skip this match
              (setq pos end)))))))
    result))








;; inputs: role

(defvar necromancer--role "dev")

(defvar necromancer--known-roles
  '("analyst"
    "architect"
    "dev"
    "mlops"
    "panel"
    "python"
    "sre"
    "staff"))

(defvar necromancer--role-map
  (make-sparse-keymap "[a]analyst [A]rchitect [d]ev [p]ython_dev [s]re [S]taff_eng [m]lops"))

(define-key necromancer--role-map (kbd "a")
  (lambda ()
    (interactive)
    (setq necromancer--role "analyst")
    (force-mode-line-update t)
    (message "Product Analyst = Problem -> Requirements")))

(define-key necromancer--role-map (kbd "A")
  (lambda ()
    (interactive)
    (setq necromancer--role "architect")
    (force-mode-line-update t)
    (message "Software solutions architect")))

(define-key necromancer--role-map (kbd "d")
  (lambda ()
    (interactive)
    (setq necromancer--role "dev")
    (force-mode-line-update t)
    (message "Pragmatic developer (polyglot)")))

(define-key necromancer--role-map (kbd "m")
  (lambda ()
    (interactive)
    (setq necromancer--role "mlops")
    (force-mode-line-update t)
    (message "ML Infrastructure Engineer")))

(define-key necromancer--role-map (kbd "p")
  (lambda ()
    (interactive)
    (setq necromancer--role "dev")
    (force-mode-line-update t)
    (message "Pragmatic developer (Python 3.13+)")))

(define-key necromancer--role-map (kbd "s")
  (lambda ()
    (interactive)
    (setq necromancer--role "staff")
    (force-mode-line-update t)
    (message "Staff Engineer for expert code/design reviews")))

(define-key necromancer--role-map (kbd "S")
  (lambda ()
    (interactive)
    (setq necromancer--role "sre")
    (force-mode-line-update t)
    (message "Site Reliability Enginer (SRE)")))

(global-set-key (kbd "C-c r") necromancer--role-map)




;; inputs: job

(defvar necromancer--mode "code")

(defvar necromancer--known-modes
  '("answer"
    "code"
    "component"
    "requirement"
    "review_code"
    "review_design"
    "panel"
    "sketch"))

(defvar necromancer--mode-map
  (make-sparse-keymap "[r]equirement [d]esign_code [D]esign_big [c]code [C]ode_review [R]isk [p]anel [q]&a"))

(define-key necromancer--mode-map (kbd "r")
  (lambda ()
    (interactive)
    (setq necromancer--mode "requirement")
    (force-mode-line-update t)
    (message "Requirements analysis")))

(define-key necromancer--mode-map (kbd "d")
  (lambda ()
    (interactive)
    (setq necromancer--mode "component")
    (force-mode-line-update t)
    (message "Per-component design (psuedocode)")))

(define-key necromancer--mode-map (kbd "D")
  (lambda ()
    (interactive)
    (setq necromancer--mode "sketch")
    (force-mode-line-update t)
    (message "Big picture systems design (pseudocode)")))
  
(define-key necromancer--mode-map (kbd "c")
  (lambda ()
    (interactive)
    (setq necromancer--mode "code")
    (force-mode-line-update t)
    (message "Write code!!")))

(define-key necromancer--mode-map (kbd "C")
  (lambda ()
    (interactive)
    (setq necromancer--mode "review_code")
    (force-mode-line-update t)
    (message "Code review")))

(define-key necromancer--mode-map (kbd "R")
  (lambda ()
    (interactive)
    (setq necromancer--mode "review_design")
    (force-mode-line-update t)
    (message "Design review. Risk & gotchas")))

(define-key necromancer--mode-map (kbd "q")
  (lambda ()
    (interactive)
    (setq necromancer--mode "answer")
    (force-mode-line-update t)
    (message "Q&A mode")))

(global-set-key (kbd "C-c j") necromancer--mode-map)


;; don't change these, they match strings hard coded in templates
(defun necromancer--annotate-task ()
  (cond
   ((string-equal necromancer--mode "answer")        "QUERY")
   ((string-equal necromancer--mode "code")          "CODING TASK")
   ((string-equal necromancer--mode "component")     "ENGINEERING DESIGN TASK")
   ((string-equal necromancer--mode "panel")         "DISCUSSION")
   ((string-equal necromancer--mode "review_code")   "CODE REVIEW TASK")
   ((string-equal necromancer--mode "review_design") "ENGINEERING REVIEW TASK")
   ((string-equal necromancer--mode "sketch")        "SOLUTION DESIGN TASK")))

;; don't change these, they match strings hard coded in templates
(defun necromancer--annotate-region ()
  (cond
   ((string-equal necromancer--mode "answer")        "QUERY CONTEXT")
   ((string-equal necromancer--mode "code")          "REFACTOR")
   ((string-equal necromancer--mode "component")     "DESIGN REFINEMENT")
   ((string-equal necromancer--mode "panel")         "DISCUSSION FOCUS")
   ((string-equal necromancer--mode "review_code")   "CODE FOR REVIEW")
   ((string-equal necromancer--mode "review_design") "ANALYSIS FOCUS")
   ((string-equal necromancer--mode "sketch")        "DESIGN REFINEMENT")))





;; inputs: task + role combos

(defvar necromancer--known-combos
  '("answer"
    "code"
    "component"
    "requirement"
    "review_code"
    "review_design"
    "panel"
    "sketch"))

(defvar necromancer--mode-map
  (make-sparse-keymap "[r]equirement [d]esign_code [D]esign_arch [c]code [C]ode_review [R]isk [p]anel [q]&a"))

(define-key necromancer--mode-map (kbd "r")
  (lambda ()
    (interactive)
    (setq necromancer--mode "requirement")
    (force-mode-line-update t)
    (message "Requirements analysis")))

(define-key necromancer--mode-map (kbd "d")
  (lambda ()
    (interactive)
    (setq necromancer--mode "sketch")
    (force-mode-line-update t)
    (message "Per-component design (psuedocode)")))

(define-key necromancer--mode-map (kbd "D")
  (lambda ()
    (interactive)
    (setq necromancer--mode "sketch")
    (force-mode-line-update t)
    (message "Big picture systems design (pseudocode)")))
  
(define-key necromancer--mode-map (kbd "c")
  (lambda ()
    (interactive)
    (setq necromancer--mode "code")
    (force-mode-line-update t)
    (message "Write code!!")))

(define-key necromancer--mode-map (kbd "C")
  (lambda ()
    (interactive)
    (setq necromancer--mode "review_code")
    (force-mode-line-update t)
    (message "Code review")))

(define-key necromancer--mode-map (kbd "R")
  (lambda ()
    (interactive)
    (setq necromancer--mode "review_design")
    (force-mode-line-update t)
    (message "Design review. Risk & gotchas")))

(define-key necromancer--mode-map (kbd "q")
  (lambda ()
    (interactive)
    (setq necromancer--mode "answer")
    (force-mode-line-update t)
    (message "Q&A mode")))

(global-set-key (kbd "C-c j") necromancer--mode-map)





(defvar necromancer--known-combos
  '("answer"
    "code"
    "component"
    "requirement"
    "review_code"
    "review_design"
    "panel"
    "sketch"))

(defvar necromancer--combo-map
  (make-sparse-keymap "[r]equirement [d]esign_code [D]esign_arch [c]code [C]ode_review [R]isk [p]anel [q]&a"))

(define-key necromancer--combo-map (kbd "r")
  (lambda ()
    (interactive)
    (setq necromancer--role "analyst")
    (setq necromancer--mode "requirement")
    (force-mode-line-update t)
    (message "Requirements analysis by product analyst")))

(define-key necromancer--combo-map (kbd "d")
  (lambda ()
    (interactive)
    (setq necromancer--role "python")
    (setq necromancer--mode "component")
    (force-mode-line-update t)
    (message "Per-component design (psuedocode) by Python dev")))

(define-key necromancer--combo-map (kbd "D")
  (lambda ()
    (interactive)
    (setq necromancer--role "architect")
    (setq necromancer--mode "sketch")
    (force-mode-line-update t)
    (message "Big picture systems design (pseudocode) by solution architect")))
  
(define-key necromancer--combo-map (kbd "c")
  (lambda ()
    (interactive)
    (setq necromancer--role "python")
    (setq necromancer--mode "code")
    (force-mode-line-update t)
    (message "Write code by Python dev")))

(define-key necromancer--combo-map (kbd "C")
  (lambda ()
    (interactive)
    (setq necromancer--role "staff")
    (setq necromancer--mode "review_code")
    (force-mode-line-update t)
    (message "Code review, by senior staff engineer")))

(define-key necromancer--combo-map (kbd "R")
  (lambda ()
    (interactive)
    (setq necromancer--role "staff")
    (setq necromancer--mode "review_design")
    (force-mode-line-update t)
    (message "Design review. Risk & gotchas. By senior staff engineer")))

(define-key necromancer--combo-map (kbd "q")
  (lambda ()
    (interactive)
    ;; no default role for Q&A
    (setq necromancer--mode "answer")
    (force-mode-line-update t)
    (message (format "Q&A by %s" necromancer--role))))

(define-key necromancer--combo-map (kbd "p")
  (lambda ()
    (interactive)
    (setq necromancer--role "panel")
    (setq necromancer--mode "panel")
    (force-mode-line-update t)
    (message (format "Panel of experts: Solution Architect + SRE + ML Infra Expert"))))

(global-set-key (kbd "C-c n") necromancer--combo-map)





;; input: input-manner

(defvar necromancer--input-manner nil
  "controls how context is sent to necromancer:
     -1     Least amount of context. Only the TASK description & selected REGION
     0      Also include GOAL & CONTEXT
     1      Also include the current buffer PREAMBLE (range before the point/region)
     2      Also include the current buffer POSTAMBLE (range after the point/region)
     NIL    If region selected, defaults to 0; otherwise defaults to 1
   ")

(defvar necromancer--input-manner-map
  (make-sparse-keymap "Include context: [n]one [c]ontext [p]reamble [t]ail [RET]accept_defaults"))

(define-key necromancer--input-manner-map (kbd "n")
  (lambda ()
    (interactive)
    (setq necromancer--input-manner -1)
    (force-mode-line-update t)
    (message "No context. Just selected region & task, as provided")))

(define-key necromancer--input-manner-map (kbd "c")
  (lambda ()
    (interactive)
    (setq necromancer--input-manner 0)
    (force-mode-line-update t)
    (message "Include goal & context")))

(define-key necromancer--input-manner-map (kbd "p")
  (lambda ()
    (interactive)
    (setq necromancer--input-manner 1)
    (force-mode-line-update t)
    (message "Include goal, context, & preamble")))

(define-key necromancer--input-manner-map (kbd "t")
  (lambda ()
    (interactive)
    (setq necromancer--input-manner 2)
    (force-mode-line-update t)
    (message "Include goal, context, & the entire source buffer")))

(define-key necromancer--input-manner-map (kbd "RET")
  (lambda ()
    (interactive)
    (setq necromancer--input-manner nil)
    (force-mode-line-update t)
    (message "Accept defaults")))

(global-set-key (kbd "C-c i") necromancer--input-manner-map)





;; input: output-manner

(defvar necromancer--output-manner 1
  "controls how responses are inserted:
     -1     insert ABOVE selected region
     0      OVERWRITE selected region
     1      insert AFTER selected region
     2      insert at END of current buffer
     3      insert at end of SIDEBAR buffer
     NIL    no_op")

(defvar necromancer--output-manner-map
  (make-sparse-keymap "Insert responses: [a]bove [o]verwrite [b]elow [e]of [s]idebar [RET]default/no_op"))

(define-key necromancer--output-manner-map (kbd "a")
  (lambda ()
    (interactive)
    (setq necromancer--output-manner -1)
    (force-mode-line-update t)
    (message "Completion will be inserted ABOVE the selected region")))

(define-key necromancer--output-manner-map (kbd "o")
  (lambda ()
    (interactive)
    (setq necromancer--output-manner 0)
    (force-mode-line-update t)
    (message "Completion will OVERWRITE the selected region")))

(define-key necromancer--output-manner-map (kbd "b")
  (lambda ()
    (interactive)
    (setq necromancer--output-manner 1)
    (force-mode-line-update t)
    (message "Completion will be inserted BELOW the selected region")))

(define-key necromancer--output-manner-map (kbd "e")
  (lambda ()
    (interactive)
    (setq necromancer--output-manner 2)
    (force-mode-line-update t)
    (message "Completion will be inserted at the END OF THE BUFFER")))

(define-key necromancer--output-manner-map (kbd "s")
  (lambda ()
    (interactive)
    (setq necromancer--output-manner 3)
    (force-mode-line-update t)
    (message "Completion will be inserted at the end of the SIDEBAR buffer")))

(define-key necromancer--output-manner-map (kbd "DEL")
  (lambda ()
    (interactive)
    (setq necromancer--output-manner nil)
    (force-mode-line-update t)
    (message "no_op")))

(global-set-key (kbd "C-c o") necromancer--output-manner-map)


;; input: task

(defvar necromancer--task nil)

(defun necromancer--edit-task ()
  (interactive)
  (let ((new-value (necromancer--read-multiline-string 
                    (format "Edit %s: " (necromancer--annotate-task))
		    nil)))
    (when new-value  ; Only update if not aborted
      (setq necromancer--task new-value)
      (message (format "%s updated" (necromancer--annotate-task)))
      t)))


;; building prompts
(defun necromancer--build-system-prompt ()
  (let ((buffer-name necromancer--system-prompt-buffer)
        (role-file (expand-file-name 
                    (concat "role_" necromancer--role ".txt")
                    necromancer--template-dir))
        (mode-file (expand-file-name 
                    (concat "mode_" necromancer--mode ".txt")
                    necromancer--template-dir))
	(region-file (when (use-region-p)
		       (expand-file-name 
			(concat "region_" necromancer--mode ".txt")
			necromancer--template-dir))))
    ;; Work with buffer without displaying it
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert-file-contents role-file)
      (goto-char (point-max))
      (insert "\n")
      (insert-file-contents mode-file)
      (when region-file
	(goto-char (point-max))
	(insert "\n")
        (insert-file-contents region-file)))))

(defun necromancer--string-empty-p (str)
  (or (zerop (length str))
      (string-empty-p (string-trim str))))

(defun necromancer--build-user-prompt (&optional input)
  (let ((buffer-name necromancer--user-prompt-buffer)
        (source-buffer (current-buffer))
        (input (or input 1))
        ;; Capture region info before switching buffers
        (has-region (use-region-p))
        (region-start (and (use-region-p) (region-beginning)))
        (region-end (and (use-region-p) (region-end))))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)

      ;; if input >= 0, insert goal string ------------------------------------
      (when (>= input 0)
        (unless (necromancer--string-empty-p necromancer--goal)
          (insert "# GOAL \n")
          (insert (necromancer--expand-references necromancer--goal))
	  (insert "\n")))

      ;; if input >= 0, insert context string ---------------------------------
      (when (and (>= input 0)
		 (not (necromancer--string-empty-p necromancer--context)))
	(insert "\n")
	(insert "\n# CONTEXT\n")
        (insert (necromancer--expand-references necromancer--context))
	(insert "\n"))

      (if (<= input 0)
	  
	  ;; if input <= 0, insert just the region -----------------------------
	  (when has-region
	    (when (necromancer--string-empty-p necromancer--task)
	      (insert "\n")
	      (insert (format "# %s \n" (necromancer--annotate-task))))
	    (insert (format "\n# START %s\n"
			    (necromancer--annotate-region)))
            (insert (with-current-buffer source-buffer
		      (buffer-substring-no-properties region-start region-end)))
	    (insert (format "\n# END %s\n"
			    (necromancer--annotate-region)))
	    (insert "\n"))

	(when (>= input 1)
	  (when (necromancer--string-empty-p necromancer--task)
	    (insert "\n")
	    (insert (format "# %s \n" (necromancer--annotate-task))))
	  (let ((current-file-name (buffer-file-name source-buffer)))
	    (if has-region
		
		;; input >= 1 && region, so insert annotated source buffer -------
		(progn 
		  (insert (format "\n## %s: %s"
				  (if current-file-name "File" "Buffer")
				  (or current-file-name (buffer-name source-buffer))))
		  (insert "\n```")
		  (insert (with-current-buffer source-buffer
			    (buffer-substring-no-properties (point-min) region-start)))
		  (insert (format "\n# START %s\n"
				  (necromancer--annotate-region)))
		  (insert (with-current-buffer source-buffer
			    (buffer-substring-no-properties region-start region-end)))
		  (insert (format "\n# END %s\n"
				  (necromancer--annotate-region)))
		  (when (>= input 2)
		    (insert (with-current-buffer source-buffer
			      (buffer-substring-no-properties region-end (point-max)))))
		(insert "\n```")
		(insert "\n"))

	      
	      ;; input >=1 && no region, so insert raw source buffer  -----------
	      (progn
		(insert (format "\n## %s: %s"
				(if current-file-name "File" "Buffer")
				(or current-file-name (buffer-name source-buffer))))
		(insert "\n```")
		(insert (with-current-buffer source-buffer
			  (buffer-substring-no-properties (point-min) (point))))
		
		;; if requested, also add postamble
		(when (>= input 2)
		  (insert (with-current-buffer source-buffer
			    (buffer-substring-no-properties (point) (point-max))))
		  (insert "\n"))
		(insert "\n```")
		(insert "\n"))))))

      ;; insert the TASK ----------------------------------------------------------
      (unless (necromancer--string-empty-p necromancer--task)
	(insert "\n")
	(insert (format "# %s \n" (necromancer--annotate-task)))
	(insert (necromancer--expand-references necromancer--task))
	(insert "\n")))))



;; the sidebar buffer
(defun necromancer--open-sidebar ()
  (interactive)
  (let ((buffer (get-buffer-create necromancer--sidebar-buffer)))
    (switch-to-buffer-other-window buffer)))

(global-set-key (kbd "C-c s") 'necromancer--open-sidebar)

(defun necromancer--redirect-sidebar ()
  (interactive)
  (let ((buffer-name (read-buffer "Configure sidebar buffer: "
                                  necromancer--sidebar-buffer
                                  t))) ; require match to existing buffer
    (when buffer-name
      (setq necromancer--sidebar-buffer buffer-name))))

(global-set-key (kbd "C-c S") 'necromancer--redirect-sidebar)





;; sending requests to LLM

(defun necromancer--send ()
  "Send context buffer + region/buffer-to-point to LLM, rewrite selection.
The behavior of insertion is controlled by NECROMANCER--OUTPUT-MANNER:
- NIL: Does all setup but does not send the GPTEL request.
- -1: Inserts ABOVE the selected region (or at point if no region).
- 0: Rewrites the selected region (or inserts at point if no region).
- 1: Inserts BELOW the selected region (or at point if no region).
- 2: Inserts at the end of the current buffer.
- 3: Inserts at the end of the buffer specified by `necromancer--sidebar-buffer`.
     The sidebar buffer is created if it doesn't exist and cleared if it does.
     The current buffer is not switched."
  (interactive)
;;  (setq gptel-model necromancer--model)
  (necromancer--configure-gptel-backend-and-model)
  (setq gptel--num-messages-to-send 1)
  (let* ((system-prompt-buffer (get-buffer necromancer--system-prompt-buffer))
         (system-prompt-text (with-current-buffer system-prompt-buffer
                               (buffer-substring-no-properties (point-min) (point-max))))
         (user-prompt-buffer (get-buffer necromancer--user-prompt-buffer))
         (user-prompt-text (with-current-buffer user-prompt-buffer
                               (buffer-substring-no-properties (point-min) (point-max))))
         (has-region (use-region-p))
         (region-start (if has-region (region-beginning) nil))
         (region-end (if has-region (region-end) nil))
         (original-point (point)) ; Capture original point for restoration or default insertion
         (effective-insert-point nil)
         (should-delete-region nil)
         (target-buffer (current-buffer))
         (target-position-marker (point-marker)))
    (setq gptel--system-message system-prompt-text)

    ;; Determine insertion strategy 
    (cond
     ((null necromancer--output-manner)   ;; no op
      (message "Necromancer: no_op")
      (goto-char original-point))
     ((eq necromancer--output-manner 0)   ;; rewrite
      (setq should-delete-region has-region)
      (setq effective-insert-point (if has-region region-start original-point)))
     ((eq necromancer--output-manner -1)  ;; insert above
      (setq effective-insert-point (if has-region region-start original-point)))
     ((eq necromancer--output-manner 1)   ;; insert below
      (setq effective-insert-point (if has-region region-end original-point)))
     ((eq necromancer--output-manner 2)    ;; insert at end of current buffer
      (setq effective-insert-point (point-max)))
     ((eq necromancer--output-manner 3)   ;; insert at end of sidebar buffer
      (let* ((sidebar-buf-name necromancer--sidebar-buffer)
             (sidebar-buf (get-buffer-create sidebar-buf-name)))
        ;; Perform operations on the sidebar buffer without switching to it
        (with-current-buffer sidebar-buf
          (erase-buffer) ; Clear existing content
          ;; Set the target buffer and a marker at its end for gptel-request
          (setq target-buffer sidebar-buf)
          (setq target-position-marker (copy-marker (point-max) t))
          ;; GPTEL-INCLUDE-REASONING is buffer local! needs to be set in the sidebar buffer
          (setq-local gptel-include-reasoning necromancer--thinking-buffer))))
     (t (error "Invalid NECROMANCER--OUTPUT-MANNER")))

    ;; setup in current buffer if called for
    (when (and necromancer--output-manner
               (not (eq necromancer--output-manner 3)))
      (when should-delete-region
        (delete-region region-start region-end))
      (goto-char effective-insert-point)
      ;; Update the target position marker after point movement or region deletion
      (setq target-position-marker (point-marker))
      (setq-local gptel-include-reasoning necromancer--thinking-buffer))

    ;; Only send the GPTEL request if output manner is not NIL
    (when necromancer--output-manner
      (message "Necromancing... (this may take a moment)")
      (gptel-request user-prompt-text
        :buffer target-buffer
        :position target-position-marker ; Insert at the calculated or specified point/marker
        :in-place t
        :stream t
        ))

    ;; Restore original point, if updates happened in this buffer
    (unless (eq necromancer--output-manner 3)
      (goto-char original-point))))








(defun necromancer (&optional skip-edit-task)
  (interactive)

  ;; set input defaults
  (unless necromancer--input-manner
    (setq necromancer--input-manner
          (if (use-region-p)
              0
            1)))
  (force-mode-line-update t)
  (when skip-edit-task
    (setf necromancer--task nil))
  (when (or skip-edit-task
            (necromancer--edit-task))
    (let ((thinking-buf (get-buffer-create necromancer--thinking-buffer)))
      (with-current-buffer thinking-buf
        (erase-buffer)
        (insert (format "\n# %s: \n" (necromancer--annotate-task)))
        (when necromancer--task
          (insert necromancer--task)
          (insert "\n"))
        (insert "\n")))
    (necromancer--build-system-prompt)
    (necromancer--build-user-prompt necromancer--input-manner)
    (necromancer--send))
  (setf necromancer--input-manner nil)
  )

(global-set-key (kbd "C-c <return>") 'necromancer)

(defun necromancer-skip-task ()
  (interactive)
  (necromancer t))

(global-set-key (kbd "C-c C-<return>") 'necromancer-skip-task)




(defun necromancer--generate-state-string ()
  (if necromancer-mode
      (format " Necro:%s,%s%s"
              (cond
               ((equal necromancer--role "architect") "arch")
               (t                                     necromancer--role))
              (cond
               ((equal necromancer--mode "answer")        "q&a")
               ((equal necromancer--mode "review_code")    "cr")
               ((equal necromancer--mode "review_design")  "dr")
               ((equal necromancer--mode "sketch")         "pseudo")
               (t                                          necromancer--mode))
              (if (or necromancer--input-manner
                      necromancer--output-manner)
                  (format ",%s/%s"
                          (cond
                            ((equal necromancer--input-manner -1) "n")
                            ((equal necromancer--input-manner 0)  "c")
                            ((equal necromancer--input-manner 1)  "p")
                            ((equal necromancer--input-manner 2)  "t"))
                          (cond
                           ((equal necromancer--output-manner -1) "b")
                           ((equal necromancer--output-manner 0)  "o")
                           ((equal necromancer--output-manner 1)  "a")
                           ((equal necromancer--output-manner 2)  "e")
                           ((equal necromancer--output-manner 3)  "s")))
                ""))
    ""))

;;   (force-mode-line-update)

(define-minor-mode necromancer-mode
  "Display Necromancer variables in the mode line"
  :lighter (:eval (necromancer--generate-state-string))
  :global t
  :init-value nil)





