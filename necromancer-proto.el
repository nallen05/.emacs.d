



;; C-c ~             edit model
;; C-c g             edit goal
;; C-c r             edit role
;; C-c m             edit mode
;; C-c c             edit context
;; C-c RET           run necromancer (it will first launch edit task)
;;
;; C-c s <var>       within this buffer, shadow <var>
;; C-c S <var>       kill the buffer local shadow; global will be used instead


;;;;;;;;;;;;; WITHIN EDIT STRING MENU ;;;;;;;;;;;;;;;;;;
;;
;; C-c DEL           abort
;; C-c -             run: no context. Only the TASK description & selected REGION
;; C-c 0             run: also include GOAL & CONTEXT
;; C-c 1             run: also include the current buffer PREAMBLE (range before the point/region).
;; C-c 2             run: also include the current buffer POSTAMBLE (range after the point/region).
;; C-c RET           submit
;; C-c f             insert file reference
;; C-c b             insert buffer reference


;; SCRATCH
;;  * gptel-abort
;;  *  custom prefix map: https://chatgpt.com/c/68e4365f-cd2c-8333-a5cf-3c5d22a0b1f9


(gptel-make-gemini "Gemini"
  :key 'gptel-api-key  ; or specify your Gemini API key
  :stream t)

(setq gptel-backend
      (gptel-make-gemini "Gemini"
        :key (getenv "GEMINI_API_KEY")
        :stream t))

(setq gptel-model "gemini-2.5-flash-lite")


;; main configuration

(defvar necromancer--project-dir "~/"
  "used for expanding references inside context")

(defvar necromancer--template-dir "~/.emacs.d/templates/"
  "for finding templates needed to build prompts")




;; buffers

(defvar necromancer--edit-string-buffer "*necromancer-edit-string*"
  "Ephemeral buffer for editing multi-line strings.")

(defvar necromancer--system-prompt-buffer "*necromancer-system-prompt*"
  "User system gets built here before sending. Exposed for troubleshooting.")

(defvar necromancer--user-prompt-buffer "*necromancer-user-prompt*"
  "User prompt gets built here before sending. Exposed for troubleshooting.")

(defvar necromancer--thinking-buffer "*necromancer-thinking*"
  "Reasoning model outputs get redirected here. Cleared upon each new request.")

(defvar necromancer--buffer-sidebar  "*necromancer-sidebar*"
  "Answers can be redirected here, so that it doesn't pollute the source file.")



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

(global-set-key (kbd "C-c ~") 'necromancer--set-model)


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
	    (local-set-key (kbd "C-c -")
			   (lambda ()
			     (interactive)
			     (setq necromancer--input-manner -1)
			     (exit-recursive-edit)))
	    (local-set-key (kbd "C-c 0")
			   (lambda ()
			     (interactive)
			     (setq necromancer--input-manner 0)
			     (exit-recursive-edit)))
	    (local-set-key (kbd "C-c 1")
			   (lambda ()
			     (interactive)
			     (setq necromancer--input-manner 1)
			     (exit-recursive-edit)))
	    (local-set-key (kbd "C-c 2")
			   (lambda ()
			     (interactive)
			     (setq necromancer--input-manner 2)
			     (exit-recursive-edit)))
	    (local-set-key (kbd "C-c RET")
			   (lambda ()
			     (interactive)
			     (setq necromancer--input-manner nil)
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








;; inputs: role & mode

(defvar necromancer--role "dev")

(defvar necromancer--known-roles
  '("architect"
    "dev"
    "mlops"
    "panel"
    "sre"
    "staff"))

(defun necromancer--set-role ()
  (interactive)
  (let ((choice (completing-read (concat "Select role (current: "
			                                   (propertize necromancer--role 'face 'bold)
			                                   "): ")
                                 necromancer--known-roles
                                 nil                      ;; predicate (not needed here)
                                 t                        ;; require match
                                 nil)))                   ;; no initial value
    (setq necromancer--role
          (cond
           ((member choice necromancer--known-roles) choice)
           ((member necromancer--role necromancer--known-roles) necromancer--model)
           (t (first necromancer--known-roles))))
    (message "Role set to: %s" necromancer--role)))

(global-set-key (kbd "C-c r") 'necromancer--set-role)

(defvar necromancer--mode "code")

(defvar necromancer--known-modes
  '("answer"
    "code"
    "review_code"
    "review_design"
    "panel"
    "sketch"))

(defun necromancer--set-mode ()
  (interactive)
  (let ((choice (completing-read (concat "Select mode (current: "
			                                   (propertize necromancer--mode 'face 'bold)
			                                   "): ")
                                 necromancer--known-modes
                                 nil                      ;; predicate (not needed here)
                                 t                        ;; require match
                                 nil)))                   ;; no initial value
    (setq necromancer--mode
          (cond
           ((member choice necromancer--known-modes) choice)
           ((member necromancer--role necromancer--known-modes) necromancer--mode)
           (t (first necromancer--known-modes))))
    (message "Mode set to: %s" necromancer--mode)))

(global-set-key (kbd "C-c m") 'necromancer--set-mode)

;; don't change these, they match strings hard coded in templates
(defun necromancer--annotate-task ()
  (cond
   ((string-equal necromancer--mode "answer")        "QUERY")
   ((string-equal necromancer--mode "code")          "CODING TASK")
   ((string-equal necromancer--mode "panel")         "DISCUSSION")
   ((string-equal necromancer--mode "review_code")   "CODE REVIEW TASK")
   ((string-equal necromancer--mode "review_design") "DESIGN REVIEW TASK")
   ((string-equal necromancer--mode "sketch")        "DESIGN TASK")))

;; don't change these, they match strings hard coded in templates
(defun necromancer--annotate-region ()
  (cond
   ((string-equal necromancer--mode "answer")        "QUERY CONTEXT")
   ((string-equal necromancer--mode "code")          "REFACTOR")
   ((string-equal necromancer--mode "panel")         "DISCUSSION FOCUS")
   ((string-equal necromancer--mode "review_code")   "CODE FOR REVIEW")
   ((string-equal necromancer--mode "review_design") "DESIGN FOR REVIEW")
   ((string-equal necromancer--mode "sketch")        "DESIGN REFINEMENT")))
 




;; input: task

(defvar necromancer--task nil)

(defun necromancer--edit-task ()
  (interactive)
  (let ((new-value (necromancer--read-multiline-string 
                    (format "Edit %s: " (necromancer--annotate-task))
		    nil)))
    (when new-value  ; Only update if not aborted
      (setq necromancer--task new-value)
      (message (format "%s updated" (necromancer--annotate-task))))))


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

(defvar necromancer--progress-timer nil)
(defvar necromancer--progress-dots 0)

(defun necromancer--send ()
  "Send context buffer + region/buffer-to-point to LLM, rewrite selection."
  (interactive)
  (setq gptel-model necromancer--model)
  (setq gptel--num-messages-to-send 1)
  (let* ((system-prompt-buffer (get-buffer necromancer--system-prompt-buffer))
         (system-prompt-text (with-current-buffer system-prompt-buffer 
                               (buffer-substring-no-properties (point-min) (point-max))))
         (user-prompt-buffer (get-buffer necromancer--user-prompt-buffer))
         (user-prompt-text (with-current-buffer user-prompt-buffer 
                               (buffer-substring-no-properties (point-min) (point-max))))
         (has-region (use-region-p))
         (start (if has-region (region-beginning) (point-min)))
         (end (if has-region (region-end) (point)))
         (insert-pos (if has-region start (point))))
    (setq gptel--system-message system-prompt-text)
    (when has-region
      (delete-region start end))
    (goto-char insert-pos)
    (message "Necromancing... (this may take a moment)")
    (setq-local gptel-include-reasoning necromancer--thinking-buffer)
    (gptel-request user-prompt-text
      :buffer (current-buffer)
      :position (point-marker)
      :in-place t
      :stream t)))

(defvar necromancer--input-manner nil
  "controls how context is sent to necromancer:
     -1     Least amount of context. Only the TASK description & selected REGION
     0      Also include GOAL & CONTEXT
     1      Also include the current buffer PREAMBLE (range before the point/region)
     2      Also include the current buffer POSTAMBLE (range after the point/region)
     NIL    If region selected, defaults to 0; otherwise defaults to 1
   ")


(defun necromancer ()
  (interactive)
  (necromancer--edit-task)
  (necromancer--build-system-prompt)
  (necromancer--build-user-prompt (or necromancer--input-manner
				      (if (use-region-p)
					  0
					  1)))
  (necromancer--send))

(global-set-key (kbd "C-c RET") 'necromancer)
