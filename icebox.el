





;;;;;;;;;;;;; fast nav to key project files

(global-set-key (kbd "C-c i")
                (lambda ()
                  (interactive)
                  (find-file "~/cl/ipo4/README.md")))


(global-set-key (kbd "C-c o")
                (lambda ()
                  (interactive)
                  (find-file "~/cl/old-norse/old-norse.asd")))

(global-set-key (kbd "C-c s")
                (lambda ()
                  (interactive)
                  (find-file "~/cl/old-norse/skald/README.md")))

;; (global-set-key (kbd "C-c b")
;;                 (lambda ()
;;                   (interactive)
;;                   (find-file "~/cl/old-norse/bifrost/README.md")))










;;;;;;;;;;;;; launching openai / claude / gemini

(defun send-to-ai-service (service-name url &optional show-buffer-threshold)
  "Send selected text to AI service.
SERVICE-NAME is a string identifying the service for messages.
URL is the target URL to open.
SHOW-BUFFER-THRESHOLD (default 50) determines when to show temp buffer."
  (unless (use-region-p)
    (error "No region selected"))
  
  (let ((text (buffer-substring-no-properties 
               (region-beginning) (region-end)))
        (threshold (or show-buffer-threshold 50)))
    
    ;; Copy to clipboard using macOS pbcopy for reliability
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "pbcopy"))
    
    ;; Also add to kill ring for Emacs-internal use
    (kill-new text)
    
    ;; Open service in Chrome
    (call-process "open" nil nil nil 
                  "-a" "Google Chrome" 
                  url)
    
    ;; User feedback
    (message "%s opened. Text copied (%d chars). Press Cmd+V to paste." 
             service-name (length text))
    
    ;; Show text in temp buffer if it's long
    (when (> (length text) threshold)
      (with-output-to-temp-buffer (format "*%s Prompt*" service-name)
        (princ (format "Prompt for %s (copied to clipboard):\n" service-name))
        (princ (make-string 50 ?-))
        (princ "\n\n")
        (princ text)
        (princ "\n")))))

(defun send-to-gemini ()
  (interactive)
  (send-to-ai-service 
   "Gemini 2.5 Pro"
   "https://gemini.google.com/app?model=gemini-2.5-pro&authuser=0"))

(defun send-to-claude ()
  (interactive)
  (send-to-ai-service 
   "Claude 4.1 Opus"
   "https://claude.ai/new"))

(defun send-to-openai ()
  (interactive)
  (send-to-ai-service 
   "OpenAI ChatGPT"
   "https://chatgpt.com/"))

;; Key bindings
;; (global-set-key (kbd "C-c C-g") 'send-to-gemini)
;; (global-set-key (kbd "C-c C-c") 'send-to-claude)
;; (global-set-key (kbd "C-c C-o") 'send-to-openai)


;; (defun send-to-gemini-clipboard ()
;;   "Open Gemini and copy selected text to clipboard for pasting."
;;   (interactive)
;;   (let ((text (if (use-region-p)
;;                   (buffer-substring-no-properties 
;;                    (region-beginning) (region-end))
;;                 (error "No region selected"))))
    
;;     ;; Copy to clipboard
;;     (kill-new text)
    
;;     ;; Open Gemini in Chrome
;;     (call-process "open" nil nil nil 
;;                   "-a" "Google Chrome" 
;;                   "https://gemini.google.com/app?model=gemini-2.5-pro&authuser=0")
    
;;     ;; Show instructions
;;     (message "Gemini opened. Text copied to clipboard. Press Cmd+V to paste.")
    
;;     ;; Optional: Show the text in a temporary buffer
;;     (when (> (length text) 50)
;;       (with-output-to-temp-buffer "*Gemini Prompt*"
;;         (princ "Prompt copied to clipboard:\n\n")
;;         (princ text)))))

;; (global-set-key (kbd "C-c C-g") 'send-to-gemini-clipboard)








