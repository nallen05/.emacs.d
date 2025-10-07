

  ;;;; markdown

;; M-x package-install markdown-mode

(require 'markdown-mode)

(defun toggle-markdown-strikethrough ()
  "Toggle strikethrough on the current line, preserving indentation, nested structure, and cursor position."
  (interactive)
  (let ((original-point (point))) ;; Save the original cursor position
    (save-excursion
      ;; Move to the beginning of the line
      (beginning-of-line)
      ;; Match any line with leading indentation and content
      (if (looking-at "^\\(\\s-*\\)\\(.*\\)$")
          (let ((indentation (match-string 1))  ;; Leading spaces for indentation
                (content (match-string 2)))     ;; The actual line content
            ;; Toggle strikethrough by checking for existing `~~`
            (if (string-match-p "^~~.*~~$" content)
                ;; Remove `~~` if present
                (replace-match (concat indentation (replace-regexp-in-string "^~~\\(.*\\)~~$" "\\1" content)) t t)
              ;; Add `~~` if not present
              (replace-match (concat indentation "~~" content "~~") t t)))
        (message "Not a valid Markdown list item")))
    ;; Restore the cursor position
    (goto-char original-point)))

(setq my-highlight-markdown-tags '("#asap" "#now" "#1thing" "#2thing"))

(defface my-markdown-tag-face
  '((t :foreground "black" :background "grey" :weight bold))
  "Face for highlighting Markdown tags.")


(defun my-markdown-highlight-tags ()
  "Highlight tags defined in `my-hogh;oght=markdown-tags` in Markdown mode."
  (font-lock-add-keywords
   nil
   `((,(concat "\\(?:\\s-\\|^\\)\\(" (regexp-opt my-highlight-markdown-tags) "\\)\\>")
      (1 'my-markdown-tag-face prepend)))))


(defun my-touch-file ()
  (interactive)
  (when (and buffer-file-name
             (file-exists-p buffer-file-name))
    (set-file-times buffer-file-name)))

(defun my-markdown-mode-hook ()
  
  ;; Set tab width to 2 spaces
  (setq-local tab-width 2)

  ;; Use spaces instead of tab characters
  (setq-local indent-tabs-mode nil)

  ;; strikethrough
  (local-set-key (kbd "C-c C-x") 'toggle-markdown-strikethrough)

  ;; tags
  (my-markdown-highlight-tags)

  ;; see changes from obsidian
  ;; C-c RET to trigger
  (auto-revert-mode 1)
  (local-set-key (kbd "C-c C-<return>") 'my-touch-file)
  (my-touch-file)
  )

(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)
