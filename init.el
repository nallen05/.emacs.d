






(gptel--inspect-query)






;; mac specific
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
 


;; API keys

;;;;;;;;;;;;;;;;;
;;
;; https://help.openai.com/en/articles/5112595-best-practices-for-api-key-safety
;;
;;;;;;;;;;;;;;;;;





;; melpa

(require 'package)

(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

;; (package-initialize)
;; (package-refresh-contents)
;; (package-list-packages)      ;; works as refresh?








;; Make Emacs use the shell's PATH to find executables

;; M-x package-install RET exec-path-from-shell RET.

(require 'exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; (mapcar (lambda (f) (insert (format "%s\n" f))) (exec-path))










;;;; misc useful key bindings

(defun delete-after-point ()
  "Delete all text from the current point to the end of the buffer."
  (interactive)
  (delete-region (point) (point-max)))

(defun delete-before-point ()
  "Delete all text from the current point to the end of the buffer."
  (interactive)
  (delete-region (point-min) (point)))


(global-set-key (kbd "C-c d") 'delete-after-point)
(global-set-key (kbd "C-c D") 'delete-before-point)












;;;;;;;;;;;;; open-file-clister utility

(defun split-row-into-equal-windows (num-windows)
  "Split the current window horizontally into NUM-WINDOWS equal windows.
Return a list of the resulting windows in left-to-right order.
Assumes the current window represents the full width of the row."
  (let ((result (list (selected-window)))
        (target (floor (/ (window-total-width (selected-window))
                          num-windows))))
    (dotimes (_ (1- num-windows))
      (let ((cur (selected-window)))
        ;; Split the current window so that the left one will have TARGET width.
        (split-window-horizontally target)
        ;; Append the newly created right window.
        (setq result (append result (list (next-window cur 'no-other-frame))))
        ;; Move to the new window for subsequent splits.
        (select-window (next-window cur 'no-other-frame))))
    ;; Ensure each window is exactly TARGET columns wide.
    (dolist (win result)
      (let ((cw (window-total-width win)))
        (when (/= cw target)
          (window-resize win (- target cw) t))))
    result))


(defun open-file-cluster (files)
  "Open a grid of windows for FILES.
The grid is arranged in reading order (top-to-bottom, left-to-right).
The frame is split into as many windows as needed.
This is the original algorithm that creates multiple rows when there are many files."
  (delete-other-windows)
  (let* ((n (length files))
         ;; Compute a nearly square grid.
         (cols (if (> n 1) (ceiling (sqrt n)) 1))
         (rows (if (> n 1) (ceiling (/ (float n) cols)) 1))
         (grid-windows '())
         (files-assigned 0))
    ;; Split vertically to create the rows.
    (let ((row-windows (list (selected-window))))
      (dotimes (_ (1- rows))
        (push (split-window-vertically) row-windows))
      (setq row-windows (nreverse row-windows))
      ;; For each row, split it horizontally into equal windows.
      (dolist (row row-windows)
        (select-window row)
        (let* ((remaining (- n files-assigned))
               (files-in-row (if (> remaining cols) cols remaining))
               (row-windows-list (split-row-into-equal-windows files-in-row)))
          (setq grid-windows (append grid-windows row-windows-list))
          (setq files-assigned (+ files-assigned files-in-row)))))
    ;; Open each file in its designated window.
    (while (and files grid-windows)
      (with-selected-window (car grid-windows)
        (find-file (car files)))
      (setq files (cdr files))
      (setq grid-windows (cdr grid-windows)))))

;; (defun open-file-cluster-vertical (files)
;;   "Open a horizontal row of windows for FILES.
;; Each file appears in its own window, arranged side-by-side.
;; This splits the frame horizontally only (i.e. creates one row) regardless of the number of files."
;;   (delete-other-windows)
;;   (let ((n (length files)))
;;     (when (> n 1)
;;       ;; Create additional windows by splitting; after each split, move to the new window.
;;       (dotimes (_ (1- n))
;;         (split-window-horizontally)
;;         (other-window 1))
;;       (balance-windows))
;;     ;; Now collect the windows in left-to-right order by iterating over the window cycle.
;;     (let ((wins '())
;;           (w (selected-window)))
;;       (while (not (memq w wins))
;;         (push w wins)
;;         (setq w (next-window w 'no-other-frame)))
;; ;;      (setq wins (nreverse wins))

;;       (setq wins (sort wins (lambda (win1 win2)
;;                                (< (window-start win1)
;;                                   (window-start win2)))))
      
;;       ;; Open each file in its corresponding window.
;;       (dotimes (i (length wins))
;;         (with-selected-window (nth i wins)
;;           (when (nth i files)
;;             (find-file (nth i files))))))))

(defun open-file-cluster-vertical (files)
  "Open a horizontal row of windows for FILES.
Each file appears in its own window, arranged side-by-side.
Windows are opened left-to-right in the order of FILES."
  (delete-other-windows)
  (let ((n (length files)))
    (when (> n 1)
      ;; Create additional windows by splitting; after each split, move to the new window.
      (dotimes (_ (1- n))
        (split-window-horizontally)
        (other-window 1))
      (balance-windows))
    ;; Collect all windows and sort them by their left edge position
    (let ((wins '())
          (start-win (selected-window)))
      ;; Collect all windows in the current frame
      (walk-windows (lambda (w) (push w wins)) nil t)
      ;; Sort windows by their left edge position (x-coordinate)
      (setq wins (sort wins (lambda (w1 w2)
                              (< (car (window-edges w1))
                                 (car (window-edges w2))))))
      ;; Open each file in its corresponding window
      (dotimes (i (min (length wins) (length files)))
        (with-selected-window (nth i wins)
          (find-file (nth i files)))))))


(defmacro def-file-cluster (name file-list)
  "Define a cluster command NAME that opens FILE-LIST using the grid layout.
Calls `open-file-cluster`."
  `(defun ,name ()
     (interactive)
     (open-file-cluster ,file-list)))

(defmacro def-file-cluster-vertical (name file-list)
  "Define a cluster command NAME that opens FILE-LIST using the vertical layout.
Calls `open-file-cluster-vertical`."
  `(defun ,name ()
     (interactive)
     (open-file-cluster-vertical ,file-list)))




;;;;; file opening shortcuts

;; .emacs
(global-set-key (kbd "C-c .")
                (lambda ()
                  (interactive)
                  (find-file "~/.emacs.d/init.el")))

(def-file-cluster-vertical gtd-q3
  '("~/Documents/obsidian/gtd/2025-q3/backlog - search.md"
    "~/Documents/obsidian/gtd/2025-q3/backlog - pp.md"
    "~/Documents/obsidian/gtd/2025-q3/learning.md"
    ))
(global-set-key (kbd "C-c g") 'gtd-q3)

(def-file-cluster-vertical gtd-q3-r
  '("~/Documents/obsidian/gtd/2025-q3/backlog - r&d.md"
    "~/Documents/obsidian/gtd/2025-q3/backlog - pp.md"
    "~/Documents/obsidian/gtd/2025-q3/learning.md"
    ))
(global-set-key (kbd "C-c r") 'gtd-q3-r)



(def-file-cluster-vertical gtd-q3-gamedev
  '("~/Documents/obsidian/gtd/2025-q2/backlog-gamedev.md"
    "~/Documents/obsidian/gtd/2025-q3/backlog - pp.md"
    ))
(global-set-key (kbd "C-c G") 'gtd-q3-gamedev)








;; AI codegen
(load "~/.emacs.d/lisp.el")
(load "~/.emacs.d/python.el")
(load "~/.emacs.d/ai-codegen.el")
(load "~/.emacs.d/markdown-obsidian.el")












(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(transient project pytest ruff-format eglot gptel seq aidermacs exec-path-from-shell json-rpc copilot f quelpa-use-package quelpa use-package editorconfig dash s markdown-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
