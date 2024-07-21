;; Core


(setq ddg-ai-executable (or (executable-find "ddg-ai-chat")
                            (error "Unable to find ddg-ai-chat")))


(defun ddg-ai-shell-exec (command)
  (let ((default-directory "~"))
    (shell-command-to-string
     (format "%s %s" ddg-ai-executable command))))


(defun ddg-ai-model ()
  (string-trim-right
   (ddg-ai-shell-exec "--print-model")))


(defun ddg-ai-cleanup-cache ()
  (interactive)
  (ddg-ai-shell-exec "--remove-cache")
  (message "Cache has been deleted"))


(defun ddg-ai (question)
  "Ask DDG AI about something, get an answer (in org-mode format)"
  (let* ((question (replace-regexp-in-string "'" "" question)))
    (with-temp-buffer
      (insert (replace-regexp-in-string
               "^\s*```\\(.+\\)"
               "#+begin_src \\1"
               (replace-regexp-in-string
                ".*Contacting DuckDuckGo chat AI.*\n"
                ""
                (ddg-ai-shell-exec
                 (format "'%s'" question)))))
      (beginning-of-buffer)
      (while (search-forward "#+begin_src" nil t)
        (let ((p (point)))
          (re-search-forward "^\s*```" nil t)
          (replace-regexp "^\s*```" "#+end_src" nil p (point) t)))
      (beginning-of-buffer)
      (while (re-search-forward "^\s*```" nil t)
        (replace-regexp "^\s*```" "#+begin_example" nil nil nil t)
        (let ((p (point)))
          (re-search-forward "^\s*```" nil t)
          (replace-regexp "^\s*```" "#+end_example" nil p (point) t)))
      (buffer-substring 1 (point-max)))))


;; The chat


(defun ddg-ai-chat-buffer-name ()
  (format "*ddg-ai-chat:%s*" (ddg-ai-model)))


(defun setup-ddg-ai-buffer-content ()
  (insert "#+TITLE: DuckDuckGo AI Chat 🦆🤖\n\n* "))


(defun setup-ddg-ai-buffer ()
  (org-mode)
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key
   (kbd "RET")
   'ddg-ai-org-insert-answer)
  (local-set-key
   (kbd "C-c C-m")
   'ddg-ai-set-model)
  (local-set-key
   (kbd "C-c C-k")
   'ddg-ai-reset-chat)
  (add-hook 'kill-buffer-hook 'ddg-ai-cleanup-cache nil t)
  (setup-ddg-ai-buffer-content))


(defun ddg-ai-org-insert-answer (&optional question)
  (interactive)
  (let ((question (or question
                      (save-excursion
                        (goto-char (line-beginning-position))
                        (and (looking-at-p "\\*+ ")
                             (forward-word)
                             (backward-word)
                             (buffer-substring
                              (point)
                              (line-end-position)))))))
    (call-interactively 'org-return)
    (if question
        (progn
          (add-to-history 'ddg-ai-query-history (car (string-split question "\n")))
          (insert "# Generating answer...")
          (sit-for 0.05)
          (let ((a (ddg-ai question)))
            (kill-whole-line)
            (insert a))
          (untabify 1 (point-max))
          (indent-region 1 (point-max))
          (whitespace-cleanup)
          (call-interactively 'org-next-visible-heading)
          (org-return)
          (call-interactively 'org-previous-visible-heading))
      (org-return))))


(defun ddg-ai-set-model (model)
  (interactive (list (completing-read
                      "Model: "
                      (split-string (string-trim-right
                                     (ddg-ai-shell-exec "--list-models"))
                                    "," nil " "))))
  (let ((buffer-old-name (ddg-ai-chat-buffer-name)))
    (ddg-ai-shell-exec (format "--set-model %s" model))
    (ddg-ai-cleanup-cache)
    (when (get-buffer buffer-old-name)
      (with-current-buffer buffer-old-name
        (rename-buffer (ddg-ai-chat-buffer-name))))
    (message "Model has been set to %s" model)))


(defun ddg-ai-reset-chat ()
  (interactive)
  (ddg-ai-cleanup-cache)
  (erase-buffer)
  (setup-ddg-ai-buffer-content))


(defun ask-ddg-ai (question)
  (interactive (list (read-string "Ask DDG AI: " nil 'ddg-ai-query-history)))
  (let* ((ddg-ai-chat-buffer (ddg-ai-chat-buffer-name))
         (freshp (not (get-buffer ddg-ai-chat-buffer)))
         (b (get-buffer-create ddg-ai-chat-buffer))
         (details (when (use-region-p)
                    (buffer-substring (region-beginning)
                                      (region-end)))))
    (unless (member b (mapcar #'window-buffer (window-list)))
      (switch-to-buffer b))
    (with-current-buffer b
      (when freshp
        (setup-ddg-ai-buffer))
      (end-of-buffer)
      (unless (looking-back "* ")
        (insert "\n* "))
      (insert (concat question
                      (when details
                        (format "\n#+begin_example\n%s\n#+end_example\n"
                                details))))
      (ddg-ai-org-insert-answer
       (concat question (when details (concat "\n" details)))))))
