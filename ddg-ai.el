(setq ddg-ai-executable (or (executable-find "hey.exe")
                            (error "Unable to find hey.exe")))


(defun ddg-ai (question)
  "Ask DDG AI about something, get an answer"
  (let* ((question (replace-regexp-in-string "'" "" question)))
    (with-temp-buffer
      (insert (replace-regexp-in-string
               "```\\(.+\\)"
               "#+begin_src \\1"
               (replace-regexp-in-string
                ".*Contacting DuckDuckGo chat AI.*\n"
                ""
                (shell-command-to-string
                 (format "%s '%s'" ddg-ai-executable question)))))
      (beginning-of-buffer)
      (while (search-forward "#+begin_src" nil t)
        (let ((p (point)))
          (search-forward "```")
          (replace-regexp "```" "#+end_src" nil p (point) t)))
      (beginning-of-buffer)
      (while (search-forward "```" nil t)
        (replace-string "```" "#+begin_example" nil nil nil t)
        (let ((p (point)))
          (search-forward "```")
          (replace-string "```" "#+end_example" nil p (point) t)))
      (buffer-substring 1 (point-max)))))


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
          (dotimes (i 2)
            (call-interactively 'org-return))
          (org-meta-return)
          (call-interactively 'org-previous-visible-heading))
      (org-return))))


(defun cleanup-ddg-ai-cache ()
  (shell-command-to-string
   (format "%s --remove-cache" ddg-ai-executable)))


(defun setup-ddg-ai-buffer ()
  (org-mode)
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key
   (kbd "RET")
   'ddg-ai-org-insert-answer)
  (add-hook 'kill-buffer-hook 'cleanup-ddg-ai-cache nil t)
  (insert (format "# AI Chat\n# Model: %s\n\n"
                  (shell-command-to-string (format "%s --print-model" ddg-ai-executable)))))


(defun ask-ddg-ai (question)
  (interactive (list (read-string "Ask DDG AI: " nil 'ddg-ai-query-history)))
  (let* ((n "*ddg-ai-chat*")
         (freshp (not (get-buffer n)))
         (b (get-buffer-create n))
         (details (when (use-region-p)
                    (buffer-substring (region-beginning)
                                      (region-end)))))
    (unless (member b (mapcar #'window-buffer (window-list)))
      (switch-to-buffer b))
    (with-current-buffer b
      (when freshp
        (setup-ddg-ai-buffer))
      (end-of-buffer)
      (when freshp (insert "* "))
      (insert (concat question
                      (when details
                        (format "\n#+begin_example\n%s\n#+end_example\n"
                                details))))
      (ddg-ai-org-insert-answer
       (concat question (when details (concat "\n" details)))))))
