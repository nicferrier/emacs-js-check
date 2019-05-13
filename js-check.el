;;; js-check   -*- lexical-binding: t -*-

(make-variable-buffer-local 'js-check-error-overlays)

(defun js-check-point-entered (&rest args)
  "Called by motion hooks on an error.
Optional argument ARGS all the args."
  (let ((olay (overlays-at (point))))
    (when olay
      (message "js-check-error: %s" (overlay-get (car olay) 'js-check-error)))))

(defun js-check-parse-errors (source-buffer lint-buffer)
  "Check parse errors from eslint.

SOURCE-BUFFER the source code buffer being checked.

LINT-BUFFER the output of the lint command."
  (save-excursion
    (with-current-buffer source-buffer
      (mapc 'delete-overlay js-check-error-overlays))
    (with-current-buffer lint-buffer
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward
                "^.*:\\([0-9]+\\):\\([0-9]+\\):\\([^:]+\\): \\(.*\\)$"
                nil t)
          (let* ((line (string-to-number (match-string 1)))
                 (char-pos (string-to-number (match-string 2)))
                 (type (match-string 3))
                 (err-message (match-string 4)))
            (with-current-buffer source-buffer
              (goto-char (point-min))
              (let* ((line-start (line-beginning-position line))
                     (pos-start (+ line-start char-pos))
                     (pos-end (line-end-position line))
                     (olay (make-overlay pos-start pos-end)))
                (setq js-check-error-overlays (cons olay js-check-error-overlays))
                (overlay-put olay 'face '(:underline "red"))
                (overlay-put olay 'js-check-error err-message)
                (add-text-properties
                 pos-start pos-end
                 '(point-entered js-check-point-entered))))))))))

(defun js-check (&optional source-buffer)
  "Check a JS file with eslint.
Optional argument SOURCE-BUFFER is the buffer that the test will be on."
  (interactive)
  (with-current-buffer (or source-buffer (current-buffer))
    (let* ((name (buffer-name))
           (src-buffer (current-buffer))
           (proc (start-process
                  name
                  (let ((buf (format "*jslint-%s*" name)))
                    (with-current-buffer (get-buffer-create buf)
                      (let ((buffer-read-only nil))
                        (erase-buffer)
                        (current-buffer))))
                  "eslint"
                  "--no-eslintrc"
                  "-f"
                  "unix"
                  "--parser-options=ecmaVersion:2018"
                  (buffer-file-name))))
      (set-process-sentinel
       proc
       (lambda (process status)
         (cond
          ((or (equal "finished\n" status)
               (equal "exited abnormally with code 1\n" status))
           (js-check-parse-errors src-buffer (process-buffer process))))))
      (with-current-buffer (process-buffer proc)
        (compilation-mode)))))

(defun js-check-timer (source-buffer last-buffer-modified-tick)
  "The timer function for running the compilation.

Argument SOURCE-BUFFER is the buffer the check with be run in.
Argument LAST-BUFFER-MODIFIED-TICK the last tick of the timer."
  (let ((new-tick (buffer-chars-modified-tick source-buffer)))
    (when (> new-tick last-buffer-modified-tick)
      (message "js-check running...")
      (setq last-buffer-modified-tick new-tick)
      (save-buffer)
      (js-check source-buffer))
    (with-current-buffer source-buffer
      (setq jscheck-timer (run-at-time
                           "5 sec" nil
                           'js-check-timer
                           source-buffer last-buffer-modified-tick)))))

(defun js-check-init ()
  "Initialize a constantly checking compilation for this buffer."
  (interactive)
  ;; kick it off
  (js-check)
  (let* ((source-buffer (current-buffer))
         (last-buffer-modified-tick (buffer-chars-modified-tick source-buffer)))
    (make-variable-buffer-local 'jscheck-timer)
    (setq jscheck-timer
          (run-at-time
           "5 sec" nil
           'js-check-timer
           source-buffer last-buffer-modified-tick))))

;;; End
