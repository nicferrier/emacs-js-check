;;; js-check   -*- lexical-binding: t -*-

(require 'js)


(defvar js-check-error-overlays nil
  "List of overlays used to highlight errors.")

(make-variable-buffer-local 'js-check-error-overlays)

(defun js-check-point-entered (&rest arg)
  "Called by motion hooks on an error.
Argument ARG event arg."
  (if arg (message "js-check-point-entered %s" arg))
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
                "^.*:\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)$"
                nil t)
          (let* ((line (string-to-number (match-string 1)))
                 (char-pos (string-to-number (match-string 2)))
                 (err-message (match-string 3)))
            (with-current-buffer source-buffer
              (goto-char (point-min))
              (let* ((line-start (line-beginning-position line))
                     (pos-start (+ line-start char-pos))
                     (pos-end (line-end-position line))
                     (olay (make-overlay pos-start pos-end)))
                (setq js-check-error-overlays (cons olay js-check-error-overlays))
                (overlay-put olay 'face '(:underline "red"))
                (overlay-put olay 'js-check-error err-message)
                (condition-case err
                    (with-silent-modifications
                      (add-text-properties
                       pos-start pos-end
                       `(point-entered ,(quote js-check-point-entered))))
                  (error
                   (message "js-check couldn't add error because: %s" err)))))))))))

(defconst js-check-eslint-rules
  '(block-scoped-var
    curly
    eqeqeq
    no-multi-spaces
    ;;no-unused-vars
    no-use-before-define
    global-require
    no-invalid-regexp
    no-fallthrough
    no-loop-func
    camelcase
    capitalized-comments)
  "The list of eslint rules that we require.")

(defun js-check (&optional source-buffer)
  "Check a JS file with eslint.
Optional argument SOURCE-BUFFER is the buffer that the test will be on."
  (interactive)
  (with-current-buffer (or source-buffer (current-buffer))
    (condition-case err
        (let* ((name (buffer-name))
               (src-buffer (current-buffer))
               (rules (string-join
                       (mapcar
                        (lambda (r) (format "%s: 2" (symbol-name r)))
                        js-check-eslint-rules)
                       ","))
               (proc (start-process
                      name
                      (let ((buf (format "*jslint-%s*" name)))
                        (with-current-buffer (get-buffer-create buf)
                          (let ((buffer-read-only nil))
                            (erase-buffer)
                            (current-buffer))))
                      "eslint"
                      "--no-eslintrc"
                      "--rule" rules
                      "-f" "unix"
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
            (compilation-mode)))
      (error
       (message "error: %s ; is eslint installed in your project? try: npm i eslint --save-dev" err)))))

(defvar js-check-timer nil
  "Timer used to continually check change to a buffer.")

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
      (setq js-check-timer (run-at-time
                            "2 sec" nil
                            'js-check-timer
                            source-buffer last-buffer-modified-tick)))))


(make-variable-buffer-local 'jscheck-timer)

(defun js-check-calc-indent ()
  "Calculate the indent for a js file."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\\( \\|\t\\)+[^ \t]+.*" nil t)
    (let* ((space-end (match-end 1))
           (line-start (line-beginning-position)))
      (- space-end line-start))))

(make-variable-buffer-local 'js-indent-level)

(defun js-check-choose-indent ()
  "Define the indent for a js file and make it local."
  (let ((indent (js-check-calc-indent)))
    (setq js-indent-level indent)))


(defun js-check-init ()
  "Initialize a constantly checking compilation for this buffer."
  (interactive)
  (js-check-choose-indent)
  ;; kick it off
  (js-check)
  (let* ((source-buffer (current-buffer))
         (last-buffer-modified-tick (buffer-chars-modified-tick source-buffer)))
    (setq js-check-timer
          (run-at-time
           "5 sec" nil
           'js-check-timer
           source-buffer last-buffer-modified-tick))))

;;; End
