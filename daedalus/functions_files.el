;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ,-*
;; (_) Created on <Sun Jun 02 2019> @ 20:07:47
;;
;; @author: apoptosa
;; @function: Additional functions pertaining to files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adapted from spacemacs ------------------------------------------------------
;; Copy file path
(defun dx-fun-directory-path ()
  "Retrieve the directory path of the current buffer.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'.

Returns:
  - A string containing the directory path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (directory-name (if-let (file-name (buffer-file-name))
                                (file-name-directory file-name)
                              list-buffers-directory))
    (file-truename directory-name)))

(defun dx-fun-file-path ()
  "Retrieve the file path of the current buffer.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))

(defun dx-fun-file-path-with-line ()
  "Retrieve the file path of the current buffer, including line number.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (dx-fun-file-path))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))

(defun dx-fun-file-path-with-line-column ()
  "Retrieve the file path of the current buffer, including line and column number.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (dx-fun-file-path-with-line))
    (concat
     file-path
     ":"
     (number-to-string (if (and
                            ;; Emacs 26 introduced this variable.
                            ;; Remove this check once 26 becomes the minimum version.
                            (boundp column-number-indicator-zero-based)
                            (not column-number-indicator-zero-based))
                           (1+ (current-column))
                         (current-column))))))

(defun dx-fun-copy-directory-path ()
  "Copy and show the directory path of the current buffer.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (dx-fun-directory-path))
      (message "%s" (kill-new directory-path))
    (message "Warning: Current buffer does not have a directory!")))

(defun dx-fun-copy-file-path ()
  "Copy and show the file path of the current buffer."
  (interactive)
  (if-let (file-path (dx-fun-file-path))
      (message "%s" (kill-new file-path))
    (message "Warning: Current buffer is not attached to a file!")))

(defun dx-fun-copy-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let (file-name (file-name-nondirectory (dx-fun-file-path)))
      (message "%s" (kill-new file-name))
    (message "Warning: Current buffer is not attached to a file!")))

(defun dx-fun-copy-file-name-base ()
  "Copy and show the file name without its final extension of the current
buffer."
  (interactive)
  (if-let (file-name (file-name-base (dx-fun-file-path)))
      (message "%s" (kill-new file-name))
    (message "Warning: Current buffer is not attached to a file!")))

(defun dx-fun-copy-file-path-with-line ()
  "Copy and show the file path of the current buffer, including line number."
  (interactive)
  (if-let (file-path (dx-fun-file-path-with-line))
      (message "%s" (kill-new file-path))
    (message "Warning: Current buffer is not attached to a file!")))

(defun dx-fun-copy-file-path-with-line-column ()
  "Copy and show the file path of the current buffer, including line and column number.

This function respects the value of the `column-number-indicator-zero-based'
variable."
  (interactive)
  (if-let (file-path (dx-fun-file-path-with-line-column))
      (message "%s" (kill-new file-path))
    (message "Warning: Current buffer is not attached to a file!")))

;;------------------------------------------------------------------------------

(provide 'dx-functions-files)

;;
;;  :D
;;
