;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  sensitive-minor-mode
;;
;;  Purpose: prevent backup of sensitive files
;;
;;  Usage (first line of pws.org, say):
;;  // -*- mode: org; eval: (sensitive-minor-mode 1) fill-column:80 -*-
;;
;;  See: http://anirudhsasikumar.net/blog/2005.01.21.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'cl))

(defvar sensitive-minor-mode nil
  "Sensitive file minor mode.")

(define-minor-mode sensitive-minor-mode
  "For sensitive files like password lists.
Disables backup creation and auto saving in the current buffer.

With no argument toggles the mode. Non-null prefix argument turn on the mode,
null prefix argument turn off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-minor-mode)
      (progn
        ;; disable backups
        (set (make-local-variable 'backup-inhibited) t)
        ;; disable auto-save
        (if auto-save-default
            (auto-save-mode -1)))
    ;; resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;; resort to default auto save setting
    (if auto-save-default
        (auto-save-mode 1)))
  )

(provide 'sensitive-minor-mode)
