;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ,-*
;; (_) Created on <Sun Jun 02 2019> @ 18:39:59
;;
;; @author: apoptosa
;; @function: Additional functions for use with Hydra package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dx-fun-hydra-format-key-string (str hydra-face-type)
  "Formatter for hydra doc."
  (concat (concat "[" (propertize str 'face hydra-face-type)) "]"))


(defun dx-fun-hydra-format-key-string (str hydra-face-type)
  "Formatter for hydra doc."
  (concat (concat "[" (propertize str 'face hydra-face-type)) "]"))

;; Adapted from
;; https://github.com/jerrypnz/major-mode-hydra.el/blob/master/pretty-hydra.el
(defun dx-make-hydra-toggle (name status)
  "Create a dynamic hint that look like a radio button with given NAME.
Radio is considered on when STATUS is non-nil, otherwise off."
  (s-concat name " "
            (if status
                (propertize "(•)" 'face 'font-lock-keyword-face)
              (propertize "( )" 'face 'font-lock-comment-face))))



(provide 'dx-functions-hydra)


;;
;;  :D
;;
