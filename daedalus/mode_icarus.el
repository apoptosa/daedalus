;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ,-*
;; (_) Created on <Sun May 26 2019> @ 21:13:07
;;
;; @author: apoptosa
;; @function: icarus - a pseudo-leader mode
;;
;; Following minor-mode construction discussion of
;; https://nullprogram.com/blog/2013/02/06/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'key-chord)
(require 'key-seq)

(require 'which-key)

(make-variable-buffer-local
 (defvar foo-count 0
   "Number of foos inserted into the current buffer."))

(defun insert-foo ()
  (interactive)
  (setq foo-count (1+ foo-count))
  (insert "foo"))

;;;###autoload
(define-minor-mode icarus-minor-mode
  "Get your foos in the right places."
  :lighter " foo"
  :keymap (let ((map (make-keymap)))
            ;; (define-key map (kbd "SPC z z") 'insert-foo)
            map)

  (define-key icarus-minor-mode-map (kbd "SPC f") 'insert-foo)
  (define-key icarus-minor-mode-map (kbd "SPC s") 'helm-swoop)

  (message "initialised")
  )

(provide 'icarus-minor-mode)



;; max time between two key presses
(setq key-chord-two-keys-delay 0.05)
(key-chord-mode 1)

(key-seq-define emacs-lisp-mode-map "  " 'helm-swoop)


(let ((inhibit-message t))
  ;; Suppress spam
  (message "Listen to me, you!"))


;;;;;;;

;; build key-map on top of leader-key construct
(defvar icarus-minor-map-devise
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a")         'helm-apropos)
    (define-key map (kbd "s")         'helm-swoop)
    map))


(fset 'icarus-minor-map-devise icarus-minor-map-devise)
(key-seq-define emacs-lisp-mode-map "  " 'icarus-minor-map-devise)


(setq key-chord-two-keys-delay 0.05)
(key-chord-mode 1)



;;
;;  :D
;;






;; disable key-chords

;; ;; provide key-chord for hydra
;; (setq key-chord-two-keys-delay 0.01)
;; (setq key-chord-one-key-delay 0.2)

;; ;;(key-seq-define global-map "  " 'hydra-controller/body)


;; ;; Add some key-chords
;; ;; (key-chord-define-global "bb" 'hydra-buffer/body)

;; ;; (key-seq-define global-map "ss" 'helm-swoop)
;; ;; (key-seq-define global-map "xx" 'helm-mini)


;; (let ((inhibit-message t))
;;   ;; Suppress spam
;;   (key-chord-mode 1)
;;   )





;; (defvar dx-var-key-chord-temporary-disable-delay 0.3
;;   "Control how long key-chords are temporarily disabled for.")

;; ;; Temporarily disable key-chords while typing

;; (defun dx-fun-enable-key-chords ()
;;   ;; enable key-chord and remove timer
;;   (setq input-method-function 'key-chord-input-method)
;;   ;;  (cancel-timer dx-key-chords-timer)
;;   )


;; (defun dx-fun-disable-key-chords ()
;;   ;; temporarily disable key-chords with a timer
;;   (setq input-method-function nil)
;;   ;; (setq dx-key-chords-timer
;;   ;;       (run-with-idle-timer
;;   ;;        dx-var-key-chord-temporary-disable-delay
;;   ;;        'repeat 'dx-fun-enable-key-chords))
;;   )

;; ;;
;; (setq dx-key-chords-timer
;;       (run-with-idle-timer
;;        dx-var-key-chord-temporary-disable-delay
;;        'repeat 'dx-fun-enable-key-chords))
;; ;;
;; ;;(add-hook 'post-command-hook 'dx-fun-disable-key-chords)
;; (add-hook 'post-self-insert-hook 'dx-fun-disable-key-chords)

;; ;; hack to enable key-chords on buffer switch
;; (add-hook 'buffer-list-update-hook 'dx-fun-enable-key-chords)
