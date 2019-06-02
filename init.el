;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Initialise Daedalus                                                       ;;
;;                                                                            ;;
;;  Inspired by:                                                              ;;
;;   https://github.com/danielmai/.emacs.d                                    ;;
;;   https://github.com/dakrone/dakrone-dotfiles/blob/master/emacs.org        ;;
;;   doom-emacs                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Debug mode on - turn off once finished loading
(setq debug-on-error t)
(setq debug-on-quit t)


;; During loading increase threshold for garbage collection

(defconst dx-gc-cons-threshold 16777216
  "Change garbage collection threshold to 16mb.")

(defconst dx-gc-cons-upper-limit 268435456
  "Change garbage collection threshold to 256mb.")

(setq gc-cons-threshold dx-gc-cons-upper-limit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Provide variables for Daedalus                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst dx-dir-core
  (expand-file-name
   (concat user-emacs-directory
           (file-name-as-directory "daedalus")))
  "Full location for the Daedalus core and related files.")

(defconst dx-file-name-vars (expand-file-name "vars.el" dx-dir-core)
  "Central store of Daedalus variables.")

(message "Daedalus: Loading variables")
(let ((inhibit-message t))
  (load dx-file-name-vars)
  )

(message "Daedalus: Loading variables [sensitive]")
(let ((inhibit-message t))
  (load dx-file-name-vars-sensitive)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fire up the core                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Daedalus: Loading core")
(load dx-file-name-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Provide additional functionality                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load dx-file-name-functions-hydra)
(load dx-file-name-functions-window)
(load dx-file-name-functions-buffer)
(load dx-file-name-functions-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Add minor mode                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Daedalus: Loading mode definitions")
(load dx-file-name-mode-sensitive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fire up aliases                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Daedalus: Loading aliases")
(load dx-file-name-aliases)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Apply settings                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Daedalus: Loading settings")
(load dx-file-name-settings)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Apply key bindings                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Daedalus: Loading key bindings")
(load dx-file-name-key-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Finalise                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Daedalus: Finalising")
(load dx-file-name-finalise)

(setq debug-on-error nil)
(setq debug-on-quit nil)


(put 'list-timers 'disabled nil)
