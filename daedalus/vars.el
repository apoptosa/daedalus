;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Provide all the required constants and variables in one place             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For execution timing
(defconst dx-start-time (current-time)
  "Keep track of start time.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Provide directories                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst dx-dir-settings
  (concat dx-dir-core
          (file-name-as-directory "settings"))
  "Location for user-specific settings.")

(defconst dx-dir-snippets
  (concat dx-dir-core
          (file-name-as-directory "snippets"))
  "Location for code snippets.")

(defconst dx-dir-backups
  (expand-file-name
   (concat user-emacs-directory (file-name-as-directory "backups")))
  "Location for backups.")

(defconst dx-dir-autosaves
  (expand-file-name
   (concat user-emacs-directory (file-name-as-directory "autosaves")))
  "Location for autosaves.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Provide all filenames                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst dx-file-name-core
  (expand-file-name "core.el" dx-dir-core)
  "The Daedalus core.")

(defconst dx-file-name-aliases
  (expand-file-name "aliases.el" dx-dir-core)
  "Aliases for Daedalus.")

(defconst dx-file-name-settings
  (expand-file-name "settings.el" dx-dir-core)
  "General settings for Daedalus.")

(defconst dx-file-name-key-bindings
  (expand-file-name "key_bindings.el" dx-dir-core)
  "Key bindings.")

(defconst dx-file-name-finalise
  (expand-file-name "finalise.el" dx-dir-core)
  "Finalisation procedures.")

(defconst dx-file-name-user-settings
  (expand-file-name "user_settings.el" dx-dir-core)
  "Retain user defined settings.")

(defconst dx-file-name-info
  (expand-file-name "daedalus_info.org" dx-dir-core)
  "Daedalus information.")

(defconst dx-file-name-aliases
  (expand-file-name "aliases.el" dx-dir-core)
  "Aliases for Daedalus.")

(defconst dx-file-name-keyfreq
  (expand-file-name "keyfreq" user-emacs-directory)
  "File name for key frequency statistics.")

(defconst dx-file-name-keyfreq-lock
  (expand-file-name "keyfreq.lock" user-emacs-directory)
  "File name for key frequency statistics lock.")

;; modes
(defconst dx-file-name-mode-sensitive
  (expand-file-name "mode_sensitive.el" dx-dir-core)
  "Provide a sensitive minor mode.")

;; user-specific settings
(defconst dx-file-name-vars-sensitive
  (expand-file-name "vars_sensitive.el" dx-dir-settings)
  "User specific settings [some sensitive].")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Emacs behaviour                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dx-make-pointer-invisible t ; def. f
  "Control whether mouse pointer is hidden during typing.")

(defvar dx-var-echo-keystrokes 0.01 ;  def. 1
  "Control how rapidly unfinished commands are echoed.")

(defvar dx-var-truncwidth 80
  "Truncation width.")

(defvar dx-var-popup-size 0.4
  "Size of popup buffer.")
