;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide package archives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User defined settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Daedalus: Loading custom settings")
(setq custom-file dx-file-name-user-settings)
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide packages required
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar dx-install-packages
  '(
    ;; package management
    use-package

    ;;;;
    ;;;; Appearance
    ;;;;

    apropospriate-theme
    ;;aurora-theme
    badwolf-theme
    color-theme-sanityinc-tomorrow
    dakrone-theme
    leuven-theme
    material-theme
    moe-theme
    monokai-theme
    solarized-theme
    spaceline
    waher-theme
    zenburn-theme

    ;; the hero we all deserve [scroll]
    nyan-mode

    ;; light will shine on top of your cursor whenever a window scrolls
    beacon

    ;;;;
    ;;;; General purpose
    ;;;;
    better-defaults

    smooth-scrolling

    keyfreq                  ; retain how often commands are used

    which-key                ; give more information with C-x

    aggressive-indent        ; for automatic indentation
    smartparens              ; better parenthesis insertion

    undo-tree                ; visualization of undo

    multiple-cursors         ; harness the power of multiple cursors

    highlight-symbol         ; hilight symbols at point when switching

    whitespace               ; whitespace indication


    window-number            ; better buffer switching

    rainbow-delimiters       ; exactly what it says
    rainbow-mode             ; string colourised

    diminish                 ; fight (minor) modeline clutter
    cyphejor                 ; as above

    drag-stuff               ; move things around with meta key

    expand-region            ; selection via semantic units

    ;; dired-x                  ; file management
    dired-hacks-utils        ; various dired extensions
    dired-subtree
    dired-collapse
    dired-filter
    dired-open
    dired-rainbow
    ;; dired-ranger

    symon                    ; small system monitor


    yasnippet                ; provide code snippet mechanism

    company                  ; autocompletion
    company-quickhelp        ; for docstrings

    projectile               ; for managing projects


    recentf                  ; recently used

    bind-key                 ; for specifying key-bindings easily

    magit                    ; version control

    popwin                   ; move tmp / etc buffers to popup
    shackle

    ;; helm
    helm helm-projectile helm-ag helm-pydoc helm-swoop
    helm-gtags helm-ls-git helm-flycheck helm-flyspell helm-flx
    helm-descbinds helm-c-yasnippet
    helm-dash

    nlinum
    nlinum-hl

    pdf-tools                ; replace doc-view for pdf


    eyebrowse                ; window layout management


    wolfram

    ;; key-binding stuffs
    general

    hydra
    ))

;; Go ahead and install as required
(defvar packages-refreshed? nil)

(dolist (pack dx-install-packages)
  (unless (package-installed-p pack)
    (unless packages-refreshed?
      (package-refresh-contents)
      (setq packages-refreshed? t))
    (unwind-protect
        (condition-case ex
            (package-install pack)
          ('error (message
                   "Failed to install package [%s], caught exception: [%s]"
                   pack ex)))
      (message "Installed %s" pack))))

;; Load use-package, used for loading packages everywhere else
(require 'use-package)

;; Set to t to debug package loading or nil to disable
(setq use-package-verbose nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load required packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package diminish
  :init
  (progn
    (diminish 'auto-fill-function "")))

; behaviour
(require 'better-defaults)

(require 'keyfreq)

(require 'cyphejor)

(require 'recentf)

(require 'bind-key)

;; appearance
(require 'uniquify)

(require 'nyan-mode)

(require 'beacon)

(require 'smooth-scrolling)

(require 'aggressive-indent)

(require 'window-number)

(require 'undo-tree)

(require 'multiple-cursors)

(require 'highlight-symbol)

(require 'whitespace)

(require 'rainbow-delimiters)

(require 'drag-stuff)

(require 'smartparens)

(require 'expand-region)

(require 'doc-view)

(require 'yasnippet)

(require 'company)

(require 'company-quickhelp)

(require 'magit)

;; (require 'dired-x)
;; (require 'dired-aux)
;; (require 'dired-async)

(require 'dired-hacks-utils)
(require 'dired-subtree)
(require 'dired-collapse)
(require 'dired-filter)
(require 'dired-open)
(require 'dired-rainbow)


;; popwin, helm and friends

(require 'helm-config)
(require 'helm-flx)
(require 'helm)
(require 'helm-swoop)
(require 'helm-misc)
(require 'helm-files)
(require 'helm-grep)
(require 'helm-dash)

;;projectile and friends
(require 'projectile)
(require 'helm-projectile)


(require 'shackle)

(require 'popwin)

(require 'nlinum)
(require 'nlinum-hl)


(require 'pdf-tools)


(require 'eyebrowse)

(require 'wolfram)

(require 'general)


(require 'hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide required functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; theme related
(defun dx-fun-badwolf-theme-fixed ()
  "Extends the badwolf theme."
  (interactive)
  (load-theme 'badwolf)
  ;; for beacon
  (setq beacon-color "dodger blue"))

;; geometry related
(defun dx-fun-set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun dx-fun-set-truncwidth-columns ()
  "Set the selected window to (dx-var-truncwidth) columns."
  (interactive)
  (dx-fun-set-window-width (+ 0 dx-var-truncwidth)))



;; startup layout related
(defun dx-fun-startup-layout ()
  ;; (dx-fun-set-truncwidth-columns)

  (interactive)

  (delete-other-windows)

  (split-window-horizontally)        ;; -> |

  (split-window-horizontally)        ;; -> |

  (next-multiframe-window)
  (next-multiframe-window)

  (split-window-vertically)          ;;  -> --

  (balance-windows)


  (find-file dx-file-name-info)      ;; open general info org

  (next-multiframe-window)
  (dired "~")


  (next-multiframe-window)
  (find-file dx-file-name-settings)
  (next-multiframe-window)
  (find-file dx-file-name-core)
  )


(defun dx-fun-kill-emacs ()
  "Exit unconditionally after being prompted for save."
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(defun dx-fun-volatile-kill-buffer ()
  "Kill current buffer unconditionally."
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))

;; dired related
(defun dx-xah-open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

When called in emacs lisp, if @fname is given, open that.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-01-18"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" (replace-regexp-in-string
                                      "/" "\\" $fpath t t))) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath)))
         $file-list))))))




;; provide function for cycling between themes
(defvar dx-var-input-methods
  '(nil "TeX" "latin-1-prefix")
  "Collection of input methods to cycle amongst.")

(setq dx-var-input-methods-index 0)

(defun dx-fun-input-methods-cycler ()
  (interactive)

  ;; bump up index
  (setq dx-var-input-methods-index
        (% (1+ dx-var-input-methods-index)
           (length dx-var-input-methods)))

  ;; extract next method and apply
  (let ((next-method (nth dx-var-input-methods-index
                          dx-var-input-methods)))

    (set-input-method next-method)

    ;; distinct message handling for nil
    (if next-method
        (message (concat "Switching input method to "
                         current-input-method))
      (message "Switching input method to default")
      )

    )
  )


(defun dx-fun-toggle-selective-display (column)
  "Toggle (with at point information) selective display (indent level)."
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))


;; quality of life (spawn new buffer)
(defun dx-fun-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “*scratch*” or “*scratch*<2>”, “*scratch*<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "*scratch*")))
    (switch-to-buffer $buf)
    (emacs-lisp-mode)
    (setq buffer-offer-save t)
    $buf
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((elapsed (float-time (time-subtract (current-time)
                                          dx-start-time))))
  (message "Daedalus core: Loaded in %.3fs" elapsed))
