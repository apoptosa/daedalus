;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ,-*
;; (_) Created on <Sat May 25 2019> @ 22:29:03
;;
;; @author: apoptosa
;; @function: Main settings function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Tweak general settings [see vars]                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name dx-var-sensitive-user-full-name
      user-mail-address dx-var-sensitive-user-mail-address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs behaviour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(semantic-mode 1)

;; Ignore obsolete function warnings during byte compilation
(setq byte-compile-warnings '(not obsolete))

;; Delete selected region when typing
(delete-selection-mode 1)

;; Disable gui interaction modes specific to mouse input
(when (functionp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (functionp 'set-scroll-bar-mode)
  (set-scroll-bar-mode 'nil))
(when (functionp 'mouse-wheel-mode)
  (mouse-wheel-mode -1))
(when (functionp 'tooltip-mode)
  (tooltip-mode -1))
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
;;(when (functionp 'blink-cursor-mode)
;;  (blink-cursor-mode -1))


(setq make-pointer-invisible dx-make-pointer-invisible)
(setq echo-keystrokes dx-var-echo-keystrokes)


(setq visible-bell t)              ;; Don't beep at me
(setq ring-bell-function 'ignore)  ;; Ignore flashing


(setq line-move-visual t)          ;; Move by display around long lines
(setq system-uses-terminfo nil)    ;; Fix terminal colour escapes


;; Suppress stupid messages
(setq tooltip-use-echo-area t)
(setq inhibit-startup-message t)   ;; hide the startup message
(setq initial-scratch-message "")  ;; *scratch* message

(global-font-lock-mode t)          ;; Syntax highlighting for all buffers



(setq-default find-file-visit-truename t)   ;; Resolve symlinks
(global-auto-revert-mode t)                 ;; monitor for file changes

;; Retain more backups: https://www.emacswiki.org/emacs/BackupFiles
;; sx/questions/151945/how-do-i-control-how-emacs-makes-backup-files

;; ensure backup directory exists and make use of it
(if (not (file-exists-p dx-dir-backups))
    (make-directory dx-dir-backups t))
(setq backup-directory-alist `(("." . ,dx-dir-backups)))

;; ensure autosave directory exists and make use of it
(if (not (file-exists-p dx-dir-autosaves))
    (make-directory dx-dir-autosaves t))

(setq auto-save-file-name-transforms `((".*" ,dx-dir-autosaves t)))
(setq auto-save-list-file-prefix dx-dir-autosaves)

(setq
 make-backup-files t          ; backup on first save
 backup-by-copying t          ; don't clobber symlinks
 kept-new-versions 10         ; keep 10 latest versions
 kept-old-versions 0          ; don't bother with old versions
 delete-old-versions t        ; don't ask about deleting old versions
 delete-by-moving-to-trash t
 version-control t            ; number backups
 vc-make-backup-files t       ; backup version controlled files
 auto-save-default t          ; save every buffer visited
 auto-save-interval 2       ; number of keys-strokes between saves
 auto-save-timeout 20         ; number of seconds between saves
 create-lockfiles nil         ; do not create .#filename
 )


;; disable auto-save for sensitive minor modes
(setq auto-mode-alist
      (append
       (list
        '("\\.\\(vcf\\|gpg\\)$" . sensitive-minor-mode)
        )
       auto-mode-alist))


(show-paren-mode 1)                           ;; show matching parentheses
(setq show-paren-delay 0)                     ;; associated delay
(setq-default fill-column dx-var-truncwidth)  ;; line wrap at this column
(setq-default indent-tabs-mode nil)           ;; Tabs -> whitespace


(setq confirm-nonexistent-file-or-buffer nil) ;; Nonexistent ignore

(setq large-file-warning-threshold 150000000) ;; allow larger files

;; Enable ANSI colors for the shell by default.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; hide time but show line and column number
(setq display-time-day-and-date t) ;;24hr date and time in modeline
(setq display-time-24hr-format t)

(display-time-mode 0)
(line-number-mode 1)
(column-number-mode 1)

;; Hide uninteresting buffers
(setq ibuffer-never-show-predicates
      (list
       ;; others
       "^\\*Completions\\*$"
       ))

;; Hide dired
(add-to-list 'ibuffer-never-show-predicates "^\\*dired")
(add-to-list 'ibuffer-never-show-predicates "^\\*magit")

;; Nonexistent ignore
(setq confirm-nonexistent-file-or-buffer nil)

;; Kill attached without query
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Kill active procs
;; http://stackoverflow.com/questions/2706527/
;;   make-emacs-stop-asking-active-processes-exist-kill-them-and-exit-anyway
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))


;; Smaller fringe
(fringe-mode '(1 . 0))

;; fixed linum
;; (add-hook 'prog-mode-hook 'linum-mode)
;; (setq linum-format "%4d\u2502")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs behaviour - clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Improve clipboard integration
(setq x-select-enable-clipboard t)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Save the current (system) clipboard before replacing it
(setq save-interprogram-paste-before-kill t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs behaviour - encoding - prefer UTF-8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify behaviour: buffer names with angle brackets: foo, foo<2>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nyan-cat scroller
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nyan-mode 1)
(setq nyan-animate-nyancat t)
(setq nyan-animation-frame-interval 0.1)
(setq nyan-bar-length 8)
(setq nyan-wavy-trail t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; beacon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(beacon-mode 1)
(setq beacon-blink-duration 0.1)
(setq beacon-blink-when-focused t)
(setq beacon-blink-when-window-changes t)
(setq beacon-size 15)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smooth-scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq smooth-scroll-margin 3
      scroll-margin 3                 ; space between the cursor and bottom
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key-mode  -  https://github.com/justbur/emacs-which-key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-key-mode 1)
(setq which-key-side-window-location 'bottom)    ; fix location
(setq which-key-idle-delay 0)                    ; def. 1.0

;; use descriptions
(setq which-key-sort-order 'which-key-description-order)
(setq which-key-separator " ↪ ")                  ; def. →


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window-number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Faster window switching we use window-number together with windmove bindings
(window-number-mode 0)     ;; Only use meta keys
(window-number-meta-mode)  ;; binds shift + direction

;; shift + arrow jump between windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aggressive-indent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-aggressive-indent-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo-tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-undo-tree-mode 1)
;; enable relative timestamps
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-relative-timestamps t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight-symbol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Automatic symbol highlighting and kbd
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(setq highlight-symbol-on-navigation-p 1)
(add-hook 'prog-mode-hook
          (lambda ()
            (define-key prog-mode-map (kbd "C-n")
              'highlight-symbol-next)
            (define-key prog-mode-map (kbd "C-p")
              'highlight-symbol-prev)))

(setq highlight-symbol-idle-delay 0)  ; how long to wait before symbol hilight


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; whitespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; highlight lines that pass the limit
(setq whitespace-line-column dx-var-truncwidth) ;; limit line length
(setq whitespace-style '(face lines-tail))
;;(setq whitespace-style '(tabs newline space-mark
;;                         tab-mark newline-mark
;;                         face lines-tail))
;; show trailing whitspace
;; (setq-default show-trailing-whitespace t)

;; disable whitespace in some modes
(setq whitespace-global-modes '(not org-mode
                                    eshell-mode
                                    shell-mode))

(global-whitespace-mode 1)

;; Only show trailing whitespace in programming modes
(defun set-show-trailing ()
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'set-show-trailing)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable recent files mode.
(recentf-mode t)

(setq recentf-max-saved-items 300)

(setq recentf-exclude '("/auto-install/" ".recentf" "/repos/" "/elpa/"
                        "\\.mime-example" "\\.ido.last" "COMMIT_EDITMSG"
                        ".gz"
                        "~$" "/tmp/" "/ssh:" "/sudo:" "/scp:")
      recentf-auto-cleanup 600  ; cleanup after this many idle seconds
      )

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; drag-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)        ; activates M-up, M-down, M-right, M-left

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smartparens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smartparens-global-mode 1)    ; use globally

; enforce stricly in some modes
(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; saveplace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; emacs 25.1 >
(save-place-mode 1)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(setq save-place-forget-unreadable-files nil)  ; don't check readable (NFS fix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doc-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq doc-view-resolution 300)
(setq doc-view-continuous t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq mc/always-run-for-all t)  ; run commands for all selected by default

;; enable for specific commends
;; (setq mc/cmds-to-run-for-all '(sp-backward-delete-char))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hippie-exp - expansion Using M+/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'hippie-exp "hippie-exp" t)
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company - can work anywhere!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq company-idle-delay 0.3)
(setq company-minimum-prefix-length 1)
(setq company-tooltip-limit 10)
(setq company-tooltip-margin 0)

(add-hook 'after-init-hook 'global-company-mode)
(company-quickhelp-mode t)

(setq company-quickhelp-color-foreground "white")
(setq company-quickhelp-color-background "#242321")
(setq company-quickhelp-delay 0)
(setq company-quickhelp-max-lines 15)


;; Prevent yasnippet and company collision when it comes to the TAB key:

;; weight by frequency
(setq company-transformers '(company-sort-by-occurrence))

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas)
          (and (listp backend)
               (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends
      (mapcar #'company-mode/backend-with-yas company-backends))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq yas-snippet-dirs (quote (dx-dir-snippets)))
(yas-reload-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default dired-omit-files-p t)

;; additional extensions to ignore
;; (add-to-list 'dired-omit-extensions ".DS_Store")
;;(customize-set-variable 'diredp-hide-details-initially-flag nil)

(put 'dired-find-alternate-file 'disabled nil)
(setq ls-lisp-dirs-first t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      ;; make split target
      dired-dwim-target t
      ;; -F marks links with @
      dired-ls-F-marks-symlinks t
      delete-by-moving-to-trash t
      ;; Auto refresh dired but be quiet about it
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      wdired-allow-to-change-permissions t
      dired-listing-switches "-lFaGh1v --group-directories-first")

(setq dired-subtree-use-backgrounds nil)

(defun dx-fun-dired-mode-hook ()
  (toggle-truncate-lines 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(projectile-global-mode)

;; global ignores
(add-to-list 'projectile-globally-ignored-files "__pycache__")
(add-to-list 'projectile-globally-ignored-files ".DS_Store")
(add-to-list 'projectile-globally-ignored-file-suffixes ".pyc")

;; Projects defined by a few markers - reorder so other cases are secondary
;; See [reddit]: /r/emacs/comments/920psp/projectile_ignoring_projectile_files/
;;
;; This should also fix nested repo behaviour
(setq projectile-project-root-files #'( ".projectile" )
      projectile-project-root-files-functions
      #'(projectile-root-top-down
         projectile-root-top-down-recurring
         projectile-root-bottom-up
         projectile-root-local))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start by providing flx to use in lieu of helm's fuzzy matching
(helm-flx-mode +1)

;; description of bindings via helm
(fset 'describe-bindings 'helm-descbinds)
(setq helm-descbinds-window-style 'split-window)

;; Via:
;; https://www.reddit.com/r/emacs/comments/3asbyn/
;; new_and_very_useful_helm_feature_enter_search/

(setq helm-echo-input-in-header-line t)
(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
(add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

(setq helm-truncate-lines t
      ;; may be overridden if 'ggrep' is in path (see below)
      helm-grep-default-command
      "grep -a -d skip %e -n%cH -e %p %f"
      helm-grep-default-recurse-command
      "grep -a -d recurse %e -n%cH -e %p %f"
      ;; do not display invisible candidates
      helm-quick-update t
      ;; be idle for this many seconds, before updating in delayed sources.
      helm-idle-delay 0
      helm-input-idle-delay 0
      ;; wider buffer name in helm-buffers-list
      helm-buffer-max-length 25 ;; default is 20
      ;; open helm buffer in another window
      ;;helm-split-window-default-side 'other
      ;; open helm buffer inside current window, don't occupy whole other window
      ;;helm-split-window-in-side-p t
      ;; limit the number of displayed canidates
      ;; helm-candidate-number-limit 200
      ;; don't use recentf stuff in helm-ff
      helm-ff-file-name-history-use-recentf nil
      ;; move to end or beginning of source when reaching top or bottom
      ;; of source
      helm-move-to-line-cycle-in-source t
      ;; don't display the header line
      helm-display-header-line nil
      ;; fuzzy matching
      helm-recentf-fuzzy-match t
      helm-locate-fuzzy-match nil ;; locate fuzzy is worthless
      helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-semantic-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-lisp-fuzzy-completion t
      helm-completion-in-region-fuzzy-match t
      ;; Here are the things helm-mini shows, I add `helm-source-bookmarks'
      ;; here to the regular default list
      helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-bookmarks
                                  helm-source-buffer-not-found))

;; for top
(helm-top-poll-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq helm-ff-file-compressed-list '("gz" "bz2" "zip" "tgz" "xz" "txz"))

;; skip meaningless files, e.g. .DS_Store
(setq helm-ff-skip-boring-files t)
(delete '"\\.bbl$" helm-boring-file-regexp-list)    ;show .bbl file
;;(add-to-list 'helm-boring-file-regexp-list "\\.el#$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-swoop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t
      ;; If this value is t, split window inside the current window
      ;;helm-swoop-split-with-multiple-windows t
      ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
      ;;helm-swoop-split-direction 'split-window-vertically
      ;; If nil, you can slightly boost invoke speed in exchange for text color
      helm-swoop-speed-or-color t
      ;; For fuzzy-matching
      helm-swoop-use-fuzzy-match t)

(setq helm-swoop-split-window-function 'display-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm hide uninteresting buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq helm-boring-buffer-regexp-list
      '("\\` "
        "\\`\\*helm"
        "\\`\\*Echo Area"
        "\\`\\*Minibuf"
        "\\`\\*Help"
        "\\`\\*WoMan-Log"
        "\\`\\*Man -l"
        "\\`\\*Completions"))

;; https://github.com/emacs-helm/helm/issues/975
(defun dx-fun-filter-dired-buffers (buffer-list)
  (delq nil (mapcar
             (lambda (buffer)
               (if (eq (with-current-buffer buffer major-mode)  'dired-mode)
                   nil
                 buffer))
             buffer-list)))

(advice-add 'helm-skip-boring-buffers
            :filter-return 'dx-fun-filter-dired-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq projectile-completion-system 'helm)
;; no fuzziness for projectile-helm
(setq helm-projectile-fuzzy-match nil)

(helm-projectile-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq helm-yas-display-key-on-candidate t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-dash
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Download docsets from https://github.com/Kapeli/feeds

;; use emacs browser for docs
(setq helm-dash-browser-func 'eww-browse-url
      helm-dash-enable-debugging nil)

;; (defun python-doc ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("Python 3" "NumPy" "SciPy" "Matplotlib")))

;; (add-hook 'python-mode-hook 'python-doc)
(add-hook 'python-mode-hook (lambda () (setq-local helm-dash-docsets '("Python 3"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shackle - have various things appear as a popup
;; See:
;; https://github.com/ShingoFukuyama/helm-swoop/issues/62
;; https://github.com/wasamasa/shackle
;;
;; Use shackle to restrict helm:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq shackle-default-alignment 'below) ; default below

(setq shackle-rules
      '(("\\`\\*helm.*?\\*\\'"
         :regexp t :align t :size 0.4)
        ("\\*Async Shell.*\\*" :regexp t :ignore t )
        ("\\`\\*magit.*?\\*\\'" :regexp t :align t :size 0.4)
        ;; ("*undo-tree*"
        ;;  :align t :size 0.4)
        ;;("\\*[Wo]*Man.*\\*" :regexp t :align 'below :size 0.4)
        ;;("*Messages*" :align 'below :size 0.4)
        ))

;; also require the split mode to be set as follows
(setq helm-split-window-in-side-p t)
(shackle-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; popwin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar popwin:special-display-config-backup popwin:special-display-config)

;; provide behaviour for specific windows
(setq popwin:special-display-config
      '(
        (" *undo-tree*"
         :height 0.4
         :position bottom)
        ("*frequencies*"
         :height 0.4
         :position bottom)
        (" *Messages*"
         :height 0.4
         :position bottom)
        (" *Help*"
         :height 0.4
         :position bottom)
        (help-mode
         :height 0.4
         :position bottom)
        (term-mode
         :height 0.4
         :position bottom)
        (magit-mode
         :height 0.4
         :position bottom)
        (magit-status-mode
         :height 0.4
         :position bottom)
        (magit-log-mode
         :height 0.4
         :position bottom)
        ;; (" *Async Shell Command*"
        ;;  :height 0.4
        ;;  :position bottom)
        ("*Shell Command Output*")
        ("\\*[Wo]*Man.*\\*"
         :regexp t
         :height 0.4
         :position bottom)
        ))

;; '(popwin:special-display-config
;;   (quote
;;    (("*Miniedit Help*" :noselect t)
;;     (help-mode)
;;     (completion-list-mode :noselect t)
;;     (compilation-mode :noselect t)
;;     (grep-mode :noselect t)
;;     (occur-mode :noselect t)
;;     ("*Pp Macroexpand Output*" :noselect t)
;;     ("*Shell Command Output*")
;;     ("*vc-diff*")
;;     ("*vc-change-log*")
;;     (" *undo-tree*" :width 60 :position bottom)
;;     ("^\\*anything.*\\*$" :regexp t)
;;     ("*slime-apropos*")
;;     ("*slime-macroexpansion*")
;;     ("*slime-description*")
;;     ("*slime-compilation*" :noselect t)
;;     ("*slime-xref*")
;;     (sldb-mode :stick t)
;;     (slime-repl-mode)
;;     (slime-connection-list-mode))))

(popwin-mode 1)


;; (setq display-buffer-function 'popwin:display-buffer)

;; (push '("*undo-tree*" :height 0.4 :position bottom)
;;       popwin:special-display-config)

;; (push '("\\*[Wo]*Man.*\\*" :regexp t :height 0.4 :position bottom)
;;       popwin:special-display-config)

;; ;;(push '("*ansi-term.*\\*" :regexp t :height 0.4 :position bottom)
;; ;;      popwin:special-display-config)

;; (push '("*ansi-term*" :height 0.4 :position bottom)
;;       popwin:special-display-config)


;; ;; fixes for helm
;; (defun *-popwin-help-mode-off ()
;;   "Turn `popwin-mode' off for *Help* buffers."
;;   (when (boundp 'popwin:special-display-config)
;;     (customize-set-variable 'popwin:special-display-config
;;                             (delq 'help-mode popwin:special-display-config))))

;; (defun *-popwin-help-mode-on ()
;;   "Turn `popwin-mode' on for *Help* buffers."
;;   (when (boundp 'popwin:special-display-config)
;;     (customize-set-variable
;;      'popwin:special-display-config
;;      (add-to-list 'popwin:special-display-config 'help-mode nil #'eq))))

;; (add-hook 'helm-minibuffer-set-up-hook #'*-popwin-help-mode-off)
;; (add-hook 'helm-cleanup-hook #'*-popwin-help-mode-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm time zones
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(setq display-time-world-list '(("Europe/Berlin" "Berlin/Jena")
                                ("America/New_York" "New York/Boston")
                                ("Australia/Melbourne" "Melbourne")
                                ("Pacific/Auckland" "Auckland")
                                ("America/Los_Angeles" "Los Angeles")
                                ("CET" "CET")
                                ("GMT" "GMT")
                                ("UTC" "UTC")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook `c-mode-hook `rainbow-delimiters-mode)
(add-hook `emacs-lisp-mode-hook `rainbow-delimiters-mode)
(add-hook `latex-mode-hook `rainbow-delimiters-mode)
(add-hook `octave-mode-hook `rainbow-delimiters-mode)
(add-hook `org-mode-hook `rainbow-delimiters-mode)
(add-hook `sh-mode-hook `rainbow-delimiters-mode)


(add-hook 'text-mode-hook 'turn-on-auto-fill)            ; Text mode autofill


;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; provide yasnippet in various modes
(add-hook 'python-mode-hook #'yas-minor-mode)
(add-hook 'c++-mode-hook #'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook #'yas-minor-mode)

;; org-mode needs a fix
(add-hook 'org-mode-hook #'yas-minor-mode)

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

;; The way Org mode binds the <TAB> key (binding to [tab] instead of "\t")
;; overrules YASnippet's access to this key. The following code fixes this:
(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

;; dired specific
(add-hook 'dired-mode-hook #'dired-collapse-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dx-fun-dired-mode-hook)

(dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
(dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html"
                                      "jhtm" "mht" "eml" "mustache" "xhtml"))
(dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib"
                                     "json" "msg" "pgn" "rss" "yaml" "yml"
                                     "rdata"))
(dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb"
                                          "pdf" "ps" "rtf" "djvu" "epub" "odp"
                                          "ppt" "pptx"))
(dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md"
                                          "mkd" "nfo" "pod" "rst" "tex"
                                          "textfile" "txt"))
(dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb"
                                          "sqlite" "nc"))
(dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg"
                                       "mpg" "flv" "ogg" "mov" "mid" "midi"
                                       "wav" "aiff" "flac"))
(dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg"
                                       "jpg" "png" "psd" "eps" "svg"))
(dired-rainbow-define log "#c17d11" ("log"))
(dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh"
                                       "vim"))
(dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql"
                                             "mysql" "pgsql" "sql" "r" "clj"
                                             "cljs" "scala" "js"))
(dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h"
                                          "c++" "h++" "hpp" "hxx" "m" "cc" "cs"
                                          "cp" "cpp" "go" "f" "for" "ftn" "f90"
                                          "f95" "f03" "f08" "s" "rs" "hi" "hs"
                                          "pyc" ".java"))
(dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
(dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz"
                                            "xz" "z" "Z" "jar" "war" "ear"
                                            "rar" "sar" "xpi" "apk" "xz" "tar"))
(dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab"
                                          "pak" "pk3" "vdf" "vpk" "bsp"))
(dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc"
                                           "signature" "sig" "p12" "pem"))
(dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf"
                                       "otf"))
(dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow"
                                           "toast" "vcd" "vmdk" "bak"))
(dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes"
                                    "gitmodules"))
(dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customise mode names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme and initial layout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; treat as safe
(setq custom-safe-themes t)

;; Use Liberation Mono-9
(add-to-list 'default-frame-alist
             '(font . "Liberation Mono-9"))

(dx-fun-badwolf-theme-fixed)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-swoop patches
;; editing is broken is current release [07-05-19] - fix:
;; https://github.com/ShingoFukuyama/helm-swoop/issues/133
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun helm-c-source-swoop ()
  `((name . ,(buffer-name helm-swoop-target-buffer))
    (candidates . ,(if helm-swoop-list-cache
                       (progn
                         (helm-swoop--split-lines-by
                          helm-swoop-list-cache "\n"
                          helm-swoop-last-prefix-number))
                     (helm-swoop--split-lines-by
                      (setq helm-swoop-list-cache
                            (helm-swoop--get-content
                             helm-swoop-target-buffer t))
                      "\n" helm-swoop-last-prefix-number)))
    (get-line . ,(if helm-swoop-speed-or-color
                     'helm-swoop--buffer-substring
                   'buffer-substring-no-properties))
    (keymap . ,helm-swoop-map)
    (header-line . ,(substitute-command-keys
                     "[\\<helm-swoop-map>\\[helm-swoop-edit]] Edit mode, \
[\\<helm-swoop-map>\\[helm-multi-swoop-all-from-helm-swoop]] apply all buffers"))
    (action . (("Go to Line" . helm-swoop--goto-line-action)
               ("Edit" . helm-swoop--edit)))
    ,(if (and helm-swoop-last-prefix-number
              (> helm-swoop-last-prefix-number 1))
         '(multiline))
    (match . ,(helm-swoop-match-functions))
    (search . ,(helm-swoop-search-functions))))



(cl-defun helm-multi-swoop--exec (ignored &key $query $buflist $func $action)
  (interactive)
  (setq helm-swoop-synchronizing-window (selected-window))
  (setq helm-swoop-last-point
        (or helm-multi-swoop-all-from-helm-swoop-last-point
            (cons (point) (buffer-name (current-buffer)))))
  (setq helm-swoop-last-line-info
        (cons (current-buffer) (line-number-at-pos)))
  (unless (get-buffer helm-multi-swoop-buffer-list)
    (get-buffer-create helm-multi-swoop-buffer-list))
  (helm-swoop--set-prefix (prefix-numeric-value current-prefix-arg))
  (let (($buffs (or $buflist
                    (helm-multi-swoop--get-marked-buffers)
                    `(,(buffer-name helm-swoop-target-buffer))
                    (error "No buffer selected")))
        $contents
        $preserve-position
        ($prefix-arg (prefix-numeric-value
                      (or current-prefix-arg helm-swoop-last-prefix-number 1))))
    (helm-swoop--set-prefix $prefix-arg)
    (setq helm-multi-swoop-last-selected-buffers $buffs)
    ;; Create buffer sources
    (mapc (lambda ($buf)
            (when (get-buffer $buf)
              (with-current-buffer (get-buffer $buf)
                (let* (($func
                        (or $func
                            (lambda ()
                              (helm-swoop--split-lines-by
                               (helm-swoop--get-content $buf t)
                               "\n" $prefix-arg))))
                       ($action
                        (or $action
                            `(("Go to Line"
                               . (lambda ($line)
                                   (switch-to-buffer ,$buf)
                                   (helm-swoop--goto-line
                                    (when (string-match "^[0-9]+" $line)
                                      (string-to-number
                                       (match-string 0 $line))))
                                   (when (re-search-forward
                                          (mapconcat 'identity
                                                     (split-string
                                                      helm-pattern " ") "\\|")
                                          nil t)
                                     (helm-swoop-flash-word (match-beginning 0) (match-end 0))
                                     (goto-char (match-beginning 0)))
                                   (helm-swoop--recenter)))
                              ("Edit" . helm-multi-swoop--edit)))))
                  (setq $preserve-position
                        (cons (cons $buf (point)) $preserve-position))
                  (setq
                   $contents
                   (cons
                    (helm-c-source-multi-swoop $buf $func $action $prefix-arg)
                    $contents))))))
          $buffs)
    (unwind-protect
        (progn
          (ad-enable-advice 'helm-next-line 'around
                            'helm-multi-swoop-next-line)
          (ad-activate 'helm-next-line)
          (ad-enable-advice 'helm-previous-line 'around
                            'helm-multi-swoop-previous-line)
          (ad-activate 'helm-previous-line)
          (ad-enable-advice 'helm-toggle-visible-mark 'around
                            'helm-multi-swoop-toggle-visible-mark)
          (ad-activate 'helm-toggle-visible-mark)
          (ad-enable-advice 'helm-move--next-line-fn 'around
                            'helm-multi-swoop-next-line-cycle)
          (ad-activate 'helm-move--next-line-fn)
          (ad-enable-advice 'helm-move--previous-line-fn 'around
                            'helm-multi-swoop-previous-line-cycle)
          (ad-activate 'helm-move--previous-line-fn)
          (add-hook 'helm-update-hook 'helm-swoop--pattern-match)
          (add-hook 'helm-after-update-hook 'helm-swoop--keep-nearest-position t)
          (setq helm-swoop-line-overlay
                (make-overlay (point) (point)))
          (overlay-put helm-swoop-line-overlay
                       'face 'helm-swoop-target-line-face)
          (helm-swoop--target-line-overlay-move)
          ;; Execute helm
          (let ((helm-display-function helm-swoop-split-window-function)
                (helm-display-source-at-screen-top nil)
                (helm-completion-window-scroll-margin 5))
            (helm :sources $contents
                  :buffer helm-multi-swoop-buffer
                  :input (or $query helm-multi-swoop-query "")
                  :prompt helm-swoop-prompt
                  :candidate-number-limit
                  helm-multi-swoop-candidate-number-limit
                  :preselect
                  (regexp-quote
                   (format "%s %s" (line-number-at-pos)
                           (helm-swoop--get-string-at-line))))))
      ;; Restore
      (progn
        (when (= 1 helm-exit-status)
          (helm-swoop-back-to-last-point t)
          (helm-swoop--restore-unveiled-overlay))
        (setq helm-swoop-invisible-targets nil)
        (ad-disable-advice 'helm-next-line 'around
                           'helm-multi-swoop-next-line)
        (ad-activate 'helm-next-line)
        (ad-disable-advice 'helm-previous-line 'around
                           'helm-multi-swoop-previous-line)
        (ad-activate 'helm-previous-line)
        (ad-disable-advice 'helm-toggle-visible-mark 'around
                           'helm-multi-swoop-toggle-visible-mark)
        (ad-activate 'helm-toggle-visible-mark)
        (ad-disable-advice 'helm-move--next-line-fn 'around
                           'helm-multi-swoop-next-line-cycle)
        (ad-activate 'helm-move--next-line-fn)
        (ad-disable-advice 'helm-move--previous-line-fn 'around
                           'helm-multi-swoop-previous-line-cycle)
        (ad-activate 'helm-move--previous-line-fn)
        (remove-hook 'helm-update-hook 'helm-swoop--pattern-match)
        (remove-hook 'helm-after-update-hook 'helm-swoop--keep-nearest-position)
        (setq helm-multi-swoop-last-query helm-swoop-pattern)
        (helm-swoop--restore-unveiled-overlay)
        (setq helm-multi-swoop-query nil)
        (setq helm-multi-swoop-all-from-helm-swoop-last-point nil)
        (mapc (lambda ($buf)
                (let (($current-buffer (buffer-name (current-buffer))))
                  (with-current-buffer (car $buf)
                    ;; Delete overlay
                    (delete-overlay helm-swoop-line-overlay)
                    (helm-swoop--delete-overlay 'target-buffer)
                    ;; Restore each buffer's position
                    (unless (equal (car $buf) $current-buffer)
                      (goto-char (cdr $buf))))))
              $preserve-position)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nlinum-hl fixes [ripped directly from github]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A shotgun approach that refreshes line numbers on a regular basis:
;; Runs occasionally, though unpredictably
(add-hook 'post-gc-hook #'nlinum-hl-flush-all-windows)

;; whenever Emacs loses/gains focus
(add-hook 'focus-in-hook  #'nlinum-hl-flush-all-windows)
(add-hook 'focus-out-hook #'nlinum-hl-flush-all-windows)
;; ...or switches windows
(advice-add #'select-window :before #'nlinum-hl-do-select-window-flush)
(advice-add #'select-window :after  #'nlinum-hl-do-select-window-flush)

;; after X amount of idle time
(run-with-idle-timer 5 t #'nlinum-hl-flush-window)
(run-with-idle-timer 30 t #'nlinum-hl-flush-all-windows)


;; With `markdown-fontify-code-blocks-natively' enabled in `markdown-mode',
;; line numbers tend to vanish next to code blocks.
(advice-add #'markdown-fontify-code-block-natively
            :after #'nlinum-hl-do-markdown-fontify-region)

;; When using `web-mode's code-folding an entire range of line numbers will
;; vanish in the affected area.
(advice-add #'web-mode-fold-or-unfold :after #'nlinum-hl-do-generic-flush)

;; Changing fonts can leave nlinum line numbers in their original size; this
;; forces them to resize.
(add-hook 'after-setting-font-hook #'nlinum-hl-flush-all-windows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nlinum
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Preset `nlinum-format' for minimum width.
(defun dx-fun-nlinum-mode-hook ()
  (when nlinum-mode
    (setq-local nlinum-format
                (concat "%" (number-to-string
                             ;; Guesstimate number of buffer lines.
                             (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
                        "d"))))

(add-hook 'nlinum-mode-hook #'dx-fun-nlinum-mode-hook)

(setq global-nlinum-mode nil)

(add-hook 'c-mode-hook #'nlinum-mode)
(add-hook 'c++-mode-hook #'nlinum-mode)
(add-hook 'python-mode-hook #'nlinum-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pdf-tools
;; https://github.com/politza/pdf-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initialise
(pdf-tools-install)
(setq-default pdf-view-display-size 'fit-page)
;; automatically annotate highlights
(setq pdf-annot-activate-created-annotations t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eyebrowse
;; https://github.com/wasamasa/eyebrowse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eyebrowse-mode t)
(setq eyebrowse-mode-line-separator ",")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq magit-display-buffer-function (quote display-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; term-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; kill buffer after terminal process terminates
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyfreq
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq keyfreq-file dx-file-name-keyfreq)
(setq keyfreq-file-lock dx-file-name-keyfreq-lock)

(setq keyfreq-excluded-commands
      '(
        abort-recursive-edit
        forward-char
        backward-char
        left-char
        right-char
        left-word
        right-word
        backward-paragraph
        forward-paragraph
        self-insert-command
        next-line
        previous-line
        windmove-left
        windmove-right
        windmove-up
        windmove-down
        move-end-of-line
        move-beginning-of-line
        dired-next-line
        dired-previous-line
        helm-next-line
        helm-previous-line
        company-ignore
        keyboard-quit
        set-mark-command
        sp-backward-delete-char
        newline-and-indent
        ))

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(defun dx-fun-keyfreq-save-html ()
  "Save the table of frequently used commands (and their associated bindings
to an html file in `user-emacs-directory'."
  (interactive)
  (keyfreq-html (locate-user-emacs-file "keyfreq.html"))
  (message "Dumped key frequency statistics to html"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modeline renaming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "beacon" '(diminish 'beacon-mode ""))
(eval-after-load "which-key" '(diminish 'which-key-mode ""))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode ""))
(eval-after-load "highlight-symbol" '(diminish 'highlight-symbol-mode ""))
(eval-after-load "whitespace" '(diminish 'global-whitespace-mode ""))
(eval-after-load "drag-stuff" '(diminish 'drag-stuff-mode ""))
(eval-after-load "smartparens" '(diminish 'smartparens-mode ""))
(eval-after-load "company" '(diminish 'company-mode ""))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode ""))
(eval-after-load "mode_sensitive" '(diminish 'sensitive-minor-mode "§"))
(eval-after-load "abbrev" '(diminish 'abbrev-mode ""))
(eval-after-load "eldoc" '(diminish 'eldoc-mode ""))

;; (add-hook 'dired-mode-hook (lambda () (diminish 'dired-omit-mode "ø")))
;;(eval-after-load "dired" '(diminish 'dired-omit-mode "ø"))

(defadvice dired-omit-startup (after diminish-dired-omit activate)
  "Make sure to remove \"Omit\" from the modeline."
  (diminish 'dired-omit-mode) dired-mode-map)

;; projectile needs a hack
(eval-after-load "projectile"
  '(diminish 'projectile-mode ""))

(setq projectile-after-switch-project-hook
      (lambda()
        (diminish 'projectile-mode (format " Π[%s]" (projectile-project-name)))
        ))

;; shorten major modes
(setq cyphejor-rules
      '(:upcase
        ;; ("bookmark"    "→")
        ;; ("buffer"      "β")
        ;; ("diff"        "Δ")
        ("dired"       "δ")
        ("emacs"       "ε")
        ("inferior"    "i" :prefix)
        ("interaction" "i" :prefix)
        ("interactive" "i" :prefix)
        ("lisp"        "λ" :postfix)
        ("magit"        "μ" :prefix)
        ;; ("menu"        "▤" :postfix)
        ("mode"        "")
        ("org"       "Ω")
        ;; ("package"     "↓")
        ("python"      "π")
        ;; ("shell"       "sh" :postfix)
        ("term"        "τ")
        ;; ("text"        "ξ")
        ("wdired"      "↯δ")
        ))

(cyphejor-mode 1)

;;
;;  :D
;;
