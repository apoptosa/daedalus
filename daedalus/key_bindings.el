;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove some bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-keys :map global-map
           ("<menu>" . nil)      ;; M-x
           ("C-z" . nil)         ;; suspend-frame
           ("C-x C-n" . nil)     ;; set-goal-column
           ("M-o" . nil)         ;; facemenu-keymap
           ("C-x o" . nil)       ;; other-window
           ("C-x ]" . nil)       ;; forward-page
           ("C-x [" . nil)       ;; backward-page
           ("C-x <" . nil)       ;; scroll-left
           ("C-x >" . nil)       ;; scroll-right
           ("C-b" . nil)         ;; backward-char
           ("C-f" . nil)         ;; forward-char
           ("C-v" . nil)         ;; scroll-up-command
           ("M-v" . nil)         ;; scroll-down-command
           ("C-r" . nil)         ;; isearch-backward-regexp
           ;; with helm and many buffers these are useless:
           ;; next-buffer
           ("<XF86Forward>" . nil)
           ("C-x <C-right>" . nil)
           ("C-x <right>" . nil)
           ;; previous-buffer
           ("<XF86Back>" . nil)
           ("C-x <C-left>" . nil)
           ("C-x <left>" . nil)
           ("C-x h" . nil)       ;; mark-whole-buffer
           ("C-x C-p" . nil)     ;; mark-page
           ("C-x $" . nil)       ;; selective-display
           ("C-x C-z" . nil)     ;; suspend-frame
           ("C-t" . nil)         ;; transpose-chars
           )

(bind-keys :map global-map
           ("<down-mouse-1>" . nil)
           ("<down-mouse-2>" . nil)
           ("<down-mouse-3>" . nil)
           ("<mouse-1>" . nil)
           ("<mouse-2>" . nil)
           ("<mouse-3>" . nil)
           ("<wheel-up>" . nil)
           ("<wheel-down>" . nil)
           )


;; Nuke all C-h commands
(global-unset-key (kbd "C-h"))
(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<help>"))

(global-unset-key (kbd "C-c C-w"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Specify key bindings                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; emacs specific
;;
(define-key global-map (kbd "RET") 'newline-and-indent)  ;; auto indent

(global-set-key (kbd "C-x C-c") 'dx-fun-kill-emacs)
(global-set-key (kbd "C-x k") 'dx-fun-volatile-kill-buffer)

(global-set-key (kbd "C-.") 'highlight-symbol-next)
(global-set-key (kbd "C-,") 'highlight-symbol-prev)


;; Key bindings for multiple cursors.
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)


;; Shotcut for running ansi-term and bash
(global-set-key
 (kbd "C-c a") '(lambda ()  (interactive) (ansi-term "/bin/bash")))


;; Expand / contract region
(global-set-key (kbd "C-c e") 'er/expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c c") 'er/contract-region)


;; dired
(global-set-key (kbd "C-x C-j") 'dired-jump)
(define-key dired-mode-map (kbd "C-M-u") 'dired-up-directory)
;; (define-key dired-mode-map (kbd "M-o") #'my/dired-open)
(define-key dired-mode-map (kbd "C-x C-q") 'wdired-change-to-wdired-mode)
(bind-key "l" #'dired-up-directory dired-mode-map)
(bind-key "M-!" #'async-shell-command dired-mode-map)


;; Projectile bindings
;;(bind-key "C-c p b" #'projectile-switch-to-buffer #'projectile-command-map)
;;(bind-key "C-c p K" #'projectile-kill-buffers #'projectile-command-map)


;; recentf
;; get rid of `find-file-read-only' and replace it with something
;; more useful.
;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)
;; DEPRECATED for helm


;; helm
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-`") 'helm-resume)
(global-set-key (kbd "C-M-z") 'helm-resume)

(global-set-key (kbd "M-x") 'helm-M-x)
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x C-i") 'helm-semantic-or-imenu)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; for swoop
;; (global-set-key (kbd "C-s") 'helm-swoop-from-isearch)
(define-key isearch-mode-map (kbd "C-s") 'helm-swoop-from-isearch)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "C-z") 'helm-swoop)
(global-set-key (kbd "C-S-z") 'helm-multi-swoop-all)


(bind-key* "C-M-i" 'helm-multi-swoop-all)

(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)


;; Move up and down like isearch
;; (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
;; (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
;; (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
;; (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)


;; helm-mini
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-s") 'helm-mini)


;; helm-occur
;; (bind-key* "C-o" 'helm-occur)
(global-set-key (kbd "C-c C-o") 'helm-occur)
(global-set-key (kbd "C-x M-o") 'helm-occur)


(bind-keys :map helm-swoop-map
           ("C-z e" . helm-swoop-edit)
           ("C-z w" . helm-yank-selection))

(bind-keys :map helm-multi-swoop-map
           ("C-z e" . helm-multi-swoop-edit)
           ("C-z w" . helm-yank-selection))

;; dired
(bind-keys :map dired-mode-map
           ("i" . dired-subtree-insert)
           (";" . dired-subtree-remove)
           ("n" . dired-hacks-next-file)
           ("p" . dired-hacks-previous-file)
           ("e" . dx-xah-open-in-external-app)
           ;; With C-u prefix expands N deep.
           ;; C-u TAB will expand 4
           ;; C-u C-u TAB will expand 16
           ;; C-u C-u C-u TAB will expand 64
           ("TAB" . dired-subtree-cycle))


;; magit
(global-unset-key (kbd "C-x g"))
(global-set-key (kbd "C-x g h") 'magit-log)
(global-set-key (kbd "C-x g f") 'magit-file-log)
(global-set-key (kbd "C-x g b") 'magit-blame-mode)
(global-set-key (kbd "C-x g m") 'magit-branch-manager)
(global-set-key (kbd "C-x g c") 'magit-branch)
(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-x g r") 'magit-reflog)
(global-set-key (kbd "C-x g t") 'magit-tag)

;; eyebrowse
;; remove stupid default
(bind-keys :map eyebrowse-mode-map
           ("C-c C-w" . nil)
           )

;; uniformise
(bind-keys :map help-mode-map ("z" . quit-window))

;; prettify descriptions of keys
;; (which-key-add-key-based-replacements
;;   "C-c a" "cmd: ansi-term"
;;   "C-c C-q" "inp: indent to stagnation"
;;   "C-c e" "inp: expand region"
;;   "C-c c" "inp: contract region"
;;   "C-c C->" "inp: mark all selected"
;;   )

;; C-; is free, use that
(bind-keys :map eyebrowse-mode-map
           ("C-; 0" . eyebrowse-switch-to-window-config-0)
           ("C-; 1" . eyebrowse-switch-to-window-config-1)
           ("C-; 2" . eyebrowse-switch-to-window-config-2)
           ("C-; 3" . eyebrowse-switch-to-window-config-3)
           ("C-; 4" . eyebrowse-switch-to-window-config-4)
           ("C-; 5" . eyebrowse-switch-to-window-config-5)
           ("C-; 6" . eyebrowse-switch-to-window-config-6)
           ("C-; 7" . eyebrowse-switch-to-window-config-7)
           ("C-; 8" . eyebrowse-switch-to-window-config-8)
           ("C-; 9" . eyebrowse-switch-to-window-config-9)
           ("C-; c" . eyebrowse-close-window-config)
           ("C-; r" . eyebrowse-rename-window-config)
           )


;; prettify descriptions of keys
(which-key-add-key-based-replacements
  "C-; 0" "Switch to [0]"
  "C-; 1" "Switch to [1]"
  "C-; 2" "Switch to [2]"
  "C-; 3" "Switch to [3]"
  "C-; 4" "Switch to [4]"
  "C-; 5" "Switch to [5]"
  "C-; 6" "Switch to [6]"
  "C-; 7" "Switch to [7]"
  "C-; 8" "Switch to [8]"
  "C-; 9" "Switch to [9]"
  "C-; c" "Window config: close"
  "C-; r" "Window config: rename"
  )



;; Insert some useful commands as prefix
(bind-keys :map global-map
           ("C-v z k" . describe-key)
           ("C-v z \\" . describe-input-method)
           ("C-v z f" . describe-function)
           ("C-v z s" . describe-symbol)
           ("C-v z v" . describe-variable)
           ("C-v z w" . where-is)
           )

;; prettify descriptions of keys
(which-key-add-key-based-replacements
  "C-v z" "info [describe-]"
  "C-v z k" "key"
  "C-v z \\" "input-method"
  "C-v z f" "function"
  "C-v z s" "symbol"
  "C-v z v" "variable"
  "C-v z w" "where-is"
  )

;; Add helm bindings
(global-set-key (kbd "C-v SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-v a") 'helm-apropos)
(global-set-key (kbd "C-v b") 'helm-descbinds)
(global-set-key (kbd "C-v c") 'helm-calcul-expression)
(global-set-key (kbd "C-v f") 'helm-find-files)
(global-set-key (kbd "C-v g") 'helm-do-ag)
(global-set-key (kbd "C-v i") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-v k") 'helm-show-kill-ring)
(global-set-key (kbd "C-v l") 'helm-locate)
(global-set-key (kbd "C-v m") 'helm-man-woman)
(global-set-key (kbd "C-v o") 'helm-top)
(global-set-key (kbd "C-v p") 'helm-browse-project)
(global-set-key (kbd "C-v q") 'keyfreq-show)
(global-set-key (kbd "C-v r") 'helm-bookmarks)
(global-set-key (kbd "C-v t") 'helm-world-time)
(global-set-key (kbd "C-v u") 'helm-google-suggest)
(global-set-key (kbd "C-v v") 'magit)
(global-set-key (kbd "C-v w") 'helm-surfraw)
(global-set-key (kbd "C-v x") 'helm-regexp)
(global-set-key (kbd "C-v y") 'helm-yas-complete)

;; (global-set-key (kbd "C-v h g") 'helm-info-gnus)
;; (global-set-key (kbd "C-v h p") 'helm-info-at-point)
;; (global-set-key (kbd "C-v h e") 'helm-info-emacs)

(global-set-key (kbd "C-v h c") 'helm-colors)
(global-set-key (kbd "C-v h p") 'helm-list-emacs-process)


;; prettify descriptions of keys
(which-key-add-key-based-replacements
  "C-v SPC" "all-mark-rings"
  "C-v a" "apropos"
  "C-v b" "descbinds"
  "C-v c" "calcul-expression"
  "C-v f" "find-files"
  "C-v g" "do-ag"
  "C-v i" "semantic-or-imenu"
  "C-v k" "show-kill-ring"
  "C-v l" "locate"
  "C-v m" "man-woman"
  "C-v o" "top"
  "C-v p" "browse-project"
  "C-v q" "keyfreq-show"
  "C-v r" "bookmarks"
  "C-v t" "world-time"
  "C-v u" "google-suggest"
  "C-v v" "magit"
  "C-v w" "surfraw"
  "C-v x" "regexp"
  "C-v y" "yas-complete"
  "C-v h" "misc"
  "C-v h c" "colors"
  "C-v h p" "list-emacs-process"
  )



;; cycle over input methods
(bind-keys :map global-map
           ("C-\\" . dx-fun-input-methods-cycler)
           )


(global-set-key (kbd "<f1>") 'dx-fun-toggle-selective-display)


;; project specific
(bind-keys :map global-map
           ("C-f a" . helm-projectile-find-file-in-known-projects)
           ("C-f f" . helm-projectile-find-file)
           ("C-f g" . helm-projectile-ag)
           ("C-f o" . helm-projectile-find-other-file)
           ("C-f p" . helm-projectile)
           ("C-f s" . helm-projectile-switch-project)
           ("C-f z" . helm-multi-swoop-projectile)
           )


;; prettify descriptions of keys
(which-key-add-key-based-replacements
  "C-f a" "find-file-in-known-projects"
  "C-f f" "find-file"
  "C-f g" "ag"
  "C-f o" "find-other-file"
  "C-f p" "projectile"
  "C-f s" "switch-project"
  "C-f z" "multi-swoop"
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer centric
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-keys :map global-map
           ("C-b b" . dx-fun-new-empty-buffer)
           )

(which-key-add-key-based-replacements
  "C-b b" "new-empty-buffer"
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hydras
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq hydra-hint-display-type #'message)

(setq hydra-amaranth-warn-message
      (concat "Select an option or quit with "
              (dx-fun-hydra-format-key-string
               "q"   ;; quit key
               #'hydra-face-blue)
              "."))


(defhydra hydra-controller
  (:foreign-keys warn :hint nil)
  ;; "
  ;; [_a_] applications
  ;; "
  ;;  "Controller for nested hydras."
  ("<menu>" helm-M-x :exit t)
  ("SPC" helm-swoop :exit t)
  ("<tab>" helm-mini :exit t)
  ("r" hydra-applications/body "run/applications" :exit t)
  ("b" hydra-buffer/body "buffer" :exit t)
  ("f" hydra-files/body "files" :exit t)
  ("s" hydra-search-symbol/body "search/symbol" :exit t)
  ("i" hydra-insertion/body "insertion" :exit t)
  ("p" hydra-project/body "project" :exit t)
  ("g" hydra-git-vc/body "git/vc" :exit t)
  ("c" hydra-compile/body "compile" :exit t)
  ("x" hydra-text/body "text" :exit t)
  ("m" hydra-major-mode "mode" :exit t)
  ("w" hydra-window/body "window" :exit t)
  ("t" hydra-toggle/body "toggles" :exit t)
  ("h" hydra-help/body "help/info" :exit t)
  ("z" hydra-quit/body "quit" :exit t)
  ("<f1>" helm-apropos "apropos" :exit t)
  ("C" hydra-config/body "config" :exit t)
  ("q" nil :exit t)
  )

(defhydra hydra-applications
  (:foreign-keys warn :hint nil)
  ("t" (lambda ()  (interactive) (ansi-term "/bin/bash"))
   "ansi-term" :exit t)
  ("c" helm-calcul-expression "calcul-expression" :exit t)
  ("o" helm-top "top" :exit t)
  ("u" helm-surfraw "surfraw" :exit t)
  ("g" helm-google-suggest "google-suggest" :exit t)
  ("w" wolfram-alpha "wolfram-alpha" :exit t)
  ("l" helm-list-emacs-process "list-emacs-process" :exit t)
  ("q" nil :exit t)
  )

(defhydra hydra-buffer-switch
  (:foreign-keys warn :hint nil)
  ("m" daedalus/fun/buffer/switch-buffer-to-messages "messages" :exit t)
  ("s" daedalus/fun/buffer/switch-buffer-to-scratch "scratch" :exit t)
  ("q" nil :exit t)
  )


(defhydra hydra-buffer
  (:foreign-keys warn :hint nil)
  ("SPC" helm-all-mark-rings "all-mark-rings" :exit t)
  ("u" undo-tree-visualize "undo-tree-visualize" :exit t)
  ("w" kill-ring-save "kill-ring-save" :exit t)
  ("y" yank "yank" :exit t)
  ("W" daedalus/fun/buffer/copy-whole-buffer-to-clipboard
   "copy-whole-buffer-to-clipboard" :exit t)
  ("Y" daedalus/fun/buffer/copy-clipboard-to-whole-buffer
   "copy-clipboard-to-whole-buffer" :exit t)
  ("n" dx-fun-new-empty-buffer "new-empty-buffer" :exit t)
  ("k" dx-fun-volatile-kill-buffer "kill-buffer" :exit t)
  ("s" hydra-buffer-switch/body
   (propertize "+switch-buffer" 'face #'hydra-face-teal)
   :exit t)
  ("q" nil :exit t)
  )

(defhydra hydra-insertion
  (:foreign-keys warn :hint nil)
  ("y" helm-yas-complete "yas-complete" :exit t)
  ("q" nil :exit t)
  )

(defhydra hydra-files-yank
  (:foreign-keys warn :hint nil)
  ("f" dx-fun-copy-file-name "file-name" :exit t)
  ("p" dx-fun-copy-file-path "file-path" :exit t)
  ("d" dx-fun-copy-directory-path "directory-path" :exit t)
  ("P" dx-fun-copy-file-path-with-line-column
   "file-path-with-line-column" :exit t)
  ("q" nil :exit t)
  )

(defhydra hydra-files
  (:foreign-keys warn :hint nil)
  ("b" helm-bookmarks "bookmarks" :exit t)
  ("f" helm-find-files "find-files" :exit t)
  ("l" helm-locate "locate" :exit t)
  ("y" hydra-files-yank/body
   (propertize "+yank" 'face #'hydra-face-teal)
   :exit t)
  ("s" save-buffer "save-buffer" :exit t)
  ("r" dx-fun-rename-current-buffer-file
   "rename-current-buffer-file" :exit t)
  ("d" dx-fun-delete-current-buffer-file
   "delete-current-buffer-file" :exit t)
  ("q" nil :exit t)
  )

(defhydra hydra-git-vc
  (:foreign-keys warn :hint nil)
  ("s" magit-status "magit-status" :exit t)
  ("q" nil :exit t)
  )

(defhydra hydra-compile
  (:foreign-keys warn :hint nil)
  ;; ("e" eval-buffer "eval-buffer" :exit t)
  ("q" nil :exit t)
  )

(defhydra hydra-search-symbol-do-ag
  (:foreign-keys warn :hint nil)
  ("a" helm-do-ag "do-ag" :exit t)
  ("b" helm-do-ag-buffers "do-ag-buffers" :exit t)
  ("p" helm-do-ag-project-root "do-ag-project-root" :exit t)
  ("q" nil :exit t)
  )


(defhydra hydra-search-symbol
  (:foreign-keys warn :hint nil)
  ("s" helm-swoop "swoop" :exit t)
  ("m" helm-multi-swoop-all "multi-swoop-all" :exit t)
  ("b" helm-mini "mini" :exit t)
  ("i" helm-semantic-or-imenu "semantic-or-imenu" :exit t)
  ("o" helm-occur "occur" :exit t)
  ("a" hydra-search-symbol-do-ag/body
   (propertize "+ag" 'face #'hydra-face-teal)
   :exit t)
  ("q" nil :exit t)
  )

(defhydra hydra-project
  (:foreign-keys warn :hint nil)
  ("o" helm-projectile-find-other-file "find-other-file")
  ("p" helm-projectile-ag "ag" :exit t)
  ("m" helm-multi-swoop-projectile "multi-swoop" :exit t)
  ("f" helm-projectile-find-file "find-file" :exit t)
  ("a" helm-projectile-find-file-in-known-projects
   "find-file-in-known-projects" :exit t)
  ("b" helm-browse-project :exit t)
  ("s" helm-projectile-switch-project "switch-project" :exit t)
  ("q" nil :exit t)
  )

(defhydra hydra-help
  (:foreign-keys warn :hint nil)
  ("b" helm-descbinds "descbinds" :exit t)
  ("m" helm-man-woman "man-woman" :exit t)
  ("t" helm-world-time "world-time" :exit t)
  ("k" keyfreq-show "keyfreq-show" :exit t)
  ("r" helm-regexp "regexp" :exit t)
  ("c" helm-colors "colors" :exit t)
  ("q" nil :exit t)
  )

(defhydra hydra-text
  (:foreign-keys warn :hint nil)
  ("c" dx-fun-input-methods-cycler "input-methods-cycler")
  ("q" nil :exit t)
  )

(defhydra hydra-window
  (:foreign-keys warn :hint nil)
  (concat (dx-fun-hydra-format-key-string "0...9" #'hydra-face-amaranth)
          ": switch-to-window-config, "
          " [_b_]: balance-windows,"
          " [_m_]: maximize-window,"
          " [_o_]: other-window"
          " [_s_]: window-split-single-column"
          " [_d_]: window-split-double-column"
          " [_t_]: window-split-triple-column"
          " [_g_]: window-split-grid"
          ".")
  ("0" eyebrowse-switch-to-window-config-0)
  ("1" eyebrowse-switch-to-window-config-1)
  ("2" eyebrowse-switch-to-window-config-2)
  ("3" eyebrowse-switch-to-window-config-3)
  ("4" eyebrowse-switch-to-window-config-4)
  ("5" eyebrowse-switch-to-window-config-5)
  ("6" eyebrowse-switch-to-window-config-6)
  ("7" eyebrowse-switch-to-window-config-7)
  ("8" eyebrowse-switch-to-window-config-8)
  ("9" eyebrowse-switch-to-window-config-9)
  ("b" balance-windows)
  ("o" other-window)
  ("m" maximize-window)
  ("s" spacemacs/window-split-single-column)
  ("d" spacemacs/window-split-double-columns)
  ("t" spacemacs/window-split-triple-columns)
  ("g" spacemacs/window-split-grid)
  ("q" nil :exit t)
  )

(defhydra hydra-quit
  (:foreign-keys warn :hint nil)
  ("z" dx-fun-kill-emacs "kill-emacs" :exit t)
  ("q" nil :exit t)
  )

(defhydra hydra-config
  (:foreign-keys warn :hint nil)
  ("c" customize-group "customize-group" :exit t)
  ;;("k" (spacemacs/find-dotfile)
  ;; "key_bindings.el" :exit t)
  ("q" nil :exit t)
  )


;; major mode Hydras
;; (pretty-hydra-define hydra-mm-elisp
;;   (:hint nil :color amaranth :quit-key "z" :title nil)
;;   ("Basic"
;;    (("e" eval-buffer "eval-buffer" :toggle nil :exit t)
;;     ("w" whitespace-mode "whitespace" :toggle t)
;;     )
;;    )
;;   )

(defhydra hydra-mm-elisp
  (:foreign-keys warn :hint nil)
  ("e" eval-buffer "eval-buffer" :exit t)
  ("r" eval-region "eval-region" :exit t)
  ("q" nil :exit t)
  )



(defun hydra-major-mode ()
  (interactive)
  (cl-case major-mode
    (emacs-lisp-mode
     (hydra-mm-elisp/body))
    (lisp-mode                         ;; fall-back
     (hydra-mm-elisp/body))
    (lisp-interaction-mode             ;; fall-back
     (hydra-mm-elisp/body))
    ;; (c-mode
    ;;  (hydra-mm-c/body))
    ;; (org-mode
    ;;  (hydra-mm-org/body))
    (t
     (error "Hydra for %S not supported" major-mode))))



(defhydra hydra-toggle
  (:foreign-keys warn :hint nil)
  ;; ("c" dx-fun-input-methods-cycler "input-methods-cycler")
  ("s" smartparens-strict-mode
   (dx-make-hydra-toggle "smartparens-strict-mode" smartparens-strict-mode))
  ("n" nlinum-mode
   (dx-make-hydra-toggle "nlinum-mode" nlinum-mode))
  ("q" nil :exit t)
  )

;; allow hydra to take over binding
;; (global-set-key (kbd "<menu>") 'hydra-controller/body)


;; shortcut for hydra-controller to prefix keys

(general-define-key
 :state 'normal
 "<menu>" 'hydra-controller/body
 "C-r" 'hydra-applications/body
 "C-b" 'hydra-buffer/body
 "C-f" 'hydra-files/body
 "C-t" 'hydra-toggle/body
 ;; "C-m" 'major-mode-hydra
 )


