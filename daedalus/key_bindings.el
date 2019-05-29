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
(global-set-key (kbd "C-,") 'highlight-symbol-previous)


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



(defhydra hydra-controller (global-map "<menu>")
  ;;  "Controller for nested hydras."
  ("a" hydra-applications/body "applications" :exit t)
  ("b" hydra-buffer/body "buffer" :exit t)
  ("f" hydra-files/body "files" :exit t)
  ("s" hydra-search-symbol/body "search/symbol" :exit t)
  ("p" hydra-project/body "project" :exit t)
  ("g" hydra-git-vc/body "git/version control" :exit t)
  ("c" hydra-compile/body "compile" :exit t)
  ("h" hydra-help/body "help/info" :exit q)
  ("t" nil "exit" :exit t)
  )

(defhydra hydra-applications ()
  ("b" helm-bookmarks "bookmarks" :exit t)
  ("q" nil "exit" :exit t)
  )


(defhydra hydra-buffer ()
  ("n" dx-fun-new-empty-buffer "new-empty-buffer" :exit t)
  ("k" dx-fun-volatile-kill-buffer "kill-buffer" :exit t)
  ("s" save-buffer "save-buffer" :exit t)
  ("w" kill-ring-save "kill-ring-save" :exit t)
  ("y" yank "yank" :exit t)
  ("q" nil "exit" :exit t)
  )


(defhydra hydra-files ()
  ("b" helm-bookmarks "bookmarks" :exit t)
  ("f" helm-find-files "find-files" :exit t)
  ("l" helm-locate "locate" :exit t)
  ("q" nil "exit" :exit t)
  )

(defhydra hydra-git-vc ()
  ("s" magit-status "magit-status" :exit t)
  ("q" nil "exit" :exit t)
  )

(defhydra hydra-compile ()
  ("e" eval-buffer "eval-buffer" :exit t)
  ("q" nil "exit" :exit t)
  )

(defhydra hydra-search-symbol ()
  ("s" helm-swoop "swoop" :exit t)
  ("q" nil "exit" :exit t)
  )

(defhydra hydra-project ()
  ("s" helm-projectile-switch-project "switch-project" :exit t)
  ("q" nil "exit" :exit t)
  )

(defhydra hydra-help ()
  ("t" helm-world-time "world-time" :exit t)
  ("q" nil "exit" :exit t)
  )


;; allow hydra to take over binding
(global-set-key (kbd "<menu>") 'hydra-controller/body)

;; provide key-chord for hydra
(setq key-chord-two-keys-delay 0.02)
(key-chord-define-global "  " 'hydra-controller/body)

(key-chord-define-global "ss" 'helm-swoop)


(let ((inhibit-message t))
  ;; Suppress spam
  (key-chord-mode 1)
  )


