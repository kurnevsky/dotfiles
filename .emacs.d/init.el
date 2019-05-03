;; Hotkeys (C = ctrl, M = alt, S = shift):
;; C-h k - see commant binded to hotkey.
;; C-h v - info about variable.
;; C-h f - info about function.
;; C-h a - search by all variables.
;; C-x b - switch buffer.
;; C-x o - switch frame.
;; C-x k - kill buffer.
;; C-x 0 - close buffer.
;; C-x C-e - run lisp command.
;; C-home - go to beginning of buffer.
;; C-end - go to ending of buffer.
;; C-f C-w - find the word after cursor.

;; ========== Configure emacs ==========

;; Remove gap in maximized window mode.
(setq frame-resize-pixelwise t)
;; Start in maximized window mode.
(toggle-frame-maximized)
;; Disable tool bar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; Set font.
(set-face-attribute 'default nil :font "DejaVu Sans Mono:pixelsize=15")
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono:pixelsize=15"))
;; Don't do backup files.
(setq make-backup-files nil)
;; Don't save discarded files.
(setq auto-save-default nil)
;; Inhibit startup/splash screen.
(setq inhibit-splash-screen t)
;; Cursor as line.
(setq-default cursor-type 'bar)
;; Turn off auto tabbing.
(electric-indent-mode -1)
;; Don't use tabs for tabing.
(setq-default indent-tabs-mode nil)
;; Turn on parens auto closing.
(electric-pair-mode 1)
;; Disable dialog boxes.
(setq use-dialog-box nil)
;; Show clock.
(display-time-mode t)
;; 24 hours time format.
(setq display-time-24hr-format t)
;; Minimum shown number of lines before and after cursor.
(setq scroll-margin 2)
;; Don't jump when scrolling.
(setq scroll-conservatively 10000)
;; Don't show scratch message.
(setq initial-scratch-message nil)
;; File size in percents.
(size-indication-mode t)
;; Short messages.
(defalias 'yes-or-no-p 'y-or-n-p)
;; Add new line at the end of file if it doesn't exist.
(setq require-final-newline t)
;; Highlight trailing whitespaces.
(setq-default show-trailing-whitespace t)
(add-hook 'minibuffer-setup-hook (lambda () (setq show-trailing-whitespace nil)))
;; Easy transition between buffers: M-arrow-keys.
(windmove-default-keybindings 'meta)
;; Don't show cursor in inactive buffers.
(setq-default cursor-in-non-selected-windows nil)
;; Lisp indent - 2 spaces.
(setq lisp-indent-offset 2)
;; Enable scroll while searching.
(setq isearch-allow-scroll t)
;; Don't exit search mode on navigation.
(setq search-exit-option nil)
;; Make prompts uneditable.
(setq comint-prompt-read-only t)
;; Show line and column numbers.
(line-number-mode t)
(column-number-mode t)
;; Enable the downcase-region and upcase-region commands.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; File to write custom-set-variables.
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
;; Ask before killing new buffer.
(setq-default new-untitled nil)
(put 'new-untitled 'permanent-local t)
(defun kill-buffer-ask-first (orig-fun &rest args)
  "Prompts before killing buffer if it isn't associated with a file"
  (let ((buffer (get-buffer (if args (car args) (buffer-name)))))
    (if (and (buffer-local-value 'new-untitled buffer)
          (buffer-modified-p buffer)
          (not (buffer-file-name buffer)))
      (if (yes-or-no-p (format "Buffer '%s' modified and not associated with a file, kill it anyway?" (buffer-name buffer)))
        (apply orig-fun args))
      (apply orig-fun args))))
(advice-add 'kill-buffer :around 'kill-buffer-ask-first)
;; Add possibility to use C-m as hotkey in graphic mode.
(when (display-graphic-p)
  (define-key input-decode-map [?\C-m] [C-m]))
(when (daemonp)
  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (when (display-graphic-p frame)
        (with-selected-frame frame
          (define-key input-decode-map [?\C-m] [C-m]))))))
;; Unbind keys.
(dolist (key '("\C-a" "\C-n" "\C-s"))
  (global-unset-key key))
;; Disable bell on scroll.
(setq ring-bell-function (lambda ()
                           (unless (memq this-command
                                     '(mwheel-scroll
                                        down
                                        up
                                        next-line
                                        previous-line
                                        backward-char
                                        left-char
                                        right-char
                                        forward-char))
                             (ding))))

;; ========== Install packages ==========

;; List of necessary packages.
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
  '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; Activate all the packages (in particular autoloads).
(package-initialize)
;; Fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))
;; Install the missing packages.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; ========== Configure plugins ==========

;; Lazy packages loading.
(require 'use-package)
(setq use-package-always-ensure t)

;; Dark theme.
(use-package base16-theme
  :after color
  :config
  (setq base16-highlight-mode-line 'contrast)
  (setq base16-distinct-fringe-background nil)
  (setq base16-theme-256-color-source 'colors)
  (defun color-blend (c1 c2 a)
    "Combine A C1 with (1-a) C2."
    (apply
      'color-rgb-to-hex
      (cl-mapcar
        (lambda (c1 c2) (+ (* a c1) (* (- 1 a) c2)))
        (color-name-to-rgb c1)
        (color-name-to-rgb c2))))
  (defun modify-theme (theme)
    (let* ((colors (symbol-value (intern (concat (symbol-name theme) "-colors"))))
            (base00 (plist-get colors :base00))
            (base01 (plist-get colors :base01))
            (base08 (plist-get colors :base08))
            (base0A (plist-get colors :base0A))
            (base0B (plist-get colors :base0B))
            (base08-highlight (color-darken-name (color-saturate-name base08 20) 10))
            (base0A-highlight (color-darken-name (color-saturate-name base0A 20) 10))
            (base0B-highlight (color-darken-name (color-saturate-name base0B 20) 10)))
      (base16-set-faces theme (symbol-value (intern (concat (symbol-name theme) "-colors")))
        `( ;; Make it slightly different from highlighting
           (hl-line :background ,(color-blend base00 base01 0.5))
           ;; Ediff
           (ediff-current-diff-A :foreground base08 :inverse-video t)
           (ediff-current-diff-B :foreground base0B :inverse-video t)
           (ediff-current-diff-C :foreground base0A :inverse-video t)
           (ediff-even-diff-A :inverse-video t)
           (ediff-even-diff-B :inverse-video t)
           (ediff-even-diff-C :inverse-video t)
           (ediff-fine-diff-A :foreground ,base08-highlight :inverse-video t)
           (ediff-fine-diff-B :foreground ,base0B-highlight :inverse-video t)
           (ediff-fine-diff-C :foreground ,base0A-highlight :inverse-video t)
           (ediff-odd-diff-A :foreground base04 :inverse-video t)
           (ediff-odd-diff-B :foreground base04 :inverse-video t)
           (ediff-odd-diff-C :foreground base04 :inverse-video t)
           ;; Magit
           (magit-diff-base :foreground base0A :inverse-video t)
           (magit-diff-added :foreground base0B :inverse-video t)
           (magit-diff-removed :foreground base08 :inverse-video t)
           (magit-diff-base-highlight :foreground ,base0A-highlight :inverse-video t)
           (magit-diff-added-highlight :foreground ,base0B-highlight :inverse-video t)
           (magit-diff-removed-highlight :foreground ,base08-highlight :inverse-video t)
           ;; Smerge
           (smerge-base :foreground base0A :inverse-video t)
           (smerge-upper :foreground base08 :inverse-video t)
           (smerge-lower :foreground base0B :inverse-video t)
           (smerge-refined-added :foreground ,base0B-highlight :inverse-video t)
           (smerge-refined-removed :foreground ,base08-highlight :inverse-video t)
           ;; Highlight foreground instead of background
           (show-paren-match :foreground base0D :background nil :weight extra-bold)
           (show-paren-mismatch :foreground base09 :background nil :weight extra-bold)
           ;; Make comments italic
           (font-lock-comment-face :foreground base03 :slant italic)
           ;; Apply string foreground for docstring and make it italic
           (font-lock-doc-face :foreground base0B :slant italic)))))
  (defun theme ()
    (if (display-graphic-p) 'base16-onedark 'base16-isotope))
  (defun set-theme (theme)
    (load-theme theme t)
    (modify-theme theme))
  (if (daemonp)
    (add-hook 'after-make-frame-functions
      (lambda (frame)
        (with-selected-frame frame
          (set-theme (theme)))))
    (progn
      (set-theme (theme))
      ;; color-name-to-rgb will work correctly only after tty initialization
      (add-hook 'tty-setup-hook
        (lambda ()
          (modify-theme (theme)))))))

(use-package epm
  :commands (epm-list
              epm-install
              epm-reinstall
              epm-delete
              epm-refresh
              epm-info
              epm-search
              epm-outdated
              epm-upgrade
              epm-version))

(use-package cl-macs
  :ensure cl
  :config
  ;; Fix hotkeys for Russian keyboard layout.
  (cl-loop
    for from across "йцукенгшщзхъфывапролджэячсмитьбю"
    for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,."
    do
    (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat "C-" (string to)))))
    (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat "M-" (string to))))))
  (cl-loop
    for from across "ЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ"
    for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,."
    do
    (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat "C-S-" (string to)))))
    (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat "M-S-" (string to)))))))

;; Cua mode.
(use-package cua-base
  :config
  (cua-mode t)
  (define-key cua-global-keymap [C-return] nil))

(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode 1)
  ;; Workaround for bug #35404.
  (when (version<= "26" emacs-version)
    (advice-add 'posn-at-point :around (lambda (orig-fun &rest args)
                                         (let ((pos (if (car args) (car args) (point)))
                                                (res (apply orig-fun args)))
                                           (if (and (not (nth 1 args)) (get-text-property pos 'display))
                                             (let ((p (car (nth 2 res))))
                                               (setcar (car (nthcdr 2 res)) (+ p (line-number-display-width t)))
                                               res)
                                             res))))))

(use-package paren
  :config
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis))

;; Highlight current line.
(use-package hl-line
  :config
  (global-hl-line-mode 1))

;; Highlight word under point.
(use-package highlight-thing
  :config
  (global-highlight-thing-mode)
  (setq highlight-thing-what-thing 'symbol))

;; Highlight indentation levels.
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  ;; Don't know why it's needed but it seems something overwrites font-lock-extra-managed-props in emacs-lisp-mode.
  ;; See https://github.com/DarthFennec/highlight-indent-guides/issues/15#issuecomment-280505591
  (add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'font-lock-extra-managed-props 'display)))
  (add-hook 'haskell-mode-hook (lambda () (add-to-list 'font-lock-extra-managed-props 'display)))
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'stack))

;; Spell checking.
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
          (prog-mode . flyspell-prog-mode))
  :config
  (advice-add 'uncomment-region :before (lambda (BEG END &optional ARG)
                                          (flyspell-delete-region-overlays BEG END))))

;; Somehow solves the issue when flyspell-mode affects delete-selection-mode.
(use-package flyspell-lazy
  :after flyspell
  :config
  (flyspell-lazy-mode 1))

;; Automatically change language for spell checking.
(use-package guess-language
  :hook (text-mode . guess-language-mode)
  :config
  (setq guess-language-langcodes
    '((en . ("en" "English"))
       (ru . ("ru" "Russian"))))
  (setq guess-language-languages '(en ru))
  (setq guess-language-min-paragraph-length 15))

;; Graphical scroll.
(use-package minimap
  :commands minimap-mode
  :config
  (setq minimap-window-location 'right))

(use-package ido
  :commands (ido-completing-read
              ido-read-directory-name
              ido-read-file-name
              ido-read-buffer)
  :config
  (setq ido-enable-flex-matching t)
  ;; Show directories first.
  (defun ends-with-/ (s)
    (eq (aref s (1- (length s))) ?/))
  (defun ido-file-lessp (a b)
    (cond
      ((and (ends-with-/ a) (not (ends-with-/ b))) t)
      ((and (not (ends-with-/ a)) (ends-with-/ b)) nil)
      (t (string-lessp a b)))))

(use-package flx-ido
  :after ido
  :config
  (flx-ido-mode 1)
  (setq flx-ido-use-faces t)
  (setq ido-use-faces nil)
  (defun clear-flx-highlight-face (str)
    "Clear flx-highlight-face from str"
    (remove-text-properties 0 (length str) '(face flx-highlight-face) str))
  (advice-add 'ido-complete :before (lambda ()
                                      (dolist (str ido-matches)
                                        (clear-flx-highlight-face str))
                                      (clear-flx-highlight-face ido-common-match-string)))
  ;; Remove ido-max-prospects limit from flx-ido-decorate since we can use ido-prev-match and ido-next-match.
  (defun flx-ido-decorate (things &optional clear)
    "Add ido text properties to THINGS.
If CLEAR is specified, clear them instead."
    (if flx-ido-use-faces
      (cl-loop for thing in things
        for i from 0 below (length things)
        collect (if clear
                  (flx-propertize thing nil)
                  (flx-propertize (car thing) (cdr thing))))
      (if clear
        things
        (mapcar 'car things)))))

;; Smart M-x command line.
(use-package smex
  :commands (smex
              smex-major-mode-commands)
  :config
  (smex-initialize))

(use-package ivy
  :demand t
  :ensure t
  :ensure flx
  :ensure smex
  :bind (:map ivy-minibuffer-map
          ("RET" . ivy-alt-done)
          ("<C-return>" . ivy-immediate-done))
  :config
  (ivy-mode 1)
  (setq ivy-magic-tilde nil)
  (setq ivy-extra-directories nil)
  (setq ivy-fixed-height-minibuffer t)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (setq ivy-format-function 'ivy-format-function-line))

(use-package counsel
  :config
  (counsel-mode 1))

(use-package all-the-icons
  :commands (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-icon-for-url
              all-the-icons-icon-for-weather
              all-the-icons-install-fonts)
  :init
  (when (display-graphic-p)
    (unless (member "all-the-icons" (font-family-list))
      (all-the-icons-install-fonts t))))

(use-package ivy-rich
  :ensure t
  :ensure all-the-icons
  :init
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
      (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode :height 0.7 :v-adjust 0.05)))
	(if (symbolp icon)
	  (all-the-icons-icon-for-mode 'fundamental-mode :height 0.7 :v-adjust 0.05)
	  icon))))
  (defun ivy-rich-find-file-icon (candidate)
    (all-the-icons-icon-for-file candidate :height 0.7 :v-adjust 0.05))
  (defun ivy-rich-file-size (candidate)
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
        ""
        (let ((size (file-attribute-size (file-attributes candidate))))
          (cond
            ((> size 1000000) (format "%.1fM " (/ size 1000000.0)))
            ((> size 1000) (format "%.1fk " (/ size 1000.0)))
            (t (format "%d " size)))))))
  (defun ivy-rich-file-modes (candidate)
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
        ""
        (format "%s" (file-attribute-modes (file-attributes candidate))))))
  (defun ivy-rich-file-user (candidate)
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
        ""
        (let* ((user-id (file-attribute-user-id (file-attributes candidate)))
                (user-name (user-login-name user-id)))
          (format "%s" user-name)))))
  (defun ivy-rich-file-group (candidate)
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
        ""
        (let* ((group-id (file-attribute-group-id (file-attributes candidate)))
                (group-function (if (fboundp #'group-login-name) #'group-login-name #'identity))
                (group-name (funcall group-function group-id)))
          (format "%s" group-name)))))
  (setq ivy-rich-display-transformers-list
    '(ivy-switch-buffer
       (:columns
         ((ivy-rich-switch-buffer-icon :width 2)
           (ivy-rich-candidate (:width 30 :highlight t))
           (ivy-rich-switch-buffer-size (:width 7))
           (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
           (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
           (ivy-rich-switch-buffer-project (:width 15 :face success))
           (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate
         (lambda (cand) (get-buffer cand)))
       counsel-M-x
       (:columns
         ((counsel-M-x-transformer (:width 40 :highlight t))
           (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
       counsel-describe-function
       (:columns
         ((counsel-describe-function-transformer (:width 40 :highlight t))
           (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
       counsel-describe-variable
       (:columns
         ((counsel-describe-variable-transformer (:width 40 :highlight t))
           (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
       counsel-recentf
       (:columns
         ((ivy-rich-candidate (:width 0.8 :highlight t))
           (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
       counsel-find-file
       (:columns
         ((ivy-rich-find-file-icon :width 2)
           (ivy-rich-candidate (:width 60 :highlight t))
           (ivy-rich-file-user (:width 10 :face font-lock-doc-face))
           (ivy-rich-file-group (:width 4 :face font-lock-doc-face))
           (ivy-rich-file-modes (:width 11 :face font-lock-doc-face))
           (ivy-rich-file-size (:width 6 :face font-lock-doc-face))
           (ivy-rich-file-last-modified-time (:width 30 :face font-lock-doc-face))))))
  :config
  ;; Handle 'highlight' option to prevent highlighting helper columns
  (defun ivy-rich-format-column (candidate column)
    (let* ((fn (car column))
            (props (cadr column))
            (width (plist-get props :width))
            (align (plist-get props :align))
            (face (plist-get props :face))
            (highlight (plist-get props :highlight))
            (formated (funcall fn candidate)))
      (when width
        (if (functionp width)
          (setq formated (funcall width formated))
          (if (floatp width)
            (setq width (floor (* (window-width (minibuffer-window)) width))))
          (setq formated (ivy-rich-normailze-width formated width (eq align 'left)))))
      (if face
        (setq formated (propertize formated 'face face)))
      (when highlight
        (setq formated (concat (char-to-string #x200B) formated (char-to-string #x200B))))
      formated))
  (advice-add 'ivy--highlight-fuzzy :around (lambda (orig-fun &rest args)
                                                (pcase (split-string (car args) (char-to-string #x200B))
                                                  (`(,left ,candidate ,right) (concat left (apply orig-fun (list candidate)) right))
                                                  (_ (apply orig-fun args)))))
  (ivy-rich-mode 1))

;; TODO: (mapcar 'window-buffer (window-list))
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

;; Buffer switcher.
(use-package bs
  :bind (("<f2>" . bs-show)
          :map bs-mode-map
          ("<f2>" . bs-abort)
          ("<escape>" . bs-abort)
          ("<mouse-1>" . bs-mouse-select)))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

;; The Silver Searcher.
(use-package ag
  :commands (ag
              ag-files
              ag-regexp
              ag-project
              ag-project-files
              ag-project-regexp
              ag-dired
              ag-dired-regexp
              ag-project-dired
              ag-project-dired-regexp))

;; Smart find.
(use-package anzu
  :demand t
  :bind (([remap query-replace] . anzu-query-replace)
          ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :config
  (global-anzu-mode t))

;; Smart mode line.
(use-package smart-mode-line
  :config
  (setq sml/theme nil)
  (sml/setup)
  ;; apply the theme for clock right after start
  (sml/propertize-time-string))

;; Project management.
(use-package projectile
  :demand t
  :bind (:map projectile-mode-map
          ("C-p f" . projectile-find-file)
          ("C-p o" . projectile-find-file))
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

(use-package projectile-ripgrep
  :after projectile
  :bind (:map projectile-mode-map
          ("C-p g" . projectile-ripgrep)))

;; Autocompletion.
(use-package company
  :demand t
  :bind (:map company-mode-map
          ("TAB" . company-indent-or-complete-common))
  :config
  (global-company-mode 1)
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-align-annotations t))

;; Multiple cursors.
(use-package multiple-cursors-core
  :after cl-macs ;; for cl-letf
  :ensure multiple-cursors
  :commands (mc/multiple-cursors-mode-when-num-cursors>1
              mc/quit-leaving-cursors)
  :bind (("C-b" . mc/mark-all-like-this)
          ("C-S-b" . mc/edit-lines)
          :map mc/keymap
          ("C-S-b" . mc/keyboard-quit))
  :config
  (defun mc/multiple-cursors-mode-when-num-cursors>1 ()
    (interactive)
    (when (> (mc/num-cursors) 1)
      (multiple-cursors-mode 1)))
  (defun mc/quit-leaving-cursors ()
    (interactive)
    (cl-letf (((symbol-function 'mc/remove-fake-cursors)
                (lambda ())))
      (multiple-cursors-mode 0)))
  (defun mc/load-lists ())
  (defun mc/save-lists ())
  (setq mc/cmds-to-run-once '(hydra-multiple-cursors/body
                               hydra-multiple-cursors/nil
                               mc/toggle-fake-cursor))
  (setq mc/cmds-to-run-for-all '(back-to-indentation-or-beginning
                                  end-of-code-or-line
                                  indent-for-tab-command)))

(use-package mc-mark-more
  :ensure multiple-cursors
  :commands mc/toggle-fake-cursor
  :bind (("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (defun mc/toggle-fake-cursor ()
    (interactive)
    (let ((existing (mc/fake-cursor-at-point)))
      (if existing
        (mc/remove-fake-cursor existing)
        (mc/create-fake-cursor-at-point)))))

(use-package hydra
  :bind (("C-<return>" . hydra-multiple-cursors/body))
  :config
  (defhydra hydra-multiple-cursors (:foreign-keys run
                                     :body-pre (progn
                                                 (mc/quit-leaving-cursors)
                                                 (mc/toggle-fake-cursor))
                                     :post (mc/multiple-cursors-mode-when-num-cursors>1))
    "multiple-cursors"
    ("C-<return>" mc/toggle-fake-cursor)
    ("<return>" nil "quit")))

;; TODO: undo-tree-visualize hotkey
(use-package undo-tree
  :demand t
  :bind (:map undo-tree-map
          ([remap undo] . undo-tree-undo)
          ([remap undo-only] . undo-tree-undo)
          ("C-S-z" . undo-tree-redo)
          ("C-y" . undo-tree-redo))
  :init
  (setq undo-tree-map (make-sparse-keymap))
  :config
  (global-undo-tree-mode)
  (setq undo-tree-enable-undo-in-region nil)
  (defun undo-tree-overridden-undo-bindings-p () nil))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind (("C-`" . hs-toggle-hiding)))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-support-shift-select t))

(use-package yasnippet
  :ensure t
  :ensure yasnippet-snippets
  :hook (prog-mode . yas-minor-mode)
  :bind (:map yas-keymap
          ("<return>" . yas-next-field-or-maybe-expand))
  :config
  (yas-reload-all))

(use-package magit
  :demand t
  :bind (("<C-m> <C-m>" . magit-status)
          ("<C-m> b" . magit-blame-addition)
          ("<C-m> s" . magit-show-commit)
          :map magit-blame-mode-map
          ("<C-m> b" . magit-blame-quit)
          :map magit-status-mode-map
          ("TAB" . magit-section-cycle))
  :config
  (add-hook 'magit-status-mode-hook
    (lambda () (company-mode -1))))

;; Indicate uncommited changes on the fringe.
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package neotree
  :bind ("<f8>" . neotree-project-dir-toggle)
  :config
  (defun neotree-project-dir-toggle ()
    "Open NeoTree using the project root, using find-file-in-project,
or the current buffer directory."
    (interactive)
    (let ((project-dir
            (ignore-errors
              (projectile-project-root)))
           (file-name (buffer-file-name))
           (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
            (neo-global--window-exists-p))
        (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
            (neotree-dir project-dir))
          (if file-name
            (neotree-find file-name))))))
  (setq neo-window-position 'right)
  (setq neo-show-hidden-files t)
  (setq neo-autorefresh t)
  (setq projectile-switch-project-action 'neotree-projectile-action))

;; Side bar.
;; TODO: try to display in side window: (display-buffer-in-side-window buffer `((side . , 'right)))
(use-package treemacs
  :commands (treemacs-toggle treemacs-select-window treemacs-delete-other-windows treemacs treemacs-find-file)
  :config
  (setq treemacs-collapse-dirs 3)
  (setq treemacs-position 'right)
  (setq treemacs-project-follow-cleanup t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (add-hook 'treemacs-mode-hook (lambda () (setq cursor-type 'bar))))

;; Projectile support for treemacs.
(use-package treemacs-projectile
  :bind ("S-<f8>" . treemacs-projectile))

(use-package flycheck
  :demand t
  :bind (("C-e" . flycheck-list-errors))
  :config
  (global-flycheck-mode)
  (add-to-list 'display-buffer-alist
    `(,(rx bos "*Flycheck errors*" eos)
       (display-buffer-reuse-window display-buffer-below-selected)
       (reusable-frames . visible)
       (side            . bottom)
       (window-height   . 0.3))))

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(use-package scala-mode
  :mode ("\\.scala\\'" . scala-mode)
  :interpreter ("scala" . scala-mode)
  :config
  (defun right-arrow ()
    (interactive)
    (cond ((looking-back "=" nil)
            (backward-delete-char 1) (insert "⇒"))
      ((looking-back "-" nil)
        (backward-delete-char 1) (insert "→"))
      (t (insert ">"))))
  (defun left-arrow ()
    (interactive)
    (if (looking-back "<" nil)
      (progn (backward-delete-char 1)
        (insert "←"))
      (insert "-")))
  (add-hook 'scala-mode-hook '(lambda ()
                                (local-set-key (kbd "-") 'left-arrow)
                                (local-set-key (kbd ">") 'right-arrow))))

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :interpreter ("ghci" . haskell-mode))

(use-package eldoc
  :commands (eldoc-mode turn-on-eldoc-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (unless (getenv "RUST_SRC_PATH")
    (when (executable-find "rustc")
      (setenv "RUST_SRC_PATH" (concat
                                (replace-regexp-in-string "\n$" ""
                                  (shell-command-to-string "rustc --print sysroot"))
                                "/lib/rustlib/src/rust/src"))))
  (add-hook 'rust-mode-hook 'lsp))

(use-package racer
  :after rust-mode
  :commands racer-mode
  :config
  (setq racer-cmd "/bin/racer")
  (setq racer-rust-src-path "~/rust-nightly-src/src")
  (add-hook 'racer-mode-hook 'turn-on-eldoc-mode))

(use-package dockerfile-mode
  :mode ("Dockerfile'" . dockerfile-mode))

(use-package pkgbuild-mode
  :mode ("PKGBUILD'" . pkgbuild-mode))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode))

(use-package csharp-mode
  :mode ("\\.cs\\'" . csharp-mode))

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (setq lua-indent-level 2))

;; Agda.
(when (executable-find "agda-mode")
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))
  (custom-set-faces
    '(agda2-highlight-bound-variable-face ((t (:foreground "violet"))))
    '(agda2-highlight-coinductive-constructor-face ((t (:foreground "green"))))
    '(agda2-highlight-datatype-face ((t (:foreground "#0087ff"))))
    '(agda2-highlight-dotted-face ((t (:foreground "violet"))))
    '(agda2-highlight-field-face ((t (:foreground "orange red"))))
    '(agda2-highlight-function-face ((t (:foreground "orange red"))))
    '(agda2-highlight-inductive-constructor-face ((t (:foreground "green"))))
    '(agda2-highlight-keyword-face ((t (:foreground "orange"))))
    '(agda2-highlight-module-face ((t (:foreground "brightgreen"))))
    '(agda2-highlight-number-face ((t (:foreground "brightgreen"))))
    '(agda2-highlight-postulate-face ((t (:foreground "cyan"))))
    '(agda2-highlight-primitive-face ((t (:foreground "#0087ff"))))
    '(agda2-highlight-primitive-type-face ((t (:foreground "#0087ff"))))
    '(agda2-highlight-record-face ((t (:foreground "#0087ff"))))
    '(agda2-highlight-string-face ((t (:foreground "brightred"))))
    '(agda2-highlight-symbol-face ((t (:foreground "brightblue"))))
    '(custom-themed ((t (:background "blue1" :foreground "white"))))))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package lsp-mode
  :ensure t
  :ensure company-lsp
  :ensure lsp-ui
  :ensure yasnippet
  :commands lsp
  :config
  (add-hook 'lsp-mode-hook '(lambda () (highlight-thing-mode -1)))
  (setq lsp-auto-guess-root t)
  (setq lsp-prefer-flymake nil)
  (lsp-register-client
    (make-lsp-client
      :new-connection (lsp-stdio-connection '("rls"))
      :major-modes '(rust-mode)
      :priority 1
      :server-id 'my-rls
      :initialized-fn (lambda (workspace)
                        (with-lsp-workspace workspace
                          (lsp--set-configuration `(:rust (:clippy_preference "on"
                                                            :build_on_save t)))))
      :notification-handlers (lsp-ht ("window/progress" 'lsp-clients--rust-window-progress)))))

;; Mail.
(use-package mu4e
  :ensure nil ;; https://github.com/jwiegley/use-package/issues/190
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :commands mu4e
  :config
  (setq mu4e-maildir "~/Mail")
  (setq mu4e-update-interval 300)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-headers-results-limit 1000)
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-get-mail-command "mbsync --all")
  ;; Remove padding so that content won't be shifted comparing to the header
  (dolist (hook '(mu4e-main-mode-hook mu4e-headers-mode-hook mu4e-view-mode-hook mu4e-compose-mode-hook))
    (add-hook hook '(lambda ()
                      (display-line-numbers-mode -1)
                      (setq left-fringe-width 0)
                      (setq show-trailing-whitespace nil))))
  (defun mu4e-shr2text ()
    "Html to text using the shr engine."
    (interactive)
    (let ((shr-inhibit-images t)
           (shr-width (- (window-body-width) 8)))
      (shr-render-region (point-min) (point-max))
      (goto-char (point-min))))
  (setq mu4e-contexts
    `( ,(make-mu4e-context
          :name "Gmail"
          :enter-func (lambda () (mu4e-message "Entering gmail context"))
          :leave-func (lambda () (mu4e-message "Leaving gmail context"))
          :match-func (lambda (msg) (when msg
                                      (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
          :vars '((mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail")
                   (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
                   (mu4e-trash-folder . "/gmail/[Gmail]/Trash")
                   (mu4e-refile-folder . "/gmail/[Gmail]/Archive")))
       ,(make-mu4e-context
          :name "Yandex"
          :enter-func (lambda () (mu4e-message "Entering yandex context"))
          :leave-func (lambda () (mu4e-message "Leaving yandex context"))
          :match-func (lambda (msg) (when msg
                                      (string-prefix-p "/yandex" (mu4e-message-field msg :maildir))))
          :vars '((mu4e-sent-folder . "/yandex/Sent")
                   (mu4e-drafts-folder . "/yandex/Drafts")
                   (mu4e-trash-folder . "/yandex/Trash")
                   (mu4e-refile-folder . "/yandex/Archive")))
       ,(make-mu4e-context
          :name "Adform"
          :enter-func (lambda () (mu4e-message "Entering adform context"))
          :leave-func (lambda () (mu4e-message "Leaving adform context"))
          :match-func (lambda (msg) (when msg
                                      (string-prefix-p "/adform" (mu4e-message-field msg :maildir))))
          :vars '((mu4e-sent-folder . "/adform/Sent Items")
                   (mu4e-drafts-folder . "/adform/Drafts")
                   (mu4e-trash-folder . "/adform/Deleted Items")
                   (mu4e-refile-folder . "/adform/Archive"))))))

;; Screensaver.
(use-package zone
  :commands zone)

;; Weather.
(use-package wttrin
  :commands wttrin
  :config
  (setq wttrin-default-cities '("Minsk"))
  (setq wttrin-default-accept-language '("Accept-Language" . "en-EN")))

;; ========== Key bindings ==========

;; Function for create new empty buffer.
(defun new-empty-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (switch-to-buffer buffer)
    (text-mode)
    (setq buffer-offer-save t)
    (setq-local new-untitled t)))
;; Functions for pane switching.
(defun move-cursor-next-pane ()
  "Move cursor to the next pane."
  (interactive)
  (other-window 1))
(defun move-cursor-previous-pane ()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1))
;; Search from beginning of the document.
(defun isearch-forward-from-begin ()
  "Search from beginning of document."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (isearch-forward)))
;; Go to line beginning.
(defun back-to-indentation-or-beginning ()
  (interactive "^")
  (if (= (point) (progn (back-to-indentation) (point)))
    (beginning-of-line)))
;; Go to line ending.
(defun point-in-comment ()
  "Determine if the point is inside a comment."
  (interactive)
  (let ((syn (syntax-ppss)))
    (and (nth 8 syn)
      (not (nth 3 syn)))))
(defun end-of-code-or-line (arg)
  "Move to end of line, or before start of comments depending on situation.
Toggle back and forth positions if we are already at one.
Comments are recognized in any mode that sets 'syntax-ppss'
properly."
  (interactive "^P")
  (when (catch 'bol
          (let ((start (point))
                 (bol (save-excursion
                        (beginning-of-line)
                        (point)))
                 (eol (progn (move-end-of-line arg) (point))))
            (while (point-in-comment)
              (backward-char)
              (when (= (point) bol)
                (throw 'bol t)))
            (skip-chars-backward " \t")
            (throw 'bol (and (not (= eol start)) (>= start (point))))))
    (move-end-of-line arg)))
;; Comment block or line.
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
      (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))
;; Print current local modes.
(defun which-active-modes ()
  "Gives a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                           (if (and (symbolp mode) (symbol-value mode))
                             (add-to-list 'active-modes mode))
                           (error nil)))
      minor-mode-list)
    (message "Active modes are %s" active-modes)))
;; Format json with jq.
(defun jq-region ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "jq ." (buffer-name) t)))
(defun jq-buffer ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "jq ." (buffer-name) t)))
;; Format xml with xmllint.
(defun xmllint-region ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -" (buffer-name) t)))
(defun xmllint-buffer ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)))
;; Format xml with xmlstarlet.
(defun xmlstarlet-region ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmlstarlet format" (buffer-name) t)))
(defun xmlstarlet-buffer ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmlstarlet format" (buffer-name) t)))
;; Key bindings.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(global-set-key (kbd "C-S-f") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)
(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-n") 'new-empty-buffer)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-,") 'move-cursor-previous-pane)
(global-set-key (kbd "C-.") 'move-cursor-next-pane)
(global-set-key (kbd "<home>") 'back-to-indentation-or-beginning)
(global-set-key (kbd "<end>") 'end-of-code-or-line)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-k") 'kill-buffer)
(global-set-key (kbd "C-|") 'split-window-horizontally)
(global-set-key (kbd "C-_") 'split-window-vertically)

;;https://stackoverflow.com/questions/4918707/in-emacs-how-to-go-back-to-previous-line-position-after-using-semantic-jump-to
;; pop-global-mark
;; C-x C-x (exchange-point-and-mark)

;; ========== Startup actions ==========

;; Kill scratch buffer.
(kill-buffer "*scratch*")
