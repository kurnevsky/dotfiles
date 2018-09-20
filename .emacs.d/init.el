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
(tool-bar-mode -1)
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
;; Cua mode.
(cua-mode t)
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
;; (use-package darcula-theme)
(use-package atom-one-dark-theme)

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

(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode 1))

(use-package paren
  :config
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis)
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "red")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold))

;; Highlight current line.
(use-package hl-line
  :config
  (global-hl-line-mode 1))

;; Highlight word under point.
(use-package highlight-thing
  :config
  (global-highlight-thing-mode)
  (setq highlight-thing-what-thing 'symbol)
  (custom-set-faces
    '(highlight-thing ((t (:background "dark slate blue" :foreground "gray"))))))

;; Highlight indentation levels.
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  ;; Don't know why it's needed but it seems something overwrites font-lock-extra-managed-props in emacs-lisp-mode.
  ;; See https://github.com/DarthFennec/highlight-indent-guides/issues/15#issuecomment-280505591
  (add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'font-lock-extra-managed-props 'display)))
  (add-hook 'haskell-mode-hook (lambda () (add-to-list 'font-lock-extra-managed-props 'display)))
  :config
  (setq highlight-indent-guides-method 'character))

;; Spell checking.
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
          (prog-mode . flyspell-prog-mode)))

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

;; Smart file choosing.
(use-package ido
  :config
  (ido-mode t)
  ;; Don't enable ido-everywhere since ido-ubiquitous-mode will be used.
  ;; (ido-everywhere t)
  (setq ido-enable-flex-matching t)
  ;; Show directories first.
  (defun ends-with-/ (s)
    (eq (aref s (1- (length s))) ?/))
  (defun ido-file-lessp (a b)
    (cond
      ((and (ends-with-/ a) (not (ends-with-/ b))) t)
      ((and (not (ends-with-/ a)) (ends-with-/ b)) nil)
      (t (string-lessp a b)))))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))

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

(use-package ido-grid
  :load-path "~/.emacs.d/ido-grid"
  :config
  (ido-grid-enable))

;; TODO: (mapcar 'window-buffer (window-list))
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (custom-set-faces
    '(ediff-current-diff-A ((t (:background "red" :foreground "white"))))
    '(ediff-current-diff-B ((t (:background "red" :foreground "white"))))
    '(ediff-current-diff-C ((t (:background "red" :foreground "white"))))
    '(ediff-fine-diff-A ((t (:background "darkgoldenrod" :foreground "white"))))
    '(ediff-fine-diff-B ((t (:background "darkgoldenrod" :foreground "white"))))
    '(ediff-fine-diff-C ((t (:background "darkgoldenrod" :foreground "white"))))
    '(ediff-odd-diff-A ((t (:background "darkred" :foreground "white"))))
    '(ediff-odd-diff-B ((t (:background "darkred" :foreground "white"))))
    '(ediff-odd-diff-C ((t (:background "darkred" :foreground "white"))))
    '(ediff-even-diff-A ((t (:background "darkred" :foreground "white"))))
    '(ediff-even-diff-B ((t (:background "darkred" :foreground "white"))))
    '(ediff-even-diff-C ((t (:background "darkred" :foreground "white"))))))

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

;; Smart M-x command line.
(use-package smex
  :bind (("M-x" . smex)
          ("M-X" . smex-major-mode-commands)
          ("C-c M-x" . execute-extended-command)) ;; This is old M-x. TODO: to init
  :config
  (smex-initialize))

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
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

;; Project management.
(use-package projectile
  :demand t
  :bind (:map projectile-mode-map
          ("C-p f" . projectile-find-file)
          ("C-p o" . projectile-find-file))
  :config
  (projectile-mode))

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
  :ensure multiple-cursors
  :bind (("C-S-b" . mc/edit-lines)
          ("C-S-<mouse-1>" . mc/add-cursor-on-click)
          :map mc/keymap
          ("C-S-b" . mc/keyboard-quit)))

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
  :ensure yasnippet-snippets
  :hook (prog-mode . yas-minor-mode)
  :bind (:map yas-keymap
          ("<return>" . yas-next-field-or-maybe-expand))
  :config
  (yas-reload-all))

(use-package magit
  :demand t
  :bind (("<C-m> b" . magit-blame)
          ("<C-m> s" . magit-show-commit)
          :map magit-blame-mode-map
          ("<C-m> b" . magit-blame-quit)))

;; Indicate uncommited changes on the fringe.
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package neotree
  :init
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
  :bind ("<f8>" . neotree-project-dir-toggle)
  :config
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
  :ensure t
  :init (global-flycheck-mode)
  :config
  (add-hook 'flycheck-after-syntax-check-hook
    (lambda ()
      (if flycheck-current-errors
        (flycheck-list-errors)
        (when (get-buffer "*Flycheck errors*")
          (switch-to-buffer "*Flycheck errors*")
          (kill-buffer (current-buffer))
          (delete-window)))))
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
  (setq rust-indent-offset 2))

(use-package racer
  :after rust-mode
  :commands racer-mode
  :config
  (setq racer-cmd "/bin/racer")
  (setq racer-rust-src-path "~/rust-nightly-src/src")
  (add-hook 'racer-mode-hook 'turn-on-eldoc-mode))

(use-package dockerfile-mode
  :mode ("Dockerfile'" . dockerfile-mode))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode))

(use-package csharp-mode
  :mode ("\\.cs\\'" . csharp-mode))

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

(use-package lsp-mode
  :config
  (add-hook 'lsp-mode-hook '(lambda () (highlight-thing-mode -1))))

(use-package company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-rust
  :commands lsp-rust-enable)

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
    (next-line)))
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

;; shell-command
;; shell

;; ========== Startup actions ==========

;; Kill scratch buffer.
(kill-buffer "*scratch*")
(put 'upcase-region 'disabled nil)
