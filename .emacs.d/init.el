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
;; Select expression between parens.
(show-paren-mode t)
(setq show-paren-style 'expression)
;; Don't show scratch message.
(setq initial-scratch-message nil)
;; File size in percents.
(size-indication-mode t)
;; Short messages.
(defalias 'yes-or-no-p 'y-or-n-p)
;; Add new line at the end of file if it doesn't exist.
(setq require-final-newline t)
;; Easy transition between buffers: M-arrow-keys.
(windmove-default-keybindings 'meta)
;; Don't show cursor in inactive buffers.
(setq-default cursor-in-non-selected-windows nil)
;; Lisp indent - 2 spaces.
(setq lisp-indent-offset 2)
;; JS indent - 2 spaces.
(setq js-indent-level 2)
;; Enable scroll while searching.
(setq isearch-allow-scroll t)
;; Don't exit search mode on navigation.
(setq search-exit-option nil)
;; File to write custom-set-variables.
(defconst custom-el "~/.emacs.d/custom.el")
(unless (file-exists-p custom-el)
  (write-region "" nil custom-el))
(setq custom-file custom-el)
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

;; ========== Install packages ==========

;; List of necessary packages.
(setq package-list '(use-package
                      rust-mode
                      racer
                      helm))
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
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; ========== Unset keys ==========

(define-key input-decode-map [?\C-m] [C-m])

(dolist (key '("\C-a" "\C-n" "\C-s"))
  (global-unset-key key))

;; ========== Configure plugins ==========

;; Use package - lazy plugins loading.
(require 'use-package)
(setq use-package-always-ensure t)

;; Dark theme.
;; (use-package darcula-theme)
(use-package atom-one-dark-theme)

;; Linum - line numbers (default).
(use-package linum
  :config
  (line-number-mode t)
  (column-number-mode t)
  (global-linum-mode t)
  ;; Fix linum scaling.
  (set-face-attribute 'linum nil :height 100)
  (setq linum-disabled-modes
    '(mu4e-main-mode mu4e-headers-mode mu4e-view-mode mu4e-compose-mode))
  (defun linum-on ()
    (unless (or (minibufferp)
              (member major-mode linum-disabled-modes)
              (> (buffer-size) 3000000)) ;; disable linum on buffer greater than 3MB, otherwise it's unbearably slow
      (linum-mode 1))))

;; hl-line - highlight current line.
(use-package hl-line
  :config
  (global-hl-line-mode 1))

;; Highlight word under point.
(use-package highlight-thing
  :config
  (global-highlight-thing-mode)
  (setq highlight-thing-what-thing 'word)
  (custom-set-faces
    '(highlight-thing ((t (:background "dark slate blue" :foreground "gray"))))))

;; highlight-indent-guides - highlight indentation levels
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; minmap - graphical scroll
(use-package minimap
  :commands minimap-mode
  :config
  (setq minimap-window-location 'right))

;; Ido - smart file choosing (default).
(use-package ido
  :bind ("C-o" . ido-find-file)
  :config
  (ido-mode t)
  (setq ido-everywhere t)
  (setq ido-enable-flex-matching t)
  ;; Show directories first.
  (defun ends-with-/ (s)
    (eq (aref s (1- (length s))) ?/))
  (defun ido-file-lessp (a b)
    (cond
      ((and (ends-with-/ a) (not (ends-with-/ b))) t)
      ((and (not (ends-with-/ a)) (ends-with-/ b)) nil)
      (t (string-lessp a b)))))

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

;; Buffer switcher (default).
(use-package bs
  :bind (("<f2>" . bs-show)
          :map bs-mode-map
          ("<f2>" . bs-abort)
          ("<escape>" . bs-abort)))

;; Speedbar (default).
(use-package speedbar
  :defer t
  :config
  (setq speedbar-show-unknown-files t))

;; Cedet (default).
(require 'cedet)
(require 'semantic/ia)
(require 'semantic/bovine/gcc)
(require 'ede/generic)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(semantic-mode t)
(global-ede-mode t)

;; Tramp (default).
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh"))

;; Smex - smart M-x command line.
(use-package smex
  :bind (("M-x" . smex)
          ("M-X" . smex-major-mode-commands)
          ("C-c M-x" . execute-extended-command)) ;; This is old M-x. TODO: to init
  :config
  (smex-initialize))

;; The Silver Searcher.
(use-package ag
  :defer t)

;; Anzu - smart find.
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

;; Projectile - project management.
(use-package projectile
  :demand t
  :bind (:map projectile-mode-map
          ("C-p f" . projectile-find-file)
          ("C-p o" . projectile-find-file)
          ("C-p g" . projectile-ag)
          ("C-p G" . projectile-grep))
  :config
  (projectile-global-mode))

;; Company mode - autocompletion.
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

;; Undo tree.
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

(use-package helm
  :defer t)

;; Sr speedbar - speedbar in the current frame.
(use-package sr-speedbar
  :bind ("M-S-<f2>" . sr-speedbar-close)
  :config
  (setq sr-speedbar-right-side t)
  (setq sr-speedbar-auto-refresh nil))

;; Projectile speedbar.
(use-package projectile-speedbar
  :bind ("M-<f2>" . projectile-speedbar-open-current-buffer-in-tree)
  :config
  ;; https://github.com/anshulverma/projectile-speedbar/pull/12
  (defun projectile-speedbar-project-refresh (root-dir)
    "Refresh the context of speedbar based on project root"
    (when (and (not (equal root-dir sr-speedbar-last-refresh-dictionary))
            (not (sr-speedbar-window-p)))
      (setq sr-speedbar-last-refresh-dictionary root-dir))
    (sr-speedbar-select-window)
    (setq default-directory root-dir)
    (speedbar-refresh)))

;; Org mode.
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-support-shift-select t)
  (use-package ox-reveal))

;; Export to reveal.js.
(use-package ox-reveal
  :ensure org
  :commands (org-reveal-export-to-html org-reveal-export-to-html-and-browse))

;; Htmlize.
(use-package htmlize
  :commands (htmlize-buffer
              htmlize-file
              htmlize-many-files
              htmlize-many-files-dired
              htmlize-region))

;; Org mode.
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-support-shift-select t)
  (use-package ox-reveal))

;; Export to reveal.js.
(use-package ox-reveal
  :ensure org
  :commands (org-reveal-export-to-html org-reveal-export-to-html-and-browse))

;; Htmlize.
(use-package htmlize
  :commands (htmlize-buffer
              htmlize-file
              htmlize-many-files
              htmlize-many-files-dired
              htmlize-region))

;; Yasnippet.
;; TODO: do I need it?
(use-package yasnippet
  :defer t)

;; Magit.
(use-package magit
  :demand t
  :bind (("<C-m> b" . magit-blame)
          :map magit-blame-mode-map
          ("<C-m> b" . magit-blame-quit)))

;; Yaml mode.
(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

;; Scala mode.
(use-package scala-mode
  :mode ("\\.scala\\'" . scala-mode)
  :interpreter ("scala" . scala-mode)
  :config
  (defun right-arrow ()
    (interactive)
    (cond ((looking-back "=")
            (backward-delete-char 1) (insert "⇒"))
      ((looking-back "-")
        (backward-delete-char 1) (insert "→"))
      (t (insert ">"))))
  (defun left-arrow ()
    (interactive)
    (if (looking-back "<") 
      (progn (backward-delete-char 1)
        (insert "←"))
      (insert "-")))
  (add-hook 'scala-mode-hook '(lambda ()
                                (local-set-key (kbd "-") 'left-arrow)
                                (local-set-key (kbd ">") 'right-arrow))))

;; Sbt mode.
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
    'minibuffer-complete-word
    'self-insert-command
    minibuffer-local-completion-map))

;; Ensime.
(use-package ensime
  ;; :pin melpa-stable
  :bind (:map ensime-mode-map
          ("C-l e" . ensime-print-errors-at-point)
          ("C-l t" . ensime-inspect-type-at-point)
          ("C-l u" . ensime-show-uses-of-symbol-at-point)
          ("C-l f" . ensime-search)
          ("C-l b" . ensime-sbt-do-compile)
          ("C-l r" . ensime-sbt-do-run)
          ("C-l i" . ensime-import-type-at-point))
  :config
  (setq ensime-ac-enable-argument-placeholders nil)
  (setq ensime-ac-override-settings nil)
  (setq ensime-startup-notification nil)
  (setq ensime-startup-snapshot-notification nil))

;; Haskell mode.
(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :interpreter ("ghci" . haskell-mode))

;; GHC mod.
(use-package ghc
  :commands ghc-init-interactive
  :config
  (defun ghc-init-interactive ()
    "Initialize ghc module"
    (interactive)
    (ghc-init)))

;; Jdee - java mode.
(use-package jdee
  :mode ("\\.java\\'" . jdee-mode)
  :config
  (setq jdee-server-dir "~/jdee"))

(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; Rust mode.
(require 'rust-mode)
(setq rust-indent-offset 2)

;; Racer.
(require 'racer)
(setq racer-cmd "/bin/racer")
(setq racer-rust-src-path "~/rust-nightly-src/src")
(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'racer-mode-hook 'eldoc-mode)

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

;; mu4e - mail.
(use-package mu4e
  :ensure nil ;; https://github.com/jwiegley/use-package/issues/190
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :commands mu4e
  :config
  (setq mu4e-maildir "~/Mail")
  (setq mu4e-update-interval 300)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-headers-full-search t)
  (setq mu4e-headers-results-limit -1)
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-get-mail-command "mbsync --all")
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
          :vars '((mu4e-sent-folder . "/gmail/[Gmail]/.Sent Mail")
                   (mu4e-drafts-folder . "/gmail/[Gmail]/.Drafts")
                   (mu4e-trash-folder . "/gmail/[Gmail]/.Trash")
                   (mu4e-refile-folder . "/gmail/[Gmail]/.Archive")))
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

;; zone mode - screensaver
(use-package zone
  :commands zone)

;; fireplace
(use-package fireplace
  :commands fireplace)

;; wttrin - weather
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
  "Determine if the point is inside a comment"
  (interactive)
  (let ((syn (syntax-ppss)))
    (and (nth 8 syn)
      (not (nth 3 syn)))))
(defun end-of-code-or-line (arg)
  "Move to end of line, or before start of comments depending on situation.
Toggle back and forth positions if we are already at one.
Comments are recognized in any mode that sets syntax-ppss
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

;; Cx0 = ???, C|-
;; clone-indirect-buffer
;; undo tree show

;;https://stackoverflow.com/questions/4918707/in-emacs-how-to-go-back-to-previous-line-position-after-using-semantic-jump-to
;; pop-global-mark
;; C-x C-x (exchange-point-and-mark)

;; shell-command
;; shell

;; ========== Startup actions ==========

;; Kill scratch buffer.
(kill-buffer "*scratch*")
