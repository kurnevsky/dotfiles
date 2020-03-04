;;; init.el --- Kurnevsky's Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This is my personal Emacs configuration.

;;; Code:

;; ========== Configure Emacs ==========

(defvar prefer-helm nil
  "Whether to prefer helm or ivy.")

;; Speed up the initialization reducing garbage collection runs.
(setq gc-cons-threshold (* 32 1024 1024))
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 4 1024 1024))))
;; Speed up the initialization temporary disabling file-name-handler-alist.
(defvar file-name-handler-alist-copy file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist file-name-handler-alist-copy)))
;; Collect the garbage when not used.
(add-hook 'focus-out-hook #'garbage-collect)
;; Remove gap in maximized window mode.
(setq frame-resize-pixelwise t)
;; Start in maximized window mode.
(toggle-frame-maximized)
;; Disable tool bar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; Set font.
(set-face-attribute 'default nil :font "DejaVu Sans Mono:size=15")
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono:size=15"))
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
;; Minimum shown number of lines before and after cursor.
(setq scroll-margin 2)
;; Reduce tab width.
(setq-default tab-width 4)
;; Don't jump when scrolling.
(setq scroll-conservatively 10000)
;; Don't show scratch message.
(setq initial-scratch-message nil)
;; File size in percents.
(size-indication-mode t)
;; Short messages.
(defalias 'yes-or-no-p #'y-or-n-p)
;; Add new line at the end of file if it doesn't exist.
(setq require-final-newline t)
;; Highlight trailing whitespaces.
(setq-default show-trailing-whitespace t)
(add-hook 'minibuffer-setup-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'Buffer-menu-mode-hook (lambda () (setq show-trailing-whitespace nil)))
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
;; Move mouse to newly selected frames.
(setq focus-follows-mouse t)
;; Resize windows pixelwise.
(setq window-resize-pixelwise t)
;; Allow minibuffer commands while in the minibuffer.
(setq enable-recursive-minibuffers t)
;; Increase undo history limits.
(setq undo-limit (* 1024 1024))
(setq undo-strong-limit (* 2 1024 1024))
(setq undo-outer-limit (* 16 1024 1024))
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
(defvar new-untitled nil)
(put 'new-untitled 'permanent-local t)
(defun kill-buffer-ask-first (orig-fun &rest args)
  "Prompts before killing buffer if it isn't associated with a file."
  (let ((buffer (get-buffer (if (and args (car args)) (car args) (buffer-name)))))
    (if (and (buffer-local-value 'new-untitled buffer)
          (buffer-modified-p buffer)
          (not (buffer-file-name buffer)))
      (if (yes-or-no-p (format "Buffer '%s' modified and not associated with a file, kill it anyway?" (buffer-name buffer)))
        (apply orig-fun args))
      (apply orig-fun args))))
(advice-add 'kill-buffer :around #'kill-buffer-ask-first)
;; Add possibility to use C-m as hotkey in graphic mode.
(when (display-graphic-p)
  (define-key input-decode-map [?\C-m] [C-m]))
(when (daemonp)
  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (when (display-graphic-p frame)
        (with-selected-frame frame
          (define-key input-decode-map [?\C-m] [C-m]))))))
;; Unbind keys
(dolist (key '("C-a" "C-b" "C-d" "C-e" "C-f" "C-j" "C-k" "C-n" "C-o" "C-p" "C-r"
                "C-s" "C-t" "C-w" "C-y" "C-z" "M-w"))
  (global-unset-key (kbd key)))
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
;; Recompile init file on exit.
(defun recompile-init ()
  "Recompile init file when it was modified."
  (let ((init-elc-attrs (file-attributes (concat user-init-file "c"))))
    (when (or (not init-elc-attrs)
            (time-less-p
              (nth 5 init-elc-attrs)
              (nth 5 (file-attributes user-init-file))))
      (byte-compile-file user-init-file))))
(add-hook 'kill-emacs-hook #'recompile-init)

;; ========== Initialize packages ==========

(require 'package)
;; Configure the list of remote archives.
(dolist (archive '(("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/")))
  (add-to-list 'package-archives archive))
(setq package-archive-priorities
  '(("melpa" . 2)
     ("melpa-stable" . 1)
     ("gnu" . 0)))
;; Initialize packages without activating.
(package-initialize t)
;; Fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

;; ========== Install use-package ==========

(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (package-activate 'use-package)
  (require 'use-package)
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-verbose t)
  (setq use-package-expand-minimally byte-compile-current-file)
  (defun use-package-normalize/:activate (_name-symbol keyword args)
    (use-package-only-one (symbol-name keyword) args
      (lambda (_label arg) arg)))
  (defun use-package-handler/:activate (name-symbol _keyword activate rest state)
    (let ((body (use-package-process-keywords name-symbol rest state)))
      (if activate
        (let ((package (if (eq activate t) name-symbol activate)))
          (use-package-concat
            `((package-activate ',package))
            body))
        body)))
  (defun insert-after (xs element after-elemnet)
    "Insert ELEMENT after AFTER_ELEMENT into XS."
    (let ((pos (1+ (cl-position after-elemnet xs))))
      (nconc
        (cl-subseq xs 0 pos)
        (list element)
        (nthcdr pos xs))))
  (setq use-package-keywords (insert-after use-package-keywords :activate :load-path))
  (add-to-list 'use-package-defaults '(:activate t t) t))
(package-activate 'bind-key)
(require 'bind-key)

;; ========== Configure packages ==========

(use-package base16-theme
  :demand t
  :custom
  (base16-highlight-mode-line 'contrast)
  (base16-distinct-fringe-background nil)
  (base16-theme-256-color-source 'colors)
  :config
  (defun color-blend (c1 c2 a)
    "Combine A C1 with (1-a) C2."
    (apply
      #'color-rgb-to-hex
      (cl-mapcar
        (lambda (c1 c2) (+ (* a c1) (* (- 1 a) c2)))
        (color-name-to-rgb c1)
        (color-name-to-rgb c2))))
  (defun modify-theme (theme)
    (require 'color)
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
  :ensure nil
  :demand t
  :config
  ;; Fix hotkeys for Russian keyboard layout.
  (cl-loop
    for from across "йцукенгшщзхъфывапролджэячсмитьбю"
    for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,."
    do
    (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat "C-" (string to)))))
    (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat "M-" (string to)))))
    (eval `(define-key key-translation-map (kbd ,(concat "C-" (string (upcase from)))) (kbd ,(concat "C-S-" (string to)))))
    (eval `(define-key key-translation-map (kbd ,(concat "M-" (string (upcase from)))) (kbd ,(concat "M-S-" (string to)))))))

(use-package cua-base
  :ensure nil
  :demand t
  :bind (:map cua-global-keymap
          ("<C-return>"))
  :config
  (cua-mode t)
  (defun cua-macro-fix (orig-fun &rest args)
    (apply orig-fun args)
    (kmacro-edit-macro)
    (let ((endl "[[:space:]]*\\(;.*\\)?\n")
           (case-fold-search nil))
      ;; fix the C-c C-c
      (goto-char (point-min))
      (forward-line 7)
      (while (search-forward-regexp (concat "C-c C-c" endl "<timeout>") nil t)
        (replace-match "C-c <timeout>"))
      ;; fix the C-x C-x
      (goto-char (point-min))
      (forward-line 7)
      (while (search-forward-regexp (concat "C-x C-x" endl "<timeout>") nil t)
        (replace-match "C-x <timeout>"))
      ;; fix C-m is being confused with <return>
      (goto-char (point-min))
      (forward-line 7)
      (while (search-forward-regexp (concat "RET" endl) nil t)
        (replace-match "<return>")))
    (edmacro-finish-edit))
  (advice-add 'kmacro-end-macro :around #'cua-macro-fix))

(use-package time
  :ensure nil
  :demand t
  :custom
  (display-time-24hr-format t "24 hours time format.")
  :config
  (display-time-update))

(use-package comint
  :ensure nil
  :custom
  (comint-prompt-read-only t "Make the prompt read only."))

(use-package mb-depth
  :ensure nil
  :demand t
  :config
  (minibuffer-depth-indicate-mode t))

(use-package display-line-numbers
  :ensure nil
  :demand t
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
  :ensure nil
  :demand t
  :custom
  (show-paren-style 'parenthesis)
  :config
  (show-paren-mode t))

(use-package hl-line
  :ensure nil
  :demand t
  :config
  (global-hl-line-mode 1))

(use-package dimmer
  :demand t
  :custom
  (dimmer-watch-frame-focus-events nil)
  :config
  (defun dimmer-lsp-ui-p ()
    (string-match-p "^ \\*lsp-ui-doc-.*\\*$" (buffer-name (current-buffer))))
  (add-to-list
    'dimmer-prevent-dimming-predicates
    #'dimmer-lsp-ui-p)
  (add-to-list 'dimmer-buffer-exclusion-regexps "^ \\*lsp-ui-doc-.*\\*$")
  (when prefer-helm
    (dimmer-configure-helm))
  (dimmer-configure-hydra)
  (dimmer-mode t))

(use-package highlight-thing
  :demand t
  :custom
  (highlight-thing-what-thing 'symbol)
  :config
  (defun highlight-thing-should-highlight-p ()
    (and
      (not (minibufferp))
      (not (member major-mode highlight-thing-excluded-major-modes))
      (not (and
             (bound-and-true-p lsp-mode)
             (fboundp 'lsp--capability)
             (lsp--capability "documentHighlightProvider")))))
  (global-highlight-thing-mode))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'stack)
  :config
  ;; Display property might be deleted if a major mode defines
  ;; font-lock-extra-managed-props via font-lock-defaults.
  (add-hook 'after-change-major-mode-hook (lambda ()
                                            (when (derived-mode-p 'prog-mode)
                                              (add-to-list 'font-lock-extra-managed-props 'display)))))

(use-package flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
          (prog-mode . flyspell-prog-mode))
  :bind (:map flyspell-mode-map
          ("C-.")
          ("C-,"))
  :config
  ;; flyspell uses sit-for for delays which breaks things like
  ;; delete-selection-mode and company-mode. One possible solution is setting
  ;; flyspell-delay to nil but this will impact performance. Instead I disable
  ;; it completely for self-insert-command when it inserts anything besides
  ;; separators. See https://en.wikipedia.org/wiki/Unicode_character_property#General_Category
  ;; for Unicode properties.
  (advice-add 'flyspell-check-word-p :around (lambda (orig-fun &rest args)
                                               (if (eq this-command 'self-insert-command)
                                                 (memq (get-char-code-property (char-before) 'general-category) '(Zs Zl Zp))
                                                 (apply orig-fun args))))
  (advice-add 'uncomment-region :before (lambda (BEG END &optional _ARG)
                                          (flyspell-delete-region-overlays BEG END))))

(use-package langtool
  :commands langtool-check
  :custom
  (langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
  (langtool-default-language "en-US")
  (langtool-mother-tongue "ru-RU")
  (langtool-autoshow-message-function (lambda (overlays)
                                        (pos-tip-show (langtool-details-error-message overlays)))))

(use-package guess-language
  :hook (text-mode . guess-language-mode)
  :custom
  (guess-language-langcodes
    '((en . ("en" "English"))
       (ru . ("ru" "Russian"))))
  (guess-language-languages '(en ru))
  (guess-language-min-paragraph-length 15))

(use-package minimap
  :commands minimap-mode
  :custom
  (minimap-window-location 'right))

(use-package ido
  :ensure nil
  :commands (ido-completing-read
              ido-read-directory-name
              ido-read-file-name
              ido-read-buffer)
  :custom
  (ido-enable-flex-matching t)
  (ido-use-faces nil)
  :config
  ;; Show directories first.
  (defun ends-with-/ (s)
    (eq (aref s (1- (length s))) ?/))
  (defun ido-file-lessp (a b)
    (cond
      ((and (ends-with-/ a) (not (ends-with-/ b))) t)
      ((and (not (ends-with-/ a)) (ends-with-/ b)) nil)
      (t (string-lessp a b)))))

(use-package flx-ido
  :demand t
  :after ido
  :custom
  (flx-ido-use-faces t)
  :config
  (flx-ido-mode 1)
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
        (mapcar #'car things)))))

(use-package smex
  :commands (smex
              smex-major-mode-commands)
  :config
  (smex-initialize))

(use-package ivy
  :if (not prefer-helm)
  :demand t
  :bind (("<f2>" . ivy-switch-buffer)
          :map ivy-minibuffer-map
          ("RET" . ivy-alt-done)
          ("<C-return>" . ivy-immediate-done)
          :map ivy-switch-buffer-map
          ("<f2>" . keyboard-escape-quit))
  :custom
  (ivy-magic-tilde nil)
  (ivy-extra-directories nil)
  (ivy-fixed-height-minibuffer t)
  (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-format-function #'ivy-format-function-line)
  :config
  (ivy-mode 1))

(use-package counsel
  :if (not prefer-helm)
  :demand t
  :after ivy
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
  :if (not prefer-helm)
  :demand t
  :after ivy
  :init
  (defun ivy-rich-switch-buffer-icon (candidate)
    (concat
      (with-current-buffer (get-buffer candidate)
        (let ((icon (all-the-icons-icon-for-mode major-mode :height 0.7 :v-adjust 0.05)))
          (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode :height 0.7 :v-adjust 0.05)
            icon)))
      "\t"))
  (defun ivy-rich-find-file-icon (candidate)
    (concat
      (all-the-icons-icon-for-file candidate :height 0.7 :v-adjust 0.05)
      "\t"))
  (defun ivy-rich-file-size (candidate)
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
        "?"
        (let ((size (file-attribute-size (file-attributes candidate))))
          (cond
            ((> size 1000000) (format "%.1fM " (/ size 1000000.0)))
            ((> size 1000) (format "%.1fk " (/ size 1000.0)))
            (t (format "%d " size)))))))
  (defun ivy-rich-file-modes (candidate)
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
        "?"
        (format "%s" (file-attribute-modes (file-attributes candidate))))))
  (defun ivy-rich-file-user (candidate)
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
        "?"
        (let* ((user-id (file-attribute-user-id (file-attributes candidate)))
                (user-name (user-login-name user-id)))
          (format "%s" user-name)))))
  (defun ivy-rich-file-group (candidate)
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
        "?"
        (let* ((group-id (file-attribute-group-id (file-attributes candidate)))
                (group-function (if (fboundp 'group-login-name) #'group-login-name #'identity))
                (group-name (funcall group-function group-id)))
          (format "%s" group-name)))))
  (setq ivy-rich-display-transformers-list
    '(ivy-switch-buffer
       (:columns
         ((ivy-rich-switch-buffer-icon :width 2)
           (ivy-rich-candidate (:width 30 :highlight t))
           (ivy-rich-switch-buffer-size (:width 7))
           (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
           (ivy-rich-switch-buffer-major-mode (:width 20 :face warning))
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
           (ivy-rich-file-size (:width 10 :face font-lock-doc-face))
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
          (setq formated (ivy-rich-normalize-width formated width (eq align 'left)))))
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

(use-package helm
  :if prefer-helm
  :demand t
  :bind (("M-x" . helm-M-x)
          ([remap find-file] . helm-find-files)
          ("<f2>" . helm-buffers-list)
          :map helm-map
          ("<escape>" . helm-keyboard-quit)
          ("<tab>" . helm-complete-prefix)
          :map helm-buffer-map
          ("<f2>" . helm-keyboard-quit)
          :map helm-find-files-map
          ("<tab>" . helm-execute-persistent-action))
  :custom
  (helm-mode-fuzzy-match t)
  (helm-completion-in-region-fuzzy-match t)
  (helm-candidate-number-limit 1000)
  :config
  (helm-mode 1)
  (defun helm-complete-prefix ()
    (interactive)
    (let ((prefix nil))
      (mapc (lambda (candidate)
              (when (string-prefix-p helm-pattern candidate)
                (setq prefix (if prefix
                               (fill-common-string-prefix prefix candidate)
                               candidate))))
        (helm-get-cached-candidates (helm-get-current-source)))
      (when prefix
        (helm-set-pattern prefix)))))

(use-package ediff-wind
  :ensure nil
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "ssh"))

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

(use-package anzu
  :demand t
  :bind (([remap query-replace] . anzu-query-replace)
          ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :config
  (global-anzu-mode t))

(use-package doom-modeline
  :demand t
  :custom
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  :config
  (doom-modeline-mode))

(use-package projectile
  :demand t
  :bind (:map projectile-mode-map
          ("C-p f" . projectile-find-file)
          ("C-p o" . projectile-find-file))
  :custom
  (projectile-completion-system (if prefer-helm 'helm 'ivy))
  :config
  (projectile-mode))

(use-package rg
  :after projectile
  :custom
  (rg-group-result t)
  (rg-command-line-flags '("--hidden"))
  :bind (:map projectile-mode-map
          ("C-p g" . rg-project)
          :map rg-mode-map
          ("C-b")
          ("C-f")
          ("F" . rg-forward-history)
          ("B" . rg-back-history)))

(use-package company
  :demand t
  :bind (:map company-mode-map
          ("TAB" . company-indent-or-complete-common))
  :custom
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  :config
  (global-company-mode 1))

(use-package company-dabbrev
  :ensure company
  :activate company
  :after company
  :commands company-dabbrev
  :custom
  (company-dabbrev-downcase nil))

(use-package company-quickhelp
  :demand t
  :after company
  :config
  (company-quickhelp-mode))

(use-package multiple-cursors-core
  :ensure multiple-cursors
  :activate multiple-cursors
  :commands (mc/multiple-cursors-mode-when-num-cursors>1
              mc/quit-leaving-cursors)
  :bind (("C-b" . mc/mark-all-like-this)
          ("C-S-b" . mc/edit-lines)
          :map mc/keymap
          ("C-S-b" . mc/keyboard-quit))
  :config
  ;; Copy/paste.
  (with-no-warnings
    (defvar-local mc/clipboard nil))
  (defun mc/cut-copy-across-cursors (cut)
    (setq mc/clipboard nil)
    (mc/for-each-cursor-ordered
      (mc/restore-state-from-overlay cursor)
      (push
        (if (region-active-p)
          (buffer-substring
            (caar (region-bounds))
            (cdar (region-bounds)))
          "")
        mc/clipboard)
      (mc/execute-command-for-fake-cursor
        (lambda ()
          (interactive)
          (if (and cut (not buffer-read-only))
            (delete-region
              (caar (region-bounds))
              (cdar (region-bounds)))
            (deactivate-mark)))
        cursor)))
  (defun mc/copy-across-cursors ()
    (interactive)
    (mc/cut-copy-across-cursors nil))
  (defun mc/cut-across-cursors ()
    (interactive)
    (mc/cut-copy-across-cursors t))
  (defun mc/paste-across-cursors ()
    (interactive)
    (if mc/clipboard
      (let ((clipboard (reverse mc/clipboard)))
        (mc/for-each-cursor-ordered
          (when clipboard
            (mc/execute-command-for-fake-cursor
              (lambda ()
                (interactive)
                (insert (car clipboard)))
              cursor)
            (setq clipboard (cdr clipboard)))))
      (mc/execute-command-for-all-cursors #'cua-paste)))
  (add-to-list
    'emulation-mode-map-alists
    `((multiple-cursors-mode . ,(-doto (make-sparse-keymap)
                                  (define-key [remap yank] #'mc/paste-across-cursors)
                                  (define-key [remap clipboard-yank] #'mc/paste-across-cursors)
                                  (define-key [remap x-clipboard-yank] #'mc/paste-across-cursors)
                                  (define-key [remap copy-region-as-kill] #'mc/copy-across-cursors)
                                  (define-key [remap kill-region] #'mc/cut-across-cursors)
                                  (define-key [remap clipboard-kill-region] #'mc/cut-across-cursors)))))
  (defun mc/clear-clipboard ()
    (setq mc/clipboard nil))
  (add-hook 'multiple-cursors-mode-hook #'mc/clear-clipboard)
  ;; Fake cursors manipulation.
  (defun mc/multiple-cursors-mode-when-num-cursors>1 ()
    (interactive)
    (when (> (mc/num-cursors) 1)
      (multiple-cursors-mode 1)))
  (defun mc/quit-leaving-cursors ()
    (interactive)
    (cl-letf (((symbol-function 'mc/remove-fake-cursors)
                (lambda ())))
      (multiple-cursors-mode 0)))
  (defun mc/remove-fake-cursors-interactive ()
    (interactive)
    (mc/remove-fake-cursors))
  ;; Disable saving/loading commands.
  (defun mc/load-lists ())
  (defun mc/save-lists ())
  ;; Define commands.
  (setq mc/cmds-to-run-once '(hydra-multiple-cursors/body
                               hydra-multiple-cursors/nil
                               cua--prefix-override-handler
                               mc/toggle-fake-cursor
                               mc/copy-across-cursors
                               mc/cut-across-cursors
                               mc/paste-across-cursors))
  (setq mc/cmds-to-run-for-all '(back-to-indentation-or-beginning
                                  end-of-code-or-line
                                  indent-for-tab-command)))

(use-package mc-mark-more
  :ensure multiple-cursors
  :activate multiple-cursors
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
  :bind (("<C-return>" . hydra-multiple-cursors/body))
  :config
  (defhydra hydra-multiple-cursors (:foreign-keys run
                                     :body-pre (progn
                                                 (mc/quit-leaving-cursors)
                                                 (mc/toggle-fake-cursor))
                                     :post (mc/multiple-cursors-mode-when-num-cursors>1))
    "multiple-cursors"
    ("<C-return>" mc/toggle-fake-cursor "toggle")
    ("<return>" nil "apply")
    ("<escape>" mc/remove-fake-cursors-interactive "quit" :exit t)))

(use-package undo-tree
  :demand t
  :bind (:map undo-tree-map
          ([remap undo] . undo-tree-undo)
          ([remap undo-only] . undo-tree-undo)
          ("C-S-z" . undo-tree-redo)
          ("C-y" . undo-tree-redo)
          ("C-w" . last-edit))
  :init
  (setq undo-tree-map (make-sparse-keymap))
  :custom
  (undo-tree-enable-undo-in-region nil)
  :config
  (global-undo-tree-mode)
  (defun undo-tree-overridden-undo-bindings-p () nil)
  (defun last-edit-next (undo tree)
    (if tree
      (undo-tree-node-previous undo)
      (cdr undo)))
  (defun last-edit (arg)
    "Go back to last add/delete edit."
    (interactive "^P")
    (unless arg
      (setq arg 1))
    (let ((undo buffer-undo-list)
           (tree))
      (unless (eq undo t)
        (while undo
          (pcase (if tree
                   (car (undo-tree-node-undo undo))
                   (car undo))
            (`(,beg . ,end) (let ((pos (cond
                                         ((and (integerp beg) (integerp end)) end)
                                         ((and (stringp beg) (integerp end)) (abs end)))))
                              (if pos
                                (progn
                                  (setq arg (1- arg))
                                  (if (<= arg 0)
                                    (progn
                                      (goto-char pos)
                                      (setq undo nil))
                                    (setq undo (last-edit-next undo tree))))
                                (setq undo (last-edit-next undo tree)))))
            (`undo-tree-canary (if tree
                                 (progn
                                   (error "Inner undo-tree-canary")
                                   (setq undo nil))
                                 (setq
                                   undo (undo-tree-current buffer-undo-tree)
                                   tree t)))
            (_ (setq undo (last-edit-next undo tree)))))))))

(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
          ("C-`" . hs-toggle-hiding)))

(use-package origami
  :bind (:map origami-mode-map
          ("C-`" . origami-toggle-node))
  :config
  (add-hook 'origami-mode-hook
    (lambda ()
      (hs-minor-mode -1))))

(use-package org
  :ensure nil
  :custom
  (org-support-shift-select t))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :bind (:map yas-keymap
          ("<return>" . yas-next-field-or-maybe-expand))
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :demand t
  :after yasnippet)

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

(use-package gitattributes-mode)

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package diff-hl
  :demand t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package treemacs
  :bind (("<f8>" . treemacs)
          :map treemacs-mode-map
          ("<M-up>")
          ("<M-down>"))
  :custom
  (treemacs-collapse-dirs 3)
  (treemacs-position 'right)
  (treemacs-project-follow-cleanup t)
  (treemacs-show-cursor t)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

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

(use-package adoc-mode
  :mode ("\\.adoc\\'" . adoc-mode))

(use-package polymode
  :config
  ;; Doesn't work well with polymode.
  (add-hook 'prog-mode-hook (lambda ()
                              (when polymode-mode
                                (set (make-local-variable 'highlight-indent-guides-responsive) nil))))
  ;; Fix highlight thing
  (add-hook 'polymode-before-switch-buffer-hook (lambda (_old _new)
                                                  (highlight-thing-remove-last))))

(use-package poly-markdown)

(use-package poly-org)

(use-package poly-rst)

(use-package conf-mode
  :ensure nil
  :mode ("/Cargo.lock\\'" . conf-toml-mode))

(use-package yaml-mode)

(use-package groovy-mode)

(use-package scala-mode
  :mode ("\\.sc\\'" . scala-mode)
  :interpreter ("scala" . scala-mode))

(use-package haskell-mode)

(use-package eldoc
  :ensure nil
  :commands (eldoc-mode turn-on-eldoc-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode))

(use-package rust-mode
  :config
  (unless (getenv "RUST_SRC_PATH")
    (when (executable-find "rustc")
      (setenv "RUST_SRC_PATH" (concat
                                (replace-regexp-in-string "\n\\'" ""
                                  (shell-command-to-string "rustc --print sysroot"))
                                "/lib/rustlib/src/rust/src")))))

(use-package matlab
  :ensure matlab-mode
  :activate matlab-mode)

(use-package ess)

(use-package csv-mode)

(use-package json-mode)

(use-package dockerfile-mode)

(use-package systemd)

(use-package pkgbuild-mode)

(use-package go-mode)

(use-package typescript-mode)

(use-package csharp-mode)

(use-package lua-mode)

(when (executable-find "agda-mode")
  (use-package agda2-mode
    :ensure nil
    :load-path (lambda ()
                 (let ((coding-system-for-read 'utf-8))
                   (file-name-directory (shell-command-to-string "agda-mode locate"))))
    :mode ("\\.l?agda\\'" . agda2-mode)
    :interpreter ("agda -I" . agda2-mode)))

(use-package editorconfig
  :demand t
  :config
  (editorconfig-mode 1))

(use-package format-all)

(use-package dumb-jump
  :custom
  (dumb-jump-selector (if prefer-helm 'helm 'ivy)))

(use-package lsp-mode
  :custom
  (lsp-auto-guess-root t)
  (lsp-prefer-flymake nil)
  (lsp-file-watch-threshold nil)
  (lsp-keep-workspace-alive nil)
  (lsp-lens-auto-enable t)
  (lsp-prefer-capf t)
  :config
  (defun lsp-activate-if-already-activated (server-id)
    (when (lsp-find-workspace server-id (buffer-file-name))
      (lsp)))
  (add-hook 'rust-mode-hook (lambda ()
                              (lsp-activate-if-already-activated 'rls)))
  (add-hook 'scala-mode-hook (lambda ()
                               (lsp-activate-if-already-activated 'metals))))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-ui-doc-position 'top)
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(no-accept-focus . t)))

(use-package lsp-origami
  :hook (lsp-after-open . lsp-origami-activate-when-supported)
  :config
  (defun lsp-origami-activate-when-supported ()
    (when (lsp--capability "foldingRangeProvider")
      (origami-mode t)
      (lsp-origami-mode t))))

(use-package lsp-treemacs
  :config
  (lsp-metals-treeview-enable t))

(use-package lsp-rust
  :ensure lsp-mode
  :activate lsp-mode
  :after lsp-mode
  :demand t
  :custom
  (lsp-rust-clippy-preference "on")
  (lsp-rust-cfg-test t)
  (lsp-rust-build-on-save t))

(use-package lsp-haskell
  :disabled
  :after lsp-mode
  :demand t)

(use-package dap-mode
  :ensure posframe
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode t))

(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :commands mu4e
  :custom
  (mu4e-maildir "~/Mail")
  (mu4e-update-interval 300)
  (mu4e-view-show-addresses t)
  (mu4e-headers-results-limit 1000)
  (mu4e-change-filenames-when-moving t)
  (mu4e-get-mail-command "mbsync --all")
  :config
  ;; Remove padding so that content won't be shifted comparing to the header
  (dolist (hook '(mu4e-main-mode-hook mu4e-headers-mode-hook mu4e-view-mode-hook mu4e-compose-mode-hook))
    (add-hook hook (lambda ()
                      (display-line-numbers-mode -1)
                      (setq left-fringe-width 0)
                      (setq show-trailing-whitespace nil))))
  (defun mu4e-shr2text ()
    "Html to text using the shr engine."
    (interactive)
    (defvar shr-inhibit-images)
    (defvar shr-width)
    (let ((shr-inhibit-images t)
           (shr-width (- (window-body-width) 8)))
      (shr-render-region (point-min) (point-max))
      (goto-char (point-min))))
  (defvar mu4e-sent-folder-alternatives '("/[Gmail]/Sent Mail" ;; gmail
                                           "/Sent" ;; yandex
                                           "/Sent Items" ;; outlook
                                           ))
  (defvar mu4e-drafts-folder-alternatives '("/[Gmail]/Drafts" ;; gmail
                                             "/Drafts" ;; yandex, outlook
                                             ))
  (defvar mu4e-trash-folder-alternatives '("/[Gmail]/Trash" ;; gmail
                                            "/Trash" ;; yandex
                                            "/Deleted Items" ;; outlook
                                            ))
  (defvar mu4e-refile-folder-alternatives '("/[Gmail]/Archive" ;; gmail
                                             "/Archive" ;; yandex, outlook
                                             ))
  (defun choose-mu4e-alternative (name alternatives)
    (string-remove-prefix mu4e-maildir
      (seq-find #'file-directory-p
        (mapcar (lambda (value) (concat mu4e-maildir "/" name value))
          (symbol-value alternatives)))))
  (defun make-mu4e-context-generic (name)
    (make-mu4e-context
      :name name
      :enter-func `(lambda () (mu4e-message (concat "Entering " ,name " context")))
      :leave-func `(lambda () (mu4e-message (concat "Leaving " ,name " context")))
      :match-func `(lambda (msg) (when msg
                                   (string-prefix-p (concat "/" ,name) (mu4e-message-field msg :maildir))))
      :vars `((mu4e-sent-folder . ,(choose-mu4e-alternative name 'mu4e-sent-folder-alternatives))
               (mu4e-drafts-folder . ,(choose-mu4e-alternative name 'mu4e-drafts-folder-alternatives))
               (mu4e-trash-folder . ,(choose-mu4e-alternative name 'mu4e-trash-folder-alternatives))
               (mu4e-refile-folder . ,(choose-mu4e-alternative name 'mu4e-refile-folder-alternatives)))))
  (setq mu4e-contexts (mapcar #'make-mu4e-context-generic (directory-files "~/Mail" nil "[^.]"))))

(use-package wttrin
  :commands wttrin
  :custom
  (wttrin-default-cities '("Minsk"))
  (wttrin-default-accept-language '("Accept-Language" . "en-EN")))

;; ========== Key bindings ==========

(defun new-empty-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (switch-to-buffer buffer)
    (text-mode)
    (setq buffer-offer-save t)
    (setq-local new-untitled t)))
(defun move-cursor-next-pane ()
  "Move cursor to the next pane."
  (interactive)
  (other-window 1))
(defun move-cursor-previous-pane ()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1))
(defun isearch-forward-from-begin ()
  "Search from the beginning of document."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (isearch-forward)))
(defun back-to-indentation-or-beginning ()
  "Move point to the first non-whitespace character on this line.
If it's already there move it to the beginning of this line."
  (interactive "^")
  (when (= (point) (progn (back-to-indentation) (point)))
    (beginning-of-line)))
(defun end-of-code-or-line (arg)
  "Move point to the end of this line ignoring comments.
If it's already there move it to the end of this line.
With argument ARG not nil or 1, move forward ARG - 1 lines first.
Comments are recognized in any mode that sets 'syntax-ppss'
properly."
  (interactive "^P")
  (let* ((start (point))
          (bol (save-excursion
                 (beginning-of-line)
                 (point)))
          (eol (progn
                 (move-end-of-line arg)
                 (point)))
          (syn (syntax-ppss))
          (boc (nth 8 syn)))
    (when (and
            boc
            (not (nth 3 syn))
            (> boc bol))
      (goto-char boc))
    (skip-chars-backward " \t")
    (when (or (= start (point)) (= bol (point)))
      (goto-char eol))))
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
      (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))
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
(defun tell-emacsclients-for-buffer-to-die ()
  "Sends error exit command to every client for the current buffer."
  (interactive)
  (dolist (proc server-buffer-clients)
    (server-send-string proc "-error die")))
(defun jq-region ()
  "Format json with jq in a region."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "jq ." (buffer-name) t)))
(defun jq-buffer ()
  "Format json with jq in a buffer."
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "jq ." (buffer-name) t)))
(defun xmllint-region ()
  "Format xml with xmllint in a region."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -" (buffer-name) t)))
(defun xmllint-buffer ()
  "Format xml with xmllint in a buffer."
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)))
(defun xmlstarlet-region ()
  "Format xml with xmlstarlet in a region."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmlstarlet format" (buffer-name) t)))
(defun xmlstarlet-buffer ()
  "Format xml with xmlstarlet in a buffer."
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmlstarlet format" (buffer-name) t)))
(defun save-as (filename)
  "Save current buffer to FILENAME."
  (interactive "F")
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) filename)
    (find-file filename)))
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(global-set-key (kbd "C-f") #'isearch-forward)
(define-key isearch-mode-map (kbd "C-f") #'isearch-repeat-forward)
(global-set-key (kbd "C-S-f") #'isearch-backward)
(define-key isearch-mode-map (kbd "C-S-f") #'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-v") #'isearch-yank-kill)
(define-key isearch-mode-map (kbd "<escape>") #'isearch-abort)
(global-set-key (kbd "C-r") #'query-replace)
(global-set-key (kbd "C-n") #'new-empty-buffer)
(global-set-key (kbd "C-o") #'find-file)
(global-set-key (kbd "C-s") #'save-buffer)
(global-set-key (kbd "C-S-s") #'save-as)
(global-set-key (kbd "C-a") #'mark-whole-buffer)
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "C-,") #'move-cursor-previous-pane)
(global-set-key (kbd "C-.") #'move-cursor-next-pane)
(global-set-key (kbd "<home>") #'back-to-indentation-or-beginning)
(global-set-key (kbd "<end>") #'end-of-code-or-line)
(global-set-key (kbd "C-/") #'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-k") #'kill-current-buffer)
(global-set-key (kbd "C-|") #'split-window-horizontally)
(global-set-key (kbd "C-_") #'split-window-vertically)
(global-set-key (kbd "C-x C-M-c") #'tell-emacsclients-for-buffer-to-die)
(global-set-key (kbd "<S-f2>") #'list-buffers)

;;https://stackoverflow.com/questions/4918707/in-emacs-how-to-go-back-to-previous-line-position-after-using-semantic-jump-to
;; pop-global-mark
;; C-x C-x (exchange-point-and-mark)

;; ========== Startup actions ==========

;; Kill scratch buffer.
(kill-buffer "*scratch*")
