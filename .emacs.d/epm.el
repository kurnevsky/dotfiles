(require 'package)
;; Configure the list of remote archives.
(dolist (archive '(("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/")))
  (add-to-list 'package-archives archive))
(setq package-archive-priorities
  '(("melpa" . 2)
     ("melpa-stable" . 1)
     ("gnu" . 0)))
;; Fix for bug 34341.
(when (version<= "26" emacs-version)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
;; Activate all the packages (in particular autoloads).
(package-initialize t)
(package-activate 'epm)
