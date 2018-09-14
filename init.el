;;;=========================BlaCk-Void Emacs=========================;;;
;;; init.el --- initialization Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copyright (c) 2017-2018 black7375
;;
;; Author:  black7375 (alsrtjr7375@daum.net)
;; Version: 1.0b;
;;
;; This file is not part of GNU Emacs.
;;
;;; License: BSD-2-Clause

;;-------------------------List Package Archives-------------------------;;
(require 'package)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t))

(package-initialize)

;;-------------------------Emacs Setup Config-------------------------;;
;;----------Setup
(defvar setup-mode nil)
(defun check-setup()
  (if (member "--setup" command-line-args)
      (progn (setq setup-mode t)
             (message "BlaCk_Void Emacs-Mode Setup.."))
    nil))

(check-setup)

(if setup-mode
    (package-refresh-contents) nil)

;;----------Install pre-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (setq setup-mode t))

;;-------------------------Emacs Setting Load-------------------------;;
(load-file "~/.emacs.d/src/editor/core-extends-mode.el")
(load-file "~/.emacs.d/src/editor/core-edit-mode.el"   )
(load-file "~/.emacs.d/src/editor/extends-mode.el"     )
(load-file "~/.emacs.d/src/editor/edit-mode.el"        )
(put 'downcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (auto-highlight-symbol evil-magit evil counsel all-the-icons use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-active-region-background ((t (:background "#2F3339"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))
