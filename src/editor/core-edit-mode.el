;;;=========================Black-Void Emacs=========================;;;
;;; core-edit-mode.el - emacs core edit features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar setup-mode nil)
(check-setup)

;;-------------------------System Config-------------------------;
(setq locale-coding-system   'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(delete-selection-mode   1)
(electric-indent-mode    1)
(global-auto-revert-mode 1)
(global-whitespace-mode  t)

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(setq scroll-step               1
      next-screen-context-lines 2
      scroll-margin             3
      whitespace-style          '(face trailing tab-mark)
      truncate-lines            t)

'(whitespace-trailing (
                       (((class color)
                         (min-colors 257))
                        (:background unspecified
                                     :foreground "#3E4451"
                                     :inverse-video t))
                       (((class color)
                         (min-colors 89))
                        (:background unspecified
                                     :foreground "#FFAF5F"
                                     :inverse-video t))))


(global-set-key (kbd "C-S-<up>")    '(lambda ()
                                       (interactive)
                                       (when (not (region-active-p))
                                         (push-mark (point) t t))
                                       (previous-line)))
(global-set-key (kbd "C-S-<down>")  '(lambda ()
                                       (interactive)
                                       (when (not (region-active-p))
                                         (push-mark (point) t t))
                                       (next-line)))
(global-set-key (kbd "C-S-<left>")  '(lambda ()
                                       (interactive)
                                       (when (not (region-active-p))
                                         (push-mark (point) t t))
                                       (backward-char)))
(global-set-key (kbd "C-S-<right>") '(lambda ()
                                       (interactive)
                                       (when (not (region-active-p))
                                         (push-mark (point) t t))
                                       (forward-char)))

(defun change-buffer-until-normal (change-fn)
  (let (current (buffer-name))
    (funcall change-fn)
    (while (and (string-prefix-p "*" (buffer-name))
                (not (eq current (buffer-name))))
      (funcall change-fn))))
(defun tab-next-buffer ()
  "Trans next buffer."
  (interactive)
  (change-buffer-until-normal 'next-buffer))
(defun tab-prev-buffer ()
  "Trans previous buffer."
  (interactive)
  (change-buffer-until-normal 'previous-buffer))
(global-set-key [C-tab]           'tab-next-buffer)
(global-set-key [C-S-iso-lefttab] 'tab-prev-buffer)

(global-set-key (kbd "C-S-a") 'mark-whole-buffer)
(global-set-key (kbd "C-S-x") 'kill-region)
(global-set-key (kbd "C-S-c") 'kill-ring-save)
(global-set-key (kbd "C-S-v") 'yank-and-indent)
(global-set-key (kbd "C-S-s") 'save-buffer)
(global-set-key (kbd "C-M-s") '(lambda ()
                                 (interactive)
                                 (save-some-buffers t)))
(global-set-key (kbd "C-S-f")  'swiper)
(global-set-key (kbd "C-S-z")  'undo-tree-undo)
(global-set-key (kbd "C-M-z")  'undo-tree-redo)
(global-set-key (kbd "S-<f3>") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "<f3>")   'kmacro-end-or-call-macro)
(setq-default indent-tabs-mode nil)
(setq initial-major-mode 'text-mode)

(global-set-key (kbd "<C-return>") (lambda ()
                                     (interactive)
                                     (end-of-line)
                                     (newline-and-indent)))

(global-set-key (kbd "<C-S-return>") (lambda ()
                                       (interactive)
                                       (beginning-of-line)
                                       (newline-and-indent)
                                       (previous-line)))
                                        ;(if (eq window-system 'x))
                                        ;    (shell-command "xmodmap -e 'clear Lock' -e 'keycode 66 = Control'"))
                                        ;(global-set-key [f13] 'execute-extended-command)

;;----------Packages
(use-package evil
  :ensure t
  :bind
  (:map evil-normal-state-map  ;for Korean.
        ("ㅑ"   . 'evil-insert     )
        ("S-ㅑ" . 'evil-insert-line)
        ("ㅁ"   . 'evil-append     )
        ("S-ㅁ" . 'evil-append-line)
        ("ㅐ"   . 'evil-open-below )
        ("ㅒ"   . 'evil-open-above )
        ("U" . 'undo-line)

   :map evil-insert-state-map
        ("C-S-k" . 'evil-insert-digraph)
        ("C-k"   . nil))
  :config
  (evil-mode 1)
  (setq evil-want-fine-undo t)

  (defun undo-line ()
    "Undo the previous change and any other changes made at the same time to the same line."
    (interactive)
    (progn
      (when (buffer-modified-p) (undo-tree-undo))
      (save-restriction
        (narrow-to-region (line-beginning-position) (line-end-position))
        (while (buffer-modified-p)
          (undo-tree-undo))))))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)

  (defun undo-tree-visualizer-update-linum (&rest args)
    (linum-update undo-tree-visualizer-parent-buffer))
  ;(advice-add 'undo-tree-visualize-undo       :after #'undo-tree-visualizer-update-linum)
  ;(advice-add 'undo-tree-visualize-redo       :after #'undo-tree-visualizer-update-linum)
  (advice-add 'undo-tree-visualize-undo-to-x  :after #'undo-tree-visualizer-update-linum)
  (advice-add 'undo-tree-visualize-redo-to-x  :after #'undo-tree-visualizer-update-linum)
  (advice-add 'undo-tree-visualizer-mouse-set :after #'undo-tree-visualizer-update-linum)
  (advice-add 'undo-tree-visualizer-set       :after #'undo-tree-visualizer-update-linum))

(use-package drag-stuff
  :ensure t
  :bind
  (("C-M-<up>"    . 'drag-stuff-up)
   ("C-M-<down>"  . 'drag-stuff-down)
   ("C-M-<left>"  . 'drag-stuff-left)
   ("C-M-<right>" . 'drag-stuff-rihgt))
  :config
  (drag-stuff-global-mode 1))

(use-package avy
  :ensure t
  :bind
  (("C-:"     . 'avy-goto-char  )
   ("C-\""    . 'avy-goto-char-2)
   ("M-g f"   . 'avy-goto-line  )
   ("M-g w"   . 'avy-goto-word-1)
   ("M-g e"   . 'avy-goto-word-0)
   ("C-c C-j" . 'avy-resume     )))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-S-d"   . 'mc/edit-lines)
  ("C->"     . 'mc/mark-next-like-this)
  ("C-<"     . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this))

(use-package yasnippet
  :ensure t
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/")

  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (yas-global-mode 1)
  (yas-reload-all))

;;-------------------------Style Config-------------------------;;
;(global-hl-line-mode t)
(show-paren-mode     1)
(setq next-screen-context-lines 3
      search-highlight          t)

(add-to-list 'default-frame-alist '(font . "Hack"))
(set-face-attribute 'default nil
                    :family "Hack")
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)
                  '("D2Coding" . "iso10646-1"))
(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
                  '("D2Coding" . "iso10646-1"))

(defun hangul-rescale ()
  "Hangul will created at the same interval."
  (interactive)
  (set-face-attribute 'default nil
                      :height 130)
  (setq face-font-rescale-alist
        '(("D2Coding" . 1.26491106407))))
(add-hook 'after-init-hook #'hangul-rescale)

(use-package emojify
  :ensure t
  :hook
  (after-init . global-emojify-mode))

(use-package nlinum-hl
  :ensure t
  :hook
  (nilnum-mode . precalculate-nilnum-mode)
  (post-gc     . nlinum-hl-flush-all-windows)
  (focus-in    . nlinum-hl-flush-all-windows)
  (focus-out   . nlinum-hl-flush-all-windows)

  :init
  ;; Preset `nlinum-format' for minimum width.
  (defun precalculate-nlinum-mode ()
    (when nlinum-mode
      (setq-local nlinum-format
                  (concat "%" (number-to-string
                               ;; Guesstimate number of buffer lines.
                               (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
                          "d"))))

  :config
  (advice-add #'select-window :before #'nlinum-hl-do-select-window-flush)
  (advice-add #'select-window :after  #'nlinum-hl-do-select-window-flush)

  (run-with-idle-timer 5 t #'nlinum-hl-flush-window)
  (run-with-idle-timer 30 t #'nlinum-hl-flush-all-windows)

  (advice-add #'markdown-fontify-code-block-natively
              :after #'nlinum-hl-do-markdown-fontify-region)
  (advice-add #'web-mode-fold-or-unfold :after #'nlinum-hl-do-generic-flush)
  (advice-add #'set-frame-font :after #'nlinum-hl-flush-all-windows)

  (global-nlinum-mode t)
  (setq nlinum-highlight-current-line t))
;;; core-edit-mode.el ends here
