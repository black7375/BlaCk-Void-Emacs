;;;=========================Black-Void Emacs=========================;;;
;;; edit-mode.el - improve editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar setup-mode nil)
(check-setup)

;;-------------------------System Config-------------------------;
(load-file "~/.emacs.d/src/download/editor/ll-debug.el") ;;simple debuger[with print]
(global-set-key (kbd "C-c C-i") 'll-debug-insert)
(global-set-key (kbd "C-c C-d") 'll-debug-revert)
(global-set-key (kbd "C-c C-t") 'll-debug-toggle-comment-region-or-line)
(global-set-key (kbd "C-c C-y") 'll-debug-copy-and-comment-region-or-line)

(use-package evil-escape
  :requires evil
  :ensure t
  :bind
  ("C-c C-g" . 'evil-escape))

(use-package vdiff
  :ensure t
  :config
  (evil-define-key 'normal vdiff-mode-map "," vdiff-mode-prefix-map)
  (evil-define-minor-mode-key 'normal 'vdiff-mode "]c" 'vdiff-next-hunk)
  (evil-define-minor-mode-key 'normal 'vdiff-mode "[c" 'vdiff-previous-hunk)
  (evil-define-minor-mode-key 'normal 'vdiff-mode "zc" 'vdiff-close-fold)
  (evil-define-minor-mode-key 'normal 'vdiff-mode "zM" 'vdiff-close-all-folds)
  (evil-define-minor-mode-key 'normal 'vdiff-mode "zo" 'vdiff-open-fold)
  (evil-define-minor-mode-key 'normal 'vdiff-mode "zR" 'vdiff-open-all-folds)
  (evil-define-minor-mode-key 'motion 'vdiff-mode "go" 'vdiff-receive-changes)
  (evil-define-minor-mode-key 'motion 'vdiff-mode "gp" 'vdiff-send-changes)

  (setq vdiff-subtraction-style 'full)
  (setq vdiff-subtraction-fill-char ?-))

(use-package dumb-jump
  :requires ivy
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy)
  (dumb-jump-mode))

(use-package origami
  :ensure t)

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . 'er/expand-region))

(use-package yasnippet-snippets
  :ensure t)

(use-package auto-yasnippet
  :ensure t)

(use-package iedit
  :ensure t
  :bind
  ("C-u" . 'iedit-mode))

(use-package visible-mark
  :ensure t
  :config
  (global-visible-mark-mode 1))

(use-package bm
  :ensure t
  :bind
  ("<C-f2>"                  . 'bm-toggle)
  ("<f2>"                    . 'bm-next)
  ("<S-f2>"                  . 'bm-previous)
  ("<left-fringe> <mouse-1>" . 'bm-toggle-mouse))

(use-package browse-kill-ring
  :ensure t)

(use-package diff-hl
  :ensure t
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh))

;;-------------------------Style Config-------------------------;;
(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode)
  :config
  (remove-hook 'c-mode-common-hook 'rainbow-mode t))

(use-package fill-column-indicator
  :ensure t
  :hook
  ((company-completion-started   . company-turn-off-fci     )
   (company-completion-finished  . company-maybe-turn-on-fci)
   (company-completion-cancelled . company-maybe-turn-on-fci)

   (after-change-major-mode     . auto-fci-mode)
   (window-configuration-change . auto-fci-mode))
  :config
  (setq fci-rule-column 80
        fci-rule-width  2
        fci-rule-color  "#1B1D1E")

  (defvar-local company-fci-mode-on-p nil)
  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))
  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (define-global-minor-mode global-fci-mode fci-mode
    (lambda () (fci-mode 1)))
  (global-fci-mode 1)
  (defun auto-fci-mode (&optional unused)
    (if (> (window-width) fci-rule-column)
        (fci-mode 1)
      (fci-mode 0))))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'text-mode-hook 'rainbow-delimiters-mode)
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"  ))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"    ))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"   ))))
  (rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "yellow"       ))))
  (rainbow-delimiters-depth-6-face ((t (:foreground "orchid"       ))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "spring green" ))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"      )))))

(use-package smartparens
  :ensure t
  :config
  (sp-pair "'" nil :actions :rem)
  (smartparens-global-mode t))

(use-package whitespace-cleanup-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

(use-package indent-guide
  :ensure t
  :config
  (set-face-background 'indent-guide-face "dimgray")
  (setq indent-guide-delay 0.1)
  (indent-guide-global-mode))

(use-package rainbow-identifiers
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

(use-package volatile-highlights
  :ensure t
  :config
  (vhl/define-extension  'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  (vhl/define-extension  'evil 'evil-paste-after 'evil-paste-before
                         'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)
  (volatile-highlights-mode t))

(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

(use-package auto-highlight-symbol
  :ensure t
  :config
  (global-auto-highlight-symbol-mode t))

(use-package highlight-symbol
  :ensure t
  :bind
  (("C-j"   . 'highlight-symbol-next)
   ("C-k"   . 'highlight-symbol-prev)
   ("C-c h" . 'highlight-symbol     ))
  :diminish highlight-symbol-mode)

(use-package highlight-parentheses
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  (add-hook 'text-mode-hook 'highlight-parentheses-mode))

(use-package visual-regexp
  :ensure t)

(use-package visual-regexp-steroids
  :requires (visual-regexp multiple-cursors)
  :ensure t
  :bind
  (("C-c r" . 'vr/replace      )
   ("C-c q" . 'vr/query-replace)
   ("C-c m" . 'vr/mc-mark      )
   :map esc-map
   ("C-r" . 'vr/isearch-backward)  ;;C-M-r, C-M-s
   ("C-s" . 'vr/isearch-backward)))
;;; edit-mode.el ends here
