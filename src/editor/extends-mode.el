;;;=========================Black-Void Emacs=========================;;;
;;; extends-mode.el - emacs extends features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar setup-mode nil)
(check-setup)

;;-------------------------System Config-------------------------;;
(use-package buffer-move
  :ensure t
  :bind
  ("M-<up>"    . 'buf-move-up   )
  ("M-<down>"  . 'buf-move-down )
  ("M-<left>"  . 'buf-move-left )
  ("M-<right>" . 'buf-move-right))

(use-package w3m
  :ensure-system-package w3m)

(use-package rg
  :disabled
  :ensure-system-package (rg . ripgrep)
  :config
  (rg-enable-default-bindings (kbd "M-s")))

(use-package fzf
  :ensure t
  :config
  (setenv "PATH" (concat (getenv "PATH") ":/home/linuxbrew/.linuxbrew/bin "))
  (setq exec-path (append exec-path '("/home/linuxbrew/.linuxbrew/bin "))))

(use-package xah-lookup
  :ensure t
  :config
  (setq xah-lookup-browser-function 'eww))

(use-package ivy-xref
  ;:requires ivy
  :ensure t
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-rich
  ;:requires (ivy counsel all-the-icons)
  :ensure t
  :init
  (setq ivy-virtual-abbreviate                      'full
        ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-path-style                         'abbrev)

  :config
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))
  (setq ivy-rich--display-transformers-list
      '(ivy-switch-buffer
        (:columns
         ((ivy-rich-switch-buffer-icon :width 2)
          (ivy-rich-candidate                (:width 30))
          (ivy-rich-switch-buffer-size       (:width 7))
          (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
          (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
          (ivy-rich-switch-buffer-project    (:width 15 :face success))
          (ivy-rich-switch-buffer-path       (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate
         (lambda (cand) (get-buffer cand)))))

  (ivy-rich-mode 1))

(use-package swiper
  ;:requires (ivy avy)
  :ensure t
  :bind
  (("C-s" . 'swiper)
   ("C-r" . 'swiper-avy)
   :map evil-insert-state-map
   ("C-r" . nil)))

(use-package flx
  ;:requires (ivy swiper)
  :ensure t
  :config
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))))

(use-package counsel-projectile
  ;:requires (counsel projectile)
  :ensure t
  :config
  (counsel-projectile-mode t))

(use-package projectile-ripgrep
  :requires projectile
  :ensure t)

(use-package project-explorer
  :requires projectile
  :ensure t)

(use-package ibuffer-projectile
  :ensure t
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
  :bind
  ("C-x C-b" . 'ibuffer))

(use-package ibuffer-sidebar
  :ensure t
  :commands (ibuffer-sidebar-toggle-sidebar))

(use-package neotree
  :requires hide-mode-line
  :ensure t
  :hook
  (neo-tree . hide-mode-line-mode)
  :config
  ;;Load patches
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (add-hook 'neotree-mode-hook 'hide-mode-line-mode))

(defun sidebar ()
  (interactive)
  (neotree-toggle)
  (ibuffer-sidebar-toggle-sidebar))
(global-set-key [f8] 'sidebar)

(use-package company-quickhelp
  :requires company
  :ensure t
  :config
  (company-quickhelp-mode))

(use-package lsp-mode
  :requires projectile
  :ensure t
  :hook
  ((prog-mode . lsp-mode)
   (lsp-before-open . my-set-projectile-root))
  :config
  (lsp-define-stdio-client
   lsp-prog-major-mode
   "language-id"
   (lambda () default-directory)
   '("/my/lsp/server" "and" "args"))

  (defun my-set-projectile-root ()
    (when lsp--cur-workspace
      (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace)))))

(use-package company-lsp
  :requires (company lsp-mode)
  :ensure t
  :config
  (push 'company-lsp company-backends))

(use-package flycheck-pos-tip
  :requires flycheck
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

(use-package magit
  :ensure t
  :init
  (remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
  :bind
  ("C-x g" . magit-status)
  :hook
  ((git-commit-setup        . magit-commit-prompt)
   (magit-section-highlight . magit-section-highlight))
  :config
  (defmacro pretty-magit (WORD ICON PROPS &optional NO-PROMPT?)
    "Replace sanitized WORD with ICON, PROPS and by default add to prompts."
    `(prog1
         (add-to-list 'pretty-magit-alist
                      (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
                            ,ICON ',PROPS))
       (unless ,NO-PROMPT?
         (add-to-list 'pretty-magit-prompt (concat ,WORD ": ")))))

  (setq pretty-magit-alist  nil
        pretty-magit-prompt nil
        magit-completing-read-function 'ivy-completing-read)
  (pretty-magit "Feature" ? (:foreground "slate gray" :height 1.2))
  (pretty-magit "Add"     ? (:foreground "#375E97" :height 1.2))
  (pretty-magit "Fix"     ? (:foreground "#FB6542" :height 1.2))
  (pretty-magit "Clean"   ? (:foreground "#FFBB00" :height 1.2))
  (pretty-magit "Docs"    ? (:foreground "#3F681C" :height 1.2))
  (pretty-magit "master"  ? (:box t :height 1.2) t)
  (pretty-magit "origin"  ? (:box t :height 1.2) t)

  (defun add-magit-faces ()
    "Add face properties and compose symbols for buffer from pretty-magit."
    (interactive)
    (with-silent-modifications
      (--each pretty-magit-alist
        (-let (((rgx icon props) it))
          (save-excursion
            (goto-char (point-min))
            (while (search-forward-regexp rgx nil t)
              (compose-region
               (match-beginning 1) (match-end 1) icon)
              (when props
                (add-face-text-property
                 (match-beginning 1) (match-end 1) props))))))))

  (advice-add 'magit-status :after 'add-magit-faces)
  (advice-add 'magit-refresh-buffer :after 'add-magit-faces)

  (setq use-magit-commit-prompt-p nil)
  (defun use-magit-commit-prompt (&rest args)
    (setq use-magit-commit-prompt-p t))

  (defun magit-commit-prompt ()
    "Magit prompt and insert commit header with faces."
    (interactive)
    (when use-magit-commit-prompt-p
      (setq use-magit-commit-prompt-p nil)
      (insert (ivy-read "Commit Type " pretty-magit-prompt
                        :require-match t :sort t :preselect "Add: "))
      ;; Or if you are using Helm...
      ;; (insert (helm :sources (helm-build-sync-source "Commit Type "
      ;;                          :candidates pretty-magit-prompt)
      ;;               :buffer "*magit cmt prompt*"))
      ;; I haven't tested this but should be simple to get the same behaior
      (add-magit-faces)
      (evil-insert 1)  ; If you use evil
      ))

  (advice-add 'magit-commit :after 'use-magit-commit-prompt)
  (add-hook 'magit-status-mode-hook '(lambda ()
                                       (face-remap-add-relative 'magit-item-highlight))))

(use-package evil-magit
  :requires (evil magit)
  :ensure t)

(use-package realgud
  :ensure t)

(use-package image-dired+
  :ensure t
  :config
  (eval-after-load 'image-dired+ '(image-diredx-async-mode 1))
  (setq image-dired-track-movement nil))

;;-------------------------Style Config-------------------------;;
(use-package tab-group
  :requires tabbar
  :ensure t)

(use-package tabbar-ruler
  :requires tabbar
  :ensure t
  :config
  (setq tabbar-ruler-global-tabbar t)      ; get tabbar
  ;;(setq tabbar-ruler-global-ruler t)     ; get global ruler
  ;;(setq tabbar-ruler-popup-menu t)       ; get popup menu.
  ;;(setq tabbar-ruler-popup-toolbar t)    ; get popup toolbar
  ;;(setq tabbar-ruler-popup-scrollbar t)  ; show scroll-bar on mouse-move
  ;;(tabbar-ruler-group-buffer-groups)
  ;;(global-set-key (kbd "C-c t") 'tabbar-ruler-move)
  )

;;; extends-mode.el ends here
