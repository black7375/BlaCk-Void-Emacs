;;;=========================Black-Void Emacs=========================;;;
;;; core-extends-mode.el - emacs core extends features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar setup-mode nil)
(check-setup)

;;-------------------------System Config-------------------------;;
(add-to-list 'exec-path "/snap/bin"                     )
(add-to-list 'exec-path "/usr/local/bin"                )
(add-to-list 'exec-path "~/.local/bin"                  )
(add-to-list 'exec-path "/home/linuxbrew/.linuxbrew/bin")
(add-to-list 'exec-path "~/.cargo/bin"                  )

(add-hook 'mouse-leave-buffer-hook #'(lambda ()
                                       "Kill Minibuffer"
                                       (when (and (>= (recursion-depth) 1)
                                                  (active-minibuffer-window))
                                         (abort-recursive-edit))))

(setq make-backup-files nil
      auto-save-default nil)

(defun new-empty-buffer ()
  "Open a New empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "New empty")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
(global-set-key (kbd "C-S-n") 'new-empty-buffer)

(defun mwheel-scroll-all-function-all (func &optional arg)
  "Scroll multiple buffers together. - with 'scroll-all-mode'"
  (if (and scroll-all-mode arg)
      (save-selected-window
        (walk-windows
         (lambda (win)
           (select-window win)
           (condition-case nil
               (funcall func arg)
             (error nil)))))
    (funcall func arg)))

(defun mwheel-scroll-all-scroll-up-all   (&optional arg)
  (mwheel-scroll-all-function-all 'scroll-up   arg))
(defun mwheel-scroll-all-scroll-down-all (&optional arg)
  (mwheel-scroll-all-function-all 'scroll-down arg))

(setq mwheel-scroll-up-function   'mwheel-scroll-all-scroll-up-all
      mwheel-scroll-down-function 'mwheel-scroll-all-scroll-down-all)

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))
(global-set-key (kbd "C-c C-o") 'switch-to-minibuffer)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;----------Performance
(setq redisplay-dont-pause         t
      jit-lock-defer-time          0.05
      fast-but-imprecise-scrolling t
      gc-cons-threshold            100000000)

(add-hook 'minibuffer-setup-hook
          #'(lambda ()
              (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          #'(lambda ()
              (setq gc-cons-threshold 800000)))

;;----------Packages
(use-package auto-package-update
  :ensure t
  :init
  (add-hook 'auto-package-update-before-hook
            #'(lambda () (message "I will update packages now")))
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

(use-package try
  :ensure t)

(use-package use-package-ensure-system-package
  :ensure t)

(use-package system-packages
  :ensure t)

(use-package dash
  :ensure t)

(use-package which-key
  :ensure t
  :init
  (global-unset-key (kbd "C-h C-h"))
  (setq which-key-frame-max-height 50)
  :config
  (which-key-mode t))

(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers      t
        enable-recursive-minibuffers t
        ivy-count-format             "(%d/%d) ")
  :bind
  (("C-c C-r" . 'ivy-resume)
   :map ivy-minibuffer-map
   ("C-j" . 'ivy-immediate-done)
   ("RET" . 'ivy-alt-done))
  :config
  (ivy-mode 1)

  ;;Sorting
  (recentf-mode 1)
  (defun eh-ivy-return-recentf-index (dir)
    (when (and (boundp 'recentf-list)
               recentf-list)
      (let ((files-list
             (cl-subseq recentf-list
                        0 (min (- (length recentf-list) 1) 20)))
            (index 0))
        (while files-list
          (if (string-match-p dir (car files-list))
              (setq files-list nil)
            (setq index (+ index 1))
            (setq files-list (cdr files-list))))
        index)))

  (defun eh-ivy-sort-file-function (x y)
    (let* ((x (concat ivy--directory x))
           (y (concat ivy--directory y))
           (x-mtime (nth 5 (file-attributes x)))
           (y-mtime (nth 5 (file-attributes y))))
      (if (file-directory-p x)
          (if (file-directory-p y)
              (let ((x-recentf-index (eh-ivy-return-recentf-index x))
                    (y-recentf-index (eh-ivy-return-recentf-index y)))
                (if (and x-recentf-index y-recentf-index)
                    ;; Directories is sorted by `recentf-list' index
                    (< x-recentf-index y-recentf-index)
                  (string< x y)))
            t)
        (if (file-directory-p y)
            nil
          ;; Files is sorted by mtime
          (time-less-p y-mtime x-mtime)))))
  (add-to-list 'ivy-sort-functions-alist
               '(read-file-name-internal . eh-ivy-sort-file-function)))

(use-package smex
  :ensure t
  :bind
  ;; This is your old M-x.
  ("C-c C-c M-x" . 'execute-extended-command))

(use-package counsel
  :ensure t
  :init
  (global-unset-key (kbd "<f1>"))
  :bind
  (("M-x"     . 'counsel-M-x               )
   ("<f1> f"  . 'counsel-describe-function )
   ("<f1> v"  . 'counsel-describe-variable )
   ("<f1> l"  . 'counsel-find-library      )
   ("<f1> i"  . 'counsel-info-lookup-symbol)
   ("<f1> u"  . 'counsel-unicode-char      )
   ("<f1> m"  . 'counsel-imenu             )
   ("C-c g"   . 'counsel-git               )
   ("C-c j"   . 'counsel-git-grep          )
   ("C-c k"   . 'counsel-rg                )
   ("C-x l"   . 'counsel-locate            )
   ("C-S-r"   . 'counsel-rhythmbox         )
   ("C-x r b" . 'counsel-bookmark          )
   ("C-c t"   . 'counsel-semantic          )
   :map minibuffer-local-map
   ("C-r"     . 'counsel-minibuffer-history))
  :config
  (counsel-mode t))

(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching     t
        projectile-completion-system 'ivy)
  :config
  (projectile-global-mode t))

(use-package company
  :ensure t
  :init
  (setq company-minimum-prefix-length 2
        vlisp-company-prefix-length   2)
  :hook
  (after-init . global-company-mode)
  :bind
  ("C-'" . company-complete)
  :config
  (defun vlisp-prefix ()
    (interactive)
    (let ((prefix (company-grab-symbol)))
      (and (eq major-mode 'vlisp-mode)
           (cons prefix (>= (length prefix)
                            vlisp-company-prefix-length))))))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'c++-mode-hook
            (lambda () (setq flycheck-clang-language-standard "c++11")))
  ;(setq flycheck-check-syntax-automatically '(mode-enabled save))
  ;(setq flycheck-highlighting-mode t)
  (global-flycheck-mode))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t))

(use-package discover-my-major
  :ensure t
  :bind
  (("C-h C-m" . 'discover-my-major)
   ("C-h M-m" . 'discover-my-mode )))

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/zsh"))

(use-package image+
  :ensure t
  :config
  (eval-after-load 'image '(require 'image+))
  (eval-after-load 'image+ '(imagex-global-sticky-mode 1)))

(use-package zoom
  :ensure t
  :bind
  ("C-x +" . 'zoom)
  :config
  (defun size-callback ()
    (cond ((> (frame-pixel-width) 1280) '(90 . 0.75)))
    (t                            '(0.5 . 0.5)))
  :custom
  (zoom-size '(0.618. 0.618))
  (zoom-size 'size-callback )
  (zoom-ignored-major-modes '(dired-mode markdown-mode minimap-mode minimap-major-modes))
  (zoom-ignored-buffer-names '("zoom.el" "init.el" " *MINIMAP*"))
  (zoom-ignored-buffer-name-regexps '("^*calc"))
  (zoom-ignore-predicates '((lambda () (> (count-lines (point-min) (point-max)) 20))))
  (temp-buffer-resize-mode t))

;;-------------------------Style Config-------------------------;;
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(use-package all-the-icons
  :requires eshell
  :ensure t
  :init
  (if setup-mode
      (all-the-icons-install-fonts))
  (setq inhibit-compacting-font-caches t))

(use-package xterm-color
  :ensure t
  :hook
  (gud-mode . xterm-color-colorize-buffer)
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda () (add-hook
                        'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

  ;; Also set TERM accordingly (xterm-256color)
  ;; You can also use it with eshell (and thus get color output from system ls):
  (require 'eshell)
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  ;;  Don't forget to setenv TERM xterm-256color

  (setq compilation-environment '("TERM=xterm-256color"))
  (add-hook 'compilation-start-hook
            (lambda (proc)
              ;; We need to differentiate between compilation-mode buffers
              ;; and running as part of comint (which at this point we assume
              ;; has been configured separately for xterm-color)
              (when (eq (process-filter proc) 'compilation-filter)
                ;; This is a process associated with a compilation-mode buffer.
                ;; We may call `xterm-color-filter' before its own filter function.
                (set-process-filter
                 proc
                 (lambda (proc string)
                   (funcall 'compilation-filter proc
                            (xterm-color-filter string))))))))

(use-package airline-themes
  :ensure t
  :config
  (load-file "~/.emacs.d/src/editor/airline-void.el") ;load 'airline-void
  (setq powerline-height 20
        powerline-utf-8-separator-left        #xe0b0
        powerline-utf-8-separator-right       #xe0b2
        airline-utf-glyph-separator-left      #xe0b0
        airline-utf-glyph-separator-right     #xe0b2
        airline-utf-glyph-subseparator-left   #xe0b1
        airline-utf-glyph-subseparator-right  #xe0b3
        airline-utf-glyph-branch              #xe0a0
        airline-utf-glyph-readonly            #xe0a2
        airline-utf-glyph-linenumber          #xe0a1)
  )

(use-package hide-mode-line
  :ensure t)

(use-package yascroll
  :ensure t
  :config
  (setq yascroll:delay-to-hide nil)
  (global-yascroll-bar-mode 1))

(use-package tabbar
  :ensure t)

(use-package minimap
  :ensure t
  :commands minimap-mode
  :hook
  (minimap-mode . hide-mode-line-mode)
  :config
  (setq minimap-minimum-width             10
        minimap-hide-scroll-bar           t
        minimap-dedicated-window          t
        minimap-window-location           'right
        minimap-display-semantic-overlays t
        minimap-width-fraction            0.1)
  (minimap-mode 1)

  (defun minimap-toggle ()
    "Toggle minimap for current buffer."
    (interactive)
    (if (not (boundp 'minimap-bufname))
        (setf minimap-bufname nil))
    (if (null minimap-bufname)
        (progn (minimap-create)
               (set-frame-width (selected-frame) 100))
      (progn (minimap-kill)
             (set-frame-width (selected-frame) 80))))

  :custom-face
  (minimap-active-region-background ((t (:background "#2F3339")))))
;;; core-extends-mode.el ends here
