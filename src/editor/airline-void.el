;;; airline-void --- Summary
;;; Commentary:
;;; Code:

(deftheme airline-void
  "source: https://github.com/hlissner/emacs-doom-theme")

(let ((normal-outer-foreground   "#1B2229") (normal-outer-background   "#F7A918")
      (normal-inner-foreground   "#ffffff") (normal-inner-background   "#373B41")
      (normal-center-foreground  "#ffffff") (normal-center-background  "#2D2D2D")

      (insert-outer-foreground   "#1B2229") (insert-outer-background   "#7DDA1D")
      (insert-inner-foreground   "#ffffff") (insert-inner-background   "#373B41")
      (insert-center-foreground  "#ffffff") (insert-center-background  "#2D2D2D")

      (visual-outer-foreground   "#1B2229") (visual-outer-background   "#11EFE5")
      (visual-inner-foreground   "#ffffff") (visual-inner-background   "#373B41")
      (visual-center-foreground  "#ffffff") (visual-center-background  "#2D2D2D")

      (replace-outer-foreground  "#1B2229") (replace-outer-background  "#E1031C")
      (replace-inner-foreground  "#ffffff") (replace-inner-background  "#373B41")
      (replace-center-foreground "#ffffff") (replace-center-background "#2D2D2D")

      (emacs-outer-foreground    "#1B2229") (emacs-outer-background    "#7E67E5")
      (emacs-inner-foreground    "#ffffff") (emacs-inner-background    "#373B41")
      (emacs-center-foreground   "#ffffff") (emacs-center-background   "#2D2D2D")

      (inactive1-foreground      "#4e4e4e") (inactive1-background      "#191919")
      (inactive2-foreground      "#4e4e4e") (inactive2-background      "#232323")
      (inactive3-foreground      "#4e4e4e") (inactive3-background      "#262626"))

  (airline-themes-set-deftheme 'airline-void)

  (when airline-cursor-colors
    (setq evil-emacs-state-cursor  `(bar, emacs-outer-background)
          evil-normal-state-cursor  normal-outer-background
          evil-insert-state-cursor `(bar, insert-outer-background)
          evil-replace-state-cursor replace-outer-background
          evil-visual-state-cursor  visual-outer-background))

  (defun cursor-mode-status ()
    "change cursor color and type according to some minor modes."
    (cond
     (buffer-read-only
      (setq cursor-type 'hbar))
     (overwrite-mode
      (setq cursor-type 'box))))
  (add-hook 'post-command-hook 'cursor-mode-status)
  )

(airline-themes-set-modeline)

(provide-theme 'airline-void)
;;; airline-void.el ends here
