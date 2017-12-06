;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 設定ルートファイル
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 参考: http://qiita.com/skkzsh/items/20af9affd5cc1e9678f8
;;;       http://qiita.com/yoshikyoto/items/3f5de63648febe897bee
;;;       https://masutaka.net/chalow/2015-10-03-1.html

;;; パスを通す
;; load-path で locate-user-emacs-file に ~/.emacs.d などが入る

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "conf/"))
(add-to-list 'load-path (locate-user-emacs-file "site-lisp/"))

;; 最初にやらなければならないもの
(load "el-get-init")
(load "startup-init")

;; 特定のパッケージの設定
(load "markdown-init")
(load "split-window-init")
(load "linum-init")
;; (load "powerline-init")

;; 色々と機能別に設定
(load "add-hook-init")
(load "font-init")
(load "window-init")
(load "backup-init")
(load "codestyle-init")
(load "info-init")
(load "operability-init")
(load "look-init")
(load "syntax-check-init")
(load "major-mode-init")
;; (load "python-init")
;; (load "md-init")
;; (load "tex-init")

;; 最後にやらなければならないもの
(load "auto-complete-init")


;;; auto-written

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ac-ignore-case nil)
;;  '(ace-isearch-function (quote avy-goto-char))
;;  '(ace-isearch-input-length 5)
;;  '(ace-isearch-jump-delay 0.5)
;;  '(ace-isearch-use-jump (quote printing-char))
;;  '(byte-compile-warnings
;;    (quote
;;     (redefine callargs free-vars unresolved obsolete noruntime interactive-only make-local)))
;;  '(package-selected-packages (quote (hlinum el-get summarye)))
;;  '(safe-local-variable-values
;;    (quote
;;     ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
;; 	   (add-hook
;; 	    (quote write-contents-functions)
;; 	    (lambda nil
;; 	      (delete-trailing-whitespace)
;; 	      nil))
;; 	   (require
;; 	    (quote whitespace))
;; 	   "Sometimes the mode needs to be toggled off and on."
;; 	   (whitespace-mode 0)
;; 	   (whitespace-mode 1))
;;      (whitespace-line-column . 80)
;;      (whitespace-style face tabs trailing lines-tail))))
;;  '(session-use-package t nil (session)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(hl-line ((t (:background "color-236"))))
;;  '(linum-highlight-face ((t (:foreground "#0d0d0d" :background "#909090")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ignore-case nil)
 '(package-selected-packages (quote (bind-key)))
 '(safe-local-variable-values
   (quote
    ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
	   (add-hook
	    (quote write-contents-functions)
	    (lambda nil
	      (delete-trailing-whitespace)
	      nil))
	   (require
	    (quote whitespace))
	   "Sometimes the mode needs to be toggled off and on."
	   (whitespace-mode 0)
	   (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face tabs trailing lines-tail))))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "color-236"))))
 '(linum-highlight-face ((t (:foreground "#0d0d0d" :background "#909090")))))
