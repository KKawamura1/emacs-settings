;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       設定ルートファイル
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

;; フック作成
(load "add-hook-init")
;; 各パッケージのメイン設定
(load "packages-init")
;; 起動時設定
(load "startup-init")
;; 画面分割
(load "split-window-init")
;; 操作性改善
(load "operability-init")
;; 見た目設定
(load "look-init")


;;; 以下，auto-written
;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ignore-case nil)
 '(package-selected-packages
   (quote
    (esup yatex which-key volatile-highlights visual-regexp-steroids use-package undo-tree smartrep smartparens smart-newline session pyenv-mode py-autopep8 powerline popwin markdown-mode magit jedi hlinum helm flycheck-pos-tip flycheck-popup-tip expand-region elpy diminish)))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum-highlight-face ((t (:foreground "#0d0d0d" :background "#909090")))))
