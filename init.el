;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       設定ルートファイル
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 参考: http://qiita.com/skkzsh/items/20af9affd5cc1e9678f8
;;;       http://qiita.com/yoshikyoto/items/3f5de63648febe897bee
;;;       https://masutaka.net/chalow/2015-10-03-1.html

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;;; パスを通す
;; load-path で locate-user-emacs-file に ~/.emacs.d などが入る
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

;; todo list
;; https://qiita.com/yynozk/items/7dce94f770e6f3f0b26c
;; http://www.shigemk2.com/entry/emacs_resudo
