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

;; 初回設定
(load "first-setup")

;; ローカル設定変数
(let ((local-conf (locate-user-emacs-file "local-conf.el")))
  (if (file-exists-p local-conf)
      (load local-conf)
    (display-warning "Userwarning" "Please make your local-conf file.")))
;; フック作成
(load "add-hook-init")
;; 起動時設定
(load "startup-init")
;; 各パッケージのメイン設定
(load "packages-init")
;; 画面分割
(load "split-window-init")
;; 操作性改善
(load "operability-init")
;; 見た目設定
(load "look-init")

;; customizeの適用
(load "custom-file")
