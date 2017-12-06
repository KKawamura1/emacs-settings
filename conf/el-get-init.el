;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; el-get 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; el-get settings
;;; パッケージ自動ダウンロードツール el-get の設定
;;; 参考: http://qiita.com/yoshikyoto/items/3f5de63648febe897bee
;;;       http://blog.ogatomo.com/blog/2014/01/08/migration-to-el-get/
;;;       https://github.com/dimitri/el-get

;;; el-get をインストール
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;; インストール失敗時にやること
;; 参考: http://myemacs.readthedocs.io/ja/latest/el-get.html
;; $ cd ~/.emacs.d
;; $ git clone https://github.com/dimitri/el-get

;;; パスを通す
;; el-get 自体の所在
(setq el-get-install-dir (locate-user-emacs-file "el-get/el-get"))
;; el-getでダウンロードしたパッケージの所在
(setq el-get-dir (locate-user-emacs-file "el-get/el-get-packages"))
;; el-get用パッケージ定義ファイル置き場の所在
(add-to-list 'el-get-recipe-path (locate-user-emacs-file "el-get/el-get-user/recipes/normal"))
(setq el-get-recipe-path-elpa (locate-user-emacs-file "el-get/el-get-user/recipes/elpa"))
(setq el-get-recipe-path-emacswiki (locate-user-emacs-file "el-get/el-get-user/recipes/emacswiki"))
;; パッケージが el-get sync で initialize された時に実行される初期化用elisp置き場の所在
(setq el-get-user-package-directory (locate-user-emacs-file "conf/el-get-package-init"))

;;; インストールするパッケージの一覧を設定
(load "el-get-specify-packages")

;;; 設定されたパッケージをインストール
;; 設定されていないものをすべてアンインストール
;; (el-get-cleanup my-packages)
;; インストール
;; この際 el-get-user-package-directory に設定されたelispが呼ばれる
(el-get 'sync my-packages)
