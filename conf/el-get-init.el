;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; el-get 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; el-get settings
;; パッケージ自動ダウンロードツール el-get の設定
;; 参考: http://qiita.com/yoshikyoto/items/3f5de63648febe897bee
;;       http://blog.ogatomo.com/blog/2014/01/08/migration-to-el-get/
;;       https://github.com/dimitri/el-get
;;       https://github.com/dimitri/el-get/issues/1265

;;; melpaなどのpackageを追加
(setq package-archives '(
("ELPA" . "http://tromey.com/elpa/")
("gnu" . "http://elpa.gnu.org/packages/")
("melpa-stable" . "http://stable.melpa.org/packages/")
;; ("melpa" . "http://melpa.milkbox.net/packages/")
("marmalade" . "http://marmalade-repo.org/packages/")
("org" . "http://orgmode.org/elpa/")
("SC" . "http://joseito.republika.pl/sunrise-commander/")))

;;; パスを決めておく
;; el-get 自体の所在
(setq el-get-install-dir (locate-user-emacs-file "el-get/el-get"))
;; パッケージが el-get sync で initialize された時に実行される初期化用elisp置き場の所在
(setq el-get-user-package-directory (locate-user-emacs-file "conf/el-get-package-init"))

;;; el-get をインストール
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (message "-- installing el-get")
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp))
  (require 'el-get)
  (require 'el-get-elpa)
  (message "-- installing el-get managed package from personal repository")
  (add-to-list 'el-get-recipe-path (locate-user-emacs-file "el-get-user/recipes"))
  (message "-- udpating elpa emacs-wiki-local-recipes")
  (el-get-elpa-build-local-recipes)
  (message "-- udpating emacs-wiki-local-recipes")
  (el-get-emacswiki-build-local-recipes))

;; el-get用パッケージ定義ファイル置き場の所在
(add-to-list 'el-get-recipe-path (locate-user-emacs-file "el-get-user/recipes"))


;;; インストール失敗時にやること
;; 参考: http://myemacs.readthedocs.io/ja/latest/el-get.html
;; $ mkdir -p ~/.emacs.d/el-get
;; $ cd ~/.emacs.d/el-get
;; $ git clone https://github.com/dimitri/el-get
;; M-x el-get-elpa-build-local-recipes
;; M-x el-get-emacswiki-build-local-recipes

;;; インストールするパッケージの一覧を設定
(load "el-get-specify-packages")

;;; 設定されたパッケージをインストール
;; 設定されていないものをすべてアンインストール
(el-get-cleanup my-packages)
;; インストール
;; この際 el-get-user-package-directory に設定されたelispが呼ばれる
(el-get 'sync my-packages)
