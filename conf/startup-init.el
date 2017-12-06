;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacsの起動に関する諸々
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; code:

;;; emacsclient
;;; 一度起動したらずっとemacsが残ってくれる
;;; 参考: http://futurismo.biz/archives/1273
;; (require 'server)
;; (unless (server-running-p) (server-start))

;;; esup
;;; 各起動処理の読み込み時間がわかる
;;; 参考: http://emacs.rubikitch.com/esup/
;; (el-get-bundle esup)

;;; garbage collection settings
;;; ガベージコレクタの発動条件のメモリ上限を引き上げて快適にする
;;; 参考: http://nagayasu-shinya.com/emacs-bc-cons-threshold/
(setq gc-cons-threshold (* 128 1024 1024)) ;; かなりデカい値

;;; liblessl
;;; emacs on macがうまくsecurityの認証をできないので
;;; liblesslにCAを認証させる
;;; 参考: https://blog.vifortech.com/posts/emacs-tls-fix/
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")



;;; startup-init.el ends here
