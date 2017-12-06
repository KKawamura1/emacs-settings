;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           Magit 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 参考
;; http://yamakichi.hatenablog.com/entry/2016/06/29/133246

(require 'magit)

;;; auto-revert-mode
;; git checkoutとかした時にemacsのbufferの内容を強制的に変更するかどうか
;; 参考: https://github.com/magit/magit/issues/1783
;;       https://github.com/magit/magit/issues/1809
(setq magit-auto-revert-mode t)

;;; vc-modeとオサラバ
(setq vc-handled-backends '())
(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))

;;; 便利コマンドの割り当て
;; magit-status
