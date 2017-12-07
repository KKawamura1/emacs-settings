;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Smartparens 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smartparens)
(require 'smartparens-config)

;;; 余計な機能を削除
;; 参考: https://qiita.com/ShingoFukuyama/items/ed1af137a98e0028e025
(ad-disable-advice 'delete-backward-char 'before 'sp-delete-pair-advice)
(ad-activate 'delete-backward-char)

(smartparens-global-mode)
