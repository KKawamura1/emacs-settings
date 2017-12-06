;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ソースコードの書き方を統一
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; delete trailing whitespace
;;; 文末の空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)
