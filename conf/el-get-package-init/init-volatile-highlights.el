;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Volatile highlights 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 参考
;; https://github.com/k-talo/volatile-highlights.el/blob/master/README-ja.org

(require 'volatile-highlights)
(volatile-highlights-mode t)

;;; undo-treeをサポート
(vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
(vhl/install-extension 'undo-tree)
