;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Undo-tree 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'undo-tree)
(require 'bind-key)

(global-undo-tree-mode t)
(bind-key "M-/" 'undo-tree-redo)
