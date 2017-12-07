;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        Expand-region 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'expand-region)
(require 'bind-key)

(bind-key "M-@" 'er/expand-region)
(transient-mark-mode t)
