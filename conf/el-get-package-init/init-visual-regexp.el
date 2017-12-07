;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        Visual regexp 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 参考
;; http://emacs.rubikitch.com/visual-regexp/

(require 'visual-regexp)
(require 'bind-key)

(bind-key "M-%" 'vr/query-replace)
(bind-key "C-M-r" 'vr/isearch-backward)
(bind-key "C-M-s" 'vr/isearch-forward)
