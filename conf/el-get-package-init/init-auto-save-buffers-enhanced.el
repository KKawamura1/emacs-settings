;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Auto save buffers enhanced 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 参考
;; http://d.hatena.ne.jp/whitypig/20110331/1301521329
;; http://emacs.rubikitch.com/auto-save-buffers-enhanced/

;; remove trailing whitespace との相性が当然悪いので，使用中止

;; (require 'auto-save-buffers-enhanced)
;; (require 'bind-key)

;; (auto-save-buffers-enhanced t)
;; (setq auto-save-buffers-enhanced-interval 1)

;; ;;; wrote のメッセージを抑制
;; (setq auto-save-buffers-enhanced-quiet-save-p t)

;; ;;; C-x a sでauto-save-buffers-enhancedの有効・無効をトグル
;; (bind-key "C-x a s" 'auto-save-buffers-enhanced-toggle-activity)
