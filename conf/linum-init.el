;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 行番号を表示
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-linum-mode t)
(setq linum-format "%4d ")


;; ;;; 行番号の表示とハイライト
;; ;;; 参考 : http://d.hatena.ne.jp/tm_tn/20110605/1307238416
;; ;;; http://lisphack.blog137.fc2.com/blog-entry-14.html

;; ;; 行番号を表示
;; ;;(el-get-bundle linum) ;; なぜかうまくいかない
;;  ;; (require 'linum)
;; (global-linum-mode t)
;; ;; たくさんの行がある時でもlinumの動作を速くする
;; ;; 参考: http://d.hatena.ne.jp/daimatz/20120215/1329248780
;; (setq linum-delay t)
;; (defadvice linum-schedule (around my-linum-schedule () activate)
;;   (run-with-idle-timer 0.2 nil #'linum-update-current))
;; ;; linumのフォーマットを変更する
;; ;; 参考: http://mgi.hatenablog.com/entry/2013/10/05/095303
;; (setq linum-format "%5d ")

;; ;; hlinumで行番号のカーソル行がある場所に色付け
;; ;;(el-get-bundle elpa:hlinum-mode)
;; ;;(require 'hlinum)
;; ;;(hlinum-activate)
