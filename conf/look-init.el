;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 見た目の変更
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; enable font-lock
;; コメントや文字列などに色をつける
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))
(set-variable 'font-lock-maximum-decoration t)

;;; highlight selected region
;; 選択中の範囲に色をつける
(transient-mark-mode t)

;;; 括弧の対応表示
;; 参考: http://keisanbutsuriya.hateblo.jp/entry/2015/02/01/162035
(show-paren-mode t)
;; 対応する括弧が画面外にある時だけ括弧の中身を全てハイライト表示
(set-variable 'show-paren-style 'mixed)

;;; 色の設定
;; 参考
;; http://yuelab82.hatenablog.com/entry/terminal_now
;; - カラーテーマ
(load-theme 'misterioso t)
