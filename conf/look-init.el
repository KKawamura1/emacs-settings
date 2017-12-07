;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 見た目の変更
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; smooth-scroll
;; C-v M-v による移動がなめらかになる
;; 参考 : http://qiita.com/ShingoFukuyama/items/429199542c38625c5554
;; (el-get-bundle smooth-scroll)
;; (require 'smooth-scroll)
;; (smooth-scroll-mode t)

;;; enable font-lock
;; コメントや文字列などに色をつける
(when(fboundp 'global-font-lock-mode)(global-font-lock-mode t))
(setq font-lock-maximum-decoration t)

;;; highlight selected region
;; 選択中の範囲に色をつける
(transient-mark-mode t)

;;; 括弧の対応表示
;; 参考: http://keisanbutsuriya.hateblo.jp/entry/2015/02/01/162035
(show-paren-mode t)
;; 対応する括弧が画面外にある時だけ括弧の中身を全てハイライト表示
(setq show-paren-style 'mixed)

;;; 色の設定
;; - カラーテーマ
(load-theme 'misterioso t)
;; - linum
;;(custom-set-faces
;; '(linum ((t (:inherit (shadow default) :background "#333333")))))
;; - hlinum
(custom-set-faces
 '(linum-highlight-face ((t (:foreground "#0d0d0d"
			     :background "#909090")))))

;; highlight cursor line
;; カーソルのある行に色をつける
;; 参考: http://keisanbutsuriya.hateblo.jp/entry/2015/02/01/162035
;; (custom-set-faces
;;  '(hl-line ((t (:background "color-236"))))
;;  )
;; (global-hl-line-mode)
