;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         Powerline 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 参考
;; http://blog.shibayu36.org/entry/2014/02/11/160945
;; https://gist.github.com/safx/3366866

;;; 文字化けする場合
;; mac iterm2で矢印が ? になる場合の解法
;; https://joppot.info/2017/04/17/3824

(require 'powerline)

;; http://safx-dev.blogspot.jp/2012/08/emacspower-line.html
;; どうもうまくいかないようだ
(defun my-theme ()
  "my powerline theme"
  (interactive)
  (defconst color-text "#000000")
  (defconst color1 "#efdfff")
  (defconst color2 "#e3d3f0")
  (defconst color-active "#ffffd0")
  (defconst color-inactive "#90809a")
  (setq-default mode-line-format
		'(
		  (:propertize " %b " face mode-line-color-1) ; file name
		  (:propertize " %m " face mode-line-color-2) ; major mode
		  minor-mode-alist			    ; minor mode
		  ;; Justify right by filling with spaces to right fringe - 16
		  ;; (16 (setq )hould be computed rahter than hardcoded)
		  (:propertize " " display ((space :align-to (- right-fringe 17))))
		  (:propertize " %Z%* " face mode-line-color-2)
		  (:propertize " %4l:%2c  " face mode-line-color-1)
		  )
		)
  (make-face 'mode-line-color-1)
  (set-face-attribute 'mode-line-color-1 nil
		      :foreground color-text
		      :background color1)
  (make-face 'mode-line-color-2)
  (set-face-attribute 'mode-line-color-2 nil
		      :foreground color-text
		      :background color2)
  (set-face-attribute 'mode-line nil
		      :foreground color-text
		      :background color-active
		      :box nil)
  (set-face-attribute 'mode-line-inactive nil
		      :foreground color-text
		      :background color-inactive)
  )
;;(my-theme)
(powerline-default-theme)


;;; 矢印の境界をきれいにする
;; 参考: http://ytsk.hatenablog.com/entry/2015/09/23/021856
;;(setq ns-use-srgb-colorspace nil)
