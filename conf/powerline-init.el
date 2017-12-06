;;; powerline
;;; 参考 : http://blog.shibayu36.org/entry/2014/02/11/160945
(el-get-bundle powerline)
;; (require 'powerline)
(powerline-default-theme)
;; http://safx-dev.blogspot.jp/2012/08/emacspower-line.html
(defun dither-xpm (color1 color2)
  "Return an XPM dither string representing."
  (format "/* XPM */
static char * dither[] = {
\"12 18 2 1\",
\".	c %s\",
\" 	c %s\",
\"....... . . \",
\".. . . .    \",
\"..... . . . \",
\".... . .    \",
\"....... .   \",
\".. . . .    \",
\"..... . . . \",
\".. . . .    \",
\"....... . . \",
\".. . .      \",
\"..... . . . \",
\".... . .    \",
\"....... .   \",
\".. . . .    \",
\"..... . . . \",
\".. . . .    \",
\"....... . . \",
\".. . .      \"};"  color1 color2))
(defconst color-text "#000000") ;eee
(defconst color1 "#efdfff") ;437
(defconst color2 "#e3d3f0") ;326
(defconst color3 "#d9c9e3") ;214
(defconst color4 "#d0c0da") ;000
(defvar arrow-right-1 (create-image (dither-xpm color1 color2) 'xpm t :ascent 'center))
(defvar arrow-right-2 (create-image (dither-xpm color2 "None") 'xpm t :ascent 'center))
(defvar arrow-left-1  (create-image (dither-xpm color2 color1) 'xpm t :ascent 'center))
(defvar arrow-left-2  (create-image (dither-xpm "None" color2) 'xpm t :ascent 'center))
(setq-default mode-line-format
 (list
        '(:eval (concat (propertize " %b " 'face 'mode-line-color-1)
                        (propertize " " 'display arrow-right-1)))
        '(:eval (concat (propertize " %m " 'face 'mode-line-color-2)
                        (propertize " " 'display arrow-right-2)))
        minor-mode-alist

        ;; Justify right by filling with spaces to right fringe - 16
        ;; (16 should be computed rahter than hardcoded)
        '(:eval (propertize " " 'display '((space :align-to (- right-fringe 17)))))

        '(:eval (concat (propertize " " 'display arrow-left-2)
                        (propertize " %Z%* " 'face 'mode-line-color-2)))
        '(:eval (concat (propertize " " 'display arrow-left-1)
                        (propertize "%4l:%2c  " 'face 'mode-line-color-1)))
))
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
                    :background color3
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :foreground color-text
                    :background color4)
