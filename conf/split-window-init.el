;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ウィンドウをいい感じに分割する
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; window split
;;; 参考: http://blog.shibayu36.org/entry/2012/12/18/161455
;;; Window 分割を画面サイズに従って計算する
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

;;; Window を与えられた比で2分割する
(defun split-window-vertically-ratio (ratio_wins)
  (split-window-vertically (floor (* (window-height) ratio_wins))))

;;; Window 分割・移動を C-o で
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-vertically-ratio 0.7)
    (if (>= (window-body-width) 120)
	(if (>= (window-body-width) 240)
	    (split-window-horizontally-n 2)
	  (split-window-horizontally)))
    )
  (other-window 1))
(global-set-key (kbd "C-o") 'other-window-or-split)
