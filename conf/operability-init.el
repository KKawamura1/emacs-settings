;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; カーソル移動などの操作性改善
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package bind-key)
(use-package diminish)

;;; 任意行ジャンプ
;;; 参考 : http://qiita.com/aita/items/d38ca96d7230d80c5e49
(bind-key "C-x l" 'goto-line)

;;; yes or no を y n で回答可にする
;;; 参考 : http://yusuke-ujitoko.hatenablog.com/entry/2016/05/29/174259
(fset 'yes-or-no-p 'y-or-n-p)

;;; Clipboardを他のアプリケーションと共通にする
;;; 参考 : http://yusuke-ujitoko.hatenablog.com/entry/2016/05/29/174259
;;;        http://d.hatena.ne.jp/l1o0/20100429/1272557315
;;;        https://coderwall.com/p/posneq/cooperation-with-the-clipboard-and-emacs-window-no-window
;; for GUI
(cond (window-system
       (set-variable 'x-select-enable-clipboard t)
       ))
;; for CUI
(defun my-cut-function (text &optional rest)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(defun my-paste-function ()
  (shell-command-to-string "pbpaste"))
(when (eq system-type 'darwin)
  (set-variable 'interprogram-cut-function 'my-cut-function)
  (set-variable 'interprogram-paste-function 'my-paste-function))

;; 改行をnewline-and-indentにする
;; python以外ではsmart-newlineに上書きしてもらう
(bind-key "<return>" 'newline-and-indent)
(bind-key "C-m" 'newline-and-indent)

;;; operability-init.el ends here
