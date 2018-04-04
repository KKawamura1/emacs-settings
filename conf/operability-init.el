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
;;;        https://gist.github.com/dcalacci/34dee28d39130070ff67
;; for GUI
(cond (window-system
       (custom-set-variables '(x-select-enable-clipboard t))
       ))
;; for CUI
(defun my-cut-function (text &optional rest)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(defun my-paste-function ()
  "Handle copy/paste intelligently on osx. https://gist.github.com/dcalacci/34dee28d39130070ff67"
  (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
    (if (and (eq system-type 'darwin)
             (file-exists-p pbpaste))
        (let ((tramp-mode nil)
              (default-directory "~"))
          (shell-command-to-string pbpaste)))))
(when (eq system-type 'darwin)
  (custom-set-variables
   '(interprogram-cut-function 'my-cut-function)
   '(interprogram-paste-function 'my-paste-function)
   )
  )

;; 改行をnewline-and-indentにする
;; python以外ではsmart-newlineに上書きしてもらう
(bind-key "<return>" 'newline-and-indent)
(bind-key "C-m" 'newline-and-indent)

;;; python shellをreopenする
;; 動作が重くなったときにひょいっと実行する
(defun reopen-python-shell ()
  "Kill and reopen existing inferior python shell."
  (interactive)
  (let (
	(python-buffer-name "*Python*")
	(python-old-buffer-name "*Python-old*")
	)
    (when (get-buffer-process python-buffer-name)
      (let* (
	     ;; (existing-python-buffer (get-buffer-process python-buffer-name))
	     (target-python-window (get-buffer-window python-buffer-name))
	     (python-old-process (get-buffer-process python-buffer-name))
	     (python-actual-old-buffer-name
	      (with-current-buffer python-buffer-name
		(rename-buffer python-old-buffer-name t)))
	     )
	(set-process-query-on-exit-flag python-old-process nil)
	(run-python (python-shell-parse-command) nil nil)
	(with-selected-window target-python-window
	  (switch-to-buffer python-buffer-name))
	(kill-process python-old-process)
	(kill-buffer python-actual-old-buffer-name)
	(select-window target-python-window)
	)
      )
    )
  )
(bind-key "C-c r" 'reopen-python-shell inferior-python-mode-map)

;;; pythonでインデント
(bind-keys :map python-mode-map
	   ("C-c <right>" . python-indent-shift-right)
	   ("C-c <left>" . python-indent-shift-left))

;;; make
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;; 	    (when (and (boundp 'cmake-ide-dir)
;; 		     (file-exists-p (concat cmake-ide-dir "/Makefile")))
;; 		(print 2)
;; 		(set (make-local-variable 'compile-command)
;; 		     (concat "make -k -C "
;; 			     cmake-ide-dir
;; 			     )))))
(bind-key "C-c C-c" 'cmake-ide-compile)
