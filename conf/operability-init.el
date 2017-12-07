;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; カーソル移動などの操作性改善
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;; ace-isearch settings
;; (el-get-bundle ace-isearch)
;; ;; (require 'ace-isearch)
;; (global-ace-isearch-mode +1)
;; (custom-set-variables
;;  '(ace-isearch-input-length 5)
;;  '(ace-isearch-jump-delay 0.5)
;;  '(ace-isearch-function 'avy-goto-char)
;;  '(ace-isearch-use-jump 'printing-char))

(use-package bind-key)
(use-package diminish)

;;; コード折りたたみ機能
;; 参考 : http://ameblo.jp/the-7str-guitarist/entry-11315679803.html
;;        http://yohshiy.blog.fc2.com/blog-entry-264.html
(defun hs-load-init ()
  (hs-minor-mode 1)
  (bind-keys :map hs-minor-mode-map
	     ("C-x /" . hs-toggle-hiding)
	     ("C-x ," . hs-hide-all)
	     ("C-x ." . hs-show-all)
	     )
  ;; diminishする
  ;; 参考
  ;; https://qiita.com/tadsan/items/c859c5c04724cbda75fc
  ;; https://www.emacswiki.org/emacs/DiminishedModes
  (diminish 'hs-minor-mode)
  )
(loop for hook in programing-hooks do
      (add-hook hook 'hs-load-init)
      )

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
       (setq x-select-enable-clipboard t)
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
  (setq interprogram-cut-function 'my-cut-function)
  (setq interprogram-paste-function 'my-paste-function))

;;; key binding
;; C-h をBackspaceにする
;; 必要性を感じなかったのでボツ
;; (global-set-key [?\C-h] 'delete-backward-char)
;; (global-set-key [?\C-?])
;; (define-key key-translation-map [?\C-h] [?\C-?])

;;; operability-init.el ends here
