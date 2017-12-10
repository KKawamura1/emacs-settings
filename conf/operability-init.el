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

;;; goto-line時に行き先がたたまれていたら展開する
;; 参考
;; https://www.emacswiki.org/emacs/HideShow
(defadvice goto-line (after expand-after-goto-line
			    activate compile)
  "hideshow-expand affected block when using goto-line in a collapsed buffer"
  (save-excursion
    (hs-show-block)))
;;; コメントだけ折りたたむ
;; 参考
;; https://www.emacswiki.org/emacs/HideShow
(defun hs-hide-all-comments ()
  "Hide all top level blocks, if they are comments, displaying only first line.
Move point to the beginning of the line, and run the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (unless hs-allow-nesting
       (hs-discard-overlays (point-min) (point-max)))
     (goto-char (point-min))
     (let ((spew (make-progress-reporter "Hiding all comment blocks..."
                                         (point-min) (point-max)))
           (re (concat "\\(" hs-c-start-regexp "\\)")))
       (while (re-search-forward re (point-max) t)
         (if (match-beginning 1)
	     ;; found a comment, probably
	     (let ((c-reg (hs-inside-comment-p)))
	       (when (and c-reg (car c-reg))
		 (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
		     (hs-hide-block-at-point t c-reg)
		   (goto-char (nth 1 c-reg))))))
         (progress-reporter-update spew (point)))
       (progress-reporter-done spew)))
   (beginning-of-line)
   (run-hooks 'hs-hide-hook)))
;; キーバインド
(defun hs-load-init ()
  (hs-minor-mode 1)
  (bind-keys :map hs-minor-mode-map
	     ("C-t" . hs-toggle-hiding)
	     ("C-x ," . hs-hide-all)
	     ("C-x ." . hs-show-all)
	     ("C-x M-," . hs-hide-all-comments)
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

;;; key binding
;; C-h をBackspaceにする
;; 必要性を感じなかったのでボツ
;; (global-set-key [?\C-h] 'delete-backward-char)
;; (global-set-key [?\C-?])
;; (define-key key-translation-map [?\C-h] [?\C-?])

;;; operability-init.el ends here
