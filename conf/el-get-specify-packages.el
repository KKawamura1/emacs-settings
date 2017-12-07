;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          パッケージ設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; sourceから読むパッケージ
(defvar my-packages-fromsource
      '(
	))

;;; nameだけで読めるパッケージ
(defvar my-packages-fromname
      '(bind-key			; key bindを簡単に指定
	helm				; 統一インターフェース
	magit				; git管理 on emacs
	powerline			; 下にいろいろ出す
	undo-tree			; undo-redoを木で操作
	expand-region			; ボタン一つで選択範囲を操作
	smartparens			; カッコいいカッコ
	session				; 途中で死んでも履歴が残る
	;; auto-save-buffers-enhanced	; 途中で死んでもバッファが残る
	visual-regexp			; 正規表現が見やすい
	visual-regexp-steroids		; 正規表現置換が見やすい
	volatile-highlights		; 直前の変化を見やすくする
	pos-tip				; ポップアップを出す
	cc-mode				; CやC++のメジャーモード
	flycheck			; 文法チェックしてくれる
	;; flycheck-pos-tip		; ツールチップで文法エラーを教えてくれる
	popwin				; ちょいちょい出て来るウィンドウを賢くする
	fuzzy				; auto-completeの依存パッケージ
	;; auto-complete			; かしこい自動補完
	;; ;; jedi				; pythonの補完拡張
	;; pyenv-mode			; elpy 依存パッケージ
	;; elpy				; pythonのIDE
	;; py-autopep8
	;; yatex
	;; which-key
	;; yasnippet
	))

;;; 最終的にinstallするパッケージ
(defvar my-packages
      (append
       (mapcar 'el-get-source-name my-packages-fromsource)
       my-packages-fromname
       ))

;;; package sequential-command minibuf-isearch auto-complete-c-headers px
