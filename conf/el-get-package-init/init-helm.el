;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Helm 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 参考
;; https://qiita.com/jabberwocky0139/items/86df1d3108e147c69e2c
;; https://github.com/tuhdo/emacs-c-ide-demo/blob/master/custom/setup-helm.el

(require 'helm)
(require 'helm-config)
(require 'bind-key)

;;; プレフィクス
;; "C-x c" は "C-x C-c" (Emacs終了) と近いので，"C-c h" にする
;; Note: We must set "C-c h" globally, because we cannot
;; change 'helm-command-prefix-key' once 'helm-config' is loaded.
(bind-key "C-c h" 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;;; helm bufferにおける選択対象へのアクション
;; C-SPCで選んだ対象へのアクションを，TABで選ぶ
;; TAB と C-z を入れ替える (TABの方が使い勝手がいいので)
(bind-key "<tab>" 'helm-execute-persistent-action helm-map)
(bind-key "C-i" 'helm-execute-persistent-action helm-map) ; terminalではC-iとTABは同じなのでこれも設定する
(bind-key "C-z" 'helm-select-action helm-map) ; アクションを選び，helm bufferを閉じる (あまり使わない)

;;; helmでググる
;; C-c h g でグーグル検索できる
;; そのときにできればcurlを使う
(setq browse-url-generic-program "google-chrome")
(bind-key "C-c h g" 'helm-google-suggest)
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

;;; helm bufferを現在のwindowで開き，minibufferも現在のウインドウのminibufferにする
;; windowが上の方にある場合でも，一番下を見る必要がなくなる
;; 参考: https://www.reddit.com/r/emacs/comments/3asbyn/new_and_very_useful_helm_feature_enter_search/
;; helm中に一番下のminibufferを隠す
(defun helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
;; と思ったけど別に隠さなくてもよくない?w
;; (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
;; windowが複数あるときに，current window内にhelmを開く
(setq helm-split-window-in-side-p t)
;; window内のminibufferに，一番下のminibufferの内容をコピー
(setq helm-echo-input-in-header-line t)

;;; 諸々設定
;; 上下をつなげる
;; helm-find-fileなどで次セクションに行けないのでやめる
;; (setq helm-move-to-line-cycle-in-source t)
;; M-<next>で何行動くかを指定する
(setq helm-scroll-amount 8)
;; よくわからんものたち
(setq helm-ff-search-library-in-sexp t
      helm-ff-file-name-history-use-recentf t)

;;; Autoresize
;; Helmバッファのサイズを候補の数に応じて自動的に変更する
;; 動的に変更されるわけじゃないので要らない
;; (setq helm-autoresize-max-height 0)
;; (setq helm-autoresize-min-height 20)
;; (helm-autoresize-mode 1)

;;; helmシリーズを使うようにキーバインドを変更
;; M-x (関数の実行)
(bind-key "M-x" 'helm-M-x)
(setq helm-M-x-fuzzy-match t)
;; M-y (キルリングの表示)
(bind-key "M-y" 'helm-show-kill-ring)
;; C-x b (バッファやその他リソースの表示)
(bind-key "C-x b" 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
;; C-x C-f (ファイルを開く)
;; C-c h で過去のファイル履歴を閲覧できる
(bind-key "C-x C-f" 'helm-find-files)
;; <prefix>-i (定義とかを検索してくれる)
;; 参考: https://qiita.com/jabberwocky0139/items/86df1d3108e147c69e2c#%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89-helm-semantic-or-imenu
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
;; <prefix>-m (manを表示)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)



(helm-mode 1)
