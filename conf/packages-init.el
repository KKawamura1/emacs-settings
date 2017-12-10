;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          パッケージ設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; use-packageを用いて設定
;; 参考
;; https://www-he.scphys.kyoto-u.ac.jp/member/shotakaha/dokuwiki/doku.php?id=toolbox:emacs:use-package:start
;; https://github.com/jwiegley/use-package


;;; package.el, use-package.el の自動インストール
;; 参考
;; http://www.wagavulin.jp/entry/2016/07/04/211631
(require 'package)
(set-variable 'package-archives '(
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ;; ("melpa-milkbox" . "http://melpa.milkbox.net/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("elpy" . "https://jorgenschaefer.github.io/packages/")
			 ))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;;; use-packageを使う
(eval-when-compile
  (require 'use-package)
  (set-variable 'use-package-always-ensure t)
  )

;;; ====== use-package に必要なもの ======
;;; bind-key
(use-package bind-key)

;;; diminish
(use-package diminish)

;;; ====== 基本常時使用するもの ======
;;; Helm
;; 参考
;; https://github.com/emacs-helm/helm/issues/1025
(use-package helm
  :diminish helm-mode
  :bind-keymap
  ;; プレフィクス
  ;; "C-x c" は "C-x C-c" (Emacs終了) と近いので，"C-c h" にする
  ;; Note: We must set "C-c h" globally, because we cannot
  ;; change 'helm-command-prefix-key' once 'helm-config' is loaded.
  ("C-c h" . helm-command-prefix)
  :bind (
	 ;; helmシリーズを使うようにキーバインドを変更
	 ("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files)
	 ;; ("C-x f" . helm-resentf)	; 最近使ったファイル (C-x b でも出る)
	 ("C-c d" . helm-imenu)		; 関数の定義一覧
	 :map helm-map
	 ;; helm bufferにおける選択対象へのアクション
	 ;; C-SPCで選んだ対象へのアクションを，TABで選ぶ
	 ;; TAB と C-z を入れ替える (TABの方が使い勝手がいいので)
	 ("[tab]" . helm-execute-persistent-action)
	 ("C-i" . helm-execute-persistent-action) ; terminalではC-iとTABは同じなのでこれも設定する
	 ("C-z" . helm-select-action) ; アクションを選び，helm bufferを閉じる (あまり使わない)
	 :map helm-command-map
	 ;; C-c h g でグーグル検索
	 ("g" . helm-google-suggest)
	 )
  :init
  (require 'helm)			; 依存関係があるので遅延ロードにはしない
  :config
  (require 'helm-config)
  (require 'helm-grep)
  ;; helmでググる
  ;; C-c h g でグーグル検索できる
  ;; そのときにできればcurlを使う
  (set-variable 'browse-url-generic-program "google-chrome")
  (when (executable-find "curl")
    (set-variable 'helm-google-suggest-use-curl-p t))
  ;; helm bufferを現在のwindowで開き，minibufferも現在のウインドウのminibufferにする
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
  (set-variable 'helm-split-window-in-side-p t)
  ;; window内のminibufferに，一番下のminibufferの内容をコピー
  (set-variable 'helm-echo-input-in-header-line t)
  ;; header-lineの色を他と合わせる
  ;; 参考
  ;; https://github.com/emacs-helm/helm/issues/1139
  ;; http://kei10in.hatenablog.jp/entry/20101101/1288617632
  ;; 本当はheader-lineではなくhelm-headerを変えたいが，
  ;; どうもhelm-headerは読んでいない模様
  (set-face-attribute 'header-line nil
		      :foreground "#e1e1e0"
		      :background "#3a3a3a"
		      :underline nil
		      :box nil
		      :inherit nil)

  ;; 諸々設定
  ;; 上下をつなげる
  ;; helm-find-fileなどで次セクションに行けないのでやめる
  ;; (set-variable 'helm-move-to-line-cycle-in-source t)
  ;; M-<next>で何行動くかを指定する
  (set-variable 'helm-scroll-amount 8)
  ;; よくわからんものたち
  (set-variable 'helm-ff-search-library-in-sexp t)
  (set-variable 'helm-ff-file-name-history-use-recentf t)

  ;; Autoresize
  ;; Helmバッファのサイズを候補の数に応じて自動的に変更する
  ;; 動的に変更されるわけじゃないので要らない
  ;; (set-variable 'helm-autoresize-max-height 0)
  ;; (set-variable 'helm-autoresize-min-height 20)
  ;; (helm-autoresize-mode 1)

  (set-variable 'helm-M-x-fuzzy-match t)
  (set-variable 'helm-buffers-fuzzy-matching t)
  (set-variable 'helm-recentf-fuzzy-match t)
  (set-variable 'helm-semantic-fuzzy-match t)
  (set-variable 'helm-imenu-fuzzy-match t)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (helm-mode 1)
  )

;;; undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :bind (
	 ("M-/" . undo-tree-redo)
	 )
  :config
  (global-undo-tree-mode t)
  )

;;; expand-region
(use-package expand-region
  :diminish transient-mark-mode
  :bind (
	 ("M-@" . er/expand-region)
	 )
  :config
  (transient-mark-mode t)
  )

;;; smartparens
(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  ;; 余計な機能を削除
  ;; 参考
  ;; https://qiita.com/ShingoFukuyama/items/ed1af137a98e0028e025
  ;; http://kawamuray.hatenablog.com/entry/2013/11/03/180543
  ;; (ad-disable-advice 'delete-backward-char 'before 'sp-delete-pair-advice)
  ;; (ad-activate 'delete-backward-char)
  (custom-set-variables '(sp-autoskip-closing-pair 'always))
  (smartparens-global-mode))

;;; auto complete
(use-package auto-complete
  :pin melpa
  :diminish auto-complete-mode
  :config
  ;; 参考
  ;; http://keisanbutsuriya.hateblo.jp/entry/2015/02/08/175005
  (require 'auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
  (add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
  (add-to-list 'ac-modes 'org-mode)
  (add-to-list 'ac-modes 'yatex-mode)
  (ac-set-trigger-key "TAB")
  (set-variable 'ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
  (set-variable 'ac-use-fuzzy t)          ;; 曖昧マッチ
  (custom-set-variables '(ac-ignore-case nil))
  )

;;; smart-newline
(use-package smart-newline
  :diminish smart-newline-mode
  :config
  ;; pythonではインデントがずれるので使用しない
  (loop for hook in programing-hooks-without-python
	do (add-hook hook 'smart-newline-mode))
  )

;;; which-key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1)
  )

;;; sequential-command
;; 参考
;; http://d.hatena.ne.jp/rubikitch/20090219/sequential_command
(use-package sequential-command
  :config
  (require 'sequential-command-config)
  (bind-key "C-a" 'seq-home)
  (bind-key "C-e" 'seq-end)
  )

;;; ====== 特定のモードで使用するもの ======
;;; magit
(use-package magit
  :pin melpa-stable
  :bind (
	 ("C-x m" . magit-status)
	 ("C-c l" . magit-blame)
	 )
  :init
  ;; vc-modeとオサラバ
  (set-variable 'vc-handled-backends '())
  (eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
  :config
  ;;; auto-revert-mode
  ;; git checkoutとかした時にemacsのbufferの内容を強制的に変更するかどうか
  ;; 参考: https://github.com/magit/magit/issues/1783
  ;;       https://github.com/magit/magit/issues/1809
  (set-variable 'magit-auto-revert-mode t)
  )

;;; pyenv-mode
(use-package pyenv-mode
  :pin melpa
  :after elpy
  :config
  (pyenv-mode)
  )

;;; jedi
(use-package jedi)

;;; flycheck
(use-package flycheck
  :after helm
  :bind (
	 ("C-c C-v" . helm-flycheck)
	 )
  :init
  (require 'flycheck)
  :config
  (set-variable 'flycheck-flake8-maximum-line-length 100)
  )

;;; elpy
;; require some pip packages:
;;     flake8 importmagic yapf autopep8 ipdb jedi ipython
;; also you must use pyenv & pyenv-virtualenv.
;; see: https://smythp.com/emacs/python/2016/04/27/pyenv-elpy.html
(use-package f)
(use-package elpy
  :pin elpy
  :after (jedi flycheck smartrep auto-complete f)
  :init
  ;; 参考
  ;; https://org-technology.com/posts/emacs-elpy.html
  ;; f-traverse-upwardsのためにf.elが必要
  (defun ssbb-pyenv-hook ()
    "Automatically activates pyenv version if .python-version file exists."
    (f-traverse-upwards
     (lambda (path)
       (let ((pyenv-version-path (f-expand ".python-version" path)))
	 (if (f-exists? pyenv-version-path)
	     (pyenv-mode-set (s-trim (f-read-text pyenv-version-path 'utf-8))))))))

  (add-hook 'find-file-hook 'ssbb-pyenv-hook)
  :config
  ;; 参考
  ;; https://org-technology.com/posts/emacs-elpy.html
  (elpy-enable)
  (elpy-use-ipython)
  (set-variable 'elpy-rpc-backend "jedi")
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (smartrep-define-key elpy-mode-map "C-c"
		       '(("C-n" . flycheck-next-error)
			 ("C-p" . flycheck-previous-error)))
  ;; python で auto-complete が起動しないようにする
  ;; 参考
  ;; https://github.com/jorgenschaefer/elpy/issues/813
  (set-variable 'ac-modes (delq 'python-mode ac-modes))
  ;; python-highlight-indentationをdisableする
  ;; 参考
  ;; https://github.com/jorgenschaefer/elpy/issues/66#event-48574382
  (set-variable 'elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  ;; enableしたいときはこっち
  ;; 参考
  ;; https://org-technology.com/posts/emacs-elpy.html
  ;; (set-face-background 'highlight-indentation-face "#313131")
  ;; (set-face-background 'highlight-indentation-current-column-face "#777777")
  ;; (add-hook 'elpy-mode-hook 'highlight-indentation-mode)
  ;; (add-hook 'elpy-mode-hook 'highlight-indentation-current-column-mode)
  )

;;; flycheck-pos-tip
(use-package flycheck-popup-tip)
(use-package flycheck-pos-tip
  :after (flycheck flycheck-popup-tip)
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)
  (if (display-graphic-p)
      (flycheck-pos-tip-mode)
    (flycheck-popup-tip-mode))
  )

;;; py-autopep8
(use-package py-autopep8
  ;; 参考
  ;; https://github.com/paetzke/py-autopep8.el
  :config
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  (set-variable 'py-autopep8-options '("--max-line-length=100"))
  )

;;; cc-mode
(use-package cc-mode)

;;; yatex
(use-package yatex
  ;; 参考
  ;; http://hikaru515.hatenablog.com/entry/2015/11/10/000000
  :mode (
	 ("\\.tex\\'" . yatex-mode)
	 ("\\.ltx\\'" . yatex-mode)
	 ("\\.sty\\'" . yatex-mode)
	 )
  :config
  ;; set YaTeX coding system
  (set-variable 'YaTeX-kanji-code 4) ; UTF-8 の設定
  (add-hook 'yatex-mode-hook
	    '(lambda ()
	       (set-variable 'YaTeX-use-AMS-LaTeX t) ; align で数式モードになる
	       (set-variable 'YaTeX-use-hilit19 nil)
	       (set-variable 'YateX-use-font-lock t)
	       (set-variable 'tex-command "latexmk") ; typeset command
	       (set-variable 'dvi2-command "open -a /Applications/Preview.app") ; preview command
	       (set-variable 'tex-pdfview-command "open -a /Applications/Preview.app")))
  )

;;; markdown-mode
;; 参考
;; http://futurismo.biz/archives/2137
(use-package markdown-mode
  :mode (
	 "\\.txt\\'"
	 "\\.text\\'"
	 "\\.md\\'"
	 "\\.markdown\\'"
	 )
  )

;;; ====== 起動時のみ使うもの ======
(use-package init-open-recentf
  :config
  (init-open-recentf)
  )

;;; ====== 見た目に関わるもの ======
;;; powerline
(use-package powerline
  :pin melpa-stable
  :config
  ;; 参考
  ;; http://blog.shibayu36.org/entry/2014/02/11/160945
  ;; https://gist.github.com/safx/3366866

  ;; 文字化けする場合
  ;; mac iterm2で矢印が ? になる場合の解法
  ;; https://joppot.info/2017/04/17/3824

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

  ;; 矢印の境界をきれいにする
  ;; 参考: http://ytsk.hatenablog.com/entry/2015/09/23/021856
  ;;(set-variable 'ns-use-srgb-colorspace nil)
  )

;;; linum mode
;; 参考
;; https://org-technology.com/posts/nlinum-mode.html
;; https://www.emacswiki.org/emacs/LineNumbers
(use-package linum
  :config
  (global-linum-mode t)
  (set-variable 'linum-format "%5d ")
  )
(use-package hlinum
  :after linum
  :config
  (hlinum-activate)
  ;; faceの色を変更
  (set-face-attribute 'linum-highlight-face nil
		      :foreground "#0d0d0d"
		      :background "#909090")
  )

;;; visual-regexp
(use-package visual-regexp
  :bind (
	 ("M-%" . vr/query-replace)
	 ("C-M-r" . vr/isearch-backward)
	 ("C-M-s" . vr/isearch-forward)
	 )
  )
(use-package visual-regexp-steroids
  :after visual-regexp
  :config
  (set-variable 'vr/engine 'python)
  )

;;; smooth-scroll
;; C-v M-v による移動がなめらかになる
;; 参考 : http://qiita.com/ShingoFukuyama/items/429199542c38625c5554
;; 動作がだいぶ遅くなるのでボツ
;; (use-package smooth-scroll
;;   :config
;;   (smooth-scroll-mode t)
;;   )

;;; volatile-highlights
(use-package volatile-highlights
  :config
  ;; 参考
  ;; https://github.com/k-talo/volatile-highlights.el/blob/master/README-ja.org
  (volatile-highlights-mode t)
  :after undo-tree
  ;; undo-treeをサポート
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  )

;;; ====== 特に使用を意識しないもの ======

;;; session
;; desktop.el でよくない?w
;; (use-package session
;;   :config
;;   ;; 参考
;;   ;; http://d.hatena.ne.jp/whitypig/20110331/1301521329
;;   (set-variable 'session-save-file-coding-system 'utf-8-unix)
;;   (set-variable 'session-save-file (expand-file-name "~/.emacs.d/.session/.session.ntemacs"))
;;   (set-variable 'session-initialize '(session places))
;;   (set-variable 'session-globals-max-size 1024)
;;   (set-variable 'session-globals-max-string (* 1024 1024))
;;   (set-variable 'session-globals-include '((kill-ring 512)
;; 				  (session-file-alist 512)
;; 				  (file-name-history 512)
;; 				  ;; TODO make it be able to use shell-command-history
;; 				  ;; keyword: comint-input-ring
;; 				  ;; (shell-command-history 512)
;; 				  (tags-table-set-list 128)))
;;   (add-hook 'after-init-hook 'session-initialize)
;;   ;; Save session info every 15 minutes
;;   (set-variable 'my-timer-for-session-save-session (run-at-time t (* 15 60) 'session-save-session))
;;   )

;;; desktop
;; コピーした内容やカーソル位置などを自動保存してくれる
;; 参考
;; http://lioon.net/emacs-desktop
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/desktop.el
(use-package desktop
  :config
  (desktop-save-mode 1)
  ;; 保存場所を指定
  (let ((desktop-directory (locate-user-emacs-file ".desktop/")))
    (unless (file-directory-p desktop-directory) (mkdir desktop-directory))
    (add-to-list 'desktop-path desktop-directory)
    (set-variable 'desktop-dirname desktop-directory)
  )
  (custom-set-variables
   ;; save時にいちいちaskしない
   '(desktop-save t)
   ;; idleになってからsessionを保存するまでの時間を指定
   '(desktop-auto-save-timeout 10)
   )
)

;;; smartrep
(use-package smartrep
  :config
  (smartrep-define-key global-map "C-x"
    '(
      ("{" . shrink-window-horizontally)  ; -> <-
      ("}" . enlarge-window-horizontally) ; <- ->
      ("@" . balance-windows)		  ; 揃える
      ("+" . enlarge-window)		  ; 縦拡大
      ("-" . shrink-window)		  ; 縦縮小
      )
    )
  )

;;; popwin
(use-package popwin
  :config
  (set-variable 'display-buffer-function 'popwin:display-buffer)
  )

;;; emacsclient
;; 一度起動したらずっとemacsが残ってくれる
;; 参考: http://futurismo.biz/archives/1273
;; (use-package server
;;   :config
;;   (unless (server-running-p) (server-start))
;;   )

;;; libressl
;; emacs on macがうまくsecurityの認証をできないので
;; liblesslにCAを認証させる
;; 参考: https://blog.vifortech.com/posts/emacs-tls-fix/
(use-package gnutls
  :config
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")
  )

;;; ====== 何かあったときだけ使うもの =======
;;; esup
;; 各起動処理の読み込み時間がわかる
;; 参考: http://emacs.rubikitch.com/esup/
(use-package esup)
