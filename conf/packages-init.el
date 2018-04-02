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
(custom-set-variables '(package-archives '(
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ;; ("melpa-milkbox" . "http://melpa.milkbox.net/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("elpy" . "https://jorgenschaefer.github.io/packages/")
			 ))
		      )
(package-initialize)
;; 初回起動時に情報を更新
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;;; use-packageを使う
(eval-when-compile
  (require 'use-package)
  (custom-set-variables '(use-package-always-ensure t)) ; 常に packageがなければinstall
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
  :config
  (require 'helm-config)
  (require 'helm-grep)
  ;; helmでググる
  ;; C-c h g でグーグル検索できる
  ;; そのときにできればcurlを使う
  (custom-set-variables '(browse-url-generic-program "google-chrome"))
  (when (executable-find "curl")
    (custom-set-variables '(helm-google-suggest-use-curl-p t)))
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
  ;; header-lineの色を他と合わせる
  ;; 参考
  ;; https://github.com/emacs-helm/helm/issues/1139
  ;; http://kei10in.hatenablog.jp/entry/20101101/1288617632
  ;; 本当はheader-lineではなくhelm-headerを変えたいが，
  ;; どうもhelm-headerは読んでいない模様
  (custom-set-faces '(header-line nil
				  :foreground "#e1e1e0"
				  :background "#3a3a3a"
				  :underline nil
				  :box nil
				  :inherit nil))
  (custom-set-variables
   ;; windowが複数あるときに，current window内にhelmを開く
   '(helm-split-window-in-side-p t)
   ;; window内のminibufferに，一番下のminibufferの内容をコピー
   '(helm-echo-input-in-header-line t)
   ;; 諸々設定
   ;; 上下をつなげる
   ;; helm-find-fileなどで次セクションに行けないのでやめる
   ;; '(helm-move-to-line-cycle-in-source t)
   ;; M-<next>で何行動くかを指定する
   '(helm-scroll-amount 8)
   ;; よくわからんものたち
   '(helm-ff-search-library-in-sexp t)
   '(helm-ff-file-name-history-use-recentf t)
   '(helm-M-x-fuzzy-match t)
   '(helm-buffers-fuzzy-matching t)
   '(helm-recentf-fuzzy-match t)
   '(helm-semantic-fuzzy-match t)
   '(helm-imenu-fuzzy-match t)
   )
  ;; Autoresize
  ;; Helmバッファのサイズを候補の数に応じて自動的に変更する
  ;; 動的に変更されるわけじゃないので要らない
  ;; (custom-set-variables
  ;;  '(helm-autoresize-max-height 0)
  ;;  '(helm-autoresize-min-height 20))
  ;; (helm-autoresize-mode 1)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (helm-mode 1)
  )

;;; helm-flycheck
(use-package helm-flycheck
  :after (helm flycheck)
  :bind (
	 ("C-c C-v" . helm-flycheck)
	 )
  )

;;; undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :bind (
	 ("M-/" . undo-tree-redo)
	 ("C-x u" . undo-tree-visualize)
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
  (smartparens-global-mode))

;;; auto complete
;; company-mode に乗り換えたのでボツ
(use-package auto-complete
  :disabled t
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
  (custom-set-variables
   '(ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
   '(ac-use-fuzzy t)          ;; 曖昧マッチ
   '(ac-ignore-case nil)
   '(ac-comphist-file (locate-user-emacs-file ".cache/ac-comphist.dat"))
   )
  )

;;; company-mode
;; auto-complete から乗り換えた
(use-package company
  :diminish company-mode
  :init
  (global-company-mode)
  :config
  (custom-set-variables '(company-show-numbers t)
			'(company-auto-complete t)
			'(company-auto-complete-chars nil))
  )

;;; help for company
(use-package company-quickhelp
  :init
  (add-hook 'global-company-mode-hook #'company-quickhelp-mode)
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

;;; ace-isearch
;; かっこいいけどisearchはゆっくりやりたいのでボツ
(use-package ace-isearch
  :disabled t
  :config
  (global-ace-isearch-mode 1)
  (custom-set-variables
   '(ace-isearch-input-length 5)
   '(ace-isearch-jump-delay 0.5)
   '(ace-isearch-function 'avy-goto-char)
   '(ace-isearch-use-jump 'printing-char)
   )
  )

;;; hideshow
;; コード折りたたみ機能
;; 参考 : http://ameblo.jp/the-7str-guitarist/entry-11315679803.html
;;        http://yohshiy.blog.fc2.com/blog-entry-264.html
(use-package hideshow
  :diminish hs-minor-mode
  :init
  (loop for hook in programing-hooks do
	(add-hook hook 'hs-minor-mode)
	)
  :bind (:map hs-minor-mode-map
	      ("C-t" . hs-toggle-hiding)
	      ("C-x ," . hs-hide-all)
	      ("C-x ." . hs-show-all)
	      ("C-x M-," . hs-hide-all-comments)
	      )
  :config
  ;; goto-line時に行き先がたたまれていたら展開する
  ;; 参考
  ;; https://www.emacswiki.org/emacs/HideShow
  (defadvice goto-line (after expand-after-goto-line
			      activate compile)
    "hideshow-expand affected block when using goto-line in a collapsed buffer"
    (save-excursion
      (hs-show-block)))
  ;; コメントだけ折りたたむ
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
  (custom-set-variables '(vc-handled-backends '()))
  (eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
  :config
  ;;; auto-revert-mode
  ;; git checkoutとかした時にemacsのbufferの内容を強制的に変更するかどうか
  ;; 参考: https://github.com/magit/magit/issues/1783
  ;;       https://github.com/magit/magit/issues/1809
  (custom-set-variables '(magit-auto-revert-mode t))
  )

;;; pyenv-mode
(use-package pyenv-mode
  :pin melpa
  :config
  (pyenv-mode)
  )

;;; jedi
(use-package jedi
  :after flycheck
  )

;;; flycheck
(use-package flycheck
  :config
  (global-flycheck-mode)
  (custom-set-variables '(flycheck-flake8-maximum-line-length 100)
			'(flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (when (boundp 'c-include-paths)
    (custom-set-variables '(flycheck-clang-include-path c-include-paths)))
  (add-hook 'c++-mode-hook
	    '(lambda ()
	       (custom-set-variables
		'(flycheck-clang-language-standard
		  (if (boundp 'std-c++-version) std-c++-version "c++14")))))
  )

;;; elpy
;; require some pip packages:
;;     flake8 importmagic yapf autopep8 ipdb jedi ipython
;; also you must use pyenv & pyenv-virtualenv.
;; see: https://smythp.com/emacs/python/2016/04/27/pyenv-elpy.html
(use-package f)
(use-package elpy
  :pin elpy
  :after (jedi flycheck smartrep pyenv-mode f)
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
  ;; tramp時に悪さをするがないよりはあったほうがよい
  (add-hook 'find-file-hook 'ssbb-pyenv-hook)
  :config
  ;; 参考
  ;; https://org-technology.com/posts/emacs-elpy.html
  (elpy-enable)
  ;; (elpy-use-ipython)
  (custom-set-variables '(elpy-rpc-backend "jedi")
			'(python-shell-interpreter "ipython")
			'(python-shell-interpreter-args "-i --simple-prompt"))
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (smartrep-define-key elpy-mode-map "C-c"
		       '(("C-n" . flycheck-next-error)
			 ("C-p" . flycheck-previous-error)))
  ;; python で auto-complete が起動しないようにする
  ;; 参考
  ;; https://github.com/jorgenschaefer/elpy/issues/813
  ;; (custom-set-variables '(ac-modes (delq 'python-mode ac-modes)))
  ;; 逆にelpyの補完を消す
  ;; 参考: http://d.hatena.ne.jp/keita44_f4/20160504
  (auto-complete-mode -1)
  ;; python-highlight-indentationをdisableする
  ;; 参考
  ;; https://github.com/jorgenschaefer/elpy/issues/66#event-48574382
  (custom-set-variables '(elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules)))
  ;; enableしたいときはこっち
  ;; 参考
  ;; https://org-technology.com/posts/emacs-elpy.html
  ;; (set-face-background 'highlight-indentation-face "#313131")
  ;; (set-face-background 'highlight-indentation-current-column-face "#777777")
  ;; (add-hook 'elpy-mode-hook 'highlight-indentation-mode)
  ;; (add-hook 'elpy-mode-hook 'highlight-indentation-current-column-mode)

  ;; ipythonでmoduleを自動的にリロードする
  ;; これがないとeditしてC-cしても変更が適用されない
  ;; 参考
  ;; https://emacs.stackexchange.com/questions/29752/emacs-python-can-not-update-imported-files
  ;; https://ipython.org/ipython-doc/3/config/intro.html
  ;; http://ipython.readthedocs.io/en/stable/config/extensions/autoreload.html?highlight=autoreload
  (let* (
  	 (profile-name "profile_for_emacs")
  	 (folder-name-getter `(substring (shell-command-to-string (concat "ipython locate profile " ,profile-name)) 0 -1))
  	 )
    (unless (file-directory-p (eval folder-name-getter))
      (shell-command (concat "ipython profile create " profile-name))
      (let ((ipython-config-folder (eval folder-name-getter)))
  	(shell-command (concat "echo \"\n\nc.InteractiveShellApp.extensions.append(\\\"autoreload\\\")\nc.InteractiveShellApp.exec_lines.append(\\\"%autoreload 2\\\")\" >> " ipython-config-folder "/ipython_config.py"))
  	)
      )
    (custom-set-variables
     `(python-shell-interpreter-args (concat "--profile " ,profile-name " " python-shell-interpreter-args)))
    )
  ;; args out of range を修正
  ;; 参考
  ;; https://github.com/jorgenschaefer/elpy/issues/992
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  )

;;; flycheck-pos-tip
(use-package flycheck-popup-tip
  :after flycheck)
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
  (custom-set-variables '(py-autopep8-options '("--max-line-length=100")))
  )

;;; markdown-mode
;; 参考
;; http://futurismo.biz/archives/2137
(use-package markdown-mode
  :mode (
	 "\\.md\\'"
	 "\\.markdown\\'"
	 )
  :init
  (custom-set-variables '(markdown-command "markdown"))
  )

;;; cc-mode
(use-package cc-mode
  :mode (
	 ("\\.cpp\\'" . c++-mode)
	 ("\\.cc\\'" . c++-mode)
	 ("\\.hpp\\'" . c++-mode)
	 ("\\.hh\\'" . c++-mode)
	 ("\\.c\\'" . c-mode)
	 ("\\.h\\'" . c++-mode)
	 )
  )

;;; clang-format
(use-package clang-format
  :config
  ;; Hook function
  ;; 参考: http://www.josephlisee.com/2015/02/21/exploring-clang-format/
  (defun clang-format-before-save ()
    "Add this to .emacs to clang-format on save
 (add-hook 'before-save-hook 'clang-format-before-save)."
  (interactive)
  (when (eq major-mode 'c++-mode) (clang-format-buffer)))
  (add-hook 'before-save-hook 'clang-format-before-save)
  )

;; ;;; ac-headers
;; (use-package auto-complete-c-headers
;;   :after auto-complete
;;   :config
;;   (loop for hook in c-like-hooks
;; 	do (add-hook hook
;; 		     '(lambda ()
;; 			(add-to-list 'ac-sources 'ac-source-c-headers)))
;; 	)
;;   )

;;; cmake-mode
(use-package cmake-mode
  :mode (
	 ("CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)
	 )
  )

;;; rtags
;; 参考: https://qiita.com/alpha22jp/items/90f7f2ad4f8b1fa089f4
(use-package rtags
  :after helm
  :config
  (custom-set-variables '(rtags-display-result-backend 'helm))
  (defun rtags-settings ()
    "rtag settings"
    (when (rtags-is-indexed)
      (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
      (local-set-key (kbd "M-C-.") 'rtags-find-symbol)
      (local-set-key (kbd "M-,") 'rtags-location-stack-back)
      (local-set-key (kbd "M-:") 'rtags-find-references)
      )
    )
  (loop for hook in c-like-hooks
      do (add-hook hook 'rtags-settings))
  )

;; ;;; ac-clang-async
;; (use-package auto-complete-clang-async
;;   :after (auto-complete auto-complete-c-headers)
;;   :config
;;   (defun ac-clang-async-setting ()
;;     "Set auto-complete-clang-async."
;;     (custom-set-variables '(ac-clang-complete-executable
;; 			    (locate-user-emacs-file "bin/clang-complete")))
;;     (if (executable-find ac-clang-complete-executable)
;; 	(progn
;; 	  (add-to-list 'ac-sources 'ac-source-clang-async)
;; 	  (ac-clang-launch-completion-process)
;; 	  (custom-set-variables '(ac-clang-cflags '("-std=c++17"))))
;;       (display-warning "UserWarning FileNotFound"
;; 		       (concat "ac-clang-complete-executable was not found!\n"
;; 			       "Please setup your emacs-clang-complete-async.\n"
;; 			       "See: https://github.com/Golevka/emacs-clang-complete-async\n")
;; 		       :warning
;; 		       )))
;;   (loop for hook in c-like-hooks
;; 	do (add-hook hook 'ac-clang-async-setting))
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   )

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
  (custom-set-variables
   '(YaTeX-kanji-code 4) ; UTF-8 の設定
   '(YaTeX-use-AMS-LaTeX t) ; align で数式モードになる
   '(YaTeX-use-hilit19 nil)
   '(YateX-use-font-lock t)
   '(tex-command "latexmk") ; typeset command
   '(dvi2-command "open -a /Applications/Preview.app") ; preview command
   '(tex-pdfview-command "open -a /Applications/Preview.app")
  )
  ;; typeset
  (bind-key "C-c C-c" 'YaTeX-typeset-menu YaTeX-mode-map)
  )

;;; yasnippet
;; 参考
;; http://vdeep.net/emacs-yasnippet
(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "conf/snippets/"))
  (yas-global-mode 1)
  )

;;; gnuplot
(use-package gnuplot
  :mode (
	 ("\\.gp\\'" . gnuplot-mode)
	 ("\\.plt\\'" . gnuplot-mode)
	 )
  :config
  (bind-key "C-c C-c" 'gnuplot-send-buffer-to-gnuplot gnuplot-mode-map)
  )

;;; yaml
(use-package yaml-mode
  :mode (
	 ("\\.ya?ml$" . yaml-mode))
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
  ;; (custom-set-variables '(ns-use-srgb-colorspace nil))
  )

;;; linum mode
;; 参考
;; https://org-technology.com/posts/nlinum-mode.html
;; https://www.emacswiki.org/emacs/LineNumbers
(use-package linum
  :config
  (global-linum-mode t)
  (custom-set-variables '(linum-format "%5d "))
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
  (custom-set-variables '(vr/engine 'python))
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
  (with-eval-after-load 'undo-tree
    ;; undo-treeをサポート
    (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
    (vhl/install-extension 'undo-tree)
    )
  )

;;; ====== 特に使用を意識しないもの ======

;;; session
;; desktop.el でよくない?w
(use-package session
  :disabled t
  :config
  ;; 参考
  ;; http://d.hatena.ne.jp/whitypig/20110331/1301521329
  (custom-set-variables
   '(session-save-file-coding-system 'utf-8-unix)
   '(session-save-file (expand-file-name "~/.emacs.d/.session/.session.ntemacs"))
   '(session-initialize '(session places))
   '(session-globals-max-size 1024)
   '(session-globals-max-string (* 1024 1024))
   '(session-globals-include '((kill-ring 512)
			       (session-file-alist 512)
			       (file-name-history 512)
			       ;; TODO make it be (and )ble to use shell-command-history
			       ;; keyword: comint-input-ring
			       ;; (shell-command-history 512)
			       (tags-table-set-list 128)))
   ;; Save session info every 15 minutes
   '(my-timer-for-session-save-session (run-at-time t (* 15 60) 'session-save-session))
   )
  (add-hook 'after-init-hook 'session-initialize)
  )

;;; desktop
;; コピーした内容やカーソル位置などを自動保存してくれる
;; 参考
;; http://lioon.net/emacs-desktop
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/desktop.el
(use-package desktop
  :config
  (desktop-save-mode 1)
  ;; 保存場所を指定
  (let ((desktop-directory (locate-user-emacs-file ".cache/desktop/")))
    (unless (file-directory-p desktop-directory) (mkdir desktop-directory t))
    (add-to-list 'desktop-path desktop-directory)
    (custom-set-variables `(desktop-dirname ,desktop-directory))
    )
  (custom-set-variables
   ;; save時にいちいちaskしない
   '(desktop-save t)
   ;; idleになってからsessionを保存するまでの時間を指定
   '(desktop-auto-save-timeout 10)
   ;; 復元するバッファ数の指定
   ;; 参考: https://ayatakesi.github.io/emacs/24.5/Saving-Emacs-Sessions.html
   '(desktop-restore-eager 3)
   )
  )

;;; savehist
;; minibufferの履歴を保存してくれる
(use-package savehist
  :config
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'foo)
  (custom-set-variables '(savehist-file (locate-user-emacs-file ".cache/savehist")))
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
  (custom-set-variables '(display-buffer-function 'popwin:display-buffer))
  )

;;; emacsclient
;; 一度起動したらずっとemacsが残ってくれる
;; 参考: http://futurismo.biz/archives/1273
;; 特に使ってなかったので消去
(use-package server
  :disabled t
  :config
  (unless (server-running-p) (server-start))
  )

;;; exec-path-from-shell
;; mac でemacsを使うときのバグを減らす
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

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
(use-package esup
  :disabled t
  )
