;; set packages
;; (setq my-packages
;;       '('(el-get-source-name :name bind-key :type elpa)			; key bindを簡単に指定
;; 	helm				; 統一インターフェース
;; 	magit				; git管理 on emacs
;; 	powerline
;; 	undo-tree
;; 	expand-region
;; 	smartparens
;; 	session
;; 	visual-regexp
;; 	visual-regexp-steroids
;; 	volatile-highlights
;; 	pos-tip
;; 	cc-mode
;; 	flycheck
;; 	popwin
;; 	epc
;; 	jedi
;; 	py-autopep8
;; 	flymake
;; 	yatex
;; 	auto-complete
;; 	which-key
;; 	yasnippet))

(setq my-packages-fromsource
      '())

(setq my-packages-fromname
      '(bind-key			; key bindを簡単に指定
	helm				; 統一インターフェース
	magit				; git管理 on emacs
	powerline
	undo-tree
	expand-region
	smartparens
	session
	visual-regexp
	visual-regexp-steroids
	volatile-highlights
	pos-tip
	cc-mode
	flycheck
	popwin
	epc
	jedi
	py-autopep8
	flymake
	yatex
	auto-complete
	which-key
	yasnippet))

(setq my-packages
      (append
       my-packages-fromname
       (mapcar 'el-get-source-name my-packages-fromsource)))

;;; package sequential-command minibuf-isearch auto-complete-c-headers px
