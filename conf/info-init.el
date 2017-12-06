;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 情報を表示
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; print function list
;;; 関数の一覧を表示 (C-c l)
;;; 参考: http://blog.livedoor.jp/hira6/archives/2010-06.html
;;; file: http://www.bookshelf.jp/elc/summarye.el
(autoload 'se/make-summary-buffer "summarye" nil t)
(define-key mode-specific-map "l" 'se/make-summary-buffer)

;;; pos-tip
;;; tipを表示できる (auto-complete, flycheckで使用)
;;; 参考 : http://d.hatena.ne.jp/m2ym/20100320/1269073216
;; (el-get-bundle pos-tip)
;; (require 'pos-tip)

;;; undo tree
;;; 参考 : http://qiita.com/takc923/items/c3d64b55fc4f3a3b0838
;;; C-x u でundo履歴がツリー形式で見れる
;;; M-/ でRedoできる
;; (el-get-bundle undo-tree)
;; ;; (require 'undo-tree)
;; (global-undo-tree-mode t)
;; (global-set-key (kbd "M-/") 'undo-tree-redo)

;;; 最近使ったファイルを表示
;;; 参考 : http://ubulog.blogspot.jp/2007/06/emacs.html
;;; http://syohex.hatenablog.com/entry/20121207/1354885367
;;(require 'recentf)
;;(setq recentf-auto-cleanup 'never) ;;tramp対策。
;;(recentf-mode 1)
;;(global-set-key "\C-xf" 'recentf-open-files) ;;履歴一覧を開く
(global-set-key "\C-xf" 'helm-recentf)

;;; キルリングの表示
;;; 参考 : http://syohex.hatenablog.com/entry/20121207/1354885367
(global-set-key "\M-y" 'helm-show-kill-ring)

;;; ミニマップ
;;; 参考 : http://qiita.com/betweens/items/ae85bd9bcf8ae48ea7e2
;;; http://yusuke-ujitoko.hatenablog.com/entry/2016/05/29/174259
;;(el-get-bundle minimap)
;; (require 'minimap)

;;; Warning: 'mapcar' called for effect; を防ぐ
;;; 参考: http://d.hatena.ne.jp/kitokitoki/20100425/p1
(setq byte-compile-warnings '(free-vars unresolved callargs redefine obsolete noruntime cl-functions interactive-only make-local))

;;; info-init.el ends here
