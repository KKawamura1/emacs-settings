;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       起動時に行う諸々の設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; garbage collection settings
;; ガベージコレクタの発動条件のメモリ上限を引き上げて快適にする
;; 参考: http://nagayasu-shinya.com/emacs-bc-cons-threshold/
(setq gc-cons-threshold (* 128 1024 1024)) ;; かなりデカい値

;;; フォント設定
;; use UTF-8
(coding-system-put 'utf-8 'category 'utf-8)
(set-language-info "Japanese" 'coding-priority(cons 'utf-8(get-language-info "Japanese" 'coding-priority)))
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;;; ウィンドウ設定
;; adjust window size
;; (setq default-frame-alist (append (list
;; 	'(width . 80)
;; 	'(height . 50))
;; 	default-frame-alist))
;; hide toolbar
(tool-bar-mode 0)
;; hide menubar
(menu-bar-mode 0)
;; show line number in mode line
(line-number-mode t)
;; show column number in mode line
(column-number-mode t)
;; inhibit startup message
;; (setq inhibit-startup-message t)

;;; バックアップ
;; 自動作成されるバックアップファイルはこのフォルダに作る
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;;; コードスタイル
;; 保存時，文末の空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Warning: 'mapcar' called for effect; を防ぐ
;; 参考: http://d.hatena.ne.jp/kitokitoki/20100425/p1
(setq byte-compile-warnings '(free-vars unresolved callargs redefine obsolete noruntime cl-functions interactive-only make-local))

;;; 拡張子設定
;; 参考: http://qiita.com/tadsan/items/a21c268021b46b8a6b33
(add-to-list 'auto-mode-alist '("\\.zshrc.[^.]*\\'" . sh-mode))

;;; recentf起動
(recentf-mode 1)
