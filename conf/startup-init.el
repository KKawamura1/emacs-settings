;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       起動時に行う諸々の設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; garbage collection settings
;; ガベージコレクタの発動条件のメモリ上限を引き上げて快適にする
;; 参考: http://nagayasu-shinya.com/emacs-bc-cons-threshold/
(custom-set-variables '(gc-cons-threshold (* 128 1024 1024))) ;; かなりデカい値

;;; フォント設定
;; use UTF-8
(coding-system-put 'utf-8 'category 'utf-8)
(set-language-info "Japanese" 'coding-priority(cons 'utf-8(get-language-info "Japanese" 'coding-priority)))
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-variable 'buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;;; ウィンドウ設定
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

;;; 自動作成ファイルのpath指定
;; 参考
;; https://masutaka.net/chalow/2014-05-11-1.html
;; http://emacs.rubikitch.com/initsplit/
;; https://emacs.stackexchange.com/questions/7602/what-is-the-point-of-quote-with-single-argument-and-comma-quote-arg
;; https://qiita.com/kawabata@github/items/ac503ea104eac3eea602#custom-set-variables-%E8%A8%AD%E5%AE%9A%E9%83%A8%E5%88%86%E3%81%AE%E5%88%86%E9%9B%A2
(let* (
      (cache-dir (locate-user-emacs-file ".cache/"))
      (backup-dir (concat cache-dir "backup/"))
      (auto-save-dir (concat cache-dir "auto-save/"))
      (auto-save-list-dir (concat cache-dir "auto-save-list/"))
      )
  (unless (file-directory-p cache-dir) (mkdir cache-dir))
  (unless (file-directory-p backup-dir) (mkdir backup-dir))
  (unless (file-directory-p auto-save-dir) (mkdir auto-save-dir))
  (unless (file-directory-p auto-save-list-dir) (mkdir auto-save-list-dir))
  (custom-set-variables
   ;; バックアップファイル
   `(backup-directory-alist '(("." . ,backup-dir)))
   ;; オートセーブファイル
   `(auto-save-file-name-transforms '(("\\([^/]*/\\)*\\([^/]*\\)$" ,auto-save-dir t)))
   ;; セッションファイル
   `(auto-save-list-file-prefix (concat ,auto-save-list-dir "saves-"))
   ;; custom-set-variables ファイル
   '(custom-file (locate-user-emacs-file "conf/custom-file.el"))
   ;; recentf ファイル
   `(recentf-save-file (concat ,cache-dir "recentf"))
   )
  )

;;; コードスタイル
;; 保存時，文末の空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Warning: 'mapcar' called for effect; を防ぐ
;; 参考: http://d.hatena.ne.jp/kitokitoki/20100425/p1
(custom-set-variables
 '(byte-compile-warnings '(free-vars unresolved callargs redefine obsolete noruntime cl-functions interactive-only make-local)))

;;; 拡張子設定
;; 参考: http://qiita.com/tadsan/items/a21c268021b46b8a6b33
(add-to-list 'auto-mode-alist '("\\.zshrc.[^.]*\\'" . sh-mode))

;;; recentf起動
(recentf-mode 1)

;;; CPPFLAGSからinclude pathを(文字列処理で無理やり)取得する
(defvar c-include-paths (list))
(let ((CPPFLAGS (getenv "CPPFLAGS")))
  (if CPPFLAGS
    (let ((cpp-flags (split-string CPPFLAGS)))
      (dolist (cpp-flag cpp-flags)
	(when (eq 0 (string-match "^-I" cpp-flag))
	  (let ((include-path (substring cpp-flag (match-end 0))))
	    (when (file-directory-p include-path)
	      (add-to-list 'c-include-paths include-path))))))))
