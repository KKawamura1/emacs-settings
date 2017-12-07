;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           Session 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 参考
;; http://d.hatena.ne.jp/whitypig/20110331/1301521329

(require 'session)

(setq session-save-file-coding-system 'utf-8-unix)
(setq session-save-file (expand-file-name "~/.session/.session.ntemacs"))
(setq session-initialize '(session places))
(setq session-globals-max-size 1024)
(setq session-globals-max-string (* 1024 1024))
(setq session-globals-include '((kill-ring 512)
				(session-file-alist 512)
				(file-name-history 512)
				;; TODO make it be able to use shell-command-history
				;; keyword: comint-input-ring
				;; (shell-command-history 512)
				(tags-table-set-list 128)))
(add-hook 'after-init-hook 'session-initialize)
;; Save session info every 15 minutes
(setq my-timer-for-session-save-session (run-at-time t (* 15 60) 'session-save-session))
