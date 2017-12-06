;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; フォント設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; use UTF-8
(coding-system-put 'utf-8 'category 'utf-8)
(set-language-info "Japanese" 'coding-priority(cons 'utf-8(get-language-info "Japanese" 'coding-priority)))
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
