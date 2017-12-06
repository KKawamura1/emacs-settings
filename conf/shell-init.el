;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shell 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;use bash
;(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")
(setq shell-command-switch "-c")

;;;hide inputting password
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;;;handle escape sequences
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
          "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;use UTF-8
(add-hook 'shell-mode-hook(lambda()(set-buffer-process-coding-system 'utf-8 'utf-8)))
