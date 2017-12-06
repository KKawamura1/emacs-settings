;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; オートコンプリート
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; auto-complete settings
;;; オートコンプリート
;;; 参考: http://keisanbutsuriya.hateblo.jp/entry/2015/02/08/175005
;;(el-get-bundle auto-complete)
;;(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'text-mode)         ;;; enable even in text-mode
(add-to-list 'ac-modes 'fundamental-mode)  ;;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(delete 'ac-modes 'python-mode) ;; disable in python-mode
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)       ;;; select with C-n/C-p
(setq ac-use-fuzzy t)          ;;; fuzzy match
(custom-set-variables '(ac-ignore-case nil))
