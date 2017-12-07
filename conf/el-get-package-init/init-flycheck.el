;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           Flycheck 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 参考
;; https://qiita.com/senda-akiha/items/cddb02cfdbc0c8c7bc2b
;; http://ochiailab.blogspot.jp/2016/03/emacsflycheck.html

(require 'flycheck)
(require 'bind-key)

(global-flycheck-mode)

(bind-key "C-c n" 'flycheck-next-error)
(bind-key "C-c p" 'flycheck-previous-error)
(bind-key "C-c d" 'flycheck-list-errors)

(add-hook 'c++-mode-hook (lambda()
                           (setq flycheck-gcc-language-standard "c++14")
                           (setq flycheck-clang-language-standard "c++14")))
