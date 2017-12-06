;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; latex設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; emacsでtexを書く

;;; YaTeX
;;; 参考: http://hikaru515.hatenablog.com/entry/2015/11/10/000000

(el-get-bundle yatex)

(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;; major-mode setting
(add-to-list 'auto-mode-alist '("\\.tex\\'" . yatex-mode))
(add-to-list 'auto-mode-alist '("\\.ltx\\'" . yatex-mode))
(add-to-list 'auto-mode-alist '("\\.sty\\'" . yatex-mode))
;; set YaTeX coding system
(setq YaTeX-kanji-code 4) ; UTF-8 の設定
(add-hook 'yatex-mode-hook
      '(lambda ()
         (setq YaTeX-use-AMS-LaTeX t) ; align で数式モードになる
         (setq YaTeX-use-hilit19 nil
           YateX-use-font-lock t)
         (setq tex-command "latexmk") ; typeset command
         (setq dvi2-command "open -a /Applications/Preview.app") ; preview command
         (setq tex-pdfview-command "open -a /Applications/Preview.app"))) ; preview command
;;; tex-init.el ends here
