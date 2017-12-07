;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        add-hook まとめ設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 複数のadd-hookを引っ掛ける共通hook
;;; 参考: http://fnwiya.hatenablog.com/entry/2015/09/21/013508

;; usage
;; (loop for hook in hogehoge-hooks
;;       do (add-hook hook 'hugahuga-mode))

(require 'cl)
;; プログラミング系 (python を除く)
(defvar programing-hooks-without-python
  '(emacs-lisp-mode-hook
    c-mode-hook
    c++-mode-hook
    js2-mode-hook
    sh-mode-hook))
;; python込み
(defvar programing-hooks
  '(prog-mode-hook
    programing-hooks-without-python
    python-mode-hook))
;; C系
(defvar c-like-hooks
  '(c-mode-hook
    c++-mode-hook))

;; txt以外の大体のmode (python を除く)
(defvar various-hooks-without-python
  '(programing-hooks-without-python
    markdown-mode-hook
    yatex-mode-hook
    ))
;; python 込み
(defvar various-hooks
  '(prog-mode-hook
    various-hooks-without-python
    python-mode-hook))
