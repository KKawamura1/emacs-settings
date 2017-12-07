;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Jedi 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 参考
;; http://tkf.github.io/emacs-jedi/latest/
;; https://www.hiroom2.com/2015/09/20/emacs%E3%81%AEjedi%E3%83%91%E3%83%83%E3%82%B1%E3%83%BC%E3%82%B8%E3%81%AE%E4%BD%BF%E3%81%84%E6%96%B9/

(require 'epc)
(require 'jedi)

(defun set-pyenv-version-path ()
  "Automatically activates pyenv version if .python-version file exists."
  (f-traverse-upwards
   (lambda (path)
     (let ((pyenv-version-path (f-expand ".python-version" path)))
       (if (f-exists? pyenv-version-path)
           (pyenv-mode-set (s-trim (f-read-text pyenv-version-path 'utf-8))))))))

(add-hook 'python-mode-hook
          '(lambda()
             (jedi:setup)
             (setq jedi:complete-on-dot t)
             (local-set-key (kbd "M-TAB") 'jedi:complete)
	     (add-hook 'find-file-hook 'set-pyenv-version-path)
	     (add-to-list 'exec-path "~/.pyenv/shims")))

;;; うまくいかないとき
;; https://github.com/tkf/emacs-jedi/issues/292
