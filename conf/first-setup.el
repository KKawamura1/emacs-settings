;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         初回起動時設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; いろいろな環境設定をファイルに書き出す・ファイルから読み込む

(defun sh-comm-to-str-without-last-return (command)
  "Get the shell-command-to-string result without last return character."
  (replace-regexp-in-string "\n+$" ""
			    (shell-command-to-string command))
  )

(defun write-local-configuration ()
  "Get and write local configuration to the current buffer."
  ;;; example
  ;; (let ((tmp "1")) (insert (concat "(print " tmp ")")))
  ;;; executable places
  ;; (let (
  ;; 	(clang-bin (sh-comm-to-str-without-last-return "which clang"))
  ;; 	(cmake-bin (sh-comm-to-str-without-last-return "which cmake"))
  ;; 	)
  ;;   (insert (concat "(custom-set-variables
  ;;    '(company-clang-executable \"" clang-bin "\")
  ;;    '(flycheck-c/c++-clang-executable \"" clang-bin "\")
  ;;    '(company-cmake-executable \"" cmake-bin "\"))\n")
  ;; 	    )
  ;;   )
  )


(let* (
       (local-conf-file-path (locate-user-emacs-file ".cache/.local-conf.el"))
       (local-conf-file-dir (file-name-directory local-conf-file-path)))
  (unless (file-directory-p local-conf-file-dir)
    (mkdir local-conf-file-dir t))
  (unless (file-exists-p local-conf-file-path)
    (with-temp-file local-conf-file-path
      (write-local-configuration)
      ))
  (load local-conf-file-path)
  )
