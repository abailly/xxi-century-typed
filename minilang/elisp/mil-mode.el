;;; mil-mode.el --- sample major mode for editing Minilang files. -*- coding: utf-8; lexical-binding: t; -*-

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq mil-font-lock-keywords
      (let* (
             ;; define several category of keywords
             (x-keywords '("Sum" "fun" "let" "rec" "def" "->" "λ" "Π" "Σ" "->" "|" ))

             ;; generate regex string for each category of keywords
             (x-keywords-regexp (regexp-opt x-keywords 'words)))

        (append
         `(
           (,x-keywords-regexp . font-lock-keyword-face)
           )
         '(("def \\(\\w+\\) :" . (1 font-lock-function-name-face))
           ("\\(\\$\\w+\\)" . (1 font-lock-constant-face)))
         )))

(setq mil-mode-syntax-table
      (let ( (synTable (make-syntax-table)))
        ;; C++ style comment “// …”
        (modify-syntax-entry ?\{  "(}1nb" synTable)
        (modify-syntax-entry ?\}  "){4nb" synTable)
        (modify-syntax-entry ?-  "_ 123" synTable)
        (modify-syntax-entry ?\n ">" synTable)
      synTable))

;;;###autoload
(define-derived-mode mil-mode c-mode "Minilang mode"
  "Major mode for editing Minilang files"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((mil-font-lock-keywords)))

  ;; activate electric parens mode
  (rainbow-delimiters-mode)
  (set-syntax-table mil-mode-syntax-table)
  )

;; add the mode to the `features' list
(provide 'mil-mode)

;;; mil-mode.el ends here
