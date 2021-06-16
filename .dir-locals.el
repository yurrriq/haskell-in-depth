;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode
   . ((eval
        . (progn
            (lsp-workspace-folders-add default-directory)
            (interactive-haskell-mode))))))
