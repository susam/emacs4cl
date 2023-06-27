(set-frame-size (selected-frame) 120 32)
(find-file "example.lisp")
(split-window-right)
(slime)
(other-window 1)
(sleep-for 1.0)
(slime-eval-buffer)
(other-window 1)
(sleep-for 0.2)
(execute-kbd-macro (kbd "(hello-world) RET"))
(sleep-for 0.2)
(execute-kbd-macro (kbd "(factorial SPC 6) RET"))
(sleep-for 0.2)
(execute-kbd-macro (kbd "(trace SPC factorial) RET"))
(sleep-for 0.2)
(execute-kbd-macro (kbd "(factorial SPC 6) RET"))
(sleep-for 0.2)
(execute-kbd-macro (kbd "(fibonacci SPC 7) RET"))
