loc:
	f() { grep -vE '^$$|^;|^ *".*"$$' ../.emacs; }; f; echo ';;;;'; f | wc -l

demo:
	mv ../.git ../git
	emacs -q -l ../.emacs -l demo.el
	mv ../git ../.git
