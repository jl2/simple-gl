sgla-automata: manifest.txt *.lisp *.asd
	buildapp --output gltest \
             --manifest-file ~/src/lisp/simple-gl/plugins/sgl-automata/manifest.txt \
             --load-system asdf \
             --load-system sb-posix \
             --load-system alexandria \
             --load-system sgl-automata \
             --entry 'sgla:main'

manifest.txt: *.asd
	sbcl --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load ~/quicklisp/setup.lisp \
         --eval '(ql:quickload :alexandria)' \
		 --eval '(ql:write-asdf-manifest-file "~/src/lisp/simple-gl/plugins/sgl-automata/manifest.txt")'

clean:
	rm -Rf manifest.txt  *.fasl

.PHONY: clean
