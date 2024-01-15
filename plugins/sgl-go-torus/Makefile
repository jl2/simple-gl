sgl-go-torus: manifest.txt *.lisp *.asd
	buildapp --output sgl-go-torus \
             --manifest-file ~/src/lisp/sgl-go-torus/manifest.txt \
             --load-system asdf \
             --load-system sb-posix \
             --load-system alexandria \
             --load-system sgl-go-torus\
             --entry 'sgl-go-torus:main'

manifest.txt: *.asd
	sbcl --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load ~/quicklisp/setup.lisp \
         --eval '(ql:quickload :alexandria)' \
		 --eval '(ql:write-asdf-manifest-file "~/src/lisp/sgl-go-torus/manifest.txt")'

clean:
	rm -Rf manifest.txt  *.fasl

.PHONY: clean
