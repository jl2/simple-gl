# Build sgl-fractal-landscape

sgl-fractal-landscape: manifest.txt *.lisp *.asd
	buildapp --output sgl-fractal-landscape \
             --manifest-file ~/src/lisp/sgl-fractal-landscape/manifest.txt \
             --load-system asdf \
             --load-system sb-posix \
             --load-system alexandria \
             --load-system sgl-fractal-landscape\
             --entry 'sgl-fractal-landscape:main'

test: t/*.lisp *.lisp *.asd
	sbcl --eval "(ql:quickload :sgl-fractal-landscape.test)" \
		 --eval "(setf 5am::*on-error* :debug)" \
		 --eval "(5am:run-all-tests :summary :suite)" \
		 --eval "(quit)"

manifest.txt: *.asd
	sbcl --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load ~/quicklisp/setup.lisp \
         --eval '(ql:quickload :alexandria)' \
		 --eval '(ql:write-asdf-manifest-file "~/src/lisp/sgl-fractal-landscape/manifest.txt")'

clean:
	rm -Rf manifest.txt  *.fasl

.PHONY: clean test
