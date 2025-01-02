# Build sgl-vector-mandel

sgl-vector-mandel: manifest.txt *.lisp *.asd
	buildapp --output sgl-vector-mandel \
             --manifest-file ~/lisp/sgl-vector-mandel/manifest.txt \
             --load-system asdf \
             --load-system sb-posix \
             --load-system alexandria \
             --load-system sgl-vector-mandel\
             --entry 'sgl-vector-mandel:main'

test: t/*.lisp *.lisp *.asd
	sbcl --eval "(ql:quickload :sgl-vector-mandel.test)" \
		 --eval "(setf 5am::*on-error* :debug)" \
		 --eval "(5am:run-all-tests :summary :suite)" \
		 --eval "(quit)"

manifest.txt: *.asd
	sbcl --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load ~/quicklisp/setup.lisp \
         --eval '(ql:quickload :alexandria)' \
		 --eval '(ql:write-asdf-manifest-file "~/lisp/sgl-vector-mandel/manifest.txt")'

clean:
	rm -Rf manifest.txt  *.fasl

.PHONY: clean test
