# Build sgl-ogl-insight-tess

sgl-ogl-insight-tess: manifest.txt *.lisp *.asd
	buildapp --output sgl-ogl-insight-tess \
             --manifest-file ~/src/lisp/sgl-ogl-insight-tess/manifest.txt \
             --load-system asdf \
             --load-system sb-posix \
             --load-system alexandria \
             --load-system sgl-ogl-insight-tess\
             --entry 'sgl-ogl-insight-tess:main'

test: t/*.lisp *.lisp *.asd
	sbcl --eval "(ql:quickload :sgl-ogl-insight-tess.test)" \
		 --eval "(setf 5am::*on-error* :debug)" \
		 --eval "(5am:run-all-tests :summary :suite)" \
		 --eval "(quit)"

manifest.txt: *.asd
	sbcl --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load ~/quicklisp/setup.lisp \
         --eval '(ql:quickload :alexandria)' \
		 --eval '(ql:write-asdf-manifest-file "~/src/lisp/sgl-ogl-insight-tess/manifest.txt")'

clean:
	rm -Rf manifest.txt  *.fasl

.PHONY: clean test
