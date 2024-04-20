# Build sgl-obj-file

sgl-obj-file: manifest.txt *.lisp *.asd
	buildapp --output sgl-obj-file \
             --manifest-file ~/src/lisp/sgl-obj-file/manifest.txt \
             --load-system asdf \
             --load-system sb-posix \
             --load-system alexandria \
             --load-system sgl-obj-file\
             --entry 'sgl-obj-file:main'

test: t/*.lisp *.lisp *.asd
	sbcl --eval "(ql:quickload :sgl-obj-file.test)" \
		 --eval "(setf 5am::*on-error* :debug)" \
		 --eval "(5am:run-all-tests :summary :suite)" \
		 --eval "(quit)"

manifest.txt: *.asd
	sbcl --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load ~/quicklisp/setup.lisp \
         --eval '(ql:quickload :alexandria)' \
		 --eval '(ql:write-asdf-manifest-file "~/src/lisp/sgl-obj-file/manifest.txt")'

clean:
	rm -Rf manifest.txt  *.fasl

.PHONY: clean test
