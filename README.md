http://www.cl.cam.ac.uk/~pes20/ppcmem/help.html hacked to make building the web interface possible.

To build the web interface:
	export JSLIBDIR=/opt/godi/lib/ocaml/pkg-lib/js_of_ocaml
	export JSBINDIR=/opt/godi/bin
	# on Debian:
	# export JSLIBDIR=/usr/lib/ocaml/js_of_ocaml
	# export JSBINDIR=/usr/bin
	make TARGET=js JSLIBDIR=$JSLIBDIR JSBINDIR=$JSBINDIR depend_js jquery-1.6.1.js js

To start the web interface:
	./pcmem-web
