debug: st.scm
	mkdir -p ../bin
	csc -O0 -d3 -debug-info st.scm -o ../bin/st

production: st.scm
	mkdir -p ../bin
	csc -O5 -optimize-leaf-routines st.scm -o ../bin/st

install: production
	cp ../bin/st /usr/bin/

clean:
	rm -rf bin
