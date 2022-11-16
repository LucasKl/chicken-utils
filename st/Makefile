st: st.scm
	mkdir -p bin
	csc -O3 st.scm -o bin/st

install: st
	cp st /usr/bin/

clean:
	rm -rf bin
