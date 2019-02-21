all:
	opam reinstall caldav
	cd mirage; mirage clean; mirage configure; make

test: clean
	dune runtest --no-buffer -j 1 test --profile=release

configure:
	cd mirage; mirage configure

depend:
	opam install -t --deps-only .

pin:
	opam pin add caldav .

clean:
	dune clean

utop:
	dune utop src --profile=release

user:
	curl -v -X PUT "http://root:toor@127.0.0.1:8080/user?name=user1&password=1"

public:
	curl -v -X PROPPATCH -d @curl/acl_public_readonly.xml "http://root:epsilon@127.0.0.1:8080/calendars/root"

private:
	curl -v -X PROPPATCH -d @curl/acl_private.xml "http://root:epsilon@127.0.0.1:8080/calendars/root"
