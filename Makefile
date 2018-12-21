all:
	opam reinstall caldav
	cd mirage; make

configure:
	cd mirage; mirage configure

test: clean
	jbuilder runtest --no-buffer -j 1 test

depend:
	opam install -t --deps-only .

clean:
	jbuilder clean

utop:
	dune utop src --profile=release

user:
	curl -v -X PUT "http://root:toor@127.0.0.1:8080/user?name=user1&password=1"

acl:
	curl -v -X PROPPATCH -d @curl/change-acl.xml "http://test:password@127.0.0.1:8080/calendars/test/calendar"

standalone:
	jbuilder build app/caldav_server.exe

run-standalone:
	_build/default/app/caldav_server.exe
