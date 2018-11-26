all:
	cd mirage; make clean && mirage configure && make depend && make

run:
	mirage/main.native --data=/tmp/calendar --http=888 --admin=epsilon --host=192.168.42.84 --tofu

propfind:
	curl -d @curl/propfind.xml -v -u root:epsilon -X PROPFIND http://localhost:888/calendars/root -H "Depth: 1" -l \*:debug

configure:
	cd mirage; mirage configure

test: clean
	jbuilder runtest --no-buffer -j 1 test

clean:
	jbuilder clean

utop:
	dune utop src --profile=release

user:
	curl -v -X PUT "http://root:toor@127.0.0.1:8080/user?name=user1&password=1"

acl:
	curl -v -X PROPPATCH -d @curl/change-acl.xml "http://test:password@127.0.0.1:8080/calendars/test/calendar"
