all:
	jbuilder build app/caldav_server.exe
test: clean
	jbuilder runtest --no-buffer -j 1
clean:
	jbuilder clean
run: all
	_build/default/app/caldav_server.exe

utop:
	dune utop src --profile=release

user:
	curl -v -X PUT "http://root:toor@127.0.0.1:8080/user?user=user1&password=1"
