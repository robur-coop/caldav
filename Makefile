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
