all:
	jbuilder build bin/caldav_server.exe
test: clean
	jbuilder runtest --no-buffer -j 1
clean:
	jbuilder clean
