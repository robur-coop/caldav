ssh into the server
install opam via package manager (e.g. apt install opam)

Since some packages are not released via opam yet, they need to be manually pinned:

    opam pin add mirage-fs-mem https://github.com/roburio/mirage-fs-mem.git
    opam pin add webmachine https://github.com/roburio/ocaml-webmachine.git#webdav
    opam pin add icalendar https://github.com/roburio/icalendar.git
    opam pin add caldav https://github.com/roburio/caldav.git

    git clone https://github.com/roburio/caldav.git (don't worry that we already pinned this, we also need the source code of the unikernel).
    cd caldav/mirage
    mirage configure
    make depend
    make
    => creates a main.native in caldav/mirage
    ./main.native --help
    => see all options for the unikernel

If you're planning to use https:

    opam install certify
    selfsign -c server.pem -k server.key "calendar.example.com"
    mv server.pem server.key caldav/mirage/tls/

To start up the server, we need a data directory and an admin password. The password needs to be set on first run only. It will then be hashed, salted and stored on disk in the data directory. The data directory persists on disk when the unikernel is not running. It's the part with your precious user data that you might want to back up.

Startup:

    mkdir /tmp/calendar

With http:
    ./main.native --data="/tmp/calendar" --admin-password="somecoolpassword" --host="calendar.example.com" --http=80

With https:
    ./main.native --data="/tmp/calendar" --admin-password="somecoolpassword" --host="calendar.example.com" --http=80 --https=443

With https + trust on first use (tofu):
    ./main.native --data="/tmp/calendar" --admin-password="somecoolpassword" --host="calendar.example.com" --http=80 --https=443 --tofu

If you don't use trust on first use, you want to create a new user:

    curl -v -X PUT "https://root:somecoolpassword@calendar.example.com/user?name=somenewuser&password=theirpassword"
