## Compilation of CalDAV server unikernel

To begin the installation, you need to `ssh` into your server.
Then, you need to install [`opam`](https://opam.ocaml.org) via your package manager (e.g. `apt install opam`).

Since some dependencies are not released via `opam` yet, they need to be manually pinned. To do this, run:

    opam pin add mirage-fs-mem https://github.com/roburio/mirage-fs-mem.git
    opam pin add webmachine https://github.com/roburio/ocaml-webmachine.git#webdav
    opam pin add icalendar https://github.com/roburio/icalendar.git
    opam pin add caldav https://github.com/roburio/caldav.git

Now we're ready to compile the CalDAV server. Let's get the code (don't worry that we already pinned caldav, we now need the source code of the unikernel):

    git clone https://github.com/roburio/caldav.git 
    cd caldav/mirage
    mirage configure
    make depend
    make
    
The `make` command creates a `main.native` executable in `caldav/mirage`. This is the unikernel.
We can see all its options:

    ./main.native --help

## Running the unikernel

The following steps vary based on your desired server features.

### HTTPS preparations

If you're planning to use https you need to create a certificate:

    opam install certify
    selfsign -c server.pem -k server.key "calendar.example.com"
    mv server.pem server.key caldav/mirage/tls/

You can also copy an existing one to that location.

### First start

To start the server, we need a data directory and an admin password, which will be used for the user `root` that always exists. The password needs to be set on first run only. It will then be hashed, salted and stored on disk in the data directory. The data directory persists on disk when the unikernel is not running. It's the part with your precious user data that you might want to back up.

Startup:

    mkdir /tmp/calendar

### With HTTP
    ./main.native --data="/tmp/calendar" --admin-password="somecoolpassword" --host="calendar.example.com" --http=80

### With HTTPS
    ./main.native --data="/tmp/calendar" --admin-password="somecoolpassword" --host="calendar.example.com" --http=80 --https=443

### With HTTPS + trust on first use (tofu):
    ./main.native --data="/tmp/calendar" --admin-password="somecoolpassword" --host="calendar.example.com" --http=80 --https=443 --tofu

## Server administration

### Create user

If you don't use trust on first use, you might want to create a new user:

    curl -v -u root:somecoolpassword -X PUT "https://calendar.example.com/user?name=somenewuser&password=theirpassword"
