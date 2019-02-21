## Compilation of CalDAV server unikernel

To begin the installation, you need to `ssh` into your server.
Then, you need to install [`opam`](https://opam.ocaml.org) via your package manager (e.g. `apt install opam`).
Make sure you have OCaml version `>=4.03.0`, and opam version `>=2.0.0` and mirage version `>=3.3.1` installed via your package manager.
You can use `ocaml --version`, `opam --version`, and `mirage --version` to find out.

Now we're ready to compile the CalDAV server. Let's get the code (don't worry that we already pinned caldav, we now need the source code of the unikernel):

    git clone https://github.com/roburio/caldav.git 
    cd caldav/mirage
    mirage configure
    make depend
    make

If the above commands fail while installing caldav, run `opam remove webmachine` and run `make depend` again.  
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

    curl -v -u root:somecoolpassword -X PUT "https://calendar.example.com/users/somenewuser?password=theirpassword"

### Update password

If someone forgot their password, root can set a new one:

    curl -v -u root:somecoolpassword -X PUT "https://calendar.example.com/users/somenewuser?password=theirpassword"

### Delete user

If someone wants to leave, root can delete their account:

    curl -v -u root:somecoolpassword -X DELETE "https://calendar.example.com/users/somenewuser"

### Create group

You might want to create a new group. Members is an optional query parameter.

    curl -v -u root:somecoolpassword -X PUT "https://calendar.example.com/groups/somenewgroup?members=ruth,viktor,carsten,else"

### Update group members

You might want to update the members of a group. The members parameter will overwrite the existing group members. Be careful not to lose your groups.

    curl -v -u root:somecoolpassword -X PUT "https://calendar.example.com/groups/somenewgroup?members=ruth,viktor,carsten,else"

You might want to add a member to a group.

    curl -v -u root:somecoolpassword -X PUT "https://calendar.example.com/groups/somenewgroup/users/ruth"

You might want to remove a member from a group.

    curl -v -u root:somecoolpassword -X DELETE "https://calendar.example.com/groups/somenewgroup/users/ruth"

### Delete group

You might want to delete a group. Root can do this.

    curl -v -u root:somecoolpassword -X DELETE "https://calendar.example.com/groups/somenewgroup"

### Make calendar public

Make the private calendar `TESTCALENDAR` publicly readable for everybody, while keeping all privileges for the `OWNER`.

    curl -v -u root:somecoolpassword -X PROPPATCH -d '<?xml version="1.0" encoding="utf-8" ?>
    <D:propertyupdate xmlns:D="DAV:">
      <D:set>
        <D:prop>
          <D:acl>
            <D:ace>
              <D:principal><D:href>/principals/OWNER/</D:href></D:principal>
              <D:grant><D:privilege><D:all/></D:privilege></D:grant>
            </D:ace>
            <D:ace>
              <D:principal><D:all/></D:principal>
              <D:grant><D:privilege><D:read/></D:privilege></D:grant>
            </D:ace>
          </D:acl>
        </D:prop>
      </D:set>
    </D:propertyupdate>' "https://calendar.example.com/calendars/TESTCALENDAR"

### Make calendar private

Make the calendar `TESTCALENDAR` private, only accessible for the `OWNER`.

    curl -v -u root:somecoolpassword -X PROPPATCH -d '<?xml version="1.0" encoding="utf-8" ?>
    <D:propertyupdate xmlns:D="DAV:">
      <D:set>
        <D:prop>
          <D:acl>
            <D:ace>
              <D:principal><D:href>/principals/OWNER/</D:href></D:principal>
              <D:grant><D:privilege><D:all/></D:privilege></D:grant>
            </D:ace>
          </D:acl>
        </D:prop>
      </D:set>
    </D:propertyupdate>' "https://calendar.example.com/calendars/TESTCALENDAR"



