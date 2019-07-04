## Live test server

A test caldav server is setup at [https://calendar.robur.coop](https://calendar.robur.coop), which is a unikernel pushing to our [calendar git repository](https://git.robur.io/?p=calendar-data.git;a=summary).
Please try using "mirage" as user and password either on our test server which serves as well a [CalDavZAP](https://www.inf-it.com/open-source/clients/caldavzap/) user interface or configure our test server in your calendar client (`calendar.robur.coop`).
You can choose any not-yet-used username and password, an account will be created by caldav (using the "trust on first use" argument described below).

## Compilation of CalDAV server unikernel

To begin the installation, you need to `ssh` into your server.
Then, you need to install [`opam`](https://opam.ocaml.org) via your package manager (e.g. `apt install opam`).
Make sure you have OCaml version `>=4.07.0`, and opam version `>=2.0.0` and mirage version `>=3.7.1` installed via your package manager.
You can use `ocaml --version`, `opam --version`, and `mirage --version` to find out.

In addition, you currently need our opam repository overlay because we need some
libraries that are not yet released for git-via-ssh. It is recommended to use a custom opam switch:

    opam switch create caldav 4.07.1
    opam repo add git-ssh-dns git+https://github.com/roburio/git-ssh-dns-mirage3-repo.git
    opam install lwt mirage

Now we're ready to compile the CalDAV server. Let's get the code (don't worry that we already pinned caldav, we now need the source code of the unikernel):

    git clone -b future https://github.com/roburio/caldav.git
    cd caldav/mirage
    mirage configure // -t xen / -t hvt works as well
    make depend
    make

If the above commands fail while installing caldav, run `opam remove webmachine` and run `make depend` again.
The `make` command creates a `caldav` executable in `caldav/mirage`. This is the unikernel.
If you compiled for unix (the default unless you specify `-t xen/hvt/..`, this is an executable you can run directly:
We can see all its options:

    ./caldav --help

For other targets you have to create a virtual machine, e.g. solo5-hvt:

    sudo solo5-hvt --net:service=tap100 -- caldav.hvt

## CalDavZAP integration

CalDavZap is an externally developed web-UI for caldav servers. Embedding into
this unikernel provides the web interface at `https://calendar.example.com/index.html`.

A minified and configured version can be obtained from [here](https://github.com/sg2342/caldavzap/), which is based on the zip file and patch below.

Download [https://www.inf-it.com/CalDavZAP_0.13.1.zip](https://www.inf-it.com/CalDavZAP_0.13.1.zip) (3936373 bytes, SHA256 1fb67a4f85c699bfd73f282407d60852f6649a34a923770ae2a378b4f2794dde)
and unpack into `mirage/caldavzap` directory.

Edit `mirage/caldavzap/config.js`
```
diff -ur caldavzap/config.js mirage/caldavzap/config.js
--- a/config.js 2015-09-22 15:29:59.000000000 +0200
+++ b/config.js  2019-11-10 00:46:12.653501000 +0100
@@ -335,12 +335,11 @@
        href: location.protocol+'//'+location.hostname+
                (location.port ? ':'+location.port: '')+
-                location.pathname.replace(RegExp('/+[^/]+/*(index\.html)?$'),'')+
-               '/caldav.php/',
+               '/principals/',
        timeOut: 90000,
        lockTimeOut: 10000,
        checkContentType: true,
        settingsAccount: true,
-       delegation: true,
+       delegation: false,
        additionalResources: [],
        hrefLabel: null,
        forceReadOnly: null,
```

## Running the unikernel

The following steps vary based on your desired server features.

### HTTPS preparations

If you're planning to use https you need to create a certificate:

    opam install certify
    certify selfsign -c server.pem -k server.key "calendar.example.com"
    mv server.pem server.key caldav/mirage/tls/
    cd caldav/mirage ; make

You can also copy an existing certificate and private key to that location.

### First start

To start the server, we need a git remote (possibly with credentials) and an admin password, which will be used for the user `root` that always exists. The password needs to be set on first run only. It will then be hashed, salted and stored in the git repository. The git repository persists on disk when the unikernel is not running. It's the part with your precious user data that you might want to back up.

You have to set up a git repository and provide access to that, either using git_daemon and `--enable=receive-pack` (then everybody can push :/), or via https, or ssh with an RSA key. You can run `awa_gen_key` to get a seed (to be passed to the unikernel) and a public key (which you need to enable access to the git repository).

The arguments for the command line vary depending on the setup:

### With HTTP server and git via TCP
    --admin-password="somecoolpassword" --host="calendar.example.com" --http=80 --remote=git://git.example.com/calendar-data.git

### With HTTPS server and git via https
    --admin-password="somecoolpassword" --host="calendar.example.com" --http=80 --https=443 --remote=https://user:pass@git.example.com/calendar-data.git

### With HTTPS + trust on first use (tofu) and git via ssh:
    --admin-password="somecoolpassword" --host="calendar.example.com" --http=80 --https=443 --tofu --remote=ssh://git@git.example.com/calendar-data.git --seed=abcdef --authenticator=SHA256:b64-encoded-hash-of-server-key

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
