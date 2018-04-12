#!/bin/sh

pidfile="/tmp/crud_lwt.pid"

#/bin/sh -c "echo \$\$ > $pidfile && exec ./crud_lwt.native > /dev/null" &
/bin/sh -c "echo \$\$ > $pidfile && exec ./crud_lwt.native " &
sleep 0.3

cleanup () {
    cat $pidfile | xargs kill
    rm $pidfile
}

curl="curl -f -s -X"

check_exit () {
    if [ $? -eq 0 ]; then
        echo "success $1"
    else
        cleanup
        echo "failed $1"
        exit 1
    fi
}

testone () {
    echo "executing: $curl $1"
    $curl $1
    check_exit "$1"
}

#testone "GET http://localhost:8080/items"
#testone "GET http://localhost:8080/item/1"
#testone "POST -d '{\"name\":\"new item\"}' http://localhost:8080/items"
#testone "PUT -H 'Content-Type: application/json' -d '{\"name\":\"modified item\"}' http://localhost:8080/item/1"
#testone "PROPFIND -H 'Content-type: application/xml; charset=\"utf-8\"' -d '<?xml version=\"1.0\" encoding=\"utf-8\" ?><propfind xmlns=\"DAV:\"><propname/></propfind>' http://localhost:8080/item/1"
testone "DELETE http://localhost:8080/item/1"

curl -f -s -X PUT -H 'Content-Type: application/json' -d '{"name":"modified item"}' http://localhost:8080/item/1
check_exit "PUT -H 'Content-Type: application/json' -d '{\"name\":\"modified item\"}' http://localhost:8080/item/1"

curl -f -s -X PROPFIND -H 'Content-type: application/xml; charset="utf-8"' -d '<?xml version="1.0" encoding="utf-8" ?><propfind xmlns="DAV:"><propname/></propfind>' http://localhost:8080/item/1
check_exit "PROPFIND -H 'Content-type: application/xml; charset=\"utf-8\"' -d '<?xml version=\"1.0\" encoding=\"utf-8\" ?><propfind xmlns=\"DAV:\"><propname/></propfind>' http://localhost:8080/item/1"


curl -f -s -X PROPFIND -H 'Content-type: applicationgxml; charset="utf-8"' -d '<?xml version="1.0" encoding="utf-8" ?><D:propfind xmlns:D="DAV:"><D:prop xmlns:R="http://ns.example.com/boxschema/"><R:bigbox/><R:author/><R:DingALing/><R:Random/></D:prop></D:propfind>' http://localhost:8080/item/1
check_exit "<?xml version=\"1.0\" encoding=\"utf-8\" ?><D:propfind xmlns:D=\"DAV:\"><D:prop xmlns:R=\"http://ns.example.com/boxschema/\"><R:bigbox/><R:author/><R:DingALing/><R:Random/></D:prop></D:propfind>"

curl -f -s -X PROPFIND -H 'Content-type: application/xml; charset="utf-8"' -d '<?xml version="1.0" encoding="utf-8" ?><D:propfind xmlns:D="DAV:"><D:allprop/></D:propfind>' http://localhost:8080/item/1
check_exit "<?xml version=\"1.0\" encoding=\"utf-8\" ?><D:propfind xmlns:D=\"DAV:\"><D:allprop/></D:propfind>"

curl -f -s -X PROPFIND -H 'Content-type: application/xml; charset="utf-8"' -d '<?xml version="1.0" encoding="utf-8" ?><D:propfind xmlns:D="DAV:"><D:allprop/><D:include><D:supported-live-property-set/><D:supported-report-set/></D:include></D:propfind>' http://localhost:8080/item/1
check_exit "<?xml version=\"1.0\" encoding=\"utf-8\" ?><D:propfind xmlns:D=\"DAV:\"><D:allprop/><D:include><D:supported-live-property-set/><D:supported-report-set/></D:include></D:propfind>"
cleanup
