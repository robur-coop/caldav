#!/bin/sh

pidfile="/tmp/crud_lwt.pid"

/bin/sh -c "echo \$\$ > $pidfile && exec ./crud_lwt.native > /dev/null" &
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

testone "GET http://localhost:8080/items"
testone "GET http://localhost:8080/item/1"
testone "POST -d '{\"name\":\"new item\"}' http://localhost:8080/items"
#testone "PUT -H 'Content-Type: application/json' -d '{\"name\":\"modified item\"}' http://localhost:8080/item/1"
testone "DELETE http://localhost:8080/item/1"

curl -f -s -X PUT -H 'Content-Type: application/json' -d '{"name":"modified item"}' http://localhost:8080/item/1
check_exit "PUT -H 'Content-Type: application/json' -d '{\"name\":\"modified item\"}' http://localhost:8080/item/1"

cleanup
