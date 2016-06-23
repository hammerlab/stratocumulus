#!/bin/sh
set -e

usage(){
    echo "Usage:"
    echo "    PREFIX=some-name TOKEN=sooper-sequre sh $0 {up,status,down}"
}

say() {
    printf "<<<<<<<<\n $0 -> $*\n>>>>>>>>\n"
}

ensure_kubectl() {
    if which kubectl ; then
        say "Kubectl already installed"
    else
        say "Kubectl not installed; getting it now"
        gcloud components install kubectl
    fi
}

ensure_prefix_set() {
    if [ "$PREFIX" = "" ]; then
        say "Missing \$PREFIX enviroment variable"
        exit 1
    else
        if [ "${#PREFIX}" -gt "15" ]; then
            say "\$PREFIX enviroment variable is too long, maximum is 15 chars"
            exit 2
        fi
    fi
}

ensure_token_set() {
    if [ "$TOKEN" = "" ]; then
        say "Missing \$TOKEN enviroment variable"
        exit 1
    fi
}

create_ketrew_container() {
    local PORT=8443
    ensure_kubectl
    ensure_prefix_set
    ensure_token_set

    gcloud container clusters create $PREFIX-cluster --num-nodes 1 --wait

    kubectl run $PREFIX-service \
       --image=smondet/ketrew-dev-server:latest \
       --env PORT=$PORT \
       --env AUTH_TOKEN=$TOKEN \
       -- ketrew start

    kubectl expose deployment $PREFIX-service \
       --port=443 --target-port=$PORT --type=LoadBalancer
}

status() {
    ensure_kubectl
    ensure_prefix_set
    ensure_token_set
    kubectl get service $PREFIX-service > /tmp/$PREFIX-status
    say "Status:"
    cat /tmp/$PREFIX-status
    cat /tmp/$PREFIX-status | awk " /^$PREFIX/ {print \"https://\"\$3\"/gui?token=$TOKEN\"}" > /tmp/$PREFIX-url
    if grep '443/TCP' /tmp/$PREFIX-url > /dev/null
    then
        say "External IP address is not ready; try again a bit later :)"
    else
        say "Ketrew should be at: $(cat /tmp/$PREFIX-url)"
    fi
    local cmd="ketrew init --conf /tmp/ketrewdocker/ --just-client $(cat /tmp/$PREFIX-url)"
    printf "Ketrew Client Configuration:\n"
    $cmd || printf "Cannot create Ketrew config but maybe you can? Use:\n   $cmd\nand "
    printf "see /tmp/ketrewdocker/configuration.ml\n"

}

take_down(){
    kubectl delete service  $PREFIX-service || say "Service deletion FAILED!"
    kubectl delete deployment  $PREFIX-service || say "Deployment deletion FAILED!"
    gcloud container clusters delete -q $PREFIX-cluster  || say "Cluster deletion FAILED!"
}

case "$1" in
    "up" ) create_ketrew_container ;;
    "status" ) status ;;
    "down" ) take_down ;;
    * )
    say "Cannnot understand command '$1'"
    usage ;;
esac
