#!/bin/sh
set -e

usage(){
    echo "Usage:"
    echo "    PREFIX=some-name TOKEN=sooper-sequre sh $0 {up,status,configure,down}"
    echo ""
    echo "Optionally SSH_CONFIG_DIR can be used to choose the output path of the SSH configuration/keys"
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

ketrew_server_image() {
    if [ "$KETREW_IMAGE" = "" ] ; then
        export KETREW_IMAGE=smondet/ketrew-dev-server:latest
        SAY_INFO=default
    fi
    say "Using $SAY_INFO Ketrew-dev-server image: $KETREW_IMAGE"
}

create_ketrew_container() {
    local PORT=8443
    ensure_kubectl
    ensure_prefix_set
    ensure_token_set
    ketrew_server_image

    gcloud container clusters create $PREFIX-cluster --num-nodes 1 --wait

    kubectl run $PREFIX-service \
       --image=$KETREW_IMAGE \
       --env PORT=$PORT \
       --env AUTH_TOKEN=$TOKEN \
       -- ketrew start

    kubectl expose deployment $PREFIX-service \
       --port=443 --target-port=$PORT --type=LoadBalancer
}

run_command_on_pod () {
    ensure_kubectl
    ensure_prefix_set
    local pod=$(kubectl get pods | grep "$PREFIX" | awk '{print $1}')
    say "Guessed POD name: $pod"
    kubectl exec -i $pod -- /bin/bash -c "$1"
}

add_ssh_config(){
    ensure_kubectl
    ensure_prefix_set

    local tmpdir=/tmp/$PREFIX-sshconfig/
    if [ "$SSH_CONFIG_DIR" != "" ] ; then
        tmpdir=$SSH_CONFIG_DIR
    fi

    local write_ssh_pub_key=$1


    mkdir -p $tmpdir

    if [ -f $tmpdir/kserver.pub ] ; then
        say "Reusing the Key-pair: $tmpdir/kserver"
    else
        ssh-keygen -t rsa -N '' -f $tmpdir/kserver
    fi
    echo 'StrictHostKeyChecking no' > $tmpdir/config
    echo "IdentityFile ~/.ssh/kserver" >> $tmpdir/config

    local pod=$(kubectl get pods | grep "$PREFIX" | awk '{print $1}')
    say "Guessed POD name: $pod"
    kubectl exec -i $pod -- /bin/bash -c 'uname -a ; echo "USER: $(whoami)"'

    kubectl exec -i $pod -- /bin/bash -c 'cat > ~/.ssh/config' < $tmpdir/config
    kubectl exec -i $pod -- /bin/bash -c 'cat > ~/.ssh/kserver' < $tmpdir/kserver
    kubectl exec -i $pod -- /bin/bash -c 'cat > ~/.ssh/kserver.pub' < $tmpdir/kserver.pub
    kubectl exec -i $pod -- /bin/bash -c 'chmod 600 ~/.ssh/*'

    say "Use the contents of $tmpdir/kserver.pub on any host that \
the Ketrew server should be able to talk to"

    if [ "$write_ssh_pub_key" = "write-pub-key" ]; then
        local dest=$HOME/.ssh/authorized_keys
        say "Writing to $dest"
        cat $tmpdir/kserver.pub >> $dest
    fi

}

status() {
    ensure_kubectl
    ensure_prefix_set
    ensure_token_set
    kubectl get service $PREFIX-service > /tmp/$PREFIX-status
    local ketrew_url=/tmp/$PREFIX-client-url
    if [ "$KETREW_URL" != "" ] ; then
        ketrew_url=$KETREW_URL
    fi
    say "Status:"
    cat /tmp/$PREFIX-status
    cat /tmp/$PREFIX-status | awk " /^$PREFIX/ {print \"https://\"\$3\"/gui?token=$TOKEN\"}" > /tmp/$PREFIX-url
    if grep '443/TCP' /tmp/$PREFIX-url > /dev/null
    then
        say "External IP address is not ready; try again a bit later :)"
    else
        cp /tmp/$PREFIX-url $ketrew_url
        say "Ketrew should be at: $(cat /tmp/$PREFIX-url)\nURL also in $ketrew_url"
    fi

}

take_down() {

    ensure_kubectl
    ensure_prefix_set
    kubectl delete service  $PREFIX-service || say "Service deletion FAILED!"
    kubectl delete deployment  $PREFIX-service || say "Deployment deletion FAILED!"
    gcloud container clusters delete -q $PREFIX-cluster  || say "Cluster deletion FAILED!"
}

case "$1" in
    "up" ) create_ketrew_container ;;
    "configure" ) add_ssh_config ;;
    "configure+local" ) add_ssh_config write-pub-key ;;
    "status" ) status ;;
    "down" ) take_down ;;
    "exec" ) run_command_on_pod "$2" ;;
    * )
    say "Cannnot understand command '$1'"
    usage ;;
esac
