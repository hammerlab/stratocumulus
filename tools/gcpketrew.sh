#!/bin/sh
set -e

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

default_docker_image=smondet/ketrew-dev-server:latest
ketrew_server_image() {
    if [ "$KETREW_IMAGE" = "" ] ; then
        export KETREW_IMAGE=$default_docker_image
        SAY_INFO=default
    fi
    say "Using $SAY_INFO Ketrew-dev-server image: $KETREW_IMAGE"
}

current_cluster_is_the_right_one() {
    ensure_prefix_set
    gcloud container clusters get-credentials $PREFIX-cluster
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
    current_cluster_is_the_right_one
    local pod=$(kubectl get pods | grep "$PREFIX" | awk '{print $1}')
    say "Guessed POD name: $pod"
    kubectl exec -i $pod -- /bin/bash -c "$1"
}

add_ssh_config(){
    ensure_kubectl
    ensure_prefix_set
    current_cluster_is_the_right_one

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
    current_cluster_is_the_right_one

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
    current_cluster_is_the_right_one

    kubectl delete service  $PREFIX-service || say "Service deletion FAILED!"
    kubectl delete deployment  $PREFIX-service || say "Deployment deletion FAILED!"
    gcloud container clusters delete -q $PREFIX-cluster  || say "Cluster deletion FAILED!"
}

default_logs_query="-M 10"

usage () {
   cat <<EOF
Usage:
    $0 <command> [args]

where <command> is:

- up: create a Ketrew server on GKE (Google Container Engine)
- status: check the status of the container
- configure: configure the container's SSH access
    - Variant: 'configure+local' does 'configure' and then edits the local '.ssh/authorized_keys'
- down: destroy the deployment
- exec <cmd>: run an arbitrary command within the container (on the “pod” in Kube-jargon)
- pubkey: output the public Key of the Ketrew server
- logs <query>: run 'ketrew logs' with a given query, see 'ketrew logs --help', the default is '$default_logs_query'


Environment variables:

- PREFIX: name prefix use to generate names (incl. hostnames, should be smaller than 15 characters)
- TOKEN: authentication token used by the Ketrew server
- KETREW_IMAGE: optional use a given docker image (default: '$default_docker_image')
- SSH_CONFIG_DIR: optional, can be used to choose the output path of the SSH configuration/keys

EOF

}

case "$1" in
    "up" ) create_ketrew_container ;;
    "configure" ) add_ssh_config ;;
    "configure+local" ) add_ssh_config write-pub-key ;;
    "status" ) status ;;
    "down" ) take_down ;;
    "exec" ) run_command_on_pod "$2" ;;
    "pubkey" ) run_command_on_pod "cat .ssh/kserver.pub" ;;
    "logs" )
        query="$default_logs_query"
        if [ "$2" != "" ]; then query="$2" ; fi
        run_command_on_pod "eval \`opam config env\` ; ketrew logs $query" ;;
    "help" )
        usage ;;
    * )
        say "Cannnot understand command '$1'"
        usage;
        exit 1
        ;;
esac
