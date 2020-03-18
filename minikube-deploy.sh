#!/bin/sh -eux

cd "$(dirname "$0")" #-- the script expects no filepath args

log () { echo "$@"; }
die () { test -n "$@" && log "$@"; exit 1; }

log "minikube is prerequisite for this script."

minikube status || die "Try running: minikube start"

log "generating API Basic Auth password"
password=$( python3 -c 'import binascii; print(binascii.hexlify(open("/dev/urandom", "rb").read(8)).decode("ascii"))' )
username=devops

log "creating Deployment"
kubectl apply -f .k8s/app/ -n app-develop

log "applying the generated creds"
kubectl -n app-develop delete secret app-basic-creds || true
kubectl -n app-develop \
    create secret generic app-basic-creds \
    --from-literal=username="$username" \
    --from-literal=password="$password"

log "exposing Deployment as NodePort Service"
kubectl -n app-develop delete service devops-api || true
kubectl -n app-develop expose deployment devops-api --type=NodePort
url="$(minikube service -n app-develop devops-api --url)"

log "All done. Poke the running api with a curl like this:"
log ""
log "$ curl -u $username:'$password' '$url'"
