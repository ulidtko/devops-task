#!/bin/sh -eu

cd "$(dirname "$0")" #-- the script expects no filepath args

log () { echo "$@"; }
die () { test -n "$@" && log "$@"; exit 1; }

log "minikube is prerequisite for this script."

minikube status || die "Try running: minikube start"

log "generating API Basic Auth password"
password=$( python3 -c 'import base64; print(base64.a85encode(open("/dev/urandom", "rb").read(8)).decode("ascii"))' )
username=devops

log "creating Deployment"
kubectl apply -f .k8s/app/ -n app-develop

log "applying the generated creds"
kubectl -n app-develop delete secret app-basic-creds || true
kubectl -n app-develop \
    create secret generic app-basic-creds \
    --from-literal=username="$username" \
    --from-literal=password="$password"

url="$(minikube service -n app-develop devops-test-task --url)"

log ""
log "All done. Poke the running api with curls like this:"
log ""
log "$ curl -u $username:'$password' '$url'"
log "$ curl -u $username:'$password' -H'content-Type: application/json' $url/api \\"
log "    -d'{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"get_raw_transactions\",\"params\":{\"address\":\"DEVADDR_K\"}}'"
log ""
log "Dashboard URL: $(minikube service kubernetes-dashboard -nkubernetes-dashboard --url)"
