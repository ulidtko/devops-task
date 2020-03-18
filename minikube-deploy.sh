#!/bin/sh -eux

cd "$(dirname "$0")" #-- the script expects no filepath args

log () { echo "$@"; }
die () { test -n "$@" && log "$@"; exit 1; }

log "minikube is prerequisite for this script."

minikube status || die "Try running: minikube start"

log "generating API Basic Auth password"
password=$( python3 -c 'import base64; print(base64.a85encode(open("/dev/urandom", "rb").read(8)).decode("ascii"))' )
username=devops

log "writing generated creds into kustomization.yaml"
#test -e .k8s/app/kustomization.yaml || {
cat > .k8s/app/kustomization.yaml << EOF
secretGenerator:
- name: app-basic-creds
- namespace: app-develop
  literals:
    - username="$username"
    - password="$password"
EOF

kubectl apply -f .k8s/app/
kubectl apply -k .k8s/app/

