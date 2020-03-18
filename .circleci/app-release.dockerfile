FROM ubuntu:18.04
ARG CI_BRANCH=develop

RUN useradd -md/opt/app -s/bin/false devops-api

RUN apt-get update \
    && apt-get install -y \
        ca-certificates \
    && apt-get clean all \
    ;

WORKDIR /opt/app

USER devops-api

COPY stack-OUT/devops-api .
COPY ./sample-data/$CI_BRANCH.db ./main.sqlite

CMD ["/opt/app/devops-api"]

EXPOSE 3000/tcp
