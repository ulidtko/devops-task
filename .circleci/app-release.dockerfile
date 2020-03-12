FROM ubuntu:18.04

RUN useradd -md/opt/app -s/bin/false devops-api

RUN apt-get update \
    && apt-get install -y \
        ca-certificates \
    && apt-get clean all \
    ;

WORKDIR /opt/app

USER devops-api

COPY stack-OUT/devops-api .

CMD ["/opt/app/devops-api"]

EXPOSE 3000/tcp
