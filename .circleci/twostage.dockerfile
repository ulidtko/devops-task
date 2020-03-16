FROM fpco/stack-build-small:lts-14.5 as BUILD

RUN mkdir -p /opt/build
COPY . /opt/build
RUN cd /opt/build \
    && stack install \
        --system-ghc \
        --local-bin-path /opt/app \
    ;

#----------------------------------------------------------------------#

FROM ubuntu:18.04

RUN useradd -md/opt/app -s/bin/false devops-api

RUN apt-get update \
    && apt-get install -y \
        ca-certificates \
    && apt-get clean all \
    ;

WORKDIR /opt/app

USER devops-api

COPY --from=BUILD /opt/app/devops-api .

CMD ["/opt/app/devops-api"]

EXPOSE 3000/tcp
