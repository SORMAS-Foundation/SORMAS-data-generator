FROM debian:buster-slim

ARG DOCKERIZED=false
ARG OPENAPI_GENERATOR_VERSION
ARG PERFORMANCE_LOG_DIR
ARG SORMAS_VERSION

# see https://github.com/debuerreotype/debuerreotype/issues/10
RUN mkdir -p /usr/share/man/man1
RUN apt update > /dev/null && \
    apt upgrade -y > /dev/null && \
    apt install --no-install-recommends -y \
        curl wget unzip vim gcc \
        python3 python3-pip python3-venv python3-dev \
        maven jq \
        postgresql libpq-dev > /dev/null

WORKDIR /srv
CMD ["./setup_and_run.sh"]

COPY config/setup_and_run.sh  setup_and_run.sh
COPY config/requirements.txt  /srv/importer/
COPY importer/gen-client.sh   /srv/importer/

RUN mkdir $PERFORMANCE_LOG_DIR && \
    importer/gen-client.sh

COPY . /srv
