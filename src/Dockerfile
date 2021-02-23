FROM debian:buster-slim
ARG SORMAS_VERSION
ARG OPENAPI_GENERATOR_VERSION
ARG DOCKERIZED=false
ARG PERFORMANCE_LOG_DIR
# see https://github.com/debuerreotype/debuerreotype/issues/10
RUN mkdir -p /usr/share/man/man1
RUN apt update && apt upgrade -y
RUN apt install --no-install-recommends -y \
    curl wget unzip vim gcc \
    python3 python3-pip python3-venv python3-dev \
    maven jq \
    postgresql libpq-dev

WORKDIR /srv
RUN mkdir $PERFORMANCE_LOG_DIR
COPY config/setup_and_run.sh setup_and_run.sh
COPY importer/gen-client.sh /srv/importer/
COPY config/requirements.txt /srv/importer/
RUN  importer/gen-client.sh
COPY . /srv
CMD ["./setup_and_run.sh"]
