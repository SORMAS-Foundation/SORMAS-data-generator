FROM debian:buster-slim
ARG SORMAS_VERSION
ENV DOCKERIZED=true

# see https://github.com/debuerreotype/debuerreotype/issues/10
RUN mkdir -p /usr/share/man/man1
RUN apt update && apt upgrade -y
RUN apt install -y \
    curl wget unzip vim \
    python3 python3-pip python3-venv  \
    maven jq \
    postgresql libpq-dev

# FIXME Debian repo version is to old
RUN pip3 install -U PyYAML


WORKDIR /srv/
COPY . /srv/


ARG OPENAPI_GENERATOR_VERSION
RUN ./importer/gen-client.sh
COPY config/setup_and_run.sh setup_and_run.sh

CMD ["./setup_and_run.sh"]

