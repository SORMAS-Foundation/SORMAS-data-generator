#!/bin/bash
set -e

if [[ -z "${DOCKERIZED}" ]]; then
  source ../../.env
fi

tmp=$(mktemp -d)
wget "https://github.com/hzi-braunschweig/SORMAS-Project/releases/download/v${SORMAS_VERSION}/sormas_${SORMAS_VERSION}.zip" -P "$tmp"
unzip "$tmp/sormas_${SORMAS_VERSION}.zip" -d "$tmp"
cp "$tmp/deploy/openapi/sormas-rest.yaml" .
rm -rf "$tmp"

python3 -m venv venv
source ./venv/bin/activate
# FIXME Debian repo version is to old
pip3 install --upgrade wheel setuptools

# use docker if we run on the host for development
if [[ -z "${DOCKERIZED}" ]]; then
  docker run --rm -v "${PWD}:/local" "openapitools/openapi-generator-cli:v${OPENAPI_GENERATOR_VERSION}" generate \
    -i local/sormas-rest.yaml \
    -g python \
    -o /local/out \
    --package-name sormas

  sudo chown -R "$USER":"$USER" out
  PIP_REQ_PATH=requirements.txt

else

  # https://github.com/OpenAPITools/openapi-generator#launcher-script
  mkdir -p ~/bin/openapitools
  curl https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/bin/utils/openapi-generator-cli.sh >~/bin/openapitools/openapi-generator-cli
  chmod u+x ~/bin/openapitools/openapi-generator-cli
  export PATH=$PATH:~/bin/openapitools/

  openapi-generator-cli generate \
    -i sormas-rest.yaml \
    -g python \
    -o out \
    --package-name sormas

  PIP_REQ_PATH=importer/requirements.txt

fi

pip3 install -r $PIP_REQ_PATH

pushd out || exit
pip3 install .
popd || exit
