#!/bin/bash
source ../.env
tmp=$(mktemp -d)
echo $tmp
wget "https://github.com/hzi-braunschweig/SORMAS-Project/releases/download/v${SORMAS_VERSION}/sormas_${SORMAS_VERSION}.zip" -P "$tmp"
unzip "$tmp/sormas_${SORMAS_VERSION}.zip" -d "$tmp"
cp "$tmp/deploy/openapi/sormas-rest.yaml" .
rm -rf "$tmp"


#sed -i 's/http-basic/basicAuth/g' sormas-rest.yaml
#sed -i 's/[[:space:]]Basic/\ basic/g' sormas-rest.yaml

python3 fix_yaml.py

docker run --rm -v "${PWD}:/local" openapitools/openapi-generator-cli generate \
-i local/sormas-rest-fixed.yaml \
-g python \
-o /local/out \
--package-name sormas

sudo chown -R $USER:$USER out

python3 -m venv venv
source ./venv/bin/activate

pip install -r requirements.txt

pushd out || exit
python3 setup.py install --record files.txt
popd || exit

