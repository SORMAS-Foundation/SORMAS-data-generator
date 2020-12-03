import yaml

with open('sormas-rest.yaml') as f:
    data = yaml.load(f, Loader=yaml.FullLoader)
    data['components']['securitySchemes'] = {'basicAuth': {'type': 'http', 'scheme': 'basic'}}
    data['security'] = [{'basicAuth': []}]
    with open('sormas-rest-fixed.yaml', 'w') as w:
        yaml.dump(data,w)
    pass
