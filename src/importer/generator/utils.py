import os
import uuid
from datetime import datetime

import psycopg2


def sormas_db_connect(host=None, port=None, dbname=None, user=None, password=None):
    if host is None:
        host = os.environ.get('DB_HOST', 'localhost')

    if port is None:
        port = os.environ.get('DB_PORT', '5432')

    if dbname is None:
        dbname = os.environ.get('DB_NAME', 'sormas_db')

    if user is None:
        user = os.environ.get('SORMAS_POSTGRES_USER', 'sormas_user')

    if password is None:
        password = os.environ.get('SORMAS_POSTGRES_PASSWORD', 'sormas')

    conn = psycopg2.connect(host=host, port=port, dbname=dbname, user=user, password=password)
    return conn


def duuid():
    # todo make det. again
    # tmp = str(uuid.UUID(int=random.getrandbits(128)))
    tmp = str(uuid.uuid4())
    return tmp


def dnow():
    # FIXME(@JonasCir) make deterministic
    return datetime.now()
