import datetime
import logging
from itertools import chain

from sormas import RegionReferenceDto

from generator.utils import sormas_db_connect, duuid


def default_region():
    with sormas_db_connect() as conn:
        with conn.cursor() as cur:
            cur.execute("SELECT uuid FROM region LIMIT 1")
            uuid = cur.fetchone()[0]
            return RegionReferenceDto(uuid=uuid)


def region_ref(uuid):
    return RegionReferenceDto(uuid=uuid)


def insert_region(region):
    logging.info(f'Inserting region {region}')
    with sormas_db_connect() as conn:
        with conn.cursor() as cur:
            cur.execute("SELECT id, uuid FROM region WHERE name=%s", [region])
            exists = cur.fetchone()
            if exists:
                logging.info(f'{region} already exists in the DB, value was {exists}')
                return exists[0], exists[1]

            cur.execute("SELECT id FROM region")
            all_ids = list(chain.from_iterable(cur.fetchall()))
            _id = max(all_ids) + 1
            date = datetime.date.today()
            uuid = duuid()
            cur.execute(
                "INSERT INTO region"
                " (id,changedate, creationdate, name, uuid, epidcode, archived)"
                " VALUES (%s,%s, %s, %s, %s, %s, %s)",
                [_id, date, date, region, uuid, 'REG', False])
            return _id, uuid
