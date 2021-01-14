import datetime
import logging
from itertools import chain

from sormas import DistrictReferenceDto

from generator.utils import sormas_db_connect, duuid


def default_district():
    with sormas_db_connect() as conn:
        with conn.cursor() as cur:
            cur.execute("SELECT uuid FROM district")
            uuid = cur.fetchone()[0]
            return DistrictReferenceDto(uuid=uuid)


def district_ref(uuid):
    return DistrictReferenceDto(uuid)


def insert_district(district, region_id):
    with sormas_db_connect() as conn:
        with conn.cursor() as cur:
            cur.execute("SELECT uuid FROM district WHERE name=%s", [district])
            exists = cur.fetchone()[0]
            if exists:
                logging.info(f'{district} already exists in the DB')
                return exists

            cur.execute("SELECT id FROM district")
            all_ids = list(chain.from_iterable(cur.fetchall()))
            _id = max(all_ids) + 1
            date = datetime.date.today()
            uuid = duuid()
            cur.execute("INSERT INTO district (id, changedate, creationdate, name, uuid, region_id, epidcode, archived)"
                        "VALUES (%s,%s, %s, %s, %s, %s, %s, %s)",
                        [_id, date, date, district, uuid, region_id, 'DIS', False])

            return uuid
