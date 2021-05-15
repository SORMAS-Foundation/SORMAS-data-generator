import os

from sormas import UserReferenceDto

from generator.utils import sormas_db_connect


# todo make this deterministic
def surv_sup_user_ref():
    sup_uuid = os.getenv('SUPERVISOR_UUID')
    if sup_uuid:
        return UserReferenceDto(uuid=sup_uuid)
    else:
        sup_first_name = os.getenv('SUPERVISOR_FIRSTNAME', 'Surveillance')
        sup_last_name = os.getenv('SUPERVISOR_LASTNAME', 'Supervisor')

        with sormas_db_connect() as conn:
            with conn.cursor() as cur:
                cur.execute(f"""
                    SELECT uuid
                      FROM users
                     WHERE firstname = '{sup_first_name}'
                       AND lastname = '{sup_last_name}'
                """)
                uuid = cur.fetchone()[0]
                return UserReferenceDto(uuid=uuid)
