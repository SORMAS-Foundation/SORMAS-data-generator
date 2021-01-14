# Data Generator for SORMAS

Generate meaningful data for SORMAS

Run `gen-client.sh` to create a python client library to SORMAS REST API based on the published OpenAPI specification.
Use the provided methods to create easily create (DB) entities. `gen-client.sh` will generate and source a fresh `venv`
for you, so this does not interferes with your setup. Please reuse the auto generated python environment by
running `source venv/bin/activate`.

To start a complete test stack run `docker-compose up -d --build --force-recreate`. This starts a SORMAS stack and the
generator/importer will run. After its work is completed, it shuts down automatically.

Third option of running this project is to first build (`$src/ docker build -t local-sormas/data-gen .`)
and run it with  `docker run --network=host -e CASE_COUNT=10 -e EVENT_COUNT=5 data-gen`.
Check `docker-compose.yml` to see what other env variables (like `DB_HOST`) can be passed.


Use this to wipe your DB:
```sql
delete from task;
delete from contact;
delete from cases;
delete from eventparticipant;
delete from events;
delete from person;
```