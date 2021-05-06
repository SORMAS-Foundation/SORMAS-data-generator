import logging
import os

import click

from performance.evaluate import analyze_performance
from universe.world import World
from sormas import Disease


# noinspection PyArgumentList
logging.basicConfig(
    level=os.environ.get('LOGLEVEL', 'WARNING').upper(),
    format='%(asctime)s %(name)s.%(funcName)s %(message)s',
)


@click.command()
@click.option('--case-count', default=1, help='Number of cases you want to import.')
@click.option('--event-count', default=1, help='Number of events you want to import.')
@click.option('--region', default='Niedersachsen', help='Region location.')
@click.option('--district', default='Wolfsburg', help='District location.')
@click.option('--disease', default=Disease.CORONAVIRUS, help='Type of disease.')
def main(case_count, event_count, region, district, disease):
    logging.info(f'Importing {case_count} cases')
    logging.info(f'Importing {event_count} events')
    # Set everything up
    # Create our world where we simulate a pandemic. This is our playground.
    # Set a beginning for our world

    world = World(disease=disease)

    # # Populate default entities in our world
    world.add_district(district, region)

    world.pre_populate_cases_and_contacts(n=case_count)

    # world.pre_populate_infection_chains() #todo
    #  ### Geolocations --- todo don't know yet
    world.pre_populate_events_and_participants(n=event_count)

    # Great, now store the world's case history in SORMAS/JSON/CSV etc
    world.export_sormas()
    # world.export_json()

    if os.environ.get('ANALYZE_PERFORMANCE', 'False').upper() == 'TRUE':
        analyze_performance()

    logging.info('Done')


if __name__ == '__main__':
    main()
