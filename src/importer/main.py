from datetime import date

import click

from universe.world import World


@click.command()
@click.option('--case-count', default=3, help='Number of cases you want to import.')
@click.option('--event-count', default=2, help='Number of events you want to import.')
def main(case_count, event_count):
    # Set everything up
    # Create our world where we simulate a pandemic. This is our playground.
    # Set a beginning for our world

    world = World(date.fromisoformat('2020-02-01'))

    # Populate default entities in our world
    # Counties of interest
    # lower_saxony = 'Niedersachsen'
    # world.add_region(lower_saxony)
    # world.add_district('Braunschweig', lower_saxony)
    # world.add_district('Salzgitter', lower_saxony)
    # world.add_district('Wolfsburg', lower_saxony)
    # Populate our world with n persons
    # world.pre_populate_susceptible()
    # world.pre_populate_infected()
    world.pre_populate_cases_and_contacts(n=case_count)

    # world.pre_populate_infection_chains() #todo
    #  ### Geolocations --- todo don't know yet
    world.pre_populate_events_and_participants(n=event_count)

    # All set! Now we start the pandemic with patient zero
    # world.patient_zero()
    # Now we let run the simulation for 5 ticks
    # world.simulate(ticks=3)

    world.stop()

    # Great, now store the world's case history in SORMAS/JSON/CSV etc
    world.export_sormas()
    # world.export_json()


if __name__ == '__main__':
    main()
