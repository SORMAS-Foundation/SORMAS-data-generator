from sormas import LocationDto

from generator.utils import duuid


def gen_location_dto(latitude=None, longitude=None, region=None, district=None):
    if latitude is None:
        raise NotImplementedError

    if longitude is None:
        raise NotImplementedError

    if region is None or district is None:
        raise NotImplementedError

    location_dto = LocationDto(
        uuid=duuid(),
        latitude=latitude,
        longitude=longitude,
        region=region,
        district=district,
    )
    return location_dto
