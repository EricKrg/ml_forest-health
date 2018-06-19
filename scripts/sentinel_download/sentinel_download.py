# -*- coding: utf-8 -*-
"""
Created on Tue Jun 19 12:13:42 2018

@author: Eric
"""

import logging

logger = logging.getLogger('sentinelsat')
logger.setLevel('INFO')

h = logging.StreamHandler()
h.setLevel('INFO')
fmt = logging.Formatter('%(message)s')
h.setFormatter(fmt)
logger.addHandler(h)

# connect to the API
from sentinelsat import SentinelAPI, read_geojson, geojson_to_wkt
from datetime import date

api = SentinelAPI('ddejon', 'Billytalent94', 'https://scihub.copernicus.eu/apihub', show_progressbars = True)


# search by polygon, time, and SciHub query keywords
footprint = geojson_to_wkt(read_geojson('/home/eric/projects/ml_forest-health/data/map.geojson'))

products = api.query(footprint,
                     date=('20150101', date(2016, 01, 01)),
                     platformname='Sentinel-2',
                     cloudcoverpercentage=(0,30))


print(products)
# download all results from the search
#api.download_all(products, directory_path = '/home/', checksum = False)

