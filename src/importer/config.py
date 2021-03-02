import logging
import os
import time

std_formatter = logging.Formatter('%(asctime)s %(name)s.%(funcName)s %(message)s')

PERFORMANCE_LOG_ROOT_DIR = os.environ.get('PERFORMANCE_LOG_DIR', f'../../timings/')
PERFORMANCE_LOG_DIR = os.path.join(PERFORMANCE_LOG_ROOT_DIR, str(int(time.time())))
os.mkdir(PERFORMANCE_LOG_DIR)
