from pprint import pprint

from pygrok import Grok

from config import PERFORMANCE_LOG_DIR


def analyze_performance():
    pattern = '^%{TIMESTAMP_ISO8601:timestamp} universe.util.export.sormas_export importing %{WORD:entity}: %{UUID:uuid}'
    grok = Grok(pattern)

    with open(PERFORMANCE_LOG_DIR, 'r') as f:
        for line in f.readlines():
            pprint(line)
            res = grok.match(line)
            pprint(res)
