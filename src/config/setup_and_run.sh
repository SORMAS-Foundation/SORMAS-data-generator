#!/bin/bash
source venv/bin/activate

echo "Starting import"

cd importer || exit

python3 main.py \
    --case-count $CASE_COUNT \
    --event-count $EVENT_COUNT
    --region $REGION \
    --district $DISTRICT
    --disease $DISEASE
