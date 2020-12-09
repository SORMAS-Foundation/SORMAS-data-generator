#!/bin/bash
source venv/bin/activate
echo "Starting import"
cd importer || exit
python3 main.py
