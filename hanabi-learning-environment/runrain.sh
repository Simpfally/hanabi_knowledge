#!/bin/bash
PYTHONPATH=${PYTHONPATH}:agents/rainbow
export PYTHONPATH
python rl_env_custom_rain.py
