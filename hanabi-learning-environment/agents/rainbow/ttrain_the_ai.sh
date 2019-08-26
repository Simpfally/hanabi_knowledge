#!/bin/bash
PYTHONPATH=${PYTHONPATH}:../..
#PYTHONPATH=${PYTHONPATH}:/home/savar/stage_hanabi/hanabi-learning-environment
export PYTHONPATH
echo $PYTHONPATH
python -um train \
  --base_dir=data \
  --gin_files='configs/hanabi_rainbow.gin'
