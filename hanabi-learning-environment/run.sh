#!/bin/bash
echo python rl_env_custom.py --players 2 --num_episodes 1 --agent_class NewAgent
python rl_env_custom.py --players 2 --num_episodes 1 --agent_class RandomAgent
