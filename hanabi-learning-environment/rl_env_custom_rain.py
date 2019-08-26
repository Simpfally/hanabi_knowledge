# Copyright 2018 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""A simple episode runner using the RL environment."""

from __future__ import print_function

import numpy as np
import sys
import getopt
import rl_env
#import gin.tf
from pprint import pprint
#import agents.rainbow.dqn_agent as dqn_agent
#from agents.rainbow.rainbow_agent import RainbowAgent
import agents.rainbow.run_experiment as run_experiment
from agents.rainbow.third_party.dopamine import logger

#import argparse
DEBUG = True

from functools import wraps

# execute only if debug, like debug prints
def debug(f):
    @wraps(f)
    def wrapper(*args, **kwargs):
        if DEBUG:
            return f(*args, **kwargs)
    return wrapper

@debug
def pp(*args, **kwargs):
        print(*args, **kwargs)

#parser = argparse.ArgumentParser()
#parser.add_argument("input", help="name of the input file")
#parser.add_argument("-s", "--silence", help="do not print in case no output filename is given", action="store_true")
#parser.add_argument("--num_espisode", type=int)
#args = parser.parse_args()

#return a list of each player's card
def get_list_cards(obs):
    cur_ply = obs["current_player"]
    obs = obs["player_observations"]
    nply = len(obs)
    assert(nply > 1)
    # observations for each players are always in the same order
    # player 1 is the first one
    # player 2 is the second one
    # each player however see "me" cards, then "next guy" cards etc..
    # so the first hand they see is always blank for example
    first_hand = obs[-1]["observed_hands"][1] # this return the obs of the hand of player 0 from the PoV of player 1, that is full knowledge
    # hands in observations are ordered by who's playing next, 0 : you 1: the next to play etc..
    # that's why the first player is the last to player (-1) for the second player
    other_hands = obs[0]["observed_hands"][1:] # return all others hands

    # ugh guess what you need to correctly copy these things
    lc = [first_hand] + other_hands
    l = []
    for x in lc:
        l.append(list(x))

    # so that first element is always first player
    shift_by = cur_ply
    n = len(l)
    shifted = l[-shift_by:] + l[:-shift_by]
    
    for h in shifted:
        h.reverse()
    return shifted

def find_new_card(obs):
    # TODO  give a null card if there is no new card

    nply = len(obs)
    cur_ply = (obs["current_player"] +0) % nply
    obs = obs["player_observations"]
    # player x has played or discarded
    # and is now the last player to play for the next player
    ply_x_plus_one = obs[cur_ply]
                                    #
    return (ply_x_plus_one["observed_hands"][1][-1]).copy() # new card is actually the last one
    # and btw DISCARD 5 would actually mean play the newest card then (in the environement)


def format_color(col):
    col_dic = { 'R' : 0,
            'Y' : 1,
            'G' : 2,
            'B' : 3,
            'W' : 4}
    scol = col
    if col in col_dic:
        scol = col_dic[col]
    return str(scol)
def format_rank(ran):
    return str(int(ran) + 0)

def format_card(c):
    col, rank = c["color"], c["rank"]
    return "{}:{}".format(format_color(col), format_rank(rank))

def format_hand(h):
    s = ""
    for c in h:
        s += format_card(c) + ", "
    return s[:-2]

def format_step(step, hand_count):

    action_dic = { "REVEAL_COLOR" : "COLORCLUE",
            "REVEAL_RANK" : "RANKCLUE",
            "PLAY" : "PLAY",
            "DISCARD" : "DISCARD"}
    s = action_dic[step["action_type"]]
    # TODO set to true to get clues offsets
    # offset = 1 => give clue to the next player (I hope, didn't verify..)
    support_more_than_two = False
    if "color" in step:
        ss = s + " " + format_color(step["color"])
        if support_more_than_two:
            return ss + " " + str(step["target_offset"])
        return ss
    if "rank" in step:
        ss = s + " " + format_rank(step["rank"])
        if support_more_than_two:
            return ss + " " + str(step["target_offset"])
        return ss
    # Play 0 mean the player played his youngest card (the one that will get replaced by a new card)
    # this is reversed orginally
    return s + " " + str(hand_count - step["card_index"] - 1 ) + " " + format_card(step["new_card"])

@debug
def pret_list(l):
    for x in l:
        print(">",x)




class Runner(object):
  """Runner class."""

  def __init__(self):
    """Initialize runner."""

  def run(self):
    """Run episodes."""
    gin_files = ['agents/rainbow/configs/hanabi_rainbow_explicit.gin']
    run_experiment.load_gin_configs(gin_files, [])
    environment = rl_env.make('Hanabi-Full-CardKnowledge', num_players="2")
    #environment_name == "Hanabi-Full-CardKnowledge"):
    obs_stacker = run_experiment.create_obs_stacker(environment)
    agent = run_experiment.create_agent(environment, obs_stacker) #verify it uses rainbow


    #get the checkpoint..
    base_dir = "agents/rainbow/data"
    checkpoint_file_prefix = "ckpt"
    checkpoint_dir = '{}/checkpoints'.format(base_dir)
    experiment_logger = logger.Logger('{}/logs'.format(base_dir))
    run_experiment.initialize_checkpointing(agent,
                                              experiment_logger,
                                              checkpoint_dir,
                                              checkpoint_file_prefix)

    obs_stacker.reset_stack()
    observations = environment.reset() # Full game observation, not to be passed to agents
    current_player, legal_moves, observation_vector = (
      run_experiment.parse_observations(observations, environment.num_moves(), obs_stacker))

    has_played = {current_player}
    action = agent.begin_episode(current_player, legal_moves, observation_vector)


    ### Stage-compliant printing related
    # observations["player_observations"] has an element for every player

    hands_list = get_list_cards(observations)
    pp(hands_list[0])
    pp(hands_list[1])
    hand_count = len(hands_list[0])
    step_list = []

    is_done = False

    reward_since_last_action = np.zeros(environment.players)
    score = 0
    while not is_done:
      pp("~~~~")
      # convert action into dict-like action
      lm = observations["player_observations"][current_player]["legal_moves"]
      lmi = observations["player_observations"][current_player]["legal_moves_as_int"]
      ind = 0
      action_unf = None
      for ind in range(0, len(lm)):
          if lmi[ind] == action:
              action_unf = lm[ind]
      assert(action_unf is not None)
      
      ### Stage compliant printing
      #print(format_step(cpacopy, hand_count))
      ############ do the step (important!)
      observations, reward, is_done, _ = environment.step(action.item())
      pp(observations["current_player"], "doing", action_unf)
      i = 0
      for x in observations["player_observations"]:
          i += 1
          pp(i, "player")
          for k in x["observed_hands"]:
              pret_list(k)
      cpa = action_unf["action_type"]
      cpacopy = action_unf.copy()
      if cpa == "PLAY" or cpa == "DISCARD":
          new_card = find_new_card(observations)
          pp("new_card:", new_card)
          
          cpacopy["new_card"] = new_card
      step_list.append(cpacopy)
      pp(observations["player_observations"][0]["fireworks"])
      
      # quit if done
      if is_done:
        fire = observations["player_observations"][0]["fireworks"]
        score = 0
        for x in fire:
            score += fire[x]
        print("Score=", score, fire)
        break
      
      current_player, legal_moves, observation_vector = (
            run_experiment.parse_observations(observations, environment.num_moves(), obs_stacker))
      if current_player in has_played:
        action = agent.step(reward_since_last_action[current_player],
                            current_player, legal_moves, observation_vector)
      else:
          # Each player begins the episode on their first turn (which may not be
          # the first move of the game).
        action = agent.begin_episode(current_player, legal_moves,
                                       observation_vector)
        has_played.add(current_player)
      #cur_ply = observation["current_player"]
      #observation = observations['player_observations'][cur_ply]
      #current_player_action = agent.act(observation)
      # Make an environment step.
      #print('Agent: {} action: {}'.format(observation['current_player'],
      #                                    current_player_action))
      ##hands_list = get_list_cards(observations)
     # for h in hands_list:
     #     i += 1
     #     print(i, format_hand(h))
      ######observations, reward, done, unused_info = environment.step(
          ########current_player_action)
      ### Finding the new card... ###
      ##cpa = current_player_action["action_type"]
      ##cpacopy = current_player_action.copy()
      ##if cpa == "PLAY" or cpa == "DISCARD":
      ##    cpacopy["new_card"] = find_new_card(observations)
          #print("new:",format_card(new_card))
      ### Stage compliant printing
      #print(format_step(cpacopy, hand_count))
      ##step_list.append(cpacopy)
      ###
      ###
    #print('Running episode: %d' % episode)
    #print('Max Reward: %.3f' % max(rewards))

    ### Stage compliant printing
    #pret_list(hands_list)
    s = ""
    for h in hands_list:
        s += format_hand(h) + "\n"
    for x in step_list:
        s += format_step(x, hand_count) + "\n"
    with open("test_{}".format(score), "w") as f:
        f.write(s)
    print(s)

if __name__ == "__main__":
  runner = Runner()
  runner.run()
