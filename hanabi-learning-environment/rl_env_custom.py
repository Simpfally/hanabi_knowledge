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

import sys
import getopt
import rl_env
from agents.random_agent import RandomAgent
from agents.simple_agent import SimpleAgent

AGENT_CLASSES = {'SimpleAgent': SimpleAgent, 'RandomAgent': RandomAgent}

#return a list of each player's card
def get_list_cards(obs):
    cur_ply = obs["current_player"]
    obs = obs["player_observations"]
    nply = len(obs)
    assert(nply > 1)
    first_hand = obs[(cur_ply+1) % nply]["observed_hands"][-1] # this return the obs of the hand of player 0 from the PoV of player 1, that is full knowledge
    # hands in observations are ordered by who's playing next, 0 : you 1: the next to play etc..
    # that's why the first player is the last to player (-1) for the second player
    other_hands = obs[cur_ply]["observed_hands"][1:] # return all others hands

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

    cur_ply = obs["current_player"]
    obs = obs["player_observations"]
    # player x has played or discarded
    # and is now the last player to play for the next player
    ply_x_plus_one = obs[cur_ply]
    return ply_x_plus_one["observed_hands"][-1][-1] # new card is actually the last one
    # and btw DISCARD 5 would actually mean play the newest card then (in the environement)


def format_color(col):
    col_dic = { 'R' : 1,
            'Y' : 2,
            'G' : 3,
            'B' : 4,
            'W' : 5}
    scol = col
    if col in col_dic:
        scol = col_dic[col]
    return str(scol)
def format_rank(ran):
    return str(int(ran) + 1)

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

    return s + " " + str(hand_count - step["card_index"] ) + " " + format_card(step["new_card"])

def pret_list(l):
    for x in l:
        print(">",x)



class Runner(object):
  """Runner class."""

  def __init__(self, flags):
    """Initialize runner."""
    self.flags = flags
    self.agent_config = {'players': flags['players']}
    self.environment = rl_env.make('Hanabi-Full', num_players=flags['players'])
    self.agent_class = AGENT_CLASSES[flags['agent_class']]

  def run(self):
    """Run episodes."""
    rewards = []
    for episode in range(flags['num_episodes']):
      observations = self.environment.reset() # Full game observation, not to be passed to agents


      ### Stage-compliant printing related
      # observations["player_observations"] has an element for every player

      #pret_list(observations["player_observations"][0]["observed_hands"])
      #pret_list(observations["player_observations"][1]["observed_hands"])
      hands_list = get_list_cards(observations)
      hand_count = len(hands_list[0])
      step_list = []
      ###

      agents = [self.agent_class(self.agent_config)
                for _ in range(self.flags['players'])]
      done = False
      episode_reward = 0
      while not done:
        for agent_id, agent in enumerate(agents):
          print("agentid", agent_id)
          observation = observations['player_observations'][agent_id]
          action = agent.act(observation)
          if observation['current_player'] == agent_id:
            assert action is not None
            current_player_action = action
          else:
            assert action is None
        # Make an environment step.
        #print('Agent: {} action: {}'.format(observation['current_player'],
        #                                    current_player_action))
        i = 0
        hands_list = get_list_cards(observations)
       # for h in hands_list:
       #     i += 1
       #     print(i, format_hand(h))
        observations, reward, done, unused_info = self.environment.step(
            current_player_action)
        episode_reward += reward
        ### Finding the new card... ###
        cpa = current_player_action["action_type"]
        cpacopy = current_player_action.copy()
        if cpa == "PLAY" or cpa == "DISCARD":
            cpacopy["new_card"] = find_new_card(observations)
            #print("new:",format_card(new_card))
        ### Stage compliant printing
        #print(format_step(cpacopy, hand_count))
        step_list.append(cpacopy)
        ###
        ###
      rewards.append(episode_reward)
      #print('Running episode: %d' % episode)
      #print('Max Reward: %.3f' % max(rewards))

      ### Stage compliant printing
      #pret_list(hands_list)
      for h in hands_list:
          print(format_hand(h))
      for s in step_list:
          print(format_step(s, hand_count))
      
      ###
    return rewards

if __name__ == "__main__":
  flags = {'players': 2, 'num_episodes': 1, 'agent_class': 'SimpleAgent'}
  options, arguments = getopt.getopt(sys.argv[1:], '',
                                     ['players=',
                                      'num_episodes=',
                                      'agent_class='])
  if arguments:
    sys.exit('usage: rl_env_example.py [options]\n'
             '--players       number of players in the game.\n'
             '--num_episodes  number of game episodes to run.\n'
             '--agent_class   {}'.format(' or '.join(AGENT_CLASSES.keys())))
  for flag, value in options:
    flag = flag[2:]  # Strip leading --.
    flags[flag] = type(flags[flag])(value)
  runner = Runner(flags)
  runner.run()
