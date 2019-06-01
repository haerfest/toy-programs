#!/usr/bin/env python3
#
# Uses Q-learning to solve the "Taxi Problem".
#
# Taxi problem:
#  https://raw.githubusercontent.com/openai/gym/master/gym/envs/toy_text/taxi.py
#
# Solution:
#   https://youtu.be/q2ZOEFAaaI0


import gym
import numpy as np
import random

from tqdm import tqdm           # Provides a progress bar.


def train(env, qtable, episodes=5000, max_steps=100, eps=1.0, min_eps=0.01, max_eps=1.0, lr=0.7, gamma=0.6, decay_rate=0.01):
    '''
    Trains the computer to play the taxi game, by building a table that lists
    for all possible environment states (rows) and all possible actions that
    the computer can take (columns), which future reward can be expected.
    Initially the computer knows nothing about this, but as it takes random
    steps (exploration), it discovers what rewards each action brings. Over
    time it will explore less, and exploit this knowledge more, thereby
    becoming better at playing the game.

    Parameters:

        env
            The game environment.

        qtable
            The #states x #actions table.

        episodes
            The number of episodes to train for.

        max_steps
            The maximum number of steps allowed to finish an episode.

        eps
            A threshold between 0 and 1 that governs whether the computer will
            take a random action (exploration) or utilize the information in
            the table as learned so far (exploitation). A value closer to one
            favors exploration, a value closer to zero exploitation.

        min_eps
            The minimum value allowed for eps.

        max_eps
            The maximumv value allowed for eps.

        lr
            A learning rate between 0 and 1 that governs how quickly the
            computer blends in the reward from an action it takes, into what it
            has learned so far. A value of zero means it does not take heed of
            any new rewards, so in effect does not learn anything. A value of
            one means that with every step it forgets what it has learned and
            completely follows the rewards it gets.

        gamma
            A value between 0 and 1 that governs how much of future expected
            rewards is taken into account. A value of zero means only the
            immediate reward given by an action is used.

        decay_rate
            A value between 0 and 1 that governs how quickly the eps parameter
            is reduced to min_eps. In effect it controls how quickly the
            computer switches from exploration to exploitation.
    '''
    for episode in tqdm(range(episodes), 'Training'):
        state = env.reset()

        for _ in range(max_steps):
            # Choose between exploitation and exploration.
            if random.uniform(0, 1) > eps:
                action = np.argmax(qtable[state, :])
            else:
                action = env.action_space.sample()

            new_state, reward, done, _ = env.step(action)

            # Bellman equation.
            qtable[state, action] = lr * (reward + gamma * np.max(qtable[new_state, :])) + (1 - lr) * qtable[state, action]

            if done:
                break

            state = new_state

        # Move from exploration to exploitation.
        eps = min_eps + (max_eps - min_eps) * np.exp(-decay_rate * episode)


def test(env, qtable, episodes=100, max_steps=100):
    '''
    Plays a few episodes of the taxi game and outputs how well the computer did
    on average.

    Parameters: see the train() function.
    '''
    rewards = []

    for _ in tqdm(range(episodes), 'Testing'):
        state = env.reset()

        rewards.append(0)
        for _ in range(max_steps):
            # Choose action that gives the highest reward.
            action = np.argmax(qtable[state, :])

            state, reward, done, _ = env.step(action)

            rewards[-1] += reward
            if done:
                break

    print(f'Average test score over {episodes} episodes: {np.average(rewards)}')


def play(env, qtable, max_steps=100):
    '''
    Plays a single episode of the taxi game and outputs how well the computer
    did.

    Parameters: see the train() function.
    '''
    state = env.reset()
    env.render()

    score = 0
    for step in range(max_steps):
        # Choose action that gives the highest reward.
        action = np.argmax(qtable[state, :])

        state, reward, done, _ = env.step(action)
        env.render()

        score += reward
        if done:
            break

    print(f'Scored {score} in {step} steps')


def main():
    random.seed()

    with gym.make('Taxi-v2') as env:
        qtable = np.zeros((env.observation_space.n, env.action_space.n))

        train(env, qtable)
        test(env, qtable)

        while input('Show an example game? [Y/n] ') in ['y', 'Y', '']:
            play(env, qtable)


if __name__ == '__main__':
    main()
