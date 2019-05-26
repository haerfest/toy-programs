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

from tqdm import tqdm


def train(env, qtable, episodes=50000, steps=100, eps=1.0, min_eps=0.01, max_eps=1.0, lr=0.7, gamma=0.6, decay_rate=0.01):
    for episode in tqdm(range(episodes), 'Training'):
        state = env.reset()

        for _ in range(steps):
            # Choose between exploitation and exploration.
            if random.uniform(0, 1) > eps:
                action = np.argmax(qtable[state, :])
            else:
                action = env.action_space.sample()

            new_state, reward, done, _ = env.step(action)

            # Bellman equation.
            qtable[state, action] += lr * \
                (reward + gamma *
                 np.max(qtable[new_state, :]) - qtable[state, action])

            if done:
                break

            state = new_state

        # Move from exploration to exploitation.
        eps = min_eps + (max_eps - min_eps) * np.exp(-decay_rate * episode)


def test(env, qtable, episodes=100, steps=100):
    rewards = []

    for _ in tqdm(range(episodes), 'Testing'):
        state = env.reset()

        rewards.append(0)
        for _ in range(steps):
            # Choose action that gives the highest reward.
            action = np.argmax(qtable[state, :])

            state, reward, done, _ = env.step(action)

            rewards[-1] += reward
            if done:
                break

    print(f'Average test score over {episodes} episodes: {np.average(rewards)}')


def play(env, qtable, steps=100):
    state = env.reset()
    env.render()

    score = 0
    for step in range(steps):
        # Choose action that gives the highest reward.
        action = np.argmax(qtable[state, :])

        state, reward, done, _ = env.step(action)
        env.render()

        score += reward
        if done:
            break

    print(f'Scored {score} in {step} steps')


def main():
    with gym.make('Taxi-v2') as env:
        qtable = np.zeros((env.observation_space.n, env.action_space.n))

        train(env, qtable)
        test(env, qtable)

        while input('Show an example game? [Y/n] ') in ['y', 'Y', '']:
            play(env, qtable)

        env.close()


if __name__ == '__main__':
    main()
