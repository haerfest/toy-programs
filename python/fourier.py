#!/usr/bin/env python3
#
# Fourier transformation example.
#
# Output:
#
# Sine wave of 3 Hz:
#
#   Hz  Strength
#   ------------
#    0         0
#    1         0
#    2         0
#    3       500
#    4         0
#    5         0
#    6         0
#    7         0
#    8         0
#    9         0
#
# Combined sine waves of 2 Hz and 5 Hz:
#
#   Hz  Strength
#   ------------
#    0         0
#    1         0
#    2       500
#    3         0
#    4         0
#    5       250
#    6         0
#    7         0
#    8         0
#    9         0


import cmath
import math
import numpy as np


def sine_wave(frequency, t):
    return math.sin(frequency * 2 * math.pi * t)


def sine_wave_3hz(t):
    return sine_wave(3, t)


def combined_sine_waves_2hz_5hz(t):
    # The 5 Hz sine wave is half as loud as the 2 Hz one.
    return sine_wave(2, t) + 0.5 * sine_wave(5, t)


def fourier(signal, test_frequency, listen_duration=1, sample_count=1000):
    # ----------------------------------------------------------
    # This is literally the Fourier transformation formula:
    #   F(f) = integrate g(t) * e^(0 + -2*pi*f*t*i)
    # ----------------------------------------------------------
    return sum(
        signal(t) * cmath.exp(complex(0, -2 * math.pi * test_frequency * t))
        for t in np.linspace(0, listen_duration, sample_count))


def demo(signal, description):
    print(f"\n{description}:\n")
    print('  Hz  Strength')
    print('  ------------')

    for test_frequency in range(10):
        strength, _ = cmath.polar(fourier(signal, test_frequency))
        print(f'  {test_frequency:2d}  {strength:8.0f}')


if __name__ == '__main__':
    demo(sine_wave_3hz, 'Sine wave of 3 Hz')
    demo(combined_sine_waves_2hz_5hz, 'Combined sine waves of 2 Hz and 5 Hz')
