#!/usr/bin/env python3
#
# Fourier transformation example.
#
# Output:
#
# Hz  Strength
# ------------
#  0     0.000
#  1     0.000
#  2     0.000
#  3   499.500
#  4     0.000
#  5     0.000
#  6     0.000
#  7     0.000
#  8     0.000
#  9     0.000


import cmath
import math
import numpy as np


def sine_wave(frequency, t):
    return math.sin(frequency * 2 * math.pi * t)


def sine_wave_3hz(t):
    return sine_wave(3, t)


def fourier(signal, test_frequency, listen_duration=1, sample_count=1000):
    # ----------------------------------------------------------
    # This is literally the Fourier transformation formula:
    #   F(f) = integrate g(t) * e^(0 + -2*pi*f*t*i)
    # ----------------------------------------------------------
    return sum(
        signal(t) * cmath.exp(complex(0, -2 * math.pi * test_frequency * t))
        for t in np.linspace(0, listen_duration, sample_count))


if __name__ == '__main__':
    print('Hz  Strength')
    print('------------')

    for test_frequency in range(10):
        strength, _ = cmath.polar(fourier(sine_wave_3hz, test_frequency))
        print(f'{test_frequency:2d}  {strength:8.3f}')

