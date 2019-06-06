#!/usr/bin/env python3
#
# Trims an image.


import numpy as np
import os.path
import PIL.Image
import sys


def find_border(image, row, column, dr, dc):
    background_color = image[row, column]
    while True:
        row    += dr
        column += dc
        if (image[row, column] != background_color).any():
            break

    return row, column


def find_all_borders(image):
    rows, columns, _ = image.shape
    middle_row       = rows    // 2
    middle_column    = columns // 2

    first_column = 0
    last_column  = columns - 1

    first_row = 0
    last_row  = rows - 1

    left, right = -1, +1
    up, down    = -1, +1
    stay        = 0

    _, left_column  = find_border(image, middle_row, first_column,  stay, right)
    _, right_column = find_border(image, middle_row, last_column,   stay, left)
    top_row, _      = find_border(image, first_row,  middle_column, down, stay)
    bottom_row, _   = find_border(image, last_row,   middle_column, up,   stay)

    return top_row, left_column, bottom_row, right_column


def trim(filepath):
    original = np.array(PIL.Image.open(filepath))

    top_row, left_column, bottom_row, right_column = find_all_borders(original)
    cropped = original[top_row:bottom_row+1, left_column:right_column+1]

    new_filepath, ext = os.path.splitext(filepath)
    new_filepath += '_cropped' + ext

    PIL.Image.fromarray(cropped).save(new_filepath, 'JPEG', quality=95)


def main():
    for filepath in sys.argv[1:]:
        trim(filepath)


if __name__ == '__main__':
    main()
