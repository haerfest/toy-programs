#!/usr/bin/env python

import sys
import cv2
import numpy as np

def canny(video_file, start_sec, speed, lower_threshold, higher_threshold):
    ESCAPE_KEY = 27

    video = cv2.VideoCapture(video_file)
    fps   = video.get(cv2.cv.CV_CAP_PROP_FPS)
    delay = int(1000 / (fps * speed))

    if start_sec > 0:
        video.set(cv2.cv.CV_CAP_PROP_POS_MSEC, start_sec * 1000)

    _, frame = video.read()
    average = np.float32(frame)
        
    while True:
        _, frame = video.read()
        if frame is None:
            break

        cv2.imshow('input', frame)

        cv2.accumulateWeighted(frame, average, 0.01)
        background = cv2.convertScaleAbs(average)
        cv2.imshow('background', background)

        foreground = cv2.subtract(frame, background)
        cv2.imshow('foreground', foreground)

        grayscale = cv2.cvtColor(foreground, cv2.cv.CV_BGR2GRAY)
        edges = cv2.Canny(grayscale, lower_threshold, higher_threshold)
        cv2.imshow('canny', edges)

        if cv2.waitKey(delay) == ESCAPE_KEY:
            break

if __name__ == "__main__":
    canny(sys.argv[1], 14 * 60, 1.0, 0, 255)

