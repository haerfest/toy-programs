#!/usr/bin/env python
#
# Gracefully based on:
#
# https://github.com/jesolem/PCV/blob/master/examples/lktrack.py
# http://robots.stanford.edu/cs223b05/notes/CS%20223-B%20T1%20stavens_opencv_optical_flow.pdf

import sys
import cv2
import numpy as np
import math

def draw_point(image, p, color):
    cv2.circle(image, (int(p[0][0]),int(p[0][1])), 3, color, -1)

def canny(video_file, start_sec, speed, lower_threshold, higher_threshold):
    NO_KEY       = -1
    QUIT_KEY     = 27   # escape
    PAUSE_KEY    = 32   # space
    SLOWER_KEY   = 45   # -
    FASTER_KEY   = 61   # =
    FEATURES_KEY = 10   # enter
    STATS_KEY    = 115  # s
    RED_COLOR   = (0, 0, 255)
    GREEN_COLOR = (0, 255, 0)
    BLUE_COLOR  = (255, 0, 0)
    DILATE_SIZE = 10
    ERODE_SIZE  = 10
    MAX_FEATURES = 100
    MIN_DISTANCE = 10

    lk_params = dict(winSize  = (15,15),
                     maxLevel = 2,
                     criteria = (cv2.TERM_CRITERIA_EPS | cv2.TERM_CRITERIA_COUNT, 10, 0.03)) 

    subpix_params = dict(zeroZone = (-1,-1),
                         winSize  = (10,10),
                         criteria = (cv2.TERM_CRITERIA_COUNT | cv2.TERM_CRITERIA_EPS, 20, 0.03))

    feature_params = dict(maxCorners   = MAX_FEATURES,
                          qualityLevel = 0.01,
                          minDistance  = MIN_DISTANCE)

    # Open the video and determine the frame rate.
    video         = cv2.VideoCapture(video_file)
    fps           = video.get(cv2.cv.CV_CAP_PROP_FPS)
    default_delay = 1000 / (fps * speed)
    delay         = int(default_delay)

    # Seek to a particular time stamp if desired.
    if start_sec > 0:
        video.set(cv2.cv.CV_CAP_PROP_POS_MSEC, start_sec * 1000)

    prev_gray    = None
    new_features = True

    stop = False
    while (not stop):
        # Read a frame as grayscale and downsample.
        _, frame = video.read()
        if frame is None:
            break
        gray = cv2.cvtColor(frame, cv2.cv.CV_RGB2GRAY)
        cv2.imshow('input', gray)

        if new_features:
            features = cv2.goodFeaturesToTrack(gray, **feature_params)
        else:
            prev_features_reshaped = np.float32(prev_features).reshape(-1, 1, 2)
            features, found, _ = cv2.calcOpticalFlowPyrLK(prev_gray, gray, prev_features_reshaped, None, **lk_params)

            found_count = 0
            for i in xrange(len(features)):
                if found[i]:
                    draw_point(frame, prev_features[i], GREEN_COLOR)
                    draw_point(frame, features[i],      RED_COLOR)
                    found_count += 1

            cv2.imshow('feature', frame)

            if found_count < 2 * MAX_FEATURES / 3:
                features = cv2.goodFeaturesToTrack(gray, **feature_params)
                print "Found %u features, new found %u" % (found_count, len(features))

        prev_gray     = gray
        prev_features = features

        # Delay to achieve the desired frame rate, and handle key presses.
        pause = False
        new_features = False
        while True:
            key = cv2.waitKey(delay)
            if key == NO_KEY:
                if not pause:
                    break
            elif key == QUIT_KEY:
                stop = True
                break
            elif key == PAUSE_KEY:
                pause = not pause
                if pause:
                    print "Paused"
                else:
                    print "Playing"
            elif key == FASTER_KEY:
                delay = delay / 2
                print "Speed is %.2fx" % (default_delay / delay)
            elif key == SLOWER_KEY:
                delay = delay * 2
                print "Speed is %.2fx" % (default_delay / delay)
            elif key == FEATURES_KEY:
                new_features = True
                print "New features"
            elif key == STATS_KEY:
                print "Number of features: %u" % len(features)

if __name__ == "__main__":
    canny(sys.argv[1], 90, 1.0, 0, 255)

