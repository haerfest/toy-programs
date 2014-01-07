#!/usr/bin/env python

import sys
import cv2

def play(video_file):
    WINDOW_TITLE  = 'video'
    ESCAPE_KEY    = 27

    video = cv2.VideoCapture(video_file)
    fps   = video.get(cv2.cv.CV_CAP_PROP_FPS)
    delay = int(1000 / fps)

    cv2.namedWindow(WINDOW_TITLE)

    while True:
        _, frame = video.read()
        if frame is None:
            break
        cv2.imshow(WINDOW_TITLE, frame)
        if cv2.waitKey(delay) == ESCAPE_KEY:
            break

if __name__ == "__main__":
    play(sys.argv[1])

