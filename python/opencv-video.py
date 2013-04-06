#!/usr/bin/env python

import sys
import cv

def play(video_file):
    WINDOW_TITLE  = 'video'
    ESCAPE_KEY    = 27

    video  = cv.CaptureFromFile(video_file)
    fps    = cv.GetCaptureProperty(video, cv.CV_CAP_PROP_FPS)
    delay  = int(1000 / fps)

    while True:
        frame = cv.QueryFrame(video)
        if frame is None:
            break
        cv.ShowImage(WINDOW_TITLE, frame)
        if cv.WaitKey(delay) == ESCAPE_KEY:
            break

if __name__ == "__main__":
    play(sys.argv[1])

