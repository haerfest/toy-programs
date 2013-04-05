#!/usr/bin/env ruby
#
# Plays back a video in a window.

require "opencv"

def escape_pressed?(wait_msec)
  OpenCV::GUI::wait_key(wait_msec) == 27
end

def play_video(video_file, fps = 3)
  wait_delay = 100 / [1, fps].max

  window = OpenCV::GUI::Window.new('video')
  capture = OpenCV::CvCapture.open(video_file)

  while not escape_pressed?(wait_delay) and capture.grab
    frame = capture.retrieve
    break unless frame
    window.show(frame)
  end
end

video_file = ARGV.shift
abort "Usage: #{$PROGRAM_NAME} <video-file>" unless video_file

play_video(video_file)
