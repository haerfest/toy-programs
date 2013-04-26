#include <iostream>
#include <opencv2/opencv.hpp>


using namespace std;
using namespace cv;


// Forward declarations.
static void playVideo (const string video_file, const unsigned int start_seconds);


// Main program.
int main (int argc, char *argv[])
{
  // We need a video file as the first parameter.
  if (argc == 1) {
    cerr << "Usage: " << argv[0] << " <video-file>" << endl;
    return 1;
  }

  // And optionally a seek time as the second position.
  const unsigned int start_seconds = (argc == 3 ? atoi(argv[2]) : 0);

  // Play the video.
  playVideo(argv[1], start_seconds);

  return 0;
}


// Plays a video file.
static void playVideo (const string video_file, const unsigned int start_seconds = 0)
{
  VideoCapture capture(video_file);
  
  if (!capture.isOpened()) {
    cerr << "Could not open video file " << video_file << endl;
    return;
  }

  // Retrieve the frame rate.
  const double frame_rate        = capture.get(CV_CAP_PROP_FPS);
  const int    inter_frame_delay = 1000 / frame_rate;

  // Seek to a startion position, if we have one.
  if (start_seconds > 0) {
    (void) capture.set(CV_CAP_PROP_POS_MSEC, start_seconds * 1000);
  }

  // Show the video.
  const string window_title = "Video";
  const int    escape_key   = 27;
  Mat          image;
  Mat          colored_image;
  
  while (capture.read(image)) {
    applyColorMap(image, colored_image, COLORMAP_JET);
    imshow(window_title, colored_image);
    if (waitKey(inter_frame_delay) == escape_key) {
      break;
    }
  }

  // Release the video.
  capture.release();
}
