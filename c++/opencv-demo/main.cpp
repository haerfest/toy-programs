#include <iostream>
#include <opencv2/opencv.hpp>


using namespace std;
using namespace cv;


// Forward declarations.
static void playVideo (const string video_file);


// Main program.
int main (int argc, char *argv[])
{
  // We need a video file as the only parameter.
  if (argc != 2) {
    cerr << "Usage: " << argv[0] << " <video-file>" << endl;
    return 1;
  }

  // Play the video.
  playVideo(argv[1]);

  return 0;
}


// Plays a video file.
static void playVideo (const string video_file)
{
  VideoCapture capture(video_file);
  
  if (!capture.isOpened()) {
    cerr << "Could not open video file " << video_file << endl;
    return;
  }

  // Retrieve the frame rate.
  const double frame_rate        = capture.get(CV_CAP_PROP_FPS);
  const int    inter_frame_delay = 1000 / frame_rate;
  
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


