#include <iostream>
#include <algorithm>
#include <vector>
#include <opencv2/opencv.hpp>


using namespace std;
using namespace cv;


// Defines.
#define MAX_GAUSSIANS_PER_PIXEL          3
#define NEW_GAUSSIAN_STANDARD_DEVIATION  25
#define NEW_GAUSSIAN_WEIGHT              0.01
#define LEARNING_RATE                    0.01
#define PI                               3.14159265359


// Types.
typedef struct {
  double mean;
  double standard_deviation;
  double weight;
} Gaussian;

typedef vector<Gaussian*> GaussianMixture;


// Forward declarations.
static void   playVideo (const string video_file, const unsigned int start_seconds);
static bool   findMatchingGaussian (const unsigned char pixel, const GaussianMixture gaussians, int& match_index);
static void   deleteLeastProbableGaussian (GaussianMixture &gaussians);
static bool   compareGaussiansDecreasingByWeight (const Gaussian* a, const Gaussian* b);
static void   addNewGaussian (GaussianMixture& gaussians, const unsigned char pixel);
static void   adjustWeights (GaussianMixture& gaussians, const int match_index = -1);
static void   updateMatchingGaussian (Gaussian* gaussians, const unsigned char pixel);
static double calculateGaussian (const Gaussian* gaussian, const unsigned char pixel);


// Main program.
int main (int argc, char *argv[]) {
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
static void playVideo (const string video_file, const unsigned int start_seconds = 0) {
  Mat          image;
  VideoCapture capture(video_file);
  
  if (!capture.isOpened()) {
    cerr << "Could not open video file " << video_file << endl;
    return;
  }

  // Retrieve the frame rate.
  const double frame_rate        = capture.get(CV_CAP_PROP_FPS);
  const int    inter_frame_delay = 1000 / frame_rate;

  // Read the first frame to know the image size.
  if (!capture.read(image)) {
    cerr << "Could not read a frame from the video" << endl;
    return;
  }
    
  // Seek to the startion position.
  (void) capture.set(CV_CAP_PROP_POS_MSEC, start_seconds * 1000);

  // Show the video.
  const int       escape_key   = 27;
  GaussianMixture gaussian_mixture[image.rows][image.cols];
  
  while (capture.read(image)) {
    Mat grayscale_image;
    cvtColor(image, grayscale_image, CV_RGB2GRAY);
    imshow("Grayscale", grayscale_image);

    Mat colored_image;
    applyColorMap(image, colored_image, COLORMAP_JET);
    imshow("Colormap", colored_image);

    for (int row = 0; row < image.rows; row++) {
      for (int col = 0; col < image.cols; col++) {
        GaussianMixture     gaussians  = gaussian_mixture[row][col];
        const unsigned char pixel      = grayscale_image.at<unsigned char>(row, col);
        int                 match_index;
        const bool          foundMatch = findMatchingGaussian(pixel, gaussians, match_index);

        if (!foundMatch) {
          deleteLeastProbableGaussian(gaussians);
          addNewGaussian(gaussians, pixel);
          adjustWeights(gaussians, match_index);
        }

        if (foundMatch) {
          adjustWeights(gaussians, match_index);
        } else {
          adjustWeights(gaussians);
        }

        if (foundMatch) {
          updateMatchingGaussian(gaussians[match_index], pixel);
        }
      }
    }
    
    if (waitKey(inter_frame_delay) == escape_key) {
      break;
    }
  }

  // Release the video.
  capture.release();
}


static bool findMatchingGaussian (const unsigned char pixel, const GaussianMixture gaussians, int& match_index) {
  for (int index = 0; index < gaussians.size(); index++) {
    if (abs(gaussians[index]->mean - pixel) <= 2 * gaussians[index]->standard_deviation) {
      return index;
    }
  }

  return -1;
}


static void deleteLeastProbableGaussian (GaussianMixture& gaussians) {
  if (gaussians.size() == MAX_GAUSSIANS_PER_PIXEL) {
    sort(gaussians.begin(), gaussians.end(), compareGaussiansDecreasingByWeight);
    delete gaussians[MAX_GAUSSIANS_PER_PIXEL - 1];
    gaussians.pop_back();
  }    
}


static bool compareGaussiansDecreasingByWeight (const Gaussian* a, const Gaussian* b) {
  return (a->weight > b->weight);
}


static void addNewGaussian (GaussianMixture& gaussians, const unsigned char pixel) {
  Gaussian* gaussian = new Gaussian();

  gaussian->mean               = pixel;
  gaussian->standard_deviation = NEW_GAUSSIAN_STANDARD_DEVIATION;
  gaussian->weight             = NEW_GAUSSIAN_WEIGHT;

  gaussians.push_back(gaussian);
}


static void adjustWeights (GaussianMixture& gaussians, const int match_index) {
  double sum = 0;

  for (int index = 0; index < gaussians.size(); index++) {
    Gaussian *gaussian = gaussians[index];

    if (index == match_index) {
      gaussian->weight = (1 - LEARNING_RATE) * gaussian->weight + LEARNING_RATE;
    } else {
      gaussian->weight = (1 - LEARNING_RATE) * gaussian->weight;
    }

    sum += gaussian->weight;
  }

  for (int index = 0; index < gaussians.size(); index++) {
    gaussians[index]->weight /= sum;
  }
}


static void updateMatchingGaussian (Gaussian* gaussian, const unsigned char pixel) {
  const double rho      = LEARNING_RATE * calculateGaussian(gaussian, pixel);
  const double variance = (1 - rho) * (gaussian->standard_deviation * gaussian->standard_deviation) + rho * (pixel - gaussian->mean) * (pixel - gaussian->mean);
    
  gaussian->mean               = (1 - rho) * gaussian->mean + rho * pixel;
  gaussian->standard_deviation = sqrt(variance);
}


static double calculateGaussian (const Gaussian* gaussian, const unsigned char pixel) {
  const double variance = gaussian->standard_deviation * gaussian->standard_deviation;
  
  return exp(-0.5 * (pixel - gaussian->mean) * (1 / variance) * (pixel - gaussian->mean)) / (sqrt(2 * PI) * gaussian->standard_deviation);
}
