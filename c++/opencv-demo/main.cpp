#include <iostream>
#include <algorithm>
#include <vector>
#include <opencv2/opencv.hpp>


using namespace std;
using namespace cv;


// Defines.
#define MAX_GAUSSIANS_PER_PIXEL          3
#define NEW_GAUSSIAN_STANDARD_DEVIATION  2
#define NEW_GAUSSIAN_WEIGHT              0.000001
#define LEARNING_RATE                    0.001
#define PI                               3.14159265359
#define T                                0.6


// Types.
typedef struct {
  double mean;
  double standard_deviation;
  double weight;
} Gaussian;

typedef vector<Gaussian*> GaussianMixture;


// Forward declarations.
static void          addNewGaussian (GaussianMixture* gaussians, const unsigned char pixel);
static void          adjustWeights (GaussianMixture* gaussians, const int match_index = -1);
static double        calculateGaussianProbability (const GaussianMixture* gaussians, const unsigned char pixel);
static bool          compareGaussiansDecreasingByWeight (const Gaussian* a, const Gaussian* b);
static bool          compareGaussiansDecreasingByWeightOverVariance (const Gaussian *a, const Gaussian *b);
static void          deleteLeastProbableGaussian (GaussianMixture* gaussians);
static bool          findMatchingGaussian (const unsigned char pixel, const GaussianMixture* gaussians, int* match_index);
static void          normalizeWeights (GaussianMixture *gaussians);
static void          playVideo (const string video_file, const unsigned int start_seconds);
static unsigned char selectGaussiansForBackgroundModel (GaussianMixture* gaussians);
static void          updateMatchingGaussian (GaussianMixture* gaussian, const int match_index, const unsigned char pixel);


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
  const int       escape_key = 27;
  GaussianMixture gaussian_mixture[image.rows][image.cols];
  Mat             background_model(image.rows, image.cols, CV_8UC1);
  
  while (capture.read(image)) {
    Mat grayscale_image;
    cvtColor(image, grayscale_image, CV_RGB2GRAY);
    imshow("Input:grayscale", grayscale_image);

    Mat colored_image;
    applyColorMap(image, colored_image, COLORMAP_JET);
    imshow("Input:colormap", colored_image);

    for (int row = 0; row < image.rows; row++) {
      for (int col = 0; col < image.cols; col++) {
        GaussianMixture     *gaussians  = &gaussian_mixture[row][col];
        const unsigned char  pixel      = grayscale_image.at<unsigned char>(row, col);
        int                  match_index;
        const bool           foundMatch = findMatchingGaussian(pixel, gaussians, &match_index);

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
          updateMatchingGaussian(gaussians, match_index, pixel);
        }

        background_model.at<unsigned char>(row, col) = selectGaussiansForBackgroundModel(gaussians);
      }
    }
    imshow("Background:grayscale", background_model);

    Mat colored_background_model;
    applyColorMap(background_model, colored_background_model, COLORMAP_JET);
    imshow("Background:colormap", colored_background_model);

    if (waitKey(inter_frame_delay) == escape_key) {
      break;
    }
  }

  // Release the video.
  capture.release();
}


static bool findMatchingGaussian (const unsigned char pixel, const GaussianMixture* gaussians, int* match_index) {
  for (int index = 0; index < gaussians->size(); index++) {
    const Gaussian *gaussian = (*gaussians)[index];      
    if (abs(gaussian->mean - pixel) <= 2 * gaussian->standard_deviation) {
      *match_index = index;
      return true;
    }
  }

  return false;
}


static void deleteLeastProbableGaussian (GaussianMixture* gaussians) {
  if (gaussians->size() == MAX_GAUSSIANS_PER_PIXEL) {
    sort(gaussians->begin(), gaussians->end(), compareGaussiansDecreasingByWeight);
    delete (*gaussians)[MAX_GAUSSIANS_PER_PIXEL - 1];
    gaussians->pop_back();
  }    
}


static bool compareGaussiansDecreasingByWeight (const Gaussian* a, const Gaussian* b) {
  return (a->weight > b->weight);
}


static bool compareGaussiansDecreasingByWeightOverVariance (const Gaussian *a, const Gaussian *b) {
  const double ratio_a = a->weight / (a->standard_deviation * a->standard_deviation);
  const double ratio_b = b->weight / (b->standard_deviation * b->standard_deviation);

  return (ratio_a > ratio_b);
}


static void addNewGaussian (GaussianMixture* gaussians, const unsigned char pixel) {
  Gaussian* gaussian = new Gaussian();

  gaussian->mean               = pixel;
  gaussian->standard_deviation = NEW_GAUSSIAN_STANDARD_DEVIATION;
  gaussian->weight             = NEW_GAUSSIAN_WEIGHT;

  gaussians->push_back(gaussian);
}


static void adjustWeights (GaussianMixture* gaussians, const int match_index) {
  double sum = 0;

  for (int index = 0; index < gaussians->size(); index++) {
    Gaussian *gaussian = (*gaussians)[index];

    if (index == match_index) {
      gaussian->weight = (1 - LEARNING_RATE) * gaussian->weight + LEARNING_RATE;
    } else {
      gaussian->weight = (1 - LEARNING_RATE) * gaussian->weight;
    }
  }

  normalizeWeights(gaussians);
}


static void normalizeWeights (GaussianMixture *gaussians) {
  double sum = 0;

  for (int index = 0; index < gaussians->size(); index++) {
    Gaussian *gaussian = (*gaussians)[index];
    sum += gaussian->weight;
  }

  for (int index = 0; index < gaussians->size(); index++) {
    Gaussian *gaussian = (*gaussians)[index];
    gaussian->weight /= sum;
  }
}


static void updateMatchingGaussian (GaussianMixture* gaussians, const int match_index, const unsigned char pixel) {
  Gaussian*    gaussian = (*gaussians)[match_index];
  const double rho      = LEARNING_RATE * calculateGaussianProbability(gaussians, pixel);
  const double variance = (1 - rho) * gaussian->standard_deviation * gaussian->standard_deviation + rho * (pixel - gaussian->mean) * (pixel - gaussian->mean);
    
  gaussian->mean               = (1 - rho) * gaussian->mean + rho * pixel;
  gaussian->standard_deviation = sqrt(variance);
}


static double calculateGaussianProbability (const GaussianMixture* gaussians, const unsigned char pixel) {
  double probability = 0;

  for (int i = 0; i < gaussians->size(); i++) {
    const Gaussian *gaussian = (*gaussians)[i];
    const double    variance = gaussian->standard_deviation * gaussian->standard_deviation;
    probability += exp(-0.5 * (pixel - gaussian->mean) * (1 / variance) * (pixel - gaussian->mean)) / (sqrt(2 * PI) * gaussian->standard_deviation);
  }

  return probability;
}


static unsigned char selectGaussiansForBackgroundModel (GaussianMixture* gaussians) {
  sort(gaussians->begin(), gaussians->end(), compareGaussiansDecreasingByWeightOverVariance);

  double weights_summed = 0;
  for (int i = 0; i < gaussians->size(); i++) {
    const Gaussian *gaussian = (*gaussians)[i];
    weights_summed += gaussian->weight;
  }

  int    b                      = 0;
  double first_b_weights_summed = 0;
  while (b < gaussians->size() && first_b_weights_summed / weights_summed <= T) {
    const Gaussian *gaussian = (*gaussians)[b];
    first_b_weights_summed += gaussian->weight;
    b++;
  }

  double pixel = 0;
  for (int i = 0; i < b; i++) {
    const Gaussian* gaussian = (*gaussians)[i];
    pixel += gaussian->weight * gaussian->mean;
  }

  return (unsigned char) pixel;
}


static unsigned char calculateBackgroundPixel (const GaussianMixture* gaussians) {
  unsigned char pixel = 0;

  for (int i = 0; i < gaussians->size(); i++) {
    const Gaussian* gaussian = (*gaussians)[i];
    pixel += gaussian->weight * gaussian->mean;
  }

  return pixel;
}
