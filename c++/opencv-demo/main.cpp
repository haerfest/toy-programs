#include <iostream>
#include <iomanip>
#include <algorithm>
#include <vector>
#include <opencv2/opencv.hpp>


using namespace std;
using namespace cv;


// Defines.
#define MAX_GAUSSIANS_PER_PIXEL          3
#define FONT                             FONT_HERSHEY_PLAIN
#define NEW_GAUSSIAN_STANDARD_DEVIATION  5             /* New Gaussians have a large standard deviation. */
#define NEW_GAUSSIAN_WEIGHT              1E-300
#define LEARNING_RATE                    0.01
#define MIN_STANDARD_DEVIATION           0.01
#define PI                               3.14159265359
#define T                                0.15


// Types.
typedef struct {
  double mean;
  double standard_deviation;
  double weight;
  bool   is_background;
} Gaussian;

typedef vector<Gaussian*> GaussianMixture;

typedef enum {
  E_GAUSSIAN_PROPERTY_WEIGHT,
  E_GAUSSIAN_PROPERTY_MEAN,
  E_GAUSSIAN_PROPERTY_VARIANCE
} GaussianProperty;


// Forward declarations.
static void          addNewGaussian (GaussianMixture* gaussians, const unsigned char pixel);
static void          adjustWeights (GaussianMixture* gaussians, const int match_index = -1);
static double        calculateGaussianProbability (const Gaussian* gaussian, const unsigned char pixel);
static double        calculateGaussianMixtureProbability (const GaussianMixture* gaussians, const unsigned char pixel);
static bool          compareGaussiansDecreasingByWeight (const Gaussian* a, const Gaussian* b);
static bool          compareGaussiansDecreasingByWeightOverVariance (const Gaussian *a, const Gaussian *b);
static void          deleteLeastProbableGaussian (GaussianMixture* gaussians);
static bool          findMatchingGaussian (const unsigned char pixel, GaussianMixture* gaussians, int* match_index);
static void          normalizeWeights (GaussianMixture *gaussians);
static void          onMouseEvent (int event, int x, int y, int, void*);
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
  const double frame_rate                 = capture.get(CV_CAP_PROP_FPS);
  const double original_inter_frame_delay = 1000 / frame_rate;
  double       inter_frame_delay          = original_inter_frame_delay;

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
  Mat             foreground_image(image.rows, image.cols, CV_8UC1);

  GaussianProperty gaussian_property_to_show = E_GAUSSIAN_PROPERTY_WEIGHT;
  int              base_line;
  Size             text_size                 = getTextSize("H", FONT, 1.0, 1, &base_line);
  const int        gaussian_image_height     = 100;
  Mat              gaussian_image(gaussian_image_height + MAX_GAUSSIANS_PER_PIXEL * 2 * text_size.height, 256, CV_8UC3);
  
  // Create windows and attach mouse handlers.
  const string input_grayscale_window      = "Input:grayscale";
  const string input_colormap_window       = "Input:colormap";
  const string background_grayscale_window = "Background:grayscale";
  const string background_colormap_window  = "Background:colormap";
  const string foreground_bw_window        = "Foreground:bw";
  const string gaussian_histogram_window   = "Gaussian:histogram";

  CvPoint clicked_point = {image.cols / 2, image.rows / 2};
  namedWindow(input_grayscale_window);      setMouseCallback(input_grayscale_window,      onMouseEvent, &clicked_point);
  namedWindow(input_colormap_window);       setMouseCallback(input_colormap_window,       onMouseEvent, &clicked_point);
  namedWindow(background_grayscale_window); setMouseCallback(background_grayscale_window, onMouseEvent, &clicked_point);
  namedWindow(background_colormap_window);  setMouseCallback(background_colormap_window,  onMouseEvent, &clicked_point);
  namedWindow(foreground_bw_window);        setMouseCallback(foreground_bw_window,        onMouseEvent, &clicked_point);
  namedWindow(gaussian_histogram_window);

  bool is_paused = false;
  while (capture.read(image)) {
    // Show the input in grayscale.
    Mat grayscale_image;
    cvtColor(image, grayscale_image, CV_RGB2GRAY);
    imshow(input_grayscale_window, grayscale_image);

    // Show the input with a colormap applied.
    Mat colored_image;
    applyColorMap(image, colored_image, COLORMAP_JET);
    imshow(input_colormap_window, colored_image);

    foreground_image.setTo(0);

    // Apply the GMM algorithm to create a model of the background.
    for (int row = 0; row < image.rows; row++) {
      for (int col = 0; col < image.cols; col++) {
        GaussianMixture     *gaussians  = &gaussian_mixture[row][col];
        const unsigned char  pixel      = grayscale_image.at<unsigned char>(row, col);
        int                  match_index;
        const bool           foundMatch = findMatchingGaussian(pixel, gaussians, &match_index);

        if (!foundMatch) {
          deleteLeastProbableGaussian(gaussians);
          addNewGaussian(gaussians, pixel);
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

        if (!foundMatch) {
          foreground_image.at<unsigned char>(row, col) = 255;
        }
      }
    }
    imshow(background_grayscale_window, background_model);

    // Show the background model with a colormap applied.
    Mat colored_background_model;
    applyColorMap(background_model, colored_background_model, COLORMAP_JET);
    imshow(background_colormap_window, colored_background_model);

    // Show the foreground pixels.
    imshow(foreground_bw_window, foreground_image);

    bool do_quit = false;
    do {

      // Show for one user-selected pixel its gaussians.
      gaussian_image.setTo(0);
      GaussianMixture *gaussians = &gaussian_mixture[clicked_point.y][clicked_point.x];

      double max_probability = 0;
      for (int i = 0; i < gaussians->size(); i++) {
        const Gaussian* gaussian = (*gaussians)[i];
        for (int col = 0; col < 256; col++) {
          const double probability = calculateGaussianProbability(gaussian, col);
          if (probability > max_probability) {
            max_probability = probability;
          }
        }
      }
    
      // Draw the gaussians backwards, so that the heavy weight is on top.
      for (int i = gaussians->size() - 1; i >= 0; i--) {
        const Gaussian*      gaussian  = (*gaussians)[i];
        const float          intensity = 64 + (int) (192 * gaussian->weight);
        const Scalar_<float> color     = (gaussian->is_background
                                          ? Scalar_<float>(0, intensity, 0)
                                          : Scalar_<float>(intensity, intensity, intensity));
      
        for (int col = 0; col < 256; col++) {
          const double probability = calculateGaussianProbability(gaussian, col);
          const int    row         = (int) (gaussian_image_height * probability / max_probability);

          if (row > 0) {
            const Point from      = Point(col, gaussian_image_height - row);
            const Point to        = Point(col, gaussian_image_height - 1);
            line(gaussian_image, from, to, color);
          }
        }

        ostringstream title_string;
        switch (gaussian_property_to_show) {
        case E_GAUSSIAN_PROPERTY_WEIGHT:
          title_string << fixed << setprecision(3) << gaussian->weight;
          break;

        case E_GAUSSIAN_PROPERTY_VARIANCE:
          title_string << fixed << setprecision(3) << (gaussian->standard_deviation * gaussian->standard_deviation);
          break;

        case E_GAUSSIAN_PROPERTY_MEAN:
          title_string << fixed << setprecision(3) << gaussian->mean;
          break;
        }        

        text_size                 = getTextSize(title_string.str(), FONT, 1.0, 1, &base_line);
        const int     left_x      = gaussian->mean - text_size.width / 2;
        const int     right_x     = left_x + text_size.width;
        const CvPoint bottom_left = {left_x < 0 ? 0 : (right_x >= gaussian_image.cols ? gaussian_image.cols - text_size.width : left_x), gaussian_image.rows - i * text_size.height};
      
        putText(gaussian_image, title_string.str(), bottom_left, FONT, 1.0, color);
      }
      imshow(gaussian_histogram_window, gaussian_image);

      const int key = waitKey((int) inter_frame_delay);
      if (key == escape_key) {
        do_quit = true;
        break;
      }
    
      switch (key) {
      case 'v':
      case 'V':
        gaussian_property_to_show = E_GAUSSIAN_PROPERTY_VARIANCE;
        break;
      
      case 'w':
      case 'W':
        gaussian_property_to_show = E_GAUSSIAN_PROPERTY_WEIGHT;
        break;

      case 'm':
      case 'M':
        gaussian_property_to_show = E_GAUSSIAN_PROPERTY_MEAN;
        break;

      case ' ':
        is_paused = !is_paused;
        if (is_paused) {
          cout << "Paused" << endl;
        } else {
          cout << "Playing" << endl;
        }
        break;

      case '+':
      case '=':
        inter_frame_delay /= 1.1;
        cout << "Speed: " << fixed << setprecision(1) << (original_inter_frame_delay / (double) inter_frame_delay) << "x" << endl;
        break;

      case '-':
        inter_frame_delay *= 1.1;
        cout << "Speed: " << fixed << setprecision(1) << (original_inter_frame_delay / (double) inter_frame_delay) << "x" << endl;
        break;

      case 'o':
      case 'O':
        inter_frame_delay = original_inter_frame_delay;
        cout << "Speed: 1.0x" << endl;
        break;
 
      default:
        break;
      }
    }
    while (is_paused);

    if (do_quit) {
      break;
    }
  }

  // Release the video.
  capture.release();
}


static bool findMatchingGaussian (const unsigned char pixel, GaussianMixture* gaussians, int* match_index) {
  sort(gaussians->begin(), gaussians->end(), compareGaussiansDecreasingByWeightOverVariance);
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
  const double rho      = LEARNING_RATE * calculateGaussianMixtureProbability(gaussians, pixel);
  const double variance = (1 - rho) * gaussian->standard_deviation * gaussian->standard_deviation + rho * (pixel - gaussian->mean) * (pixel - gaussian->mean);
    
  gaussian->mean               = (1 - rho) * gaussian->mean + rho * pixel;
  gaussian->standard_deviation = (variance > MIN_STANDARD_DEVIATION * MIN_STANDARD_DEVIATION ? sqrt(variance) : MIN_STANDARD_DEVIATION);
}


static double calculateGaussianMixtureProbability (const GaussianMixture* gaussians, const unsigned char pixel) {
  double probability = 0;

  for (int i = 0; i < gaussians->size(); i++) {
    const Gaussian *gaussian = (*gaussians)[i];
    probability += gaussian->weight * calculateGaussianProbability(gaussian, pixel);
  }

  return probability;
}


static double calculateGaussianProbability (const Gaussian* gaussian, const unsigned char pixel) {
  const double variance = gaussian->standard_deviation * gaussian->standard_deviation;

  return (1 / (sqrt(2 * PI) * gaussian->standard_deviation)) * exp(-0.5 * (pixel - gaussian->mean) * (1 / variance) * (pixel - gaussian->mean));
}


static unsigned char selectGaussiansForBackgroundModel (GaussianMixture* gaussians) {
  sort(gaussians->begin(), gaussians->end(), compareGaussiansDecreasingByWeightOverVariance);

  double weights_summed = 0;
  for (int i = 0; i < gaussians->size(); i++) {
    Gaussian *gaussian = (*gaussians)[i];
    gaussian->is_background = false;
    weights_summed += gaussian->weight;
  }

  int    b                      = 0;
  double first_b_weights_summed = 0;
  while ((b < gaussians->size()) && ((first_b_weights_summed / weights_summed) <= T)) {
    Gaussian *gaussian = (*gaussians)[b];
    gaussian->is_background = true;
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


static void onMouseEvent (int event, int x, int y, int, void* caller_object) {
  if (event == EVENT_LBUTTONDOWN) {
    CvPoint *clicked_point = (CvPoint *) caller_object;
    clicked_point->x = x;
    clicked_point->y = y;
  }
}
