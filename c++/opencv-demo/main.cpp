#include <sys/time.h>
#include <iostream>
#include <iomanip>
#include <algorithm>
#include <vector>
#include <opencv2/opencv.hpp>


using namespace std;
using namespace cv;


// Tunable GMM defines.
#define MAX_GAUSSIANS_PER_PIXEL          3
#define NEW_GAUSSIAN_STANDARD_DEVIATION  15         // New Gaussians have a large variance = std. dev. squared.
#define NEW_GAUSSIAN_WEIGHT              0.00001
#define MIN_STANDARD_DEVIATION           3          // A small std. dev. causes noise to be seen as foreground pixels.
#define LEARNING_RATE                    0.005
#define T                                0.5

// Tuneable Canny defines. Canny advises a 1:2 or 1:3 ratio.
#define CANNY_LOWER_THRESHOLD            (1 * 256 / 4)
#define CANNY_UPPER_THRESHOLD            (3 * 256 / 4)

// Fixed program defines.
#define INPUT_SCALE_FACTOR               1.0
#define PI                               3.14159265359
#define FONT                             FONT_HERSHEY_PLAIN
#define NO_MATCH_INDEX                   -1


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
static void          adjustWeights (GaussianMixture* gaussians, const int match_index, const double learning_rate);
static double        calculateGaussianProbability (const Gaussian* gaussian, const unsigned char pixel);
static double        calculateGaussianMixtureProbability (const GaussianMixture* gaussians, const unsigned char pixel);
static double        calculateNormalizedCrossCorrelation (const Mat &foreground, const Mat &background, const int row, const int col, double *ptr_er, double *ptr_eb, double *ptr_et);
static bool          compareGaussiansDecreasingByWeight (const Gaussian* a, const Gaussian* b);
static bool          compareGaussiansDecreasingByWeightOverVariance (const Gaussian *a, const Gaussian *b);
static void          deleteLeastProbableGaussian (GaussianMixture* gaussians);
static bool          findMatchingGaussian (const unsigned char pixel, GaussianMixture* gaussians, int* match_index);
static string        humanReadableTimestamp (const double msec);
static void          normalizeWeights (GaussianMixture *gaussians);
static void          onMouseEvent (int event, int x, int y, int, void*);
static void          playVideo (const string video_file, const unsigned int start_seconds);
static unsigned char selectGaussiansForBackgroundModel (GaussianMixture* gaussians);
static void          updateMatchingGaussian (GaussianMixture* gaussian, const int match_index, const unsigned char pixel, const double learning_rate);


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

  // Initialize some things.
  double           learning_rate = LEARNING_RATE;

  // Show the video.
  const int       width      = (int) (INPUT_SCALE_FACTOR *image.cols);
  const int       height     = (int) (INPUT_SCALE_FACTOR * image.rows);

  GaussianMixture gaussian_mixture[height][width];
  Mat             background_model(height, width, CV_8UC1);
  Mat             foreground_image(height, width, CV_8UC3);
  Mat             foreground_mask_image(height, width, CV_8UC1);
  Mat             foreground_intensity_drop_image(height, width, CV_8UC1);
  Mat             colored_image;
  Mat             background_edges(height, width, CV_8UC1);
  Mat             all_edges(height, width, CV_8UC1);
  Mat             foreground_edges(height, width, CV_8UC3);

  GaussianProperty gaussian_property_to_show = E_GAUSSIAN_PROPERTY_WEIGHT;
  int              base_line;
  Size             text_size                 = getTextSize("H", FONT, 1.0, 1, &base_line);
  const int        gaussian_image_height     = 100;
  Mat              gaussian_image(gaussian_image_height + (MAX_GAUSSIANS_PER_PIXEL + 1) * text_size.height, 256, CV_8UC3);

  // Create windows and attach mouse handlers.
  const string input_colormap_window            = "in:color";
  const string background_colormap_window       = "bck:color";
  const string foreground_window                = "fg:gray";
  const string foreground_mask_window           = "fg:mask";
  const string foreground_morph_window          = "fg:morph";
  const string foreground_intensity_drop_window = "fg:int.drop";
  const string foreground_edges_window          = "fg:edges";
  const string contours_window                  = "fg:contours";
  const string gaussian_histogram_window        = "pixel:gaus";

  CvPoint clicked_point = {width / 2, height / 2};
  namedWindow(input_colormap_window);            setMouseCallback(input_colormap_window,            onMouseEvent, &clicked_point);
  namedWindow(background_colormap_window);       setMouseCallback(background_colormap_window,       onMouseEvent, &clicked_point);
  namedWindow(foreground_window);                setMouseCallback(foreground_window,                onMouseEvent, &clicked_point);
  namedWindow(foreground_mask_window);           setMouseCallback(foreground_mask_window,           onMouseEvent, &clicked_point);
  namedWindow(foreground_morph_window);          setMouseCallback(foreground_morph_window,          onMouseEvent, &clicked_point);
  namedWindow(foreground_intensity_drop_window); setMouseCallback(foreground_intensity_drop_window, onMouseEvent, &clicked_point);
  namedWindow(foreground_edges_window);          setMouseCallback(foreground_edges_window,          onMouseEvent, &clicked_point);
  namedWindow(contours_window);                  setMouseCallback(contours_window,                  onMouseEvent, &clicked_point);

  namedWindow(gaussian_histogram_window);

  bool is_paused = false;
  while (capture.read(image)) {
    struct timeval start_time;
    (void) gettimeofday(&start_time, NULL);

    // Show the input resized, in grayscale.
    Mat        resized_image;
    const Size zero_size(0, 0);
    resize(image, resized_image, zero_size, INPUT_SCALE_FACTOR, INPUT_SCALE_FACTOR);

    Mat grayscale_image;
    cvtColor(resized_image, grayscale_image, CV_RGB2GRAY);

    // Show the input with a colormap applied.
    applyColorMap(grayscale_image, colored_image, COLORMAP_JET);
    imshow(input_colormap_window, colored_image);

    foreground_image                = Scalar(0, 127, 255);
    foreground_mask_image           = Scalar(0);
    foreground_intensity_drop_image = Scalar(0);

    // Apply the GMM algorithm to create a model of the background.
    for (int row = 0; row < height; row++) {
      for (int col = 0; col < width; col++) {
        GaussianMixture     *gaussians     = &gaussian_mixture[row][col];
        const unsigned char  pixel         = grayscale_image.at<unsigned char>(row, col);
        int                  match_index;
        const bool           found_match   = findMatchingGaussian(pixel, gaussians, &match_index);
        const bool           is_foreground = !found_match || !((*gaussians)[match_index])->is_background;

        if (!found_match) {
          deleteLeastProbableGaussian(gaussians);
          addNewGaussian(gaussians, pixel);
        }

        if (found_match) {
          adjustWeights(gaussians, match_index, learning_rate);
        } else {
          adjustWeights(gaussians, NO_MATCH_INDEX, learning_rate);
        }

        if (found_match) {
          updateMatchingGaussian(gaussians, match_index, pixel, learning_rate);
        }

        background_model.at<unsigned char>(row, col) = selectGaussiansForBackgroundModel(gaussians);

        if (is_foreground) {
          foreground_mask_image.at<unsigned char>(row, col) = 255;
        }
      }
    }

    // Show the background model with a colormap applied.
    applyColorMap(background_model, colored_image, COLORMAP_JET);
    imshow(background_colormap_window, colored_image);

    // Show the foreground mask.
    imshow(foreground_mask_window, foreground_mask_image);

    // Apply a morphological operator to fill small holes.
    Mat foreground_morph1_image;
    const Mat element_close(7, 7, CV_8U, Scalar(1));
    morphologyEx(foreground_mask_image, foreground_morph1_image, MORPH_CLOSE, element_close);

    // Apply a morphological operator to remove small objects.
    Mat foreground_morph2_image;
    const Mat element_open(7, 7, CV_8U, Scalar(1));
    morphologyEx(foreground_morph1_image, foreground_morph2_image, MORPH_OPEN, element_open);
    imshow(foreground_morph_window, foreground_morph2_image);

    // Find contours.
    Mat contours_image = Mat::zeros(image.rows, image.cols, CV_8UC3);
    vector<vector<Point> > contours;
    vector<Vec4i>          hierarchy;

    // Plot the contours in different colors.
    findContours(foreground_morph2_image, contours, hierarchy, CV_RETR_EXTERNAL, CV_CHAIN_APPROX_SIMPLE);
    if (!contours.empty()) {
      for (int i = 0; i >= 0; i = hierarchy[i][0])
      {
        const Scalar color(128 + rand() & 127, 128 + rand() & 127, 128 + rand() & 127);
        drawContours(contours_image, contours, i, color, CV_FILLED, 8, hierarchy);
      }
    }
    imshow(contours_window, contours_image);

    // Show the foreground pixels for the contours.
    for (int row = 0; row < image.rows; row++) {
      for (int col = 0; col < image.cols; col++) {
        const Vec3b black_pixel(0, 0, 0);
        if (contours_image.at<Vec3b>(row, col) != black_pixel) {
          const unsigned char pixel = grayscale_image.at<unsigned char>(row, col);
          foreground_image.at<Vec3b>(row, col) = Vec3b(pixel, pixel, pixel);
        }
      }
    }
    imshow(foreground_window, foreground_image);

    // Show for each foreground pixel that is darker than the background, a normalized intensity drop.
    for (int row = 0; row < image.rows; row++) {
      for (int col = 0; col < image.cols; col++) {
        if (foreground_morph2_image.at<unsigned char>(row, col) != 0) {
          const unsigned char fg = grayscale_image.at<unsigned char>(row, col);
          const unsigned char bg = background_model.at<unsigned char>(row, col);

          if (fg < bg) {
            const unsigned char drop = 256 * (bg - fg) / bg;
            foreground_intensity_drop_image.at<unsigned char>(row, col) = drop;
          }
        }
      }
    }
    applyColorMap(foreground_intensity_drop_image, colored_image, COLORMAP_JET);
    imshow(foreground_intensity_drop_window, colored_image);

    // Show foreground edges.
    // 1. Plot the contour edges.
    foreground_edges = Scalar(0, 0, 0);
    if (!contours.empty()) {
      const Scalar white(255, 255, 255);
      for (int i = 0; i >= 0; i = hierarchy[i][0])
      {
        drawContours(foreground_edges, contours, i, white, 1, 8, hierarchy);
      }
      // 2. Plot the Canny edges of the foreground areas, that are not also background edges.
      Canny(background_model, background_edges, CANNY_LOWER_THRESHOLD, CANNY_UPPER_THRESHOLD);
      Canny(grayscale_image,  all_edges,        CANNY_LOWER_THRESHOLD, CANNY_UPPER_THRESHOLD);
      for (int row = 0; row < image.rows; row++) {
        for (int col = 0; col < image.cols; col++) {
          const Vec3b black_pixel(0, 0, 0);
          if (contours_image.at<Vec3b>(row, col) != black_pixel) {
            if (all_edges.at<unsigned char>(row, col) != 0 &&
                background_edges.at<unsigned char>(row, col) == 0) {
              foreground_edges.at<Vec3b>(row, col) = Vec3b(255, 255, 255);
            }
          }
        }
      }
    }
    imshow(foreground_edges_window, foreground_edges);
 
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
            const Point from = Point(col, gaussian_image_height - row);
            const Point to   = Point(col, gaussian_image_height - 1);
            line(gaussian_image, from, to, color);
          }
        }

        // Draw the current pixel intensity as a vertical line.
        const unsigned char col  = grayscale_image.at<unsigned char>(clicked_point.y, clicked_point.x);
        const Point         from = Point(col, 0);
        const Point         to   = Point(col, gaussian_image_height - 1);
        line(gaussian_image, from, to, CV_RGB(255, 0, 0));

        // Print its intensity as well.
        ostringstream intensity_string;
        intensity_string << fixed << (int) col;

        text_size            = getTextSize(intensity_string.str(), FONT, 1.0, 1, &base_line);
        int      left_x      = col - text_size.width / 2;
        int      right_x     = left_x + text_size.width;
        CvPoint  bottom_left = {left_x < 0 ? 0 : (right_x >= gaussian_image.cols ? gaussian_image.cols - text_size.width : left_x), gaussian_image.rows - MAX_GAUSSIANS_PER_PIXEL * text_size.height};
        putText(gaussian_image, intensity_string.str(), bottom_left, FONT, 1.0, CV_RGB(255, 0, 0));

        // Draw Gaussian properties below them.
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

        text_size     = getTextSize(title_string.str(), FONT, 1.0, 1, &base_line);
        left_x        = gaussian->mean - text_size.width / 2;
        right_x       = left_x + text_size.width;
        bottom_left.x = left_x < 0 ? 0 : (right_x >= gaussian_image.cols ? gaussian_image.cols - text_size.width : left_x);
        bottom_left.y = gaussian_image.rows - i * text_size.height;

        putText(gaussian_image, title_string.str(), bottom_left, FONT, 1.0, color);
      }
      imshow(gaussian_histogram_window, gaussian_image);

      // Calculate elapsed time, so that we don't wait too long on the next frame.
      struct timeval end_time;
      (void) gettimeofday(&end_time, NULL);
      long int elapsed_time_msec      = ((end_time.tv_usec + 1000000 * end_time.tv_sec) - (start_time.tv_usec + 1000000 * start_time.tv_sec)) / 1000;
      int      this_inter_frame_delay = (elapsed_time_msec < inter_frame_delay
         ? inter_frame_delay - elapsed_time_msec
         : 1);

      const int key = waitKey(this_inter_frame_delay) % 256;
      if (key == 'q' || key == 'Q') {
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

        case 'r':
          learning_rate /= 1.1;
          cout << "Learning rate = " << setprecision(5) << learning_rate << endl;
          break;

        case 'R':
          learning_rate *= 1.1;
          cout << "Learning rate = " << setprecision(5) << learning_rate << endl;
          break;

        case 't':
        case 'T':
          cout << humanReadableTimestamp(capture.get(CV_CAP_PROP_POS_MSEC)) << endl;
          break;

        case ' ':
          is_paused = !is_paused;
          if (is_paused) {
            cout << "Paused" << endl;
          } else {
            cout << "Playing" << endl;
          }
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


static void adjustWeights (GaussianMixture* gaussians, const int match_index, const double learning_rate) {
  double sum = 0;

  for (int index = 0; index < gaussians->size(); index++) {
    Gaussian *gaussian = (*gaussians)[index];

    if (index == match_index) {
      gaussian->weight = (1 - learning_rate) * gaussian->weight + learning_rate;
    } else {
      gaussian->weight = (1 - learning_rate) * gaussian->weight;
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


static void updateMatchingGaussian (GaussianMixture* gaussians, const int match_index, const unsigned char pixel, const double learning_rate) {
  Gaussian*    gaussian = (*gaussians)[match_index];
  const double rho      = learning_rate * calculateGaussianMixtureProbability(gaussians, pixel);
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


static string humanReadableTimestamp (const double msec) {
  unsigned int seconds = msec / 1000;
  unsigned int minutes = seconds / 60; seconds %= 60;
  unsigned int hours   = minutes / 60; minutes %= 60;
  ostringstream s;

  s.fill('0');
  s.width(2);

  s << right << hours << ':';

  s.width(2);
  s << right << minutes << ':';

  s.width(2);
  s << right << seconds << '.';

  s.width(3);
  s << right << (((unsigned int) msec) % 1000);

  return s.str();
}


static double calculateNormalizedCrossCorrelation (const Mat &foreground, const Mat &background, const int row, const int col, double *ptr_er, double *ptr_eb, double *ptr_et) {
  const int    n          = 4;
  const int    left_col   = (col - n < 0 ? 0 : col - n);
  const int    right_col  = (col + n > foreground.cols - 1 ? foreground.cols - 1 : col + n);
  const int    top_row    = (row - n < 0 ? 0 : row - n);
  const int    bottom_row = (row + n > foreground.rows - 1 ? foreground.rows - 1 : row + n);

  double er = 0;  // Energy of region, as B * T.
  double eb = 0;  // Energy of background (B).
  double et = 0;  // Energy of template (T).

  for (int row = top_row; row <= bottom_row; row++) {
    for (int col = left_col; col <= right_col; col++) {
      const unsigned char background_pixel = background.at<unsigned char>(row, col);
      const unsigned char template_pixel   = foreground.at<unsigned char>(row, col);

      er += background_pixel * template_pixel;
      eb += background_pixel * background_pixel;
      et += template_pixel   * template_pixel;
    }
  }

  eb = sqrt(eb);
  et = sqrt(et);

  if (ptr_er != NULL) *ptr_er = er;
  if (ptr_eb != NULL) *ptr_eb = eb;
  if (ptr_et != NULL) *ptr_et = et;

  return er / (eb * et);
}

