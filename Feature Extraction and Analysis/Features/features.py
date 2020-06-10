import os
import sys
import pandas as pd
import numpy as np
from scipy.spatial import distance
from scipy import signal, ndimage

np.set_printoptions(threshold=sys.maxsize)

column_names = [
    'label',
    'index',
    'nr_pix',
    'height',
    'width',
    'span',
    'rows_with_5',
    'cols_with_5',
    'neigh1',
    'neigh5',
    'left2tile',
    'right2tile',
    'verticalness',
    'top2tile',
    'bottom2tile',
    'horizontalness',
    'left_diagonals',
    'right_diagonals',
    'nr_regions',
    'nr_eyes',
    'hollowness',
    'avg_pixel_weight']

output_rows = [] # List of calculated features (1 dictionary per image)


def check_directories():
    """Ensure the required directories exist before continuing"""
    if not os.path.isdir('../Doodles/'):
        sys.exit('No image folder found')
    if not os.listdir('../Doodles/'):
        sys.exit('No images')
    # if not os.path.isdir('../section2_features/'):
    #     os.mkdir('../section2_features/')

"""
FEATURE 0 - The image label and index.
"""

def get_file_details(filename):
    filename_parts = filename.replace('.', '_').split('_')
    return filename_parts[1], int(filename_parts[2])


"""
FEATURE 1 - The number of black pixels in the image
"""

def get_nr_pix(pixel_array):
    return np.count_nonzero(pixel_array == 1)


"""
FEATURE 2 - The vertical distance between the topmost and bottommost black pixels in the image
"""

def get_height(pixel_array):
    return np.ptp(np.where(pixel_array.sum(axis=1)))


"""
FEATURE 3 - The vertical distance between the leftmost and rightmost black pixels in the image
"""

def get_width(pixel_array):
    return np.ptp(np.where(pixel_array.sum(axis=0)))


"""
FEATURE 4 - The maximum Euclidean distance between any two black pixels in the image
"""

def get_span(pixel_array):
    coords = np.argwhere(pixel_array > 0) # Obtains a list of lists containing the indices of black pixels
    return np.amax(distance.cdist(coords, coords)) # Calculates the euclidean distance between each pair of pixels and returns the max


"""
FEATURE 5 - The number of rows with five or more black pixels
"""

def get_rows_with_5(pixel_array):
    return np.where(pixel_array.sum(axis=1) >= 5)[0].size
    # return sum((pixel_array.sum(axis=1))>=5)


"""
FEATURE 6 - The number of columns with five or more black pixels
"""

def get_cols_with_5(pixel_array):
    return np.where(pixel_array.sum(axis=0) >= 5)[0].size
    # return sum((pixel_array.sum(axis=0))>=5)


"""
FEATURE 7 - The number of black pixels with exactly 1 neighbouring pixel
FEATURE 8 - The number of black pixels with 5 or more neighbours
"""

def get_neighbours(pixel_array, neighbours, greater_or_equal=False):
    """
    neighbours - the number of neighbours to check for
    greater_or_equal - Whether or not to return pixels with more than than the specified number of neighbours
    """
    kernel = [[1, 1, 1],
              [1, 9, 1],
              [1, 1, 1]]
    neighbour_arr = signal.convolve(pixel_array, kernel, mode='same')
    if greater_or_equal:
        return np.count_nonzero(neighbour_arr >= neighbours + 9)
    else:
        return np.count_nonzero(neighbour_arr == neighbours + 9)


"""
FEATURE 9 - The number of unique 2-tiles in the image where the leftmost two entries are black and the rightmost two entries are white: ◧
"""

def get_left2tile(pixel_array):
    kernel = [[1, 10, 0],
              [1, 10, 0],
              [0, 0, 0]]
    neighbour_arr = signal.convolve(pixel_array, kernel, mode='same')
    return np.count_nonzero(neighbour_arr == 20)


"""
FEATURE 10 - The number of unique 2-tiles in the image where the rightmost two entries are black and the leftmost two entries are white: ◨
"""

def get_right2tile(pixel_array):
    kernel = [[0, 10, 1],
              [0, 10, 1],
              [0, 0, 0]]
    neighbour_arr = signal.convolve(pixel_array, kernel, mode='same')
    
    # for i in range(0, pixel_array.shape[0]):
    #     for j in range(0, pixel_array.shape[1]):
    #         print(pixel_array[i,j],end= ' ')
    #     print()

    # for i in range(0, neighbour_arr.shape[0]):
    #     for j in range(0, neighbour_arr.shape[1]):
    #         print(neighbour_arr[i,j],end= ' ')
    #     print()
    
    
    return np.count_nonzero(neighbour_arr == 20)


"""
FEATURE 11 - The sum of the previous two features, divided by the number of black pixels in the image
"""

def get_verticalness(features):
    return (features['left2tile'] +
            features['right2tile']) / features['nr_pix']


"""
FEATURE 12 - The number of unique 2-tiles in the image where the top two entries are black and the rightmost two entries are white
"""

def get_top2tile(pixel_array):
    kernel = [[1, 1, 0],
              [10, 10, 0],
              [0, 0, 0]]
    neighbour_arr = signal.convolve(pixel_array, kernel, mode='same')
    return np.count_nonzero(neighbour_arr == 20)


"""
FEATURE 13 - The number of unique 2-tiles in the image where the bottom two entries are black and the leftmost two entries are white
"""

def get_bottom2tile(pixel_array):
    kernel = [[0, 0, 0],
              [10, 10, 0],
              [1, 1, 0]]
    neighbour_arr = signal.convolve(pixel_array, kernel, mode='same')
    return np.count_nonzero(neighbour_arr == 20)


"""
FEATURE 14 - The sum of the previous two features, divided by the number of black pixels in the image
"""

def get_horizontalness(features):
    return (features['top2tile'] +
            features['bottom2tile']) / features['nr_pix']


"""
FEATURE 15 - The number of 1-pixel width, 3-pixel length left diagonals in the image
"""

def get_left_diagonals(pixel_array):
    kernel = [[1, -1, 0],
              [-1, 1, -1],
              [0, -1, 1]]
    diag_arr = signal.convolve(pixel_array, kernel, mode='same')
    return np.count_nonzero(diag_arr == 3)


"""
FEATURE 16 - The number of 1-pixel width, 3-pixel length right diagonals in the image
"""

def get_right_diagonals(pixel_array):
    kernel = [[0, -1, 1],
              [-1, 1, -1],
              [1, -1, 0]]
    diag_arr = signal.convolve(pixel_array, kernel, mode='same')
    return np.count_nonzero(diag_arr == 3)


"""
FEATURE 17 - The number of connected regions in the image
A connected region is a maximal set of black pixels which are connected to each other
https://stackoverflow.com/questions/46737409/finding-connected-components-in-a-pixel-array/46738834# 46738834
"""

def get_nr_regions(pixel_array):
    struct_element = [[1, 1, 1],
              [1, 1, 1],
              [1, 1, 1]]
    return ndimage.label(pixel_array, struct_element)[1]


"""
FEATURE 18 - This feature is the number of eyes in the image
A region of white pixels is an eye if there is a ring of black pixels surrounding it which are all connected
"""

def get_eyes(pixel_array, number_pixels=False):
    pixel_array = np.array([1 - x for x in np.nditer(pixel_array)]).reshape(52, 52) # Invert each black/white pixel in the array

    if number_pixels:  # Returns pixel regions for hollowness function
        return ndimage.label(pixel_array)[0]
    else:  # Returns number of eyes only
        return ndimage.label(pixel_array)[1] - 1 # Subtract 1 to ignore white background


"""
FEATURE 19 - The number of white pixels which are in eyes divided by the number of black pixels in the image
"""

def get_hollowness(pixel_array, features):
    pixel_array = get_eyes(pixel_array, True) # Obtain 2d array of pixels with black pixels as zero and white pixels a unique eye number
    return np.count_nonzero(pixel_array > 1) / features['nr_pix'] # Count pixels with a value greater than 1 (white background is 1) $ divide by black pixels


"""
FEATURE 20 - Get average weight of pixel coordinates by multiplying with a (centre-heavy) weighted grid.
Used to determine if image pixels are concentrated at the centre or edges of the image
Range: 0.0-1.0
Higher number signifies higher proportion of centralsied pixels.
"""

def get_avg_pixel_weight(pixel_array):
    """
    Creates a centre-weighted grid, the same size as pixel_array, of type float i.e.:
    0 0 0 0 0   ...   0 0 0 0 0
    0 1 2 3 4   ...   4 3 2 1 0
    0 2 4 6 8   ...   8 6 4 2 0
    0 3 6 9 12  ...  12 9 6 3 0
    0 4 8 12 16 ... 16 12 8 4 0
    . . . . . . . . . . . . . .
    . . . . . . . . . . . . . .
    0 4 8 12 16 ... 16 12 8 4 0
    0 3 6 9 12  ...  12 9 6 3 0
    0 2 4 6 8   ...   8 6 4 2 0
    0 1 2 3 4   ...   4 3 2 1 0
    0 0 0 0 0   ...   0 0 0 0 0

    The weighted average of pixel_aray is then calculated using the aforementioned grid as the weight
    """
    width = pixel_array.shape[1]
    vertical_weight_grid, horizontal_weight_grid = np.mgrid[0:width // 2, 0:width // 2]

    weight_grid = np.multiply(vertical_weight_grid, horizontal_weight_grid)
 
    weight_grid = np.append(weight_grid, np.fliplr(weight_grid), axis=1)
    weight_grid = np.append(weight_grid, np.flipud(weight_grid), axis=0)

    return np.average(pixel_array, weights=weight_grid)


def calc_features(filename):
    pixel_array = np.genfromtxt(filename, delimiter='\t', dtype=np.int8)
    pixel_array = np.pad(pixel_array, ((1, 1), (1, 1)), 'constant') # Pad outside of matrix with zeroes to simulate white border (Only a precaution)

    features = {}
    features['label'], features['index'] = get_file_details(os.path.basename(filename))
    features['nr_pix'] = get_nr_pix(pixel_array)
    features['height'] = get_height(pixel_array)
    features['width'] = get_width(pixel_array)
    features['span'] = get_span(pixel_array)
    features['rows_with_5'] = get_rows_with_5(pixel_array)
    features['cols_with_5'] = get_cols_with_5(pixel_array)
    features['neigh1'] = get_neighbours(pixel_array, 1)
    features['neigh5'] = get_neighbours(pixel_array, 5, True)
    features['left2tile'] = get_left2tile(pixel_array)
    features['right2tile'] = get_right2tile(pixel_array)
    features['verticalness'] = get_verticalness(features)
    features['top2tile'] = get_top2tile(pixel_array)
    features['bottom2tile'] = get_bottom2tile(pixel_array)
    features['horizontalness'] = get_horizontalness(features)
    features['left_diagonals'] = get_left_diagonals(pixel_array)
    features['right_diagonals'] = get_right_diagonals(pixel_array)
    features['nr_regions'] = get_nr_regions(pixel_array)
    features['nr_eyes'] = get_eyes(pixel_array)
    features['hollowness'] = get_hollowness(pixel_array, features)
    features['avg_pixel_weight'] = get_avg_pixel_weight(pixel_array)

    output_rows.append(features)


def main():
    check_directories()
    if len(sys.argv) == 1:
        for filename in sorted(os.listdir('../Doodles')):
            if os.path.isfile('../Doodles/' + filename):
                calc_features('../Doodles/' + filename)
  
    else: # If command line is used to pass images as a parameter
        for i in range(1, len(sys.argv)):
            calc_features(sys.argv[i])

    output_df = pd.DataFrame(output_rows, columns=column_names) # Create dataframe from list of dictionaries

    output_df.to_csv('40183333_features.csv', sep='\t', index=False)

    print(output_df.mean(0))

    with pd.option_context('display.max_rows', None, 'display.max_columns', None):
        print(output_df) # Console output of results


main()
