"""Utility code for supporting the assignment.

The code in this module is used to support the assignment. It is not intended to be
changed by students, but you might want to read it to understand how it works.

Most of it is concerned with loading the data and segmenting the puzzle images into
individual letter images, i.e., the first stage of feature extraction. This has been
done for you because it is a bit fiddly and not directly related to the learning
outcomes of the module. However, if you are not happy with the way in which it performs
you can bypass it and use your own code to segment the images by rewriting the
function load_puzzle_feature_vectors() that is defined in the system.py.

DO NOT ALTER THIS FILE.

version: v1.0
"""
import gzip
import itertools
import json
from dataclasses import dataclass
from typing import List

import matplotlib
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image


@dataclass
class Puzzle:
    """Dataclass to store puzzle metadata."""

    name: str
    rows: int
    columns: int
    letters: List[str]
    words: List[str]
    positions: List[tuple]


def load_puzzles(filename: str) -> List[Puzzle]:
    """Load puzzle data from a json file

    Args:
        filename (str): Name of the json file containing puzzle data

    Returns:
        List[Puzzle]: Puzzle data as a list of Puzzle objects
    """
    with open(filename, "r", encoding="utf-8") as fp:
        puzzle_json = json.load(fp)
        puzzles = [Puzzle(**p) for p in puzzle_json]
    return puzzles


def calc_centre_of_gravity(image: np.ndarray, axis: int):
    """Calculate the centre of gravity of an image along a given axis.

    This function is useful for working out how to crop the letter images
    from their background, i.e., how to place the centre of the cropping box.

    Args:
        image (np.ndarray): image to process
        axis (int): index of axis along which to calculate centre of gravity

    Returns:
        int: Centre of gravity of image along axis.
    """
    x = np.sum(image, axis=axis)
    return np.sum(x * np.arange(x.shape[0])) / np.sum(x)


def valid_range(centre, desired_size, max_pos):
    """Compute a valid range around a centre point."""
    half_size = desired_size // 2
    low = int(centre - half_size)
    high = low + desired_size
    if low < 0:
        low, high = 0, desired_size
    if high > max_pos:
        low, high = max_pos - desired_size, max_pos
    return low, high


def load_puzzle_image(puzzle_image_file: str) -> np.ndarray:
    """Load a puzzle and return as list of puzzle square images.

    Args:
        puzzle_image_file (str): Name of puzzle image file.

    Returns:
        list[np.ndarray]: List of images representing each square of the puzzle.
    """
    im = np.array(Image.open(puzzle_image_file))

    return im


def segment_image(image: np.ndarray, n_rows: int, n_cols: int) -> List[np.ndarray]:
    """Segment puzzle image into list of sub-images for each letter.

    Cuts the puzzle into sub-images - one for each letter in the image. The borders of
    each image are removed (they often contain spurious 'ink' from the boundary of
    the puzzle itself. A smaller box of DESIRED_SIZE by DESIRED_SIZE pixels is then
    cropped from around the centre of gravity of each letter. The cropping ensures that
    the letters are roughly centred and removes some of the variability and redundant
    background that exists in the uncropped images.

    Args:
        image (np.ndarray): Image to segment.
        n_rows (int): Number of rows in the puzzle.
        n_cols (int): Number of columns in the puzzle.

    Returns:
        List[np.ndarray]: List of images representing each square of the puzzle.
    """

    BORDER_WIDTH = 3  # Pixels to remove from image border prior to cropping.
    DESIRED_SIZE = 20  # Desired size of each letter image

    row_size = int(image.shape[0] / n_rows)
    col_size = int(image.shape[1] / n_cols)

    images = [
        image[
            row * row_size : (row + 1) * row_size,
            col * col_size : (col + 1) * col_size,
        ]
        for row in range(n_rows)
        for col in range(n_cols)
    ]

    # Crop borders - this ensures we don't capture the black border around the puzzle

    images = [
        im[BORDER_WIDTH:-BORDER_WIDTH, BORDER_WIDTH:-BORDER_WIDTH] for im in images
    ]

    out_images = []
    for im in images:
        im = (im - np.mean(im)) / np.std(im)
        im2 = im.copy()
        # filter background to white
        im2 = 1 - im2
        im2[im2 < 0] = 0

        # find centre of gravity
        cog_c = calc_centre_of_gravity(im2, 0)
        cog_r = calc_centre_of_gravity(im2, 1)
        # Compute range around centre of gravity
        c_low, c_high = valid_range(
            cog_c, desired_size=DESIRED_SIZE, max_pos=im2.shape[1]
        )
        r_low, r_high = valid_range(
            cog_r, desired_size=DESIRED_SIZE, max_pos=im2.shape[0]
        )

        out_images.append(im[r_low:r_high, c_low:c_high])

    return out_images


def flatten(list_of_lists: list) -> list:
    """Flatten a list of lists.

    A simple utility function to flatten a list of lists into a single list.

    e.g., [[1,2,3], [1], [2,3]] -> [1,2,3,1,2,3]

    Args:
        list_of_lists (list): List of lists to flatten.
    """
    return list(itertools.chain.from_iterable(list_of_lists))


def load_puzzle_character_images(
    image_dir: str, puzzle_data: List[Puzzle]
) -> List[np.ndarray]:
    """Loads puzzles and returns them a a list of of character images.

    Each puzzle image is loaded and segmented into a list of images representing
    individual characters. These characters are returned as a list of numpy arrays.

    Args:
        image_dir (str): Name of directory containing puzzle images.
        puzzle_data (list): List of dictionaries contain puzzle metadata.

    Returns:
        List[np.ndarray]: List of square images.
    """

    # Load squares for each board and flatten into single list
    images = flatten(
        [
            segment_image(
                load_puzzle_image(f"{image_dir}/{puzzle.name}.png"),
                puzzle.rows,
                puzzle.columns,
            )
            for puzzle in puzzle_data
        ]
    )

    return images


def load_puzzle_feature_vectors(image_dir: str, puzzles: List[Puzzle]) -> np.ndarray:
    """Reformat characters into feature vectors.

    Loads the puzzle character images and reformat them into a list
    of feature vectors. The feature vectors are raw pixel values
    and have not been dimensionality reduced.

    Args:
        image_dir (str): Name of directory containing puzzle images.
        puzzles (List[Puzzle]): List of Puzzle objects to load.

    Returns:
        np.ndarray: _description_
    """

    images = load_puzzle_character_images(image_dir, puzzles)
    h, w = images[0].shape
    n_features = h * w
    fvectors = np.empty((len(images), n_features))
    for i, image in enumerate(images):
        fvectors[i, :] = image.reshape(1, n_features)

    return fvectors


def load_puzzle_labels(puzzles: List[Puzzle]) -> np.ndarray:
    """Collates the square labels stored in board_data and returns as a single list.

    Args:
        board_data (list): List of dictionaries contain board metadata.

    Returns:
        np.ndarray: List of square labels.
    """

    return np.array(flatten(["".join(puzzle.letters) for puzzle in puzzles]))


def save_jsongz(filename: str, data: dict) -> None:
    """Save a dictionary to a gzipped json file.

    Args:
        filename (str): Name of file to save to.
        data (dict): Dictionary to save.
    """
    with gzip.GzipFile(filename, "wb") as fp:
        json_str = json.dumps(data) + "\n"
        json_bytes = json_str.encode("utf-8")
        fp.write(json_bytes)


def load_jsongz(filename: str) -> dict:
    """Load a gzipped json file.

    Args:
        filename (str): Name of file to load.

    Returns:
        dict: Dictionary loaded from file.
    """
    with gzip.GzipFile(filename, "r") as fp:
        json_bytes = fp.read()
        json_str = json_bytes.decode("utf-8")
        model = json.loads(json_str)
    return model


def display_solution(
    image_dir: str, puzzle: Puzzle, word_positions: List[tuple]
) -> None:
    """Display a solution with the words highlighted.

    Words that are correctly placed are highlighted in green, words that are
    incorrectly placed are highlighted in red.

    Args:
        image_dir (str): Directory storing the puzzle images.
        puzzle (Puzzle): The puzzle to display.
        word_positions (List[tuple]): The positions where the words have been placed.
    """
    matplotlib.use("TkAgg")

    image = load_puzzle_image(f"{image_dir}/{puzzle.name}.png")
    square_size = image.shape[0] // puzzle.rows
    plt.imshow(image, cmap="gray")
    for word_pos, true_pos in zip(word_positions, puzzle.positions):
        colour = "green" if true_pos is None or word_pos == tuple(true_pos) else "red"
        row_start, col_start, row_end, col_end = word_pos
        plt.plot(
            (0.5 + np.array([col_start, col_end])) * square_size,
            (0.5 + np.array([row_start, row_end])) * square_size,
            colour,
            alpha=0.5,
            linewidth=4,
        )
    plt.show()
