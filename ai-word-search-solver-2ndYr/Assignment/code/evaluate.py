"""Evaluate the classifier.

DO NOT ALTER THIS FILE.

To run:
python3 evaluate.py            # run and produce a text report
python3 evaluate.py --display  # ... also show a graphic display

version: v1.0
"""

import argparse
import os
import sys
from typing import List, Tuple

import numpy as np
import system
from utils import utils
from utils.utils import Puzzle

EXPECTED_DIMENSIONALITY = 50  # Expected feature vector dimensionality
MAX_MODEL_SIZE = 3145728  # Max size of model file in bytes


def solve_puzzle(
    image_dir: str, model_data: dict, puzzle: Puzzle
) -> Tuple[List[str], List[tuple]]:
    """Run the classifier over a set of puzzles.

    Take a test data set represented by a list of puzzles object containing
    the puzzle image names and their associated correct labels. The data is processed
    in three stages:

    1. Load the images and extract features.
    2. Reduce the dimensionality of the features.
    3. Classify the images using the trained model.

    The code for each stage is supplied by the system.py module. The goal of the assignment
    is to implement the stages in system.py in order to get the best performance.

    Args:
        image_dir (str): The root directory for puzzle image data.
        model_data (dict): The model that was previously learned during training.
        puzzles (list): List of puzzles to solve.

    Returns:
        tuple: list of letter labels, list of word positions
    """

    fvectors = system.load_puzzle_feature_vectors(image_dir, [puzzle])

    fvectors_reduced = system.reduce_dimensions(fvectors, model_data)

    # Check that the dimensionality of the reduced feature vectors is correct.
    n_dimensions = fvectors_reduced.shape[1]
    if n_dimensions > EXPECTED_DIMENSIONALITY:
        print(
            f"Error: Your dimensionally reduced feature vector has {n_dimensions} dimensions.",
            f"The maximum allowed is {EXPECTED_DIMENSIONALITY}.",
        )
        sys.exit()

    # Classify and evaluate the puzzles.
    output_puzzle_labels = system.classify_squares(fvectors_reduced, model_data)

    label_grid = np.reshape(output_puzzle_labels, (puzzle.rows, puzzle.columns))
    word_positions = system.find_words(label_grid, puzzle.words, model_data)

    return output_puzzle_labels, word_positions


def evaluate(
    image_dir: str, model_file: str, puzzle_data: str, display: bool = False
) -> None:
    """Evaluate the classifier and output results.

    Args:
        image_dir (str): Directory containing the puzzle images.
        model_file (str): File containing the model data.
        puzzle_data (str): File containing the puzzle metadata.
        display (bool, optional): If True then puzzles are displayed. Defaults to False.
    """

    # Check that the model file does not violate the maximum size rule.
    stat_info = os.stat(model_file)
    if stat_info.st_size > MAX_MODEL_SIZE:
        print(f"Error: model file {model_file} exceeds allowed size limit.")
        sys.exit()

    # Load the results of the training process
    model_data = utils.load_jsongz(model_file)
    puzzles = utils.load_puzzles(puzzle_data)

    total_correct, total_labels = 0, 0
    for puzzle in puzzles:
        labels, word_positions = solve_puzzle(image_dir, model_data, puzzle)

        # Score letters
        true_labels = utils.load_puzzle_labels([puzzle])
        correct = labels == true_labels
        total_correct += np.sum(correct)
        total_labels += len(true_labels)
        score = 100.0 * np.sum(correct) / len(true_labels)
        print(f"{puzzle.name}: letter score = {score:3.1f}% correct")

        # Score words
        true_positions = [tuple(t) for t in puzzle.positions]
        n_words_correct = [w == t for w, t in zip(word_positions, true_positions)]
        word_score = 100.0 * sum(n_words_correct) / len(true_positions)
        print(f"{puzzle.name}: word score = {word_score:3.1f}% correct")

        # Display the puzzle solution
        if display:
            utils.display_solution(image_dir, puzzle, word_positions)

    print(f"Overall: {100.0 * total_correct / total_labels:3.1f} correct")


def main():
    """Called function to run the evaluation."""

    parser = argparse.ArgumentParser()
    parser.add_argument("--display", action="store_true")
    args = parser.parse_args()

    print("Running evaluation with high quality data:")
    evaluate(
        image_dir="data/extracted/high",
        model_file="data/model.high.json.gz",
        puzzle_data="data/puzzles.dev.json",
        display=args.display,
    )

    print("\nRunning evaluation with low quality data:")
    evaluate(
        image_dir="data/extracted/low",
        model_file="data/model.low.json.gz",
        puzzle_data="data/puzzles.dev.json",
        display=args.display,
    )

    print("\nAll done.")


if __name__ == "__main__":
    main()
