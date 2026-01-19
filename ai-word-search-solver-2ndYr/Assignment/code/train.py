"""Train the classifier and save the models.

Generates separate models for the high quality and low quality images.
The model files will appear in data/ and will be ready for use by evaluate.py

DO NOT ALTER THIS FILE.

usage:
python3 train.py

version: v1.0
"""

import system
from utils import utils


def train(puzzle_file: str, image_dir: str, model_file: str) -> None:
    """Process training data.

    Loads the puzzle metadata and then load the puzzle squares
    as feature vectors and their corresponding labels. The
    training algorithm is then called and the resulting model is saved.

    Args:
        puzzle_file (str): The name of the json file containing the puzzle descriptions.
        image_dir (str): The root directory for image data.
        model_file (str): The name of the output model file.

    Returns:
        dict: Dictionary containing model data that has been learned during training.
    """

    # Read the board metadata from the json file.
    puzzles = utils.load_puzzles(puzzle_file)

    # images_train = utils.load_puzzle_images(image_dir, puzzle_metadata)
    labels_train = utils.load_puzzle_labels(puzzles)
    fvectors_train = system.load_puzzle_feature_vectors(image_dir, puzzles)
    model_data = system.process_training_data(fvectors_train, labels_train)

    # Save the model data to the json file.
    utils.save_jsongz(model_file, model_data)


def main():
    """Train the classifier and save the model."""

    print("Training the model with the high quality images.")
    train(
        puzzle_file="data/puzzles.train.json",
        image_dir="data/extracted/high",
        model_file="data/model.high.json.gz",
    )

    print("Training the model with the low quality images.")
    train(
        puzzle_file="data/puzzles.train.json",
        image_dir="data/extracted/low",
        model_file="data/model.low.json.gz",
    )

    print("All done.")


if __name__ == "__main__":
    main()
