"""Word Search Classifier Solution.

This system handles the dimensionality reduction, data processing,
letter classification, and word searching.

version: v1.0
"""

from typing import List

import numpy as np
import scipy as sp

from utils import utils
from utils.utils import Puzzle

# The required maximum number of dimensions for the feature vectors.
N_DIMENSIONS = 20

# The K value passed to KNN, specifying the number of nearest values to consider.
KNN_K_VALUE = 12


def load_puzzle_feature_vectors(image_dir: str, puzzles: List[Puzzle]) -> np.ndarray:
    """Extract raw feature vectors for each puzzle from images in the image_dir.

    Args:
        image_dir (str): The name of the directory where the puzzle images are stored.
        puzzles (dict): The puzzle metadata providing name and size of each puzzle.

    Returns:
        np.ndarray: The raw data matrix, consisting of rows (samples) of feature vectors.

    """
    return utils.load_puzzle_feature_vectors(image_dir, puzzles)


def reduce_dimensions(data: np.ndarray, model: dict) -> np.ndarray:
    """Reduce the dimensionality of the set of feature vectors down to N_DIMENSIONS.

    This function produces the reduced feature vectors by flattening the input
    data - if necessary - and centres said data using the mean calculated from the
    training data. The function then projects the centred data onto the principal
    components found during the training stage.

    Args:
        data (np.ndarray): The feature vectors to be reduced.
        model (dict): The dictionary containing the values for the mean and
            components calculated in the principal component analysis performed
            during the training stage, more specifically, in process_training_data.

    Returns:
        np.ndarray: The reduced feature vectors.
    """

    # Ensure the data is two-dimensional
    fvectors_2d = data.reshape(data.shape[0], -1)

    # Centre the data
    centred_fvectors = fvectors_2d - model["pca_mean"]

    # Apply a linear transformation to retrieve the reduced feature vectors
    reduced_fvectors = centred_fvectors @ model["pca_components"]

    return reduced_fvectors


def process_training_data(fvectors_train: np.ndarray, labels_train: np.ndarray) -> dict:
    """Process the labeled training data and return a dictionary storing model parameters.

    This function applies principal component analysis onto the training data
    provided, which consists of two numpy arrays containing the feature vectors
    and their associated labels, respectively. The function then extracts the newly
    reduced feature vectors, the calculated PCA components, and the PCA mean;
    storing them alongside the provided training labels.

    Args:
        fvectors_train (np.ndarray): The training data's feature vectors stored as
            rows, each being its own sample.
        labels_train (np.ndarray): The labels corresponding to the feature vectors.

    Returns:
        dict: The dictionary containing model data, such as: the training labels;
            training feature vectors; and the components and mean calculated during
            principal component analysis.
    """

    def apply_principal_component_analysis():

        # Ensure the data is two-dimensional
        data = fvectors_train.reshape(fvectors_train.shape[0], -1)

        # Calculate the mean across training samples and centre the data
        mean = np.mean(data, axis=0)
        centred_data = data - mean

        # Calculate the covariance matrix
        covariance_matrix = (centred_data.T @ centred_data) / (centred_data.shape[0] - 1)

        # Calculate the eigenvalues and vectors of the matrix, ordering the eigenvectors
        eigenvalues, eigenvectors = sp.linalg.eigh(
            covariance_matrix,
            subset_by_index=(
                covariance_matrix.shape[0] - 40,
                covariance_matrix.shape[0] - 1
            )
        )
        eigenvectors = np.fliplr(eigenvectors)

        # Calculate the principal components by applying a linear transformation
        reduced_data = centred_data @ eigenvectors

        return reduced_data, eigenvectors, mean

    fvectors_train_reduced, pca_components, pca_mean = apply_principal_component_analysis()

    model = {
        "labels_train": labels_train.tolist(),
        "fvectors_train": fvectors_train_reduced.tolist(),
        "pca_components": pca_components.tolist(),
        "pca_mean": pca_mean.tolist()
    }

    return model


def classify_squares(fvectors_test: np.ndarray, model: dict) -> List[str]:
    """Apply the K-Nearest Neighbours classifier, used to classify letters.

    This function implements the K-Nearest Neighbours classification algorithm,
    comparing samples consisting of training vectors and their associated labels
    to the K-nearest samples, using the Euclidean distance formula. The function
    returns the list of classified labels.

    Args:
        fvectors_test (np.ndarray): The input feature vectors to be classified.
        model (dict): The dictionary containing model parameters such as the
            training feature vectors as well as the training labels.

    Returns:
        List[str]: The list of KNN-classified labels.
    """

    # Euclidean distance implementation
    def calc_distance(sample_1: np.ndarray, sample_2: np.ndarray) -> float:
        return np.sqrt(np.sum(np.square(np.array(sample_1) - np.array(sample_2))))

    def calc_all_distances(query_sample: np.ndarray, dataset: np.ndarray) -> list[float]:
        distances = []
        for data_sample in dataset:
            distance = calc_distance(query_sample, data_sample)
            distances.append(distance)
        return distances

    # Initialises a list to contain the new classified labels for each sample
    classified_labels = []

    # Converts the training data located in model into their respective NumPy arrays.
    training_samples = np.array(model["fvectors_train"])
    training_labels = np.array(model["labels_train"])

    for sample in fvectors_test:

        # Retrieves the list of distances between the current sample and all other samples
        sample_distances = calc_all_distances(sample, training_samples)

        # Retrieves the sorted NumPy array containing the K-smallest distances
        k_nearest_indexes = np.argsort(sample_distances)[:KNN_K_VALUE]

        # Gathers the label values, from model, at the indexes in the prior NumPy array
        k_nearest_labels = training_labels[k_nearest_indexes]

        # Initialises a variable containing the highest-occurring label in the k-nearest labels
        present_labels, counts = np.unique(k_nearest_labels, return_counts = True)
        label = present_labels[np.argmax(counts)]

        # Adds the classified label to the list of all classified labels
        classified_labels.append(label)

    return classified_labels


def find_words(labels: np.ndarray, words: List[str], model: dict) -> List[tuple]:
    """Search for the words in the grid of classified letter labels.

    The function finds words in the grid by methodically running through each
    direction - along the x-axis, y-axis, and diagonally - building a word and
    checking if this built word is contained by any existing word to be found.
    If the word does not exist, but is only a letter off of one that exists, it
    assumes there was a mislabelled sample and guesses the word was correct.
    This only applies to built words

    Args:
        labels (np.ndarray): The 2-D array containing the character present in
            each classified square of the word-search puzzle.
        words (list[str]): The list of words to find in the word-search puzzle.
        model (dict): The model and its parameters, produced during the training
            stage. In this implementation, this model is unused.

    Returns:
        list[tuple]: The list of four-element tuples indicating the word positions.
    """

    # Traverse the given direction from the given grid position, checking if the
    # built word is or is close to any of the words to find
    def check_word(j: int, i: int, input_direction: str) -> None:

        j_delta, i_delta = directions[input_direction]
        start_j, start_i = j, i
        found_words = set()
        built_word = ""

        while 0 <= j < labels.shape[0] and 0 <= i < labels.shape[1]:

            # Extend substring with new characters in the chosen direction
            built_word += labels[j, i].lower()

            for w in range(len(words)):

                # Check if the built word is present in the list of words to find
                if words[w] == built_word and built_word not in found_words:
                    word_vectors[w] = (start_j, start_i, j, i)
                    found_words.add(built_word)

                if len(built_word) == len(words[w]) and len(words[w]) > 4:

                    mismatched_letters = 0

                    # Count the number of correctly positioned letters, halting where two or more mismatches occur
                    for c in range(len(built_word)):
                        if built_word[c] != words[w][c]:
                            mismatched_letters += 1
                            if mismatched_letters > 1:
                                break

                    # Guess the word if there exists one or less mismatched letters
                    if mismatched_letters <= 1 and built_word not in found_words:
                        word_vectors[w] = (start_j, start_i, j, i)
                        found_words.add(built_word)

            j += j_delta
            i += i_delta

        return

    directions = {
        "left": (0, -1),
        "right": (0, 1),
        "up": (-1, 0),
        "down": (1, 0),
        "up_left": (-1, -1),
        "up_right": (-1, 1),
        "down_left": (1, -1),
        "down_right": (1, 1)
    }

    rows, columns = labels.shape
    first_letters = {word[0] for word in words}
    word_vectors = np.zeros(len(words), dtype=int).tolist()

    for row in range(rows):
        for column in range(columns):

            current_label = labels[row][column].lower()

            if current_label in first_letters:
                for direction in directions:

                    check_word(row, column, direction)

    return word_vectors
