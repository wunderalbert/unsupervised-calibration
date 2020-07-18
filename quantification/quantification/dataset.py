"""Creating artificial data sets."""
import numpy as np
from sklearn.datasets import make_classification
from sklearn.linear_model import LogisticRegression


def _samples(train_prevalence: float, test_prevalence: float, train_size: int, test_size: int):
    n_positive_needed = 2 * train_prevalence * train_size + test_prevalence * test_size
    n_negative_needed = 2 * (1-train_prevalence) * train_size + (1-test_prevalence) * test_size

    n_total = n_positive_needed + n_negative_needed
    weights = (n_negative_needed/n_total, n_positive_needed/n_total)

    return int(1.2 * n_total), weights


def _merge(xn, xp, yn, yp):
    x = np.concatenate((xn, xp))
    y = np.concatenate((yn, yp))

    # Shuffle x and y
    permutation = np.random.permutation(len(y))
    return x[permutation], y[permutation]


def _split_into_datasets(x, y, train_dataset_size: int, train_prevalence: float,
                         test_dataset_size: int, test_prevalence):
    """Splits big data set into training, validation and test data sets. Validation data set is of the same size
    (and prevalence) as the training data set.

    Parameters
    ----------
    x : ndarray
        features, shape (n_samples_total, ...)
    y : ndarray
        binary label, shape (n_samples_total,)
    train_dataset_size : int
        required number of samples in the training (and validation) data set
    train_prevalence : float
        required prevalence of the positive class in the training (and validation) data set. Should be in (0, 1).
    test_dataset_size : int
        required number of samples in the test data set
    test_prevalence : float
        required prevalence of the positive class in the test data set. Should be in (0, 1).

    Raises
    ------
    ValueError
        if there are no enough samples to construct
    """
    # Split into negative and positive samples
    negative_mask = y < 0.5
    xn, yn = x[negative_mask], y[negative_mask]
    xp, yp = x[~negative_mask], y[~negative_mask]

    # Calculate required numbers of negative and positive samples. Check if there are enough data points.
    required_n = 2 * (1-train_prevalence) * train_dataset_size + (1-test_prevalence) * test_dataset_size
    required_p = 2 * train_prevalence * train_dataset_size + test_prevalence * test_dataset_size

    if len(yn) < required_n or len(yp) < required_p:
        raise ValueError(f"Not enough data points. I have {len(yn)}/{required_n} negative and {len(yp)}/{required_p} "
                         f"positive samples")

    p_end = int(train_prevalence * train_dataset_size)
    n_end = train_dataset_size - p_end

    x_train, y_train = _merge(xn=xn[:n_end], xp=xp[:p_end],
                              yn=yn[:n_end], yp=yp[:p_end])
    x_valid, y_valid = _merge(xn=xn[n_end:2*n_end], xp=xp[p_end:2*p_end],
                              yn=yn[n_end:2*n_end], yp=yp[p_end:2*p_end])
    n_begin, p_begin = 2*n_end, 2*p_end
    p_end = p_begin + int(test_prevalence * test_dataset_size)
    n_end = n_begin + test_dataset_size - (p_end - p_begin)

    x_test, y_test = _merge(xn=xn[n_begin:n_end], xp=xp[p_begin:p_end],
                            yn=yn[n_begin:n_end], yp=yp[p_begin:p_end])

    return {
        "x_train": x_train, "y_train": y_train,
        "x_valid": x_valid, "y_valid": y_valid,
        "x_test": x_test, "y_test": y_test,
    }


def generate_dataset(train_prevalence: float, test_prevalence: float, test_dataset_size: int, random_state: int,
                     train_dataset_size: int = 2000, model=None, class_sep: float = 0.4, flip_y: float = 0.1):
    """This function generates generates artificial data, trains a logistic regression model and reports the 
    predictions for the test and validation data sets, used by the quantification algorithms.

    Parameters
    ----------
    train_prevalence : float
        percentage of positive samples in the train (and validation) data sets
    test_prevalence : float
        percentage of positive samples in the test data set
    test_dataset_size : int
        number of samples in the test data set. Cannot be too large, otherwise an exception may be thrown.
    random_state : int
        random seed used to generate artificial data and train the logistic regression
    train_dataset_size : int
        (optional) number of samples in the train (and validation) data set. Cannot be too large, otherwise an exception
        may be thrown,
    model
        (optional) any model that admits methods `fit` and `predict_proba`.
    class_sep : float
        (optional) for bigger value it is easier to distinguish between classes
    flip_y : float
        (optional) random noise

    Returns
    -------
    y_test_predicted
        probability predictions for the test data set. Values are from the interval [0, 1].
    y_valid
        labels for the validation data set. Values are from the set {0, 1}.
    y_valid_predicted
        predicted probabilities. Values are from the interval [0, 1].
    """
    n_total, weights = _samples(train_prevalence=train_prevalence, test_prevalence=test_prevalence,
                                test_size=test_dataset_size, train_size=train_dataset_size)

    x_total, y_total = make_classification(n_samples=n_total,
                                           n_features=4, n_informative=2, n_redundant=2,
                                           n_classes=2,
                                           weights=weights,
                                           class_sep=class_sep,
                                           flip_y=flip_y,
                                           random_state=random_state)

    # Split generated data into train, validation and test.
    datasets = _split_into_datasets(x_total, y_total,
                                    train_prevalence=train_prevalence, train_dataset_size=train_dataset_size,
                                    test_prevalence=test_prevalence, test_dataset_size=test_dataset_size)
    # Train the model
    if model is None:
        model = LogisticRegression(random_state=random_state)
    model = model.fit(datasets["x_train"], datasets["y_train"])

    # Predict probabilities on the test and validation data sets.
    y_valid_predicted = model.predict_proba(datasets["x_valid"])[:, 1]
    y_test_predicted = model.predict_proba(datasets["x_test"])[:, 1]

    return y_test_predicted, datasets["y_valid"], y_valid_predicted
