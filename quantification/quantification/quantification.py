"""Implementation of four quantification algorithms."""
import numpy as np
import torch
import torch.optim as optim
from torch.distributions import Multinomial
from sklearn.metrics import confusion_matrix


def _reshape(y):
    """This function standarizes input (should be either a list of floats
     or an array of shape (n,), (n, 1) and (1, n)) to shape (n,)."""
    return np.array(y).reshape((-1,))


def classify_and_count(y_test_predicted) -> float:
    """Detect prevalences via Classify and Count method of Section 2.1 of
    `Karpov et al. <https://doi.org/10.18653/v1%2FS16-1025/>`_.

    Parameters
    ----------
    y_test_predicted
        Classifier predictions on the test data set. List of floats or numpy ndarray of shape (n,).

    Returns
    -------
    float
        Detected prevalence of positive samples in test data set.
    """
    y = _reshape(y_test_predicted)
    return np.sum(y >= 0.5) / len(y)


def adjusted_classify_and_count(y_test_predicted, y_valid_labels, y_valid_predicted) -> float:
    """Detect prevalences via Adjusted Classify and Count method of Section 2.2 of
    `Karpov et al. <https://doi.org/10.18653/v1%2FS16-1025/>`_. Note that this is a simplified version
    of the algorithm, that does not use cross-validation, but a separate validation data set.

    Parameters
    ----------
    y_test_predicted
        Classifier predictions on the test data set. List of floats or numpy ndarray of shape (n,).
    y_valid_labels
        Labels for the validation data set. List of floats or numpy ndarray of shape (m,).
    y_valid_predicted
        Classifier predictions on the validation data set. List of floats or numpy ndarray of shape (m,).

    Returns
    -------
    float
        Detected prevalence of positive samples in test data set.
    """
    # Estimate False Positive Rate and True Positive Rate
    # This step is applications should be replaced with cross-validation
    y_true, y_pred = _reshape(y_valid_labels), _reshape(y_valid_predicted)
    fpr = np.sum((y_pred >= 0.5) & (y_true < 0.5)) / np.sum(y_true < 0.5)
    tpr = np.sum((y_pred >= 0.5) & (y_true >= 0.5)) / np.sum(y_true >= 0.5)

    # Estimate via classify and count
    cc = classify_and_count(y_test_predicted)

    # Form the adjusted estimate for the ratio of positives.
    # We disturb the denominator slightly so we do not divide by 0.
    r = (cc - fpr) / (tpr - fpr + 1e-9)

    # Clip to range [0, 1]
    return max(0., min(r, 1.))


def expectation_maximization(y_test_predicted, y_train_labels, n_iterations: int) -> float:
    """Detect prevalences via Expectation Maximization of Section 2.5 of
    `Karpov et al. <https://doi.org/10.18653/v1%2FS16-1025/>`_. Note that this is a simplified version
    of the algorithm, that does not use cross-validation.

    Parameters
    ----------
    y_test_predicted
        Classifier predictions on the test data set. List of floats or numpy ndarray of shape (n,).
    y_train_labels
        Labels for the training data set. List of floats or numpy ndarray of shape (m,).
    n_iterations : int
        How many optimization steps should be performed.

    Returns
    -------
    float
        Detected prevalence of positive samples in test data set.
    """
    # Probability of positive samples, as observed in the training set
    y_train = _reshape(y_train_labels)
    p1_train: float = np.sum(y_train >= 0.5) / len(y_train)

    # Estimate for the probability in the set. This should improve over time.
    p1_test: float = p1_train

    y_test = _reshape(y_test_predicted)
    for i in range(n_iterations):
        numerator = y_test * p1_test / p1_train
        # We calculate the data shift denominator. Once again we slightly disturb the denominator
        # to overcome division by 0 possibility.
        denominator = (y_test * p1_test / p1_train) + \
                      (-y_test + 1.) * (1.-p1_test) / (1.-p1_train)
        # Here we have the predictions after the data shift.
        y_new = numerator / denominator
        # We update probability of observing positive case using the above predictions.
        p1_test = np.mean(y_new)

    return p1_test


def _p_y_cond_x(y_true, y_predicted):
    """Auxiliary function, for the estimation of P(Y|X) matrix of a binary classifier."""
    cm = confusion_matrix(y_true, y_predicted)
    cm = np.array(cm, dtype=float)
    p_x_and_y = cm / cm.sum()
    p_y_and_x = p_x_and_y.transpose()
    p_x = p_y_and_x.sum(axis=0)
    return p_y_and_x / p_x


def unsupervised_recalibration(y_test_predicted, y_valid_labels, y_valid_predicted,
                               steps: int = 300, lr: float = 0.005) -> float:
    """Detect prevalences via Unsupervised Recalibration method described in 
    `Ziegler and Czyż <https://arxiv.org/abs/1908.09157/>`_. Note that this is a simplified version
    of the algorithm, that does not use cross-validation, but a separate validation data set.

    Parameters
    ----------
    y_test_predicted
        Classifier predictions on the test data set. List of floats or numpy ndarray of shape (n,).
    y_valid_labels
        Labels for validation data set. List of floats or numpy ndarray of shape (m,).
    y_valid_predicted
        Classifier predictions on validation data set. List of floats or numpy ndarray of shape (m,).
    steps : int
        Number of optimization steps to be performed.
    lr : float
        Learning rate of the optimizer.

    Returns
    -------
    float
        Detected prevalence of positive samples in test data set.
    """
    y_test = _reshape(y_test_predicted)
    
    # Count number of negative and positive samples in test data set. We decide on this
    # by analysing classifier predictions on the validation data set and comparing with the median.
    m: float = np.median(y_valid_predicted)  #  Median.
    counts = (np.sum(y_test < m), np.sum(y_test >= m))
    # Upgrade counts to a (constant) tensor
    counts = torch.tensor(counts, dtype=torch.float)

    # Estimate classifier's probability matrix P(Y|X). Again we use median binning.
    p_y_cond_x = _p_y_cond_x(y_true=y_valid_labels, y_predicted=y_valid_predicted >= m)

    # Upgrade classifier probability matrix to a (constant) tensor.
    p = torch.tensor(p_y_cond_x, dtype=torch.float)
    # Logits variable (to be optimized).
    logits = torch.tensor(np.ones((2, 1)), dtype=torch.float, requires_grad=True)

    # Define the optimizer and set `logits` to be optimized.
    optimizer = optim.Adam([logits], lr=lr)

    # Optimize
    for step in range(steps):
        # Move from logits to posterior probability estimate P(X) in test data set.
        prob_x_estimate = torch.softmax(logits, dim=0)
        # Calculate expected P(Y) using matrix multiplication.
        prob_y_estimate = torch.matmul(p, prob_x_estimate)

        # Use the negative log-likelihood as loss.
        loss = -Multinomial(probs=prob_y_estimate.view(1, -1)).log_prob(counts)

        # Make the optimization step (modifies `logits`).
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()

    # Return detected prevalence for positive cases.
    with torch.no_grad():
        pred = torch.softmax(logits, dim=0).to('cpu').reshape(-1).numpy()
        return float(pred[1])
