"""balanced-test-max.py

In this experiment we change the prevalence of the training (and validation) data set leaving its size fixed.

Moreover, we change the size of the balanced test data set.

This experiment differs from `balanced-test.py` only in this detail that we train iterative algorithms for
a longer time, to see whether they converged.
"""
import sys
sys.path.append("../")
import quantification as q
import pandas as pd 


def quantify(y_test_predicted, y_valid_labels, y_valid_predicted):
    """Evaluate different quantification methods."""
    return {
        "CC": q.classify_and_count(y_test_predicted),
        "ACC": q.adjusted_classify_and_count(y_test_predicted=y_test_predicted,
                                             y_valid_labels=y_valid_labels,
                                             y_valid_predicted=y_valid_predicted),
        # Employ 1000 iterations of Expectation Maximization
        "EM": q.expectation_maximization(y_test_predicted=y_test_predicted,
                                         y_train_labels=y_valid_labels,
                                         n_iterations=3000),
        # Employ 2000 iterations of Unsupervised Recalibration
        "URC": q.unsupervised_recalibration(y_test_predicted=y_test_predicted,
                                            y_valid_labels=y_valid_labels,
                                            y_valid_predicted=y_valid_predicted, steps=5000, lr=0.002)
    }


def main():
    df = []  # Here we store separate runs

    # Controls training data set prevalence. (And validation).
    for train_prevalence in [0.05, 0.2, 0.4, 0.5, 0.6, 0.8, 0.95]:
        for test_prevalence in [0.5]:  # Controls test data set prevalence
            for test_dataset_size in (50, 100, 500, 1000, 3000):  # Controls test data set size
                # For each data set description generate 30 random data sets.
                for random_state in range(30):
                    params = {
                        "train_prevalence": train_prevalence,
                        "test_prevalence": test_prevalence,
                        "test_dataset_size": test_dataset_size,
                        "random_state": random_state,
                    }
                    # Generate predictions of the model and labels. Note that we use the default training data set size.
                    dataset = q.generate_dataset(train_prevalence, test_prevalence, test_dataset_size, random_state)
                    # Evaluate quantification algorithms
                    results = quantify(*dataset)
                    # Add run results to the data list
                    df.append({**params, **results})

    # Save the results in a CSV file.
    pd.DataFrame(df).to_csv("balanced-test-max.csv", index=False)


if __name__ == "__main__":
    main()

