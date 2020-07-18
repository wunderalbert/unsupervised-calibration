"""example.py

This is a very simple experiment, presenting how to use the module. We generate a few small
data sets and evaluate quantification algorithms on them, storing the outcomes in a CSV file.
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
        # Employ 100 iterations of Expectation Maximization
        "EM": q.expectation_maximization(y_test_predicted=y_test_predicted,
                                         y_train_labels=y_valid_labels,
                                         n_iterations=100),
        # Employ 100 iterations of Unsupervised Recalibration
        "URC": q.unsupervised_recalibration(y_test_predicted=y_test_predicted,
                                            y_valid_labels=y_valid_labels,
                                            y_valid_predicted=y_valid_predicted, steps=100, lr=0.002)
    }


def main():
    df = []  # Here we store separate runs

    for train_prevalence in [0.1, 0.5, 0.9]:  # Controls training data set prevalence. (And validation).
        for test_prevalence in [0.5]:  # Controls test data set prevalence
            for test_dataset_size in (100, 3000):  # Controls test data set size
                # For each data set description generate 3 random data sets.
                for random_state in range(3):
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
    pd.DataFrame(df).to_csv("example.csv", index=False)


if __name__ == "__main__":
    main()

