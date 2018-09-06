#!/usr/bin/env python3

from numpy import savetxt
from sklearn.datasets import make_classification
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier

TEST_PROPORTION = .75


def main():
    from argparse import ArgumentParser
    parser = ArgumentParser()
    parser.add_argument('SIZE', type=int, help='number of instances')
    parser.add_argument('--otrue', default='yTrue.csv',
                        help='path of true output (default:yTrue.csv)')
    parser.add_argument('--opred', default='yPred.csv',
                        help='path of predicted output (default:yPred.csv)')
    args = parser.parse_args()

    X, y = make_classification(int(args.SIZE / TEST_PROPORTION))
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=TEST_PROPORTION, random_state=0xdecafbad)

    model = DecisionTreeClassifier(max_depth=6).fit(X_train, y_train)
    y_pred = model.predict(X_test)

    savetxt(args.otrue, y_test, delimiter=',')
    savetxt(args.opred, y_pred, delimiter=',')


if __name__ == '__main__':
    main()
