#!/usr/bin/env python3

import numpy as np
from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split
from sklearn.neighbors import KNeighborsClassifier


def main():
    X, y = load_iris(return_X_y=True)
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, random_state=0xdecafbad)

    model = KNeighborsClassifier(
        n_neighbors=5, algorithm='brute').fit(X_train, y_train)

    train = np.append(
        X_train, y_train.reshape((len(y_train), 1)), axis=1)
    with open('irisTrain.csv', 'w') as fs:
        fs.writelines(map(print_arr, train))

    test = np.append(
        X_test, model.predict(X_test).reshape((len(X_test), 1)), axis=1)
    with open('irisTest.csv', 'w') as fs:
        fs.writelines(map(print_arr, test))


def print_arr(a):
    res = '['
    for x in a:
        res += '{:.1f}, '.format(x)
    return res[:-2] + ']\n'


if __name__ == '__main__':
    main()
