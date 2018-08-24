#!/usr/bin/env python3

from collections import OrderedDict
from random import choice, seed

ATTRS = OrderedDict([
    ('outlook', ['sunny', 'overcast', 'rain']),
    ('temperature', ['hot', 'mild', 'cool']),
    ('humidity', ['high', 'normal']),
    ('windy', ['false', 'true'])])

LABELS = ['y', 'n']


def gen_tuples():
    return [choice(v) for v in ATTRS.values()] + [choice(LABELS)]


def main():
    from argparse import ArgumentParser
    parser = ArgumentParser()
    parser.add_argument('N', type=int, help='# tuples to print')
    args = parser.parse_args()

    seed(0xdecafbad)

    for _ in range(args.N):
        print(gen_tuples())


if __name__ == '__main__':
    main()
