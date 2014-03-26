#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys


def chocoladerepen(repen):

    """
    >>> chocoladerepen([])
    (0, 0)
    >>> chocoladerepen([5])
    (1, 0)
    >>> chocoladerepen([1, 1])
    (1, 1)
    >>> chocoladerepen([2, 9, 8, 2, 7])
    (2, 3)
    >>> chocoladerepen([1, 2, 3, 4, 3, 2, 1])
    (4, 3)
    """

    # aantal chocoladerepen die Alice en Bob reeds opgegeten hebben
    alice, bob = 0, 0

    # tijd waarop Alice en Bob kunnen starten met het
    # opeten van een volgende chocoladereep
    alice_klaar, bob_klaar = 0, 0

    # blijf eten totdat alle repen opgegeten zijn
    while alice + bob < len(repen):

        if alice_klaar <= bob_klaar:
            alice_klaar += repen[alice]
            alice += 1
        else:
            bob += 1
            bob_klaar += repen[-bob]

    # aantal opgegeten repen teruggeven
    return alice, bob

for line in range(int(input().strip())):
    line = input().strip()
    bars = [int(i) for i in line.strip().split()]
    print("{} {}".format(*chocoladerepen(bars)))
