#!/usr/bin/env python
# -*- coding: utf-8 -*-


def codeer(tekst, k):

    """
    >>> codeer('And now for something completely different.', 1)
    'And now for something completely different.'
    >>> codeer('And now for something completely different.', 2)
    'Adnwfrsmtigcmltl ifrn.n o o oehn opeeydfeet'
    >>> codeer('And now for something completely different.', 3)
    'Anfstgmt fnn o o oehn opeeydfeetdwrmicllir.'
    >>> codeer('And now for something completely different.', 4)
    'Awsimlf.no  ohnopeyfetdnfrmtgclt irn oe ede'
    >>> codeer('And now for something completely different.', 5)
    'Aftm nn oehopydetdwrmicllir. o on eefensgtf'
    """

    # spoorhek opbouwen
    matrix = [['' for _ in range(len(tekst))] for _ in range(k)]
    rij, volgende = 0, -1
    if k == 1:
        volgende = 0
    for kolom in range(len(tekst)):
        matrix[rij][kolom] = tekst[kolom]
        if rij == 0 or rij == k - 1:
            volgende *= -1
        rij += volgende

    # spoorhek uitlezen
    resultaat = ''
    for rij in range(k):
        for kolom in range(len(tekst)):
            resultaat += matrix[rij][kolom]
    return resultaat


def decodeer(tekst, k):

    """
    >>> decodeer('And now for something completely different.', 1)
    'And now for something completely different.'
    >>> decodeer('Adnwfrsmtigcmltl ifrn.n o o oehn opeeydfeet', 2)
    'And now for something completely different.'
    >>> decodeer('Anfstgmt fnn o o oehn opeeydfeetdwrmicllir.', 3)
    'And now for something completely different.'
    >>> decodeer('Awsimlf.no  ohnopeyfetdnfrmtgclt irn oe ede', 4)
    'And now for something completely different.'
    >>> decodeer('Aftm nn oehopydetdwrmicllir. o on eefensgtf', 5)
    'And now for something completely different.'
    """

    # sporen uitzetten op spoorhek
    matrix = [['' for _ in range(len(tekst))] for _ in range(k)]
    rij, volgende = 0, -1
    if k == 1:
        volgende = 0
    for kolom in range(len(tekst)):
        matrix[rij][kolom] = None
        if rij == 0 or rij == k - 1:
            volgende *= -1
        rij += volgende

    # letters invullen op spoorhek
    n = 0
    for rij in range(k):
        for kolom in range(len(tekst)):
            if matrix[rij][kolom] is None:
                matrix[rij][kolom] = tekst[n]
                n += 1

    # spoorhek uitlezen
    resultaat = ''
    rij, volgende = 0, -1
    if k == 1:
        volgende = 0
    for kolom in range(len(tekst)):
        resultaat += matrix[rij][kolom]
        if rij == 0 or rij == k - 1:
            volgende *= -1
        rij += volgende

    return resultaat


if __name__ == '__main__':
    TRANSLATION_TABLE = dict(decode=decodeer, encode=codeer)

    import sys
    lines = sys.stdin
    times = int(next(lines).strip())
    for i in range(times):
        mode, width = next(lines).split()
        width = int(width.strip())
        print(TRANSLATION_TABLE[mode](next(lines).strip('\n'), width))
