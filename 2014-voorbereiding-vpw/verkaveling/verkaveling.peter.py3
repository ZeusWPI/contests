#!/usr/bin/env python
# -*- coding: utf-8 -*-


class Verkaveling:

    """
    >>> verkaveling = Verkaveling(6, 6)
    >>> print(verkaveling)
    ######
    ######
    ######
    ######
    ######
    ######
    >>> verkaveling.grootstePerceel()
    36
    >>> verkaveling.reserveer(3, 0, 5, 2)
    >>> print(verkaveling)
    ######
    ######
    ######
    ---###
    ---###
    ---###
    >>> verkaveling.grootstePerceel()
    18
    >>> verkaveling.reserveer(0, 3, 2, 5)
    >>> print(verkaveling)
    ###---
    ###---
    ###---
    ---###
    ---###
    ---###
    >>> verkaveling.grootstePerceel()
    9
    >>> verkaveling.reserveer(2, 2, 3, 3)
    Traceback (most recent call last):
    AssertionError: perceel kan niet gereserveerd worden
    >>> print(verkaveling)
    ###---
    ###---
    ###---
    ---###
    ---###
    ---###
    """

    def __init__(self, rijen, kolommen):

        self.rijen = rijen
        self.kolommen = kolommen
        self.gereserveerd = []
        for _ in range(rijen):
            self.gereserveerd.append([False] * kolommen)

    def __str__(self):

        def weergave(perceel):
            if perceel:
                return '-'
            else:
                return '#'

        return '\n'.join(''.join(weergave(perceel) for perceel in rij)
                         for rij in self.gereserveerd)

    def reserveer(self, x1, y1, x2=None, y2=None):

        if x2 is None:
            x2 = x1
        if y2 is None:
            y2 = y1

        assert 0 <= x1 <= x2 < self.rijen, 'ongeldige positie'
        assert 0 <= y1 <= y2 < self.kolommen, 'ongeldige positie'

        for x in range(x1, x2 + 1):
            for y in range(y1, y2 + 1):
                assert not self.gereserveerd[x][y], \
                    'perceel kan niet gereserveerd worden'

        for x in range(x1, x2 + 1):
            for y in range(y1, y2 + 1):
                self.gereserveerd[x][y] = True

    def grootstePerceel(self):

        resultaat = 0
        for rij in range(self.rijen):
            for kolom in range(self.kolommen):
                maxKolom = self.kolommen
                for nr in range(rij, self.rijen):
                    nc = kolom
                    while nc < maxKolom and not self.gereserveerd[nr][nc]:
                        nc += 1
                    if (nr - rij + 1) * (nc - kolom) > resultaat:
                        resultaat = (nr - rij + 1) * (nc - kolom)
                    maxKolom = nc
        return resultaat


if __name__ == '__main__':
    import doctest
    doctest.testmod()
    cases = int(input().strip())
    for _ in range(cases):
        height, width = [int(i) for i in input().split()]
        lines = []
        for _ in range(height):
            lines.append(input())
        verkaveling = Verkaveling(height, width)
        verkaveling.gereserveerd = [[c == "-" for c in line] for line in lines]
        print(verkaveling.grootstePerceel())
