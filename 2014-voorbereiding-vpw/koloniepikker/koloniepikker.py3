class PetriDish:

    """
    >>> dish = PetriDish(open('dish.txt', 'r'))
    >>> print(dish)
    <BLANKLINE>
                             ###
                             #####
                             #######
                            #######
                             ######
                             ###### ##
                             ####  #####
                               ## ######        ####
                            #    ######       ####
                          ###  ##########    #####
                        #######  ####  ##   ######
                        ######### ##   #      #####
              #           ####   ###          ###
             #####        ####    #     ##     ##
             #####                    ######    #
            ######                   ########
             ####                     ########
                                      #######
                                      #######
    >>> dish.colony(10, 35)
    42
    >>> print(dish)
    <BLANKLINE>
                             ###
                             #####
                             #######
                            #######
                             ######
                             ###### ..
                             ####  .....
                               ## ......        ####
                            #    ......       ####
                          ###  ..........    #####
                        #######  ....  ..   ######
                        ######### ..   .      #####
              #           ####   ...          ###
             #####        ####    .     ##     ##
             #####                    ######    #
            ######                   ########
             ####                     ########
                                      #######
                                      #######
    >>> dish.colony(10, 40)
    Traceback (most recent call last):
    AssertionError: no colony found at position (10, 40)
    >>> dish.colony(10, 45)
    30
    >>> print(dish)
    <BLANKLINE>
                             ###
                             #####
                             #######
                            #######
                             ######
                             ###### ..
                             ####  .....
                               ## ......        ....
                            #    ......       ....
                          ###  ..........    .....
                        #######  ....  ..   ......
                        ######### ..   .      .....
              #           ####   ...          ...
             #####        ####    .     ##     ..
             #####                    ######    .
            ######                   ########
             ####                     ########
                                      #######
                                      #######
    >>> dish.undo()
    >>> print(dish)
    <BLANKLINE>
                             ###
                             #####
                             #######
                            #######
                             ######
                             ###### ##
                             ####  #####
                               ## ######        ####
                            #    ######       ####
                          ###  ##########    #####
                        #######  ####  ##   ######
                        ######### ##   #      #####
              #           ####   ###          ###
             #####        ####    #     ##     ##
             #####                    ######    #
            ######                   ########
             ####                     ########
                                      #######
                                      #######
    >>> dish.count()
    5
    >>> dish.size()
    32.2
    """

    def __init__(self, lines):

        # read petri dish bitmap
        self._bitmap = [list(line.rstrip('\n')) for line in lines]

        # list of known colony sizes
        self._count = None

    def __str__(self):

        """
        prints an ascii representation of the petri dish bitmap, showing empty
        bits as spaces, processed bits as dots and occupied bits as pound signs
        """

        return '\n'.join(''.join(row) for row in self._bitmap)

    def colony(self, row, col):

        """
        process the colony containing the occupied bit at the given row and
        column and returns the number of bits occupied by that colony
        """

        # check whether given bit is occupied
        message = 'no colony found at position ({}, {})'
        assert self._bitmap[row][col] == '#', message.format(row, col)

        # process colony occupying the given bit and count the number of bits
        # that are marked as processed
        bits, count = {(row, col)}, 0
        while bits:

            # select a random bit
            r, c = bits.pop()

            # process selected bit by marking it with a dot
            self._bitmap[r][c] = '.'
            count += 1

            # add occupied neighbours of selected bit to bit set
            for dr, dc in ((0, 1), (0, -1), (1, 0), (-1, 0)):
                if (
                    0 <= r + dr < len(self._bitmap) and
                    0 <= c + dc < len(self._bitmap[0]) and
                    self._bitmap[r + dr][c + dc] == '#'
                ):
                    bits.add((r + dr, c + dc))

        return count

    def undo(self):

        """
        undo all processed bits
        """

        # traverse all bits and mark processed bits again as occupied
        for row in range(len(self._bitmap)):
            for col in range(len(self._bitmap[0])):
                if self._bitmap[row][col] == '.':
                    self._bitmap[row][col] = '#'

    def _analyze_count(self, minimum, func=None):

        """
        helper method that allows a single implementation for the methods
        count and size; since the petri dish bitmap is static, the list
        of colony sizes is only computed once and stored in the property
        _count in order to speed up repeated analyses of the bitmap
        """

        if self._count is None:

            # construct a list of colony sizes
            self._count = []

            # determine number of rows and columns
            rows, cols = len(self._bitmap), len(self._bitmap[0])

            # regions containing bits on the border of the bitmap are never
            # considered to be count and are thus processed upfront
            for row in range(rows):
                for kolom in range(cols):
                    if (
                        (
                            row in (0, rows - 1) or   # first or last row
                            kolom in (0, cols - 1)    # first or last column
                        )
                        and
                        self._bitmap[row][kolom] == '#'
                    ):
                        self.colony(row, kolom)

            # traverse bitmap left to right and top to bottom, and process
            # occupied bits that are encoutered along the way
            for row in range(1, rows - 1):
                for kolom in range(1, cols - 1):
                    if self._bitmap[row][kolom] == '#':
                        size = self.colony(row, kolom)
                        self._count.append(size)

            # undo bit processing
            self.undo()

        # a list of colony sizes of count that are not smaller than the
        # minimal size is returned if no aggregation function is supplied;
        # otherwise the aggregation function is applied to this list
        sizes = [g for g in self._count if g >= minimum]
        return func(sizes) if func else sizes

    def count(self, minimum=1):

        """
        returns the number of count having a size not smaller than the given
        minimal size
        """

        return self._analyze_count(minimum, func=len)

    def size(self, minimum=1):

        """
        returns the average size of the count having a size not smaller than
        the given minimal size; the value None is returned if there are no
        count larger than or equal to the given minimal size
        """

        # helper function that computes the average of a list of numbers
        def average(aList):
            return sum(aList) / len(aList) if aList else None

        return self._analyze_count(minimum, func=average)


if __name__ == '__main__':
    #import doctest
    #doctest.testmod()

    cases = int(input().strip())
    for case in range(cases):
        height, width, minimum = [int(i) for i in input().strip().split()]
        lines = []
        for line in range(height):
            lines.append(input())
            if line > 0:
                assert len(lines[-1]) == len(lines[-2]), \
                    "%d %d" % (line, height)
        dish = PetriDish(lines)
        if dish.size(minimum) is not None:
            print("{} {}".format(dish.count(minimum),
                                 round(dish.size(minimum), 3)))
        else:
            print("0 NaN")
