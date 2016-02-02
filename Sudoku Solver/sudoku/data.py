class Square(object):
    def __init__(self, value=None, solved=False):
        self.value  = value
        self.solved = solved
    def __repr__(self):
        if self.value is not None:
            return repr(self.value)
        else:
            return '_'
    def __str__(self):
        if self.value:
            return str(self.value)
        else:
            return '_'

class Board(object):
    def __init__(self, width=3, height=3):
        self.width      = width
        self.height     = height
        self.dimension  = width * height
        self.squares    = [[Square()] * self.dimension] * self.dimension
    def __repr__(self):
        result = ""
        for y in xrange(self.dimension):
            row = ' '.join([repr(x) for x in self.squares[y]])
            result += row + '\n'
        return result
    def __str__(self):
        result = ""
        for y in xrange(self.dimension):
            row = ''
            for x in xrange(self.dimension):
                row += ' '
                row += str(self.squares[y][x])
                if (x + 1) % self.width == 0 and (x + 1) != self.dimension:
                    row += ' |'
            # row = ' '.join([str(x) for x in self.squares[y]])
            result += row + '\n'
            if (y + 1) % self.height == 0 and (y + 1) != self.dimension:
                result += '+'.join(['-' * (self.height * (self.height - 1) + 1)] * self.width)
                result += '\n'
        return result
