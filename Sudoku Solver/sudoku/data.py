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

class Row(list):
    def __init__(self, dimension):
        self.dimension = dimension
        for i in xrange(self.dimension):
            self.append(Square())
    def __setitem__(self, key, value):
        if value <= 0 or value > self.dimension:
            raise ValueError("value out of valid range")
        self[key].value = value

class Board(object):
    def __init__(self, width=3, height=3):
        self.width      = width
        self.height     = height
        self.dimension  = width * height
        self.squares    = [Row(self.dimension)] * self.dimension
    def __getitem__(self, key):
        print("key: {}".format(key))
        return self.squares[key]
    def __repr__(self):
        result = "{} {}\n".format(self.width, self.height)
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
                result += '+'.join(['-' * (self.width * 2 + 1)] * self.height)
                result += '\n'
        return result
