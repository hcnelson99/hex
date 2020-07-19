from collections import deque

alphabet = 'abcdefghijklmnopqrstuvwxyz'

game_size = 5


class Hex:
    def __init__(self, row, col):
        self.row = row
        self.col = col
    def __add__(self, other):
        return Hex(self.row + other.row, self.col + other.col)
    def __rmul__(self, other):
        return Hex(self.row * other, self.col * other)
    def __repr__(self):
        return f'Hex({self.row}, {self.col})'

    def in_bounds(self):
        return 0 <= self.col < game_size and 0 <= self.row < game_size

    def adjacent_hexes(self):
        dirs = [Hex(-1, 0), Hex(-1, 1), Hex(0, 1), Hex(1, 0), Hex(1, -1), Hex(0, -1)]
        l = []
        for d in dirs:
            h = self + d
            if h.in_bounds():
                l.append(h)
        return l

    def of_move(s):
        if s[0] not in alphabet:
            return None
        col = alphabet.index(s[0])
        row = -1
        try:
            row = int(s[1:]) - 1
        except ValueError:
            pass
        h = Hex(row, col)
        if h.in_bounds():
            return h
        return None

    def __hash__(self):
        return hash((self.row, self.col))
    def __eq__(self, other):
        if not isinstance(other, Hex):
            return False
        return (self.row, self.col) == (other.row, other.col)

blue = 1
red = -1

class GameBoard:

    def __init__(self, board = None, turn = None):
        if board == None:
            board =  [[0 for _ in range(game_size)] for _ in range(game_size)]
        if turn == None:
            turn = blue

        self.board = board
        self.turn = turn


    def display(self):
        print('red goes top to bottom')
        print('  ', end='')
        for col in range(game_size):
            print(alphabet[col] + '   ', end='')
        print()
        for row in range(game_size):
            print('{:2d}  '.format(row + 1), end='')
            print(' '*(row*2), end='')
            for col in range(game_size):
                sq = self.board[row][col]
                s = ''
                if sq == 0:
                    s = '#'
                elif sq == 1:
                    s = 'b'
                elif sq == -1:
                    s = 'r'
                print(s, end='')
                print('   ', end='')
            print()

    def get(self, h):
        return self.board[h.row][h.col]

    def set(self, h, c):
        self.board[h.row][h.col] = c



    def has_won(self):
        Q = deque()
        visited = set()

        starting_hexes = None
        if self.turn == red:
            starting_hexes = [Hex(0, col) for col in range(game_size)]
        else:
            starting_hexes = [Hex(row, 0) for row in range(game_size)]

        for h in starting_hexes:
            visited.add(h)
            Q.append(h)

        while Q:
            h = Q.popleft()
            if self.turn == red and h.row == game_size - 1:
                return True
            elif self.turn == blue and h.col == game_size - 1:
                return True

            for n in h.adjacent_hexes():
                if not n in visited and self.board[n.row][n.col] == self.turn:
                    visited.add(n)
                    Q.append(n)
        return False

    def moves(self):
        m = []
        for row in range(game_size):
            for col in range(game_size):
                if self.board[row][col] == 0:
                    m.append(Hex(row, col))
        return m

    def make_move(self, h):
        board = [row[:] for row in self.board]
        board[h.row][h.col] = self.turn
        turn = self.turn * -1
        nb = GameBoard(board, turn)
        return nb
        

def play(board):
    while True:
        board.display()
        
        while True:
            move = input("What is your move?: ")
            h = Hex.of_move(move)
            if h == None or not h in board.moves():
                print('invalid move')
                continue
            board = board.make_move(h)
            break

        if board.has_won():
            print(('blue' if self.turn == 1 else 'red') + ' won!')
            board.display()
            return




play(GameBoard())
