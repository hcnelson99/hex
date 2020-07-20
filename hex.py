from collections import deque
import math
import random

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

adjacent_hexes_cache = {}
for row in range(game_size):
    for col in range(game_size):
        h = Hex(row, col)
        adjacent_hexes_cache[h] = h.adjacent_hexes()

x_number = 1
o_number = -1

class GameBoard:

    def __init__(self, board = None, turn = None):
        if board == None:
            board =  [[0 for _ in range(game_size)] for _ in range(game_size)]
        if turn == None:
            turn = x_number

        self.board = board
        self.turn = turn
        self.won = False


    def display(self):
        print('O goes top to bottom')
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
                    s = '-'
                elif sq == 1:
                    s = 'X'
                elif sq == -1:
                    s = 'O'
                print(s, end='')
                print('   ', end='')
            print()

    def get(self, h):
        return self.board[h.row][h.col]

    def set(self, h, c):
        self.board[h.row][h.col] = c


    def search_for_win(self):
        Q = deque()
        visited = set()

        starting_hexes = None
        if self.turn == o_number:
            starting_hexes = [Hex(0, col) for col in range(game_size)]
        else:
            starting_hexes = [Hex(row, 0) for row in range(game_size)]

        for h in starting_hexes:
            if self.board[h.row][h.col] == self.turn:
                visited.add(h)
                Q.append(h)

        while Q:
            h = Q.popleft()
            if self.turn == o_number and h.row == game_size - 1:
                return True
            elif self.turn == x_number and h.col == game_size - 1:
                return True

            for n in adjacent_hexes_cache[h]:
                if not n in visited and self.board[n.row][n.col] == self.turn:
                    visited.add(n)
                    Q.append(n)
        return False

    def check_for_win_or_flip_turn(self):
        if self.search_for_win():
            self.won = True
        else:
            self.turn *= -1

    def has_won(self):
        return self.won

    def moves(self):
        if self.won:
            return []
        m = []
        for row in range(game_size):
            for col in range(game_size):
                if self.board[row][col] == 0:
                    m.append(Hex(row, col))
        return m

    def make_move(self, h):
        if self.won:
            assert False
        board = [row[:] for row in self.board]
        board[h.row][h.col] = self.turn
        nb = GameBoard(board, self.turn)
        nb.check_for_win_or_flip_turn()
        return nb
        


class MCTS:
    class Node:

        def __init__(self, board, move = None, parent = None):
            self.parent = parent
            self.board = board
            self.wins = 0
            self.simulations = 0
            self.move = move
            self.unexplored_moves = board.moves()
            self.children = []

        def expand(self):
            child_move = self.unexplored_moves.pop(random.randrange(len(self.unexplored_moves)))
            child_board = self.board.make_move(child_move)
            child = MCTS.Node(child_board, move = child_move, parent = self)
            self.children.append(child)
            return child

        def playout_and_backprop(self):
            board = self.board
            while not board.has_won():
                board = board.make_move(random.choice(board.moves()))
            self.backprop(board.turn)

        def backprop(self, winner):
            self.simulations += 1
            if winner == self.board.turn:
                self.wins += 1
            if not self.parent is None:
                self.parent.backprop(winner)


    def __init__(self, board):
        self.root = MCTS.Node(board)


    def print(self):
        def print_node(node, depth):
            if depth > 2:
                return
            print(depth * '  ' + f'{node.wins}/{node.simulations}')
            sorted_children = sorted(node.children, key = lambda c: -c.wins / c.simulations)
            for child in node.children:
                print_node(child, depth + 1)
        print_node(self.root, 0)

    def search(self):
        exploitation = math.sqrt(2)

        node = self.root
        while len(node.unexplored_moves) == 0:
            if len(node.children) == 0:
                node.playout_and_backprop()
                return

            best = -1
            best_node = None
            for child in node.children:
                score = (child.wins / child.simulations) + exploitation * (math.log(node.simulations)/child.simulations)
                if score > best:
                    best = score
                    best_node = child
            node = best_node


        child = node.expand()
        child.playout_and_backprop()

    def best_move(self):
        best = 0
        best_move = None
        for child in self.root.children:
            if child.simulations > best:
                best = child.simulations
                best_move = child.move
        return best_move


def play(board):
    while True:
        board.display()

        m = MCTS(board)
        for _ in range(10000):
            m.search()
        move = m.best_move()
        board = board.make_move(move)

        if board.has_won():
            print(('x' if board.turn == 1 else 'o') + ' won!')
            board.display()
            return

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
            print(('x' if board.turn == 1 else 'o') + ' won!')
            board.display()
            return


# best_move()

play(GameBoard())
