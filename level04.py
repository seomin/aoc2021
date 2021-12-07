#!/usr/bin/env python3
from typing import Optional

class Board:
    rows: list[list[tuple[int, bool]]]
    winning: bool
    callout: Optional[int]
    winner_number: Optional[int]

    def __init__(self, rows):
        self.rows = rows
        self.winning = False
        self.callout = None
        self.winner_number = None


def update_board(board, callout_number, winner_number):
    for row_idx, row in enumerate(board.rows):
        for col_idx, (n, _) in enumerate(row):
            if n == callout_number:
                row[col_idx] = (n, True)
                if check_board(board, row_idx, col_idx):
                    board.winning = True
                    board.callout = callout_number
                    board.winner_number = winner_number
                    return True
                return False


def check_board(board, row_idx, col_idx):
    winning_row = True
    for c in range(5):
        (_, b) = board.rows[row_idx][c]
        if not b:
            winning_row = False
            break
    if winning_row:
        return True

    winning_col = True
    for r in range(5):
        (_, b) = board.rows[r][col_idx]
        if not b:
            winning_col = False
            break
    if winning_col:
        return True

    return False


def play(boards, callouts_numbers):
    winner_number = 1
    for callout in callouts_numbers:
        for board in boards:
            if not board.winning:
                if update_board(board, callout, winner_number):
                    winner_number += 1


def compute_result(board):
    assert board is not None
    # print(board)
    # print("Winning callout: " + str(board.callout))
    sum = 0
    for row in range(5):
        for col in range(5):
            (n, b) = board.rows[row][col]
            if not b:
                sum += n
    # print("Sum: " + str(sum))
    assert board.callout is not None
    print("Result: " + str(sum*board.callout))


def main():
    print("Level 4 (Bingo):")
    with open("./input/04.txt", "r") as f:
        callouts = f.readline()
        callouts_numbers = [int(i) for i in callouts.split(",") if i]

        boards = []
        lines = f.readlines()

        i = 0
        while i < len(lines):
            # print("Lines " + str(i))
            rows= []
            for j in range(1, 6):
                # print("Line i= " + str(j))
                row = [(int(s), False) for s in lines[i + j].split(" ") if s]
                rows.append(row)
            boards.append(Board(rows))
            i += 6

    play(boards, callouts_numbers)

    winner = None
    loser = None
    for board in boards:
        if board.winner_number == 1:
            winner = board
            continue
        if loser is None or loser.winner_number < board.winner_number:
            loser = board

    print("Winner:")
    compute_result(winner)
    print("Loser:")
    compute_result(loser)


if __name__ == '__main__':
    main()
