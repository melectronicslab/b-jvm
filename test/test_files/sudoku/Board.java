/** A (mostly) immutable sudoku board */
public class Board {
	final int[] board = new int[81]; // only stores 0-9

	private static int fromChar(char c) {
		int raw = c - '0';
		return raw < 1 || raw > 9 ? 0 : raw; // handle missing digits
	}

	public static Board parseFrom(String input) {
		Board board = new Board();
		for (int i = 0; i < 81; i++) {
			board.board[i] = fromChar(input.charAt(i));
		}
		return board;
	}

	public boolean isSolved() {
		for (int val : board) {
			if (val < 1 || val > 9) {
				return false;
			}
		}
		for (int i = 0; i < 9; i++) {
			int rowTotal = 0;
			int colTotal = 0;
			int squareTotal = 0;

			for (int j = 0; j < 9; j++) {
				rowTotal += board[i*9 + j];
				colTotal += board[j*9 + i];
				squareTotal += board[(i/3*3 + j/3)*9 + i%3*3 + j%3];
			}

			if (rowTotal != 45 || colTotal != 45 || squareTotal != 45) {
				return false;
			}
		}
		return true;
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (int val : board) {
			if (val == 0) {
				sb.append(".");
			} else {
				sb.append(val);
			}
		}
		return sb.toString();
	}

	public String toFormattedString() {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < 9; i++) {
			for (int j = 0; j < 9; j++) {
				int val = board[i*9 + j];
				if (val == 0) {
					sb.append(".");
				} else {
					sb.append(val);
				}
				if (j < 8) sb.append(" ");
			}
			if (i < 8) sb.append("\n");
		}
		return sb.toString();
	}
}
