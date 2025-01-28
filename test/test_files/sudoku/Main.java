import java.io.*;

public class Main {
	public static void main(String[] args) throws Exception {
		Solver solver = new Solver();

		try (BufferedReader reader = new BufferedReader(new FileReader("test_files/sudoku/sudoku.txt"))) {
			String l;
			while ((l = reader.readLine()) != null) {
				if (l.length() >= 81) {
					Board board = Board.parseFrom(l);

//					System.out.println(board.toFormattedString());
//					System.out.println("solved: " + board.isSolved());

//					System.out.println("\nsolving...\n");
					Board result = solver.solve(board);

					System.out.println(result);

//					System.out.println("visualized:");
//					System.out.println(result.toFormattedString());
//					System.out.println("solved: " + result.isSolved());
					if (!result.isSolved()) throw new IllegalArgumentException("Failed to solve " + board);

//					System.out.println("\n\n");
				}
			}
		}
	}
}