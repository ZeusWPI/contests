
import java.awt.Point;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

/**
 * Bloedgroepen
 * @author pieter
 */
public class Blood {

	// convert allelen to bloedgroep
	static int[][] bloodTable = {
		{0, 3, 0}, {3, 1, 1}, {0, 1, 2}
	};

	static int [][] rhesusTable = {
		{0, 0}, {0, 1}
	};

	static int[][] bloodParts = {
		{0, 2}, // A
		{1, 2}, // B
		{2}, // O
		{0, 1}, // AB
	};

	static int[][] rhesusParts = {
		{0, 1}, // +
		{1} // -
	};

	static int[][][] bloodTable2 = {
		{{0, 0}, {0, 2}, {2, 0}}, // A
		{{1, 1}, {1, 2}, {2, 1}},// B
		{{2, 2}},// 0
		{{0, 1}, {1, 0}}// AB
	};

	static int[][][] rhesusTable2 = {
		{{0, 0}, {1, 0}, {0, 1}}, // +
		{{1, 1}}// 0
	};

	public static void main(String[] args) throws IOException {
		String input;
		BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
		reader.readLine();
		while((input = reader.readLine()) != null) {
			solve(input.split(" "));
		}
	}

	public static void solve(String[] bloed)
	{
		String output;
		Set<String> resultSet = new TreeSet<String>();

		Point parentA = bloed[0].equals("?") ? null : parseGroep(bloed[0]);
		Point parentB = bloed[1].equals("?") ? null : parseGroep(bloed[1]);
		Point child = bloed[2].equals("?") ? null : parseGroep(bloed[2]);

		if(child == null) {
			// make all combinations of parts by parents
			List<Point> bloodPossibilities = new ArrayList<Point>();
			for(int bloodA : bloodParts[parentA.x]) {
				for(int bloodB : bloodParts[parentB.x]) {
					bloodPossibilities.add(new Point(bloodA, bloodB));
				}
			}
			List<Point> rhesusPossibilities = new ArrayList<Point>();
			for(int rhesusA : rhesusParts[parentA.y]) {
				for(int rhesusB : rhesusParts[parentB.y]) {
					rhesusPossibilities.add(new Point(rhesusA, rhesusB));
				}
			}

			for(Point bloodType : bloodPossibilities) {
				for(Point rhesusType : rhesusPossibilities) {
					resultSet.add(getGroep(
						bloodTable[bloodType.x][bloodType.y],
						rhesusTable[rhesusType.x][rhesusType.y]
					));
				}
			}

			output = bloed[0] + " " + bloed[1] + " %s\n";
		}
		else {
			if(parentA == null) {
				resultSet = findParent(parentB, child);
				output = "%s " + bloed[1] + " " + bloed[2] + "\n";
			}
			else {
				resultSet = findParent(parentA, child);
				output = bloed[0] + " %s " + bloed[2] + "\n";
			}
		}

		String answer = "";
		if(resultSet.size() == 0) answer += "ONMOGELIJK";
		else if(resultSet.size() == 1) answer += resultSet.iterator().next();
		else {
			answer += "{";
			boolean first = true;
			for(String s : resultSet) {
				if(first) { answer += s; first = false; }
				else answer += "," + s;
			}
			answer += "}";
		}
		System.out.format(output, answer);
	}

	static int[][] sources = {
		{0, 3}, // A
		{1, 3},// B
		{0, 1, 2} // O
	};

	static int[][] rhesusSources = {
		{0}, // +
		{0, 1},// -
	};

	public static Set<String> findParent(Point parent, Point child) {
		Set<String> resultSet = new TreeSet<String>();

		List<Integer> bloodPossibilities = new ArrayList<Integer>();
		for(int[] childAllelen : bloodTable2[child.x]) {
			for(int[] parentAllelen : bloodTable2[parent.x]) {
				if(childAllelen[0] == parentAllelen[0] || childAllelen[0] == parentAllelen[1]) {
					for(int i : sources[childAllelen[1]]) bloodPossibilities.add(i);
				}
				else if (childAllelen[1] == parentAllelen[0] || childAllelen[1] == parentAllelen[1]) {
					for(int i : sources[childAllelen[0]]) bloodPossibilities.add(i);
				}
			}
		}

		List<Integer> rhesusPossibilities = new ArrayList<Integer>();
		for(int[] childAllelen : rhesusTable2[child.y]) {
			for(int[] parentAllelen : rhesusTable2[parent.y]) {
				if(childAllelen[0] == parentAllelen[0] || childAllelen[0] == parentAllelen[1]) {
					for(int i : rhesusSources[childAllelen[1]]) rhesusPossibilities.add(i);
				}
				else if (childAllelen[1] == parentAllelen[0] || childAllelen[1] == parentAllelen[1]) {
					for(int i : rhesusSources[childAllelen[0]]) rhesusPossibilities.add(i);
				}
			}
		}

		for(int bloodType : bloodPossibilities) {
			for(int rhesusType : rhesusPossibilities) {
				resultSet.add(getGroep(bloodType, rhesusType));
			}
		}

		return resultSet;
	}

	public static Point parseGroep(String input)
	{
		Point result = new Point();

		if(input.length() == 3) {
			result.x = 3;
		}
		else {
			char groep = input.charAt(0);
			if(groep == 'A') result.x = 0;
			else if(groep == 'B') result.x = 1;
			else if(groep == 'O') result.x = 2;
		}

		char rhesus = input.charAt(input.length() - 1);
		if(rhesus == '+') result.y = 0;
		else result.y = 1;

		return result;
	}

	static String[] bloodLookup = {"A", "B", "O", "AB"};
	static String[] rhesusLookup = {"+", "-"};

	public static String getGroep(int bloedGroep, int rhesus)
	{
		return bloodLookup[bloedGroep] + rhesusLookup[rhesus];
	}
}
