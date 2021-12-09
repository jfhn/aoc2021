import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;

public class Day9 {
	public static void main(String[] args) {
		System.out.println("Day 9");
		ArrayList<String> lines = new ArrayList<String>();
		try {
			lines = (ArrayList<String>) Files.readAllLines(Paths.get("input.txt"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		int[][] heightMap = createHeightMap(lines);

		System.out.println("Part 1: " + solve1(heightMap));
	}

	static int[][] createHeightMap(ArrayList<String> lines) {
		int width  = lines.get(0).length();
		int height = lines.size();

		int[][] heightMap = new int[height][width];

		for (int y = 0; y < height; y++) {
			String line = lines.get(y);
			for (int x = 0; x < width; x++) {
				heightMap[y][x] = line.charAt(x) - '0';
			}
		}

		return heightMap;
	}

	static int solve1(int[][] heightMap) {
		int result = 0;

		for (int y = 0; y < heightMap.length; y++) {
			for (int x = 0; x < heightMap[0].length; x++) {
				if (isLowPoint(heightMap, y, x)) {
					result += 1 + heightMap[y][x];
				}
			}
		}

		return result;
	}

	static boolean isLowPoint(int[][] heightMap, int y, int x) {
		var result = true;

		if (x > 0 && heightMap[y][x] >= heightMap[y][x - 1])
			result = false;
		if (x < heightMap[0].length - 1 && heightMap[y][x] >= heightMap[y][x + 1])
			result = false;
		if (y > 0 && heightMap[y][x] >= heightMap[y - 1][x])
			result = false;
		if (y < heightMap.length - 1 && heightMap[y][x] >= heightMap[y + 1][x])
			result = false;

		return result;
	}
}
