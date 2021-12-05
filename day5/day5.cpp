#include <iostream>
#include <vector>
#include <fstream>
#include <sstream>
#include <string>
#include <math.h>

struct Line {
	int sx, sy, ex, ey;
};

std::vector<Line> read_coordinates(const char *filepath, int *width, int *height)
{
	std::ifstream ifs(filepath);
	if (!ifs.is_open()) {
		fprintf(stderr, "Could not open file: %s\n", filepath);
		exit(1);
	}

	std::vector<Line> res;

	std::string line;
	while (getline(ifs, line)) {
		// printf("Line: %s\n", line.c_str());
		char *endp = (char *) line.c_str();
		Line l;

		l.sx = strtol(endp, &endp, 10);
		++endp;
		l.sy = strtol(endp, &endp, 10);
		endp += 4;
		l.ex = strtol(endp, &endp, 10);
		++endp;
		l.ey = strtol(endp, &endp, 10);

		if (l.sx == l.ex || l.sy == l.ey || abs(l.sx - l.ex) == abs(l.sy - l.ey)) {
			res.push_back(l);
			if (*width < l.sx) {
				*width = l.sx;
			}
			if (*height < l.sy) {
				*height = l.sy;
			}
		}
	}

	return res;
}

void print_board(int *board, int width, int height)
{
	for (int x = 0; x < width; ++x) {
		for (int y = 0; y < height; ++y) {
			printf("%d ", board[x * height + y]);
		}
		printf("\n");
	}
}

int solve1(const std::vector<Line> &coordinates, int *board, int width, int height)
{
	memset(board, 0, width * height * sizeof(int));

	for (const auto &line : coordinates) {
		if (line.sx == line.ex) { // Vertical line
			int start = line.sy < line.ey ? line.sy : line.ey;
			int end   = line.sy > line.ey ? line.sy : line.ey;
			for (int i = start; i <= end; ++i) {
				++board[i * width + line.sx];
			}
		} else if (line.sy == line.ey) { // Horizontal line
			int start = line.sx < line.ex ? line.sx : line.ex;
			int end   = line.sx > line.ex ? line.sx : line.ex;
			for (int i = start; i <= end; ++i) {
				++board[line.sy * width + i];
			}
		}
	}

	int sum = 0;

	for (int i = 0; i < width * height; ++i) {
		if (board[i] >= 2) {
			++sum;
		}
	}

	return sum;
}

int solve2(const std::vector<Line> &coordinates, int *board, int width, int height)
{
	memset(board, 0, width * height * sizeof(int));

	for (const auto &line : coordinates) {
		if (line.sx == line.ex) { // Vertical line
			int start_y = line.sy < line.ey ? line.sy : line.ey;
			int end_y   = line.sy > line.ey ? line.sy : line.ey;
			for (int i = start_y; i <= end_y; ++i) {
				++board[i * width + line.sx];
			}
		} else if (line.sy == line.ey) { // Horizontal line
			int start_x = line.sx < line.ex ? line.sx : line.ex;
			int end_x   = line.sx > line.ex ? line.sx : line.ex;
			for (int i = start_x; i <= end_x; ++i) {
				++board[line.sy * width + i];
			}
		} else { // Diagonal line
			int x      = line.sx;
			int y      = line.sy;
			int end    = line.ey * width + line.ex;
			int x_step = line.sx < line.ex ? 1 : -1;
			int y_step = line.sy < line.ey ? 1 : -1;
			bool stop  = false;
			for (;;) {
				int idx = y * width + x;
				++board[idx];
				x += x_step;
				y += y_step;

				if (stop) {
					break;
				}

				if ((y * width + x) == end) {
					stop = true;
				}
			}
		}
	}

	int sum = 0;

	for (int i = 0; i < width * height; ++i) {
		if (board[i] >= 2) {
			++sum;
		}
	}

	return sum;
}

int main()
{
	int width, height;
	auto coordinates = read_coordinates("input.txt", &width, &height);

	// printf("Width = %d, Height = %d\n", width, height);
	++width;
	++height;

	int *board = new int[width * height];

	printf("Part 1: %d\n", solve1(coordinates, board, width, height));
	printf("Part 2: %d\n", solve2(coordinates, board, width, height));

	return 0;
}
