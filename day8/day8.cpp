#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <bitset>
#include <algorithm>

const std::vector<std::string> split(const std::string &line)
{
	std::istringstream iss(line);
	return { std::istream_iterator<std::string>{iss}, std::istream_iterator<std::string>{} };
}

struct Digit_Map {
	char d[7][2];
};

int solve1(const char *filepath)
{
	std::ifstream ifs(filepath);
	if (!ifs.is_open()) {
		fprintf(stderr, "Could not open file\n");
		exit(1);
	}

	int result = 0;

	std::string line;
	while (std::getline(ifs, line)) {
		size_t pos = line.find("| ");
		if (pos == std::string::npos) {
			fprintf(stderr, "Could not find |");
			exit(1);
		}
		std::string values = line.substr(pos + 2);
		// std::cout << "Values = " << values << std::endl;
		auto digits = split(values);
		for (const auto &digit : digits) {
			// std::cout << "Digit: " << digit << std::endl;
			size_t len = digit.size();
			if (len == 1 || len == 2 || len == 3 || len == 4 || len == 7) {
				// printf("Highlight\n");
				++result;
			}
		}
	}

	return result;
}

int segments_diff(const char *it, const char *other, int len)
{
	std::cout << "it: " << it << ", other: " << other << std::endl;

	int c = 0;
	for (int i = 0; i < len; ++i) {
		if (it[i] != other[i])
			++c;
	}

	printf("diff = %d\n", c);
	return c == 0 ? 1 : c;
}

char find_missing_char(const char *it, const char *other, int len)
{
	printf("it = %s, other = %s\n", it, other);
	for (int i = 0; i < len; ++i) {
		printf("it[%d] = %c, other[%d] = %c\n", i, it[i], i, other[i]);
		if (it[i] != other[i])
			return it[i];
	}
	return it[len];
}

bool segment_has(const char *it, char c, int len)
{
	for (int i = 0; i < len; ++i) {
		if (it[i] == c)
			return true;
	}
	return false;
}

Digit_Map analyze(std::vector<std::string> &data)
{
	// First analysis
	const char *one;
	const char *seven;
	const char *four;
	const char *two_three_five[3];
	int   ttf_idx = 0;
	const char *zero_six_nine[3];
	int   zsn_idx = 0;
	const char *eight;

	for (auto &wire : data) {
		std::sort(wire.begin(), wire.end());
		size_t len = wire.size();
		if (len == 2) {
			one = wire.c_str();
		} else if (len == 3) {
			seven = wire.c_str();
		} else if (len == 4) {
			four = wire.c_str();
		} else if (len == 5) {
			two_three_five[ttf_idx++] = wire.c_str();
		} else if (len == 6) {
			zero_six_nine[zsn_idx++] = wire.c_str();
		} else if (len == 7) {
			eight = wire.c_str();
		}
	}

#if 1
	printf("One              = %s\n", one);
	printf("Seven            = %s\n", seven);
	printf("Four             = %s\n", four);
	printf("Two, three, five = %s\n", two_three_five[0]);
	printf("Two, three, five = %s\n", two_three_five[1]);
	printf("Two, three, five = %s\n", two_three_five[2]);
	printf("Zero, six, nine  = %s\n", zero_six_nine[0]);
	printf("Zero, six, nine  = %s\n", zero_six_nine[1]);
	printf("Zero, six, nine  = %s\n", zero_six_nine[2]);
	printf("Eight            = %s\n", eight);
#endif

	const char *zero;
	for (int i = 0; i < 3; ++i) {
		if (segments_diff(zero_six_nine[i], two_three_five[i], 5)) {
			zero = zero_six_nine[i];
		}
	}

	printf("Zero: %s\n", zero);

	char c = find_missing_char(eight, zero, 6);
	printf("C = %c\n", c);

	char a = find_missing_char(seven, one, 2);
	printf("A = %c\n", a);

	char one_and_c[4];
	snprintf(one_and_c, 4, "%s%c", one, c);
	std::string one_and_c_str(one_and_c);
	std::sort(one_and_c_str.begin(), one_and_c_str.end());
	char d = find_missing_char(four, one_and_c_str.c_str(), 3);
	printf("D = %c\n", d);

	char seven_and_c[5];
	snprintf(seven_and_c, 5, "%s%c", seven, c);
	std::string seven_and_c_str(seven_and_c);
	std::sort(seven_and_c_str.begin(), seven_and_c_str.end());

	// TODO: Maybe buggy
	const char *three = one;
	int three_idx;

	for (int i = 0; i < 3; ++i) {
		if (segments_diff(seven_and_c_str.c_str(), two_three_five[i], 4) == 1) {
			three = two_three_five[i];
			three_idx = i;
		}
	}
	printf("3 = %s\n", three);
	fflush(stdout);

	char f = find_missing_char(three, seven_and_c, 4);
	printf("F = %c\n", f);

	printf("2, 5 = %s\n", two_three_five[0]);
	printf("2, 5 = %s\n", two_three_five[1]);
	fflush(stdout);

	const char *five;

	for (int i = 0; i < 3; ++i) {
		if (i == three_idx)
			continue;

		if (segment_has(two_three_five[i], d, 5)) {
			five = two_three_five[i];
			break;
		}
	}

	printf("5 = %s\n", five);

	char acdf[5];
	snprintf(acdf, 5, "%c%c%c%c", a, c, d, f);
	std::string acdf_str(acdf);
	std::sort(acdf_str.begin(), acdf_str.end());
	char e = find_missing_char(five, acdf_str.c_str(), 4);
	printf("E = %c\n", e);

	char b = find_missing_char(one, &e, 1);
	printf("B = %c\n", b);

	char abcdef[7];
	snprintf(abcdef, 7, "%c%c%c%c%c%c", a, b, c, d, e, f);
	std::string abcdef_str(abcdef);
	std::sort(abcdef_str.begin(), abcdef_str.end());
	std::cout << abcdef_str << std::endl;

	char g = find_missing_char(eight, abcdef_str.c_str(), 6);
	printf("G = %c\n", g);

	return {{{'a', a}, {'b', b}, {'c', c}, {'d', d}, {'e', e}, {'f', f}, {'g', g}}};
}

void print_as_binary(char x)
{
	std::cout << std::bitset<8>(x) << std::endl;
}

int get_value(const std::string &digit, int *codes)
{
	char bits = 0;
	for (char c : digit) {
		// printf("%c, shift by %d\n", c, c - 'a');
		bits += 1 << (c - 'a');
		// print_as_binary(bits);
	}

	return codes[bits];

#if 0
	switch (bits) {
	case 0b1111111: return 8;
	case 0b0111110: return 5;
	case 0b1101101: return 2;
	case 0b0101111: return 3;
	case 0b0001011: return 7;
	case 0b0111111: return 9;
	case 0b1111110: return 6;
	case 0b0110011: return 4;
	case 0b1011111: return 0;
	case 0b0000011: return 1;
	}
	return 0;
#endif
}

void gen_number_codes(const Digit_Map &map, int *codes)
{
	int zero = (1 << (map.d[0][1] - 'a')) +
		(1 << (map.d[1][1] - 'a')) +
		(1 << (map.d[3][1] - 'a')) +
		(1 << (map.d[4][1] - 'a')) +
		(1 << (map.d[5][1] - 'a')) +
		(1 << (map.d[6][1] - 'a'));

	int one = (1 << (map.d[1][1] - 'a')) +
		(1 << (map.d[4][1] - 'a'));

	int two = (1 << (map.d[0][1] - 'a')) +
		(1 << (map.d[1][1] - 'a')) +
		(1 << (map.d[2][1] - 'a')) +
		(1 << (map.d[6][1] - 'a')) +
		(1 << (map.d[5][1] - 'a'));

	int three = (1 << (map.d[0][1] - 'a')) +
		(1 << (map.d[1][1] - 'a')) +
		(1 << (map.d[2][1] - 'a')) +
		(1 << (map.d[4][1] - 'a')) +
		(1 << (map.d[5][1] - 'a'));

	int four = (1 << (map.d[1][1] - 'a')) +
		(1 << (map.d[4][1] - 'a')) +
		(1 << (map.d[3][1] - 'a')) +
		(1 << (map.d[2][1] - 'a'));

	int five = (1 << (map.d[0][1] - 'a')) +
		(1 << (map.d[3][1] - 'a')) +
		(1 << (map.d[2][1] - 'a')) +
		(1 << (map.d[4][1] - 'a')) +
		(1 << (map.d[5][1] - 'a'));

	int six = (1 << (map.d[0][1] - 'a')) +
		(1 << (map.d[3][1] - 'a')) +
		(1 << (map.d[2][1] - 'a')) +
		(1 << (map.d[4][1] - 'a')) +
		(1 << (map.d[6][1] - 'a')) +
		(1 << (map.d[5][1] - 'a'));

	int seven = (1 << (map.d[0][1] - 'a')) +
		(1 << (map.d[1][1] - 'a')) +
		(1 << (map.d[4][1] - 'a'));

	int eight = (1 << (map.d[0][1] - 'a')) +
		(1 << (map.d[1][1] - 'a')) +
		(1 << (map.d[3][1] - 'a')) +
		(1 << (map.d[2][1] - 'a')) +
		(1 << (map.d[4][1] - 'a')) +
		(1 << (map.d[6][1] - 'a')) +
		(1 << (map.d[5][1] - 'a'));

	int nine = (1 << (map.d[0][1] - 'a')) +
		(1 << (map.d[3][1] - 'a')) +
		(1 << (map.d[2][1] - 'a')) +
		(1 << (map.d[4][1] - 'a')) +
		(1 << (map.d[1][1] - 'a')) +
		(1 << (map.d[5][1] - 'a'));

#if 0
	printf("0 = %d\n", zero);
	print_as_binary(zero);
	printf("1 = %d\n", one);
	print_as_binary(one);
	printf("2 = %d\n", two);
	print_as_binary(two);
	printf("3 = %d\n", three);
	print_as_binary(three);
	printf("4 = %d\n", four);
	print_as_binary(four);
	printf("5 = %d\n", five);
	print_as_binary(five);
	printf("6 = %d\n", six);
	print_as_binary(six);
	printf("7 = %d\n", seven);
	print_as_binary(seven);
	printf("8 = %d\n", eight);
	print_as_binary(eight);
	printf("9 = %d\n", nine);
	print_as_binary(nine);
	fflush(stdout);
#endif

	codes[zero]  = 0;
	codes[one]   = 1;
	codes[two]   = 2;
	codes[three] = 3;
	codes[four]  = 4;
	codes[five]  = 5;
	codes[six]   = 6;
	codes[seven] = 7;
	codes[eight] = 8;
	codes[nine]  = 9;
}

int solve2(const char *filepath)
{
	std::ifstream ifs(filepath);
	if (!ifs.is_open()) {
		fprintf(stderr, "Could not open file\n");
		exit(1);
	}

	int result = 0;

	std::string line;
	while (std::getline(ifs, line)) {
		size_t pos = line.find(" |");
		if (pos == std::string::npos) {
			fprintf(stderr, "Could not find |");
			exit(1);
		}

		std::string data = line.substr(0, pos);
		std::cout << "Data = " << data << std::endl;
		auto digit_info = split(data);

		auto map = analyze(digit_info);
		for (int i = 0; i < 7; ++i) {
			printf("%c -> %c\n", map.d[i][0], map.d[i][1]);
		}

		int number_codes[128];
		for (int i = 0; i < 128; ++i) {
			number_codes[i] = -1;
		}

		printf("gen_number_codes\n");
		fflush(stdout);
		gen_number_codes(map, number_codes);

		if (pos == std::string::npos) {
			fprintf(stderr, "Could not find |");
			exit(1);
		}

		std::string values = line.substr(pos + 3);
		auto digits = split(values);
		std::cout << values << ": ";
		for (const auto &digit : digits) {
			std::cout << get_value(digit, number_codes);
		}
		std::cout << std::endl;
	}

	return result;
}

int main()
{
	// printf("Part 1: %d\n", solve1("input.txt"));
	printf("Part 2: %d\n", solve2("sample.txt"));
}
