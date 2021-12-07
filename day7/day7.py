infile = open("input.txt", "r")
indata = list(map(int, infile.read().rstrip().split(",")))
# print(indata);

def find_max_pos(indata) -> int:
	max_pos = 0
	for pos in indata:
		if pos > max_pos:
			max_pos = pos

	return max_pos

def calc_fuel_needed(dist) -> int:
	return sum(range(dist + 1))

def try_pos(indata, fpos, day2=True) -> int:
	fuel = 0
	for pos in indata:
		if day2:
			fuel += calc_fuel_needed(abs(pos - fpos))
		else:
			fuel += abs(pos - fpos)

	return fuel

def solve1(indata):
	max_pos   = find_max_pos(indata)
	best_pos  = 0
	best_fuel = 999999999999
	for fpos in range(max_pos):
		fuel = try_pos(indata, fpos, False)
		if fuel < best_fuel:
			best_fuel = fuel
			best_pos  = fpos

	return best_fuel

def solve2(indata):
	max_pos   = find_max_pos(indata)
	best_pos  = 0
	best_fuel = 999999999999
	for fpos in range(max_pos):
		fuel = try_pos(indata, fpos)
		if fuel < best_fuel:
			best_fuel = fuel
			best_pos  = fpos

	return best_fuel

print(solve1(indata))
print(solve2(indata))
