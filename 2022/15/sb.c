/* 15/sb.c */

#include <stdio.h>
#include <string.h>

enum {
	NSENSOR = 25,
};

struct point {
	int x;
	int y;
};

struct sensor {
	struct point pos;
	struct point beacon;
	int range;
};

int abs(int a) { return a < 0 ? -a : a; }

int manhattan(struct point p, struct point q) {
	return abs(q.x - p.x) + abs(q.y - p.y);
}

void parse(struct sensor ss[NSENSOR]) {
	char buf[512];
	int sn = 0;
	while (fgets(buf, sizeof(buf), stdin)) {
		sscanf(buf,
		       "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d",
		       &ss[sn].pos.x, &ss[sn].pos.y,
		       &ss[sn].beacon.x, &ss[sn].beacon.y);
		ss[sn].range = manhattan(ss[sn].pos, ss[sn].beacon);
		sn++;
	}
}

int couldbe(struct sensor ss[NSENSOR], int x, int y) {
	struct point p = { x, y };
	for (int i = 0; i < NSENSOR && ss[i].range; i++)
		if (manhattan(ss[i].pos, p) <= ss[i].range)
			return 0;
	return 1;
}

int main() {
	struct sensor ss[NSENSOR];
	memset(ss, 0, sizeof ss);
	parse(ss);
	for (int x = 0; x <= 4000000; x++) {
		for (int y = 0; y < 400; y++) {
			if (couldbe(ss, x, y))
				printf("%d,%d\n", x, y);
		}
	}
	return 0;
}
