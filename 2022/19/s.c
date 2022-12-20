/* 2022/19
 *
 * Optimization / heuristic notes, on t0's first blueprint, with -O3:
 * Plain dfs: 24.84s
 * With maxedout: 11.28s
 * With lastbuild: 1.89s
 * With both: 0.01s
 *
 * The maxedout heuristic is: never build more of something than you would need
 * to pay for the most expensive thing in that resource every turn, meaning for
 * example if your most expensive bot costs 5 clay, never build more than 5
 * clay bots.
 *
 * The lastbuild heuristic is: if we could afford to build bot b this turn,
 * then if we go down the null search tree (don't build anything) for this turn
 * we shouldn't consider building bot b in the next turn. The reason for that
 * is that if we could afford b this turn there's never a reason to wait to
 * get the same thing the next turn.
 *
 * Also a word about representations: the entire state is two 64-bit words,
 * one for the bot counts and one for the current resource levels, each
 * treated as four packed 16-bit values. This allows applying a state change
 * with a single 64-bit add, testing whether we can afford something with a
 * few shifts and masks, and (crucially) passing states in registers. The
 * packed 4x16 representation is also used for costs and maxneed in the setup
 * struct for convenience.
 */

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

enum {
	IORE = 0,
	ICLA = 1,
	IOBS = 2,
	IGEO = 3,
	I_NUM = 4,
};

#define PACK(geo, obs, cla, ore) \
	(((uint64_t)(geo) << (16 * IGEO)) | \
	 ((uint64_t)(obs) << (16 * IOBS)) | \
	 ((uint64_t)(cla) << (16 * ICLA)) | ore)
#define FMASK 0xffff

struct setup {
	uint64_t costs[4];
	uint64_t maxneed;
};

struct state {
	uint64_t mins;
	uint64_t bots;
};

struct mem {
	int best;
};

unsigned int field(uint64_t v, int i) {
	return (unsigned)((v >> (16 * i)) & FMASK);
}

int canbuild(const uint64_t *costs, struct state st, int i) {
	int c = 1;
	c &= (field(st.mins, IORE) >= field(costs[i], IORE));
	c &= (field(st.mins, ICLA) >= field(costs[i], ICLA));
	c &= (field(st.mins, IOBS) >= field(costs[i], IOBS));
	/* c &= (field(st.mins, IGEO) >= field(costs[i], IGEO)); */
	return c;
}

int maxedout(const struct setup *setup, struct state st, int i) {
	return field(st.bots, i) >= field(setup->maxneed, i);
}

struct state step(const uint64_t *costs, struct state st, int order) {
	st.mins += st.bots;
	if (order >= 0) {
		st.bots += (1UL << (16 * order));
		st.mins -= costs[order];
	}
	return st;
}

void dfs(const struct setup *setup, struct state st, struct mem *mem, int left, unsigned int cm) {
	if (!left) {
		unsigned int mg = field(st.mins, IGEO);
		if (mg > mem->best) {
			mem->best = mg;
		}
		return;
	}

	unsigned int mycm = 0;
	for (int i = 0; i < I_NUM; i++)
		if (canbuild(setup->costs, st, i))
			mycm |= (1 << i);

	for (int i = 0; i < I_NUM; i++) {
		if (!(mycm & (1 << i)) || (cm & (1 << i)))
			continue;
		if (maxedout(setup, st, i))
			continue;
		dfs(setup, step(setup->costs, st, i), mem, left - 1, 0);
	}

	dfs(setup, step(setup->costs, st, -1), mem, left - 1, mycm);
}

int readbp(uint64_t *costs) {
	char buf[512];
	if (!fgets(buf, sizeof(buf), stdin))
		return 1;
	int bn, oo, co, bo, bc, go, gb;
	if (sscanf(buf, "Blueprint %d: Each ore robot costs %d ore. "
	                "Each clay robot costs %d ore. "
			"Each obsidian robot costs %d ore and %d clay. "
			"Each geode robot costs %d ore and %d obsidian.",
		   &bn, &oo, &co, &bo, &bc, &go, &gb) != 7)
		return 2;
	costs[IORE] = PACK(0, 0,  0, oo);
	costs[ICLA] = PACK(0, 0,  0, co);
	costs[IOBS] = PACK(0, 0, bc, bo);
	costs[IGEO] = PACK(0, gb, 0, go);
	return 0;
}

void prep(struct setup *setup) {
	uint16_t mc[I_NUM] = { 0, 0, 0, 65535 };
	const uint64_t *cs = setup->costs;
	for (int i = 0; i < I_NUM; i++)
		for (int j = 0; j < I_NUM; j++)
			if (mc[j] < field(cs[i], j))
				mc[j] = field(cs[i], j);

	setup->maxneed = PACK(mc[IGEO], mc[IOBS], mc[ICLA], mc[IORE]);
}

int main() {
	struct setup setup;
	int i = 1;
	int nb = 0;
	uint64_t ta = 0;
	uint64_t tb = 1;
	while (!readbp(setup.costs)) {
		struct state st = { 0, PACK(0UL, 0UL, 0UL, 1UL) };
		struct mem mem = { 0 };

		prep(&setup);
		dfs(&setup, st, &mem, 24, 0);

		ta += mem.best * i;
		i++;

		if (nb++ < 3) {
			mem.best = 0;
			dfs(&setup, st, &mem, 32, 0);
			tb *= mem.best;
		}
	}
	printf("%lu\n", ta);
	printf("%lu\n", tb);
	return 0;
	
}
