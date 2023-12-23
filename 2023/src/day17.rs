// Day 17: Clumsy Crucible
// We're given a 2d map of tiles, each of which contains a cost to enter that
// cell. We're trying to find the lowest-cost path from the top-left corner to
// the bottom-right corner. We also can't travel the same direction 3 times
// consecutively.
//
// The condition about not travelling the same direction 3 times means a dynamic
// programming solution probably won't work, since the cost to get from (x,y) to
// (x+1,y) could depend on the path by which we got to (x,y). Instead, we can do
// a depth-first search, with a heuristic to stop considering any path whose
// cost exceeds the best we've seen so far.
//
// Update: I tried that and it didn't work. The search space is too "flat" so we
// end up having to go too deep down a bad path before we can eliminate it; even
// on the example input and in a release build it just isn't fast enough. I
// asked a couple of folks for help and one suggested A* instead. At first I
// thought I couldn't use A* because of the need to turn the paths every so
// often, but another person suggested that I can make the A* state bigger than
// just the position - if I also keep track of which turns I still have
// available (or whatever) then it makes the search space a bit bigger but A*
// (or Dijkstra) still works, I think.
//
// So, thinking this all the way through: the actual graph we're searching has
// nodes which are a 5-tuple of (location, remaining north moves, remaining
// south moves, remaining west moves, remaining east moves). The neighbors of a
// node are then the ones that both have an adjacent location and have the run
// count for the direction that was just moved decremented; if the run count
// would have to decrement below zero, that location isn't a neighbor.

use crate::map2d::{Dir2d, Map2d, Point2d};
use pathfinding::directed::dijkstra;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

const DIRS: [Dir2d; 4] = [Dir2d::North, Dir2d::South, Dir2d::West, Dir2d::East];

type Map = Map2d<usize>;

#[derive(Clone)]
struct Node {
    // We have to store Rc<Map> instead of &Map because otherwise convincing the
    // compiler that the lifetime of the map embedded in a cloned Node is long
    // enough is very tricky to get right; use an Rc to just push the problem to
    // runtime.
    map: Rc<Map>,
    loc: Point2d,
    run: [u8; 4],
}

// We need these impls so that we don't try to hash the map into the node, or
// compare it for equality.

impl Hash for Node {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.loc.hash(state);
        self.run.hash(state);
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.loc.eq(&other.loc) && self.run.eq(&other.run)
    }
}

impl Eq for Node {}

impl Node {
    fn moveby(&self, dir: Dir2d, minmove: u8, maxmove: u8) -> Option<(Node, usize)> {
        let loc = self.loc.moveby(dir);
        let didx = dir as usize;
        let oidx = dir.opposite() as usize;
        let m = self.map.clone();

        let toomuch = self.run[didx] >= maxmove;
        let turned = self.run[oidx] != 0;
        let mut notenough = false;

        for d in DIRS {
            let di = d as usize;
            if self.run[di] > 0 && self.run[di] < minmove && d != dir {
                notenough = true;
            }
        }

        if m.inboundsp(loc) && !toomuch && !notenough && !turned {
            let mut r: [u8; 4] = [0, 0, 0, 0];
            r[didx] = self.run[didx] + 1;
            Some((
                Node {
                    map: m,
                    loc: loc,
                    run: r,
                },
                *self.map.atp(loc)
            ))
        } else {
            None
        }
    }

    // Gross invariant here: these are always returned in north, south, west,
    // east order. That makes writing the unit tests below a lot easier.
    fn neighbors(&self, minmove: u8, maxmove: u8) -> Vec<(Node, usize)> {
        let mut r = Vec::new();

        for d in DIRS {
            if let Some(v) = self.moveby(d, minmove, maxmove) {
                r.push(v);
            }
        }

        r
    }
}

fn dosolve(map: &Map, min: u8, max: u8) -> usize {
    let start = Node {
        map: Rc::new(map.clone()),
        loc: map.topleft(),
        run: [0, 0, 0, 0],
    };
    let end = map.bottomright();
    let r = dijkstra::dijkstra(&start, |n| n.neighbors(min, max), |n| n.loc == end);
    if let Some((_, c)) = r {
        c
    } else {
        0
    }
}

fn parta(map: &Map) -> usize {
    dosolve(map, 0, 3)
}

fn partb(map: &Map) -> usize {
    dosolve(map, 4, 10)
}

fn parse(input: &str) -> Map {
    Map2d::from_str(input, |c| c.to_digit(10).unwrap() as usize)
}

pub fn solve(input: &str) -> (String, String) {
    let m = parse(input);
    (parta(&m).to_string(), partb(&m).to_string())
}

#[test]
fn test_neighbors() {
    let m = Map::from_str(
        "123\n456\n789\n",
        |c| c.to_digit(10).unwrap() as usize);
    const MAX: u8 = 3;
    let init = Node {
        map: Rc::new(m),
        loc: Point2d { x: 1, y: 1 },
        run: [0, 0, 0, 0],
    };

    let ns = init.neighbors(0, MAX);
    assert_eq!(ns.len(), 4);

    assert_eq!(ns[0].0.loc, Point2d { x: 1, y: 0 });
    assert_eq!(ns[0].0.run, [1, 0, 0, 0]);
    assert_eq!(ns[0].1, 2);

    assert_eq!(ns[1].0.loc, Point2d { x: 1, y: 2 });
    assert_eq!(ns[1].0.run, [0, 1, 0, 0]);
    assert_eq!(ns[1].1, 8);

    assert_eq!(ns[2].0.loc, Point2d { x: 0, y: 1 });
    assert_eq!(ns[2].0.run, [0, 0, 1, 0]);
    assert_eq!(ns[2].1, 4);

    assert_eq!(ns[3].0.loc, Point2d { x: 2, y: 1 });
    assert_eq!(ns[3].0.run, [0, 0, 0, 1]);
    assert_eq!(ns[3].1, 6);
}
