// Day 21: Step Counter
// Given a map like:
//   ...........
//   .....###.#.
//   .###.##..#.
//   ..#.#...#..
//   ....#.#....
//   .##..S####.
//   .##..#...#.
//   .......##..
//   .##.#.####.
//   .##..##.##.
//   ...........
// where . is passable, and S (also passable) is the start location,
// Part a: how many places are exactly 64 steps from the start location?
// Part b: suppose that we tiled the map infinitely; how many places are
// 26501365 steps from the start location?
//
// Thoughts for part b: we have an infinite grid of gardens. We can give each
// garden a pair of coordinates (gx,gy) which are offsets from the starting
// garden. If gx and gy are both nonzero, then our shortest route to the near
// corner of the garden at (gx,gy) involves:
// 1. Make our way to the nearest corner of the (0,0) garden to it
// 2. Go (abs(gx) - 1) * width + 1 + (abs(gy) - 1) * height + 1 cells to reach
//    its corner nearest the (0,0) garden
// 3. Enumerate all the cells in reach from the corner nearest the (0,0) garden
//
// And if gx is 0, we have a special case, depending whether there is a faster
// route horizontally through the garden or not. If gy is 0, same deal but
// vertically.
//
// So, we need a primitive that finds the cells reachable within n steps from a
// given point (which we have), and a similar primitive that finds the shortest
// path from one cell to another so that we can check for paths across the map.

use crate::map2d::{Dir2d,Map2d,Point2d};
use std::collections::{HashMap,VecDeque};

#[derive(Clone, Copy, Eq, PartialEq)]
enum Tile {
    Start,
    Rock,
    Garden,
}

type Map = Map2d<Tile>;

fn parse(input: &str) -> Map {
    Map::from_str(input, |c| {
        match c {
            'S' => Tile::Start,
            '#' => Tile::Rock,
            _   => Tile::Garden,
        }})
}

fn outs(map: &Map, pos: Point2d) -> Vec<Point2d> {
    let mut r = Vec::new();
    for dir in [Dir2d::North, Dir2d::South, Dir2d::East, Dir2d::West] {
        let p = pos.moveby(dir);
        if map.inboundsp(p) && *map.atp(p) != Tile::Rock {
            r.push(p);
        }
    }
    r
}

fn start(map: &Map) -> Point2d {
    for y in 0 .. map.height {
        for x in 0 .. map.width {
            let p = Point2d { x: x as i32, y: y as i32 };
            if *map.atp(p) == Tile::Start {
                return p;
            }
        }
    }
    panic!("no start");
}

type StepMap = HashMap<Point2d, usize>;

fn flood(map: &Map, start: Point2d) -> HashMap<Point2d, usize> {
    let mut s = HashMap::new();
    let mut q = VecDeque::new();

    q.push_back((start, 0));

    while !q.is_empty() {
        let (p, d) = q.pop_front().unwrap();

        if s.contains_key(&p) {
            continue;
        }

        s.insert(p, d);

        for n in outs(map, p) {
            q.push_back((n, d + 1));
        }
    }

    s
}

fn reachn(sm: &StepMap, n: usize) -> usize {
    sm.values().filter(|x| **x % 2 == n % 2 && **x <= n).count()
}

fn furthest(sm: &StepMap) -> usize {
    *sm.values().max().unwrap()
}

fn parta(map: &Map, dist: usize) -> usize {
    let flooded = flood(map, start(map));
    reachn(&flooded, dist)
}

// Part b solution strategy:
// 1. Build a distance map from S, and from each corner.
// 2. For each of the four quadrants:
//    a. Figure out how many gardens are completely covered (meaning enough
//       movement remains when entering their close corner to reach every
//       square); add them to the total.
//    b. For gardens that are not completely covered, figure out how many cells
//       can be reached from the close corner with the distance remaining.
// 3. For each of the four axes:
fn partb(map: &Map) -> usize {
    // The logic for strides & cover relies on this, because it assumes that the
    // width can be used for either a horizontal or vertical move.
    assert!(map.width == map.height);

    const STEPS: usize = 26501365;
    // const STEPS: usize = 5000;
    let fromstart = flood(map, start(map));
    let pts = [(map.topleft(), map.bottomright()),
               (map.topright(), map.bottomleft()),
               (map.bottomleft(), map.topright()),
               (map.bottomright(), map.topleft())];

    let mut t = reachn(&fromstart, STEPS);
    // dbg!(t);

    for (p,bp) in pts {
        let b = fromstart.get(&bp).unwrap();
        let fm = flood(map, p);
        let fd = furthest(&fm);
        let reacho = reachn(&fm, 10001);
        let reache = reachn(&fm, 10000);

        if STEPS < b + 2 {
            // Don't have enough steps to even reach the corner of the first map
            continue;
        }

        // Any garden that we reach with at least fd move left, we have full
        // coverage over. Let's iterate, starting from the near corner of the
        // nearest garden:
        let mut mul = 1;   // How many gardens there are at this many strides
        let mut left = STEPS - (b + 2);
        let stride = map.width;

        while left >= fd {
            let mt = if left % 2 == 0 { reache } else { reacho };
            // dbg!(mt, mul);
            t += mt * mul;
            mul += 1;
            left -= stride;
        }
        
        // Now we have less than one full garden of reach left, so:
        let mt = reachn(&fm, left);
        t += mt * mul;

        // Case that I missed originally - we stop the original iteration when
        // we don't have enough movement to reach *every* reachable square in a
        // garden, but we might have enough movement left to reach an adjacent
        // garden, too. Slide over to there and account for it.
        if left > stride {
            let mt = reachn(&fm, left - stride);
            t += mt * (mul + 1);
        }
    }

    // Now to handle the ones that lie on the axes.
    // For each of these, the approach is similar to above, but we use the
    // centerpoints instead of corners, and use the shortest
    // centerpoint-to-centerpoint distance as we go. There's also no increasing
    // multiplier.
    let pts = [(map.centerleft(), map.centerright()),
               (map.centerright(), map.centerleft()),
               (map.centertop(), map.centerbottom()),
               (map.centerbottom(), map.centertop())];
    for (p,bp) in pts {
        let b = fromstart.get(&bp).unwrap();
        let fm = flood(map, p);
        let fd = furthest(&fm);
        let reacho = reachn(&fm, 10001);
        let reache = reachn(&fm, 10000);

        if STEPS < b + 1 {
            // Don't have to move in one direction for this one
            continue;
        }

        let mut left = STEPS - (b + 1);
        // XXX: this is very ugly. In the real input I have (but *not* the test
        // input), there are a clear row/column at the center. This works on the
        // real input but for the test input it does not.
        let stride = map.width;

        while left >= fd {
            let mt = if left % 2 == 0 { reache } else { reacho };
            // dbg!(mt);
            t += mt;
            left -= stride;
        }

        let mt = reachn(&fm, left);
        // dbg!(mt);
        t += mt;
    }

    t
}

// 621944727930768 for my real input

pub fn solve(input: &str) -> (String, String) {
    let map = parse(input);

    (parta(&map, 64).to_string(), partb(&map).to_string())
}
