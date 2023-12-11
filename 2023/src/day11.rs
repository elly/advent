// Day 11: Cosmic Expansion
// We have a big map of a cluster of galaxies (# for galaxy, . for empty space).
// For part A, we want to load that map in, do some spatial expansion (expand
// every row/column that is empty), and then compute the sum of the shortest
// distances between all the galaxies.
//
// We could store a Vec<Vec<Tile>> but I strongly suspect part B will involve
// more expansion, so instead let's make this a sparse galaxy. We'll also
// compute the bounding box as we parse it, then we can do the expansion by
// iterating over rows/cols within the bounding box and adjusting the
// coordinates of the galaxies "below" the iteration point as we go.
//
// TODO: factor out a geometry library from this and day10

use std::collections::HashSet;
use num::abs;

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Point {
    x: i64,
    y: i64,
}

#[derive(Clone)]
struct Space {
    gals: HashSet<Point>,
    width: i64,
    height: i64,
}

impl Space {
    fn row_empty(&self, y: i64) -> bool {
        for x in 0 .. self.width {
            if self.gals.contains(&Point { x, y }) {
                return false;
            }
        }
        true
    }

    fn col_empty(&self, x: i64) -> bool {
        for y in 0 .. self.height {
            if self.gals.contains(&Point { x, y }) {
                return false;
            }
        }
        true
    }
}

fn parse(input: &str) -> Space {
    let mut gals = HashSet::new();
    let rows = input.split('\n').filter(|x| !x.is_empty()).enumerate();
    let mut width = 0;
    let mut height = 0;
    for (y, row) in rows {
        let cols = row.chars().enumerate();
        for (x, c) in cols {
            if c == '#' {
                gals.insert(Point { x: x as i64, y: y as i64 });
                if x > width {
                    width = x;
                }
                if y > height {
                    height = y;
                }
            }
        }
    }
    Space { gals, width: width as i64 + 1, height: height as i64 + 1 }
}

fn expandby(space: &Space, diff: i64) -> Space {
    // From the old coordinate space, build two vectors containing the offset we
    // need to apply to x and y coordinates to transform into the new space,
    // indexed by x and y coordinates in the old space. Then, as we copy points
    // into the new space, the new coordinates are x + dx[x] and y + dy[y].
    let mut dx = Vec::new();
    let mut dy = Vec::new();
    let mut cdx = 0;
    let mut cdy = 0;

    for x in 0 .. space.width {
        dx.push(cdx);
        if space.col_empty(x) {
            cdx += diff;
        }
    }

    for y in 0 .. space.height {
        dy.push(cdy);
        if space.row_empty(y) {
            cdy += diff;
        }
    }

    let mut gals = HashSet::new();
    for g in space.gals.iter() {
        gals.insert(Point { x: g.x + dx[g.x as usize], y: g.y + dy[g.y as usize] });
    }

    Space { gals, width: space.width + cdx, height: space.height + cdy }
}

fn shortest(ga: &Point, gb: &Point) -> usize {
    (abs(ga.x - gb.x) + abs(ga.y - gb.y)) as usize
}

fn all_shortest(space: &Space) -> usize {
    let gals: Vec<&Point> = space.gals.iter().collect();
    let mut total = 0;
    for i in 0 .. gals.len() {
        for j in i .. gals.len() {
            total += shortest(gals[i], gals[j]);
        }
    }
    total
}

fn parta(space: &Space) -> usize {
    all_shortest(&expandby(space, 1))
}

fn partb(space: &Space) -> usize {
    all_shortest(&expandby(space, 999_999))
}

pub fn solve(input: &str) -> (String, String) {
    let space = parse(input);
    (parta(&space).to_string(), partb(&space).to_string())
}
