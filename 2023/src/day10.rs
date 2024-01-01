// Day 10:
// TODO: brief description of the problem & how it is solved
// TODO: make this use Map2d and friends

use std::collections::{HashSet,VecDeque};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct Point(usize, usize);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Dir {
    North,
    South,
    East,
    West,
}

const DIRS: [Dir; 4] = [Dir::North, Dir::South, Dir::East, Dir::West];

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Tile {
    NS,
    EW,
    NE,
    NW,
    SE,
    SW,
    Dot,
    Start,
    Fake,
}

impl Tile {
    fn turn(&self, f: Dir) -> Option<Dir> {
        match (*self, f) {
            (Tile::NS, Dir::South) => Some(Dir::South),
            (Tile::NS, Dir::North) => Some(Dir::North),
            (Tile::EW, Dir::West) => Some(Dir::West),
            (Tile::EW, Dir::East) => Some(Dir::East),
            (Tile::NE, Dir::South) => Some(Dir::East),
            (Tile::NE, Dir::West) => Some(Dir::North),
            (Tile::NW, Dir::East) => Some(Dir::North),
            (Tile::NW, Dir::South) => Some(Dir::West),
            (Tile::SE, Dir::North) => Some(Dir::East),
            (Tile::SE, Dir::West) => Some(Dir::South),
            (Tile::SW, Dir::North) => Some(Dir::West),
            (Tile::SW, Dir::East) => Some(Dir::South),
            (Tile::Start, ff)      => Some(ff),
            _ => None,
        }
    }

    fn connects(&self, f: Dir) -> bool {
        if let Some(_) = self.turn(f) {
            true
        } else {
            false
        }
    }

    fn passable(&self) -> bool {
        match *self {
            Tile::Dot => true,
            Tile::Fake => true,
            _ => false,
        }
    }
}

struct Map {
    tiles: Vec<Vec<Tile>>,
    start: Point,
    width: usize,
    height: usize,
}

impl Map {
    fn tile_at(&self, p: Point) -> Tile {
        self.tiles[p.1][p.0]
    }

    fn try_move(&self, p:Point, d: Dir) -> Option<Point> {
        match d {
            Dir::North if p.1 > 0                => Some(Point(p.0, p.1 - 1)),
            Dir::South if p.1 < self.height - 1  => Some(Point(p.0, p.1 + 1)),
            Dir::West if p.0 > 0                 => Some(Point(p.0 - 1, p.1)),
            Dir::East if p.0 < self.width - 1    => Some(Point(p.0 + 1, p.1)),
            _                                    => None,
        }
    }

    fn step(&self, from: Point, face: Dir) -> Option<(Point, Dir)> {
        if let Some(np) = self.try_move(from, face) {
            let t = self.tile_at(np);
            if let Some(nd) = t.turn(face) {
                Some((np, nd))
            } else {
                None
            }
        } else {
            None
        }
    }
}

fn parse_tile(glyph: char) -> Tile {
    match glyph {
        '|' => Tile::NS,
        '-' => Tile::EW,
        'L' => Tile::NE,
        'J' => Tile::NW,
        '7' => Tile::SW,
        'F' => Tile::SE,
        '.' => Tile::Dot,
        'S' => Tile::Start,
        _ => panic!("tile?"),
    }
}

fn parse_line(input: &str) -> Vec<Tile> {
    input.chars().map(parse_tile).collect()
}

fn parse(input: &str) -> Map {
    let tiles: Vec<Vec<Tile>> = input.split('\n')
                                     .filter(|x| !x.is_empty())
                                     .map(parse_line).collect();
    let mut start = Point(0, 0);
    let height = tiles.len();
    let width = tiles[0].len();
    for y in 0 .. height {
        for x in 0 .. width {
            if tiles[y][x] == Tile::Start {
                start = Point(x, y);
            }
        }
    }

    Map { tiles, start, width, height }
}

fn follow(map: &Map, from: Point, facing: Dir) -> Vec<Point> {
    let mut p = from;
    let mut f = facing;
    let mut path = Vec::new();

    while path.is_empty() || p != from {
        if let Some((np, nf)) = map.step(p, f) {
            path.push(np);
            p = np;
            f = nf;
        } else {
            return Vec::new();
        }
    }

    path
}

fn findloop(map: &Map) -> Vec<Point> {
    for d in DIRS {
        let path = follow(map, map.start, d);
        if !path.is_empty() {
            return path;
        }
    }
    Vec::new()
}

fn parta(_map: &Map, path: &Vec<Point>) -> usize {
    path.len() / 2
}

// For part B, we want to know how many tiles are fully enclosed within the
// loop (or how many tiles are connected to the outside). A tile is connected to
// the outside if it can be reached by a path that doesn't *cross* a pipe; in
// particular, it's legal to flow through a gap between two pipes. It seems like
// a flood fill is a reasonable way to implement this, but we could also simply
// consider every point and try to path to the outside of the loop. A flood fill
// looks appealing because you are connected if one of your neighbors is
// (generally) but dealing with flooding tiles which actually contain the loop
// seems annoying.
//
// On further reflection, it will probably be annoying to try to DFS from points
// inside as well, since we'll still have to wander through the "between" spaces
// that we have no way to describe or reason about.
//
// New idea: what if we expand the map so that the between spaces actually are
// spaces? We'd take our existing map (with the loop only), intersperse dots,
// reconnect the loop, *then* flood fill. We'd need to be able to reconstruct
// which points were part of the original map but I bet it'd work... so:
//
// 1. Strip the map down to just the loop.
// 2. Expand the map by interspersing fake tiles.
// 3. Reconnect the loop. Check that it is still connected!
// 4. Flood fill.
// 5. Count flooded tiles that are not fakes.
fn strip(map: &Map, path: &Vec<Point>) -> Map {
    let mut looptiles = HashSet::new();
    for p in path {
        looptiles.insert(*p);
    }
    let mut r = Vec::new();
    for y in 0 .. map.height {
        r.push(Vec::new());
        for x in 0 .. map.width {
            let p = Point(x, y);
            if looptiles.contains(&p) {
                r[y].push(map.tile_at(p));
            } else {
                r[y].push(Tile::Dot);
            }
        }
    }
    Map { tiles: r, start: map.start, width: map.width, height: map.height }
}

fn expand(map: Map) -> Map {
    let mut tiles = Vec::new();
    tiles.push(Vec::new());
    tiles[0].push(Tile::Fake);
    for _ in 0 .. map.width {
        tiles[0].push(Tile::Fake);
        tiles[0].push(Tile::Fake);
    }
    for y in 0 .. map.height {
        tiles.push(Vec::new());
        tiles[y * 2 + 1].push(Tile::Fake);
        for x in 0 .. map.width {
            tiles[y * 2 + 1].push(map.tile_at(Point(x, y)));
            tiles[y * 2 + 1].push(Tile::Fake);
        }

        tiles.push(Vec::new());
        tiles[y * 2 + 2].push(Tile::Fake);
        for _ in 0 .. map.width {
            tiles[y * 2 + 2].push(Tile::Fake);
            tiles[y * 2 + 2].push(Tile::Fake);
        }
    }

    let start = Point(map.start.0 * 2 + 1, map.start.1 * 2 + 1);

    Map { tiles, start, width: map.width * 2 + 1, height: map.height * 2 + 1 }
}

fn reconnect_tile(map: &Map, p: Point) -> Tile {
    if p.1 > 0 && p.1 < map.height - 1 {
        let n = map.tile_at(Point(p.0, p.1 - 1));
        let s = map.tile_at(Point(p.0, p.1 + 1));
        if n.connects(Dir::North) && s.connects(Dir::South) {
            return Tile::NS;
        }
    }

    if p.0 > 0 && p.0 < map.width - 1 {
        let w = map.tile_at(Point(p.0 - 1, p.1));
        let e = map.tile_at(Point(p.0 + 1, p.1));
        if w.connects(Dir::West) && e.connects(Dir::East) {
            return Tile::EW;
        }
    }

    return map.tile_at(p);
}

fn reconnect(map: Map) -> Map {
    let mut tiles = Vec::new();
    for y in 0 .. map.height {
        tiles.push(Vec::new());
        for x in 0 .. map.width {
            tiles[y].push(reconnect_tile(&map, Point(x, y)));
        }
    }
    Map { tiles, start: map.start, width: map.width, height: map.height }
}

fn flood(map: &Map) -> HashSet<Point> {
    let mut r: HashSet<Point> = HashSet::new();
    let mut queue = VecDeque::new();

    queue.push_back(Point(0, 0));

    while !queue.is_empty() {
        let p = queue.pop_front().unwrap();
        if r.contains(&p) || !map.tile_at(p).passable() {
            continue;
        }
        r.insert(p);

        if p.0 > 0 {
            queue.push_back(Point(p.0 - 1, p.1));
        }
        if p.0 < map.width - 1 {
            queue.push_back(Point(p.0 + 1, p.1));
        }
        if p.1 > 0 {
            queue.push_back(Point(p.0, p.1 - 1));
        }
        if p.1 < map.height - 1 {
            queue.push_back(Point(p.0, p.1 + 1));
        }
    }

    r
}

fn unflooded(map: &Map, flooded: &HashSet<Point>) -> usize {
    let mut r = 0;
    for y in 0 .. map.height {
        for x in 0 .. map.width {
            let p = Point(x, y);
            if map.tile_at(p) == Tile::Dot && !flooded.contains(&p) {
                r += 1;
            }
        }
    }
    r
}

fn partb(map: &Map, path: &Vec<Point>) -> usize {
    let map = strip(map, path);
    assert!(findloop(&map).len() == path.len());

    let map = expand(map);
    let map = reconnect(map);
    assert!(findloop(&map).len() > 0);

    let flooded = flood(&map);
    unflooded(&map, &flooded)
}

pub fn solve(input: &str) -> (String, String) {
    let map = parse(input);
    let path = findloop(&map);
    (parta(&map, &path).to_string(), partb(&map, &path).to_string())
}
