// Day 16: The Floor Will Be Lava
// Empty space: . (same direction)
// Mirrors: / \ (reflected 90 degrees depending on the mirror)
// Splitters: | -
// If encountering a splitter parallel to beam, passes through as if empty
// If encountering a splitter perpendicular to beam, reflect 90 degrees both
// ways
// Tile is energized if at least one beam passes through it
// Part A: how many tiles are energized by a beam starting in the top left going
// right?

use std::collections::VecDeque;

use crate::map2d;

#[derive(Clone, Copy, Eq, PartialEq)]
enum Tile {
    Space,
    MirWE,
    MirEW,
    SplitNS,
    SplitEW,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Dir {
    North = 0,
    South = 1,
    West  = 2,
    East  = 3,
}

impl Tile {
    fn step(&self, dir: &Dir) -> (Dir, Option<Dir>) {
        match (*self, *dir) {
            (Tile::Space, d)            => (d,          None),

            // \
            (Tile::MirWE, Dir::North)   => (Dir::West,  None),
            (Tile::MirWE, Dir::South)   => (Dir::East,  None),
            (Tile::MirWE, Dir::East)    => (Dir::South, None),
            (Tile::MirWE, Dir::West)    => (Dir::North, None),

            // /
            (Tile::MirEW, Dir::North)   => (Dir::East,  None),
            (Tile::MirEW, Dir::South)   => (Dir::West,  None),
            (Tile::MirEW, Dir::East)    => (Dir::North, None),
            (Tile::MirEW, Dir::West)    => (Dir::South, None),

            (Tile::SplitNS, Dir::West)  => (Dir::North, Some(Dir::South)),
            (Tile::SplitNS, Dir::East)  => (Dir::North, Some(Dir::South)),
            (Tile::SplitNS, d)          => (d,          None),

            (Tile::SplitEW, Dir::North) => (Dir::West, Some(Dir::East)),
            (Tile::SplitEW, Dir::South) => (Dir::West, Some(Dir::East)),
            (Tile::SplitEW, d)          => (d,         None),
        }
    }
}

type Map = map2d::Map2d<Tile>;

struct Point(i32, i32);

impl Point {
    fn moveby(&self, dir: &Dir) -> Point {
        match *dir {
            Dir::North => Point(self.0, self.1 - 1),
            Dir::South => Point(self.0, self.1 + 1),
            Dir::West  => Point(self.0 - 1, self.1),
            Dir::East  => Point(self.0 + 1, self.1),
        }
    }
}

struct Beam(Point, Dir);

fn parse(input: &str) -> Map {
    map2d::Map2d::from_str(input,
        |cell| match cell {
            '.'  => Tile::Space,
            '\\' => Tile::MirWE,
            '/'  => Tile::MirEW,
            '|'  => Tile::SplitNS,
            '-'  => Tile::SplitEW,
            _    => panic!("bogus tile {}", cell),
        }
    )
}

fn zap(map: &Map, start: Beam) -> usize {
    // TODO: use Map2d for zm and vm
    let mut zm = Vec::new();
    let mut vm = Vec::new();
    for _ in 0 .. map.height {
        let mut z = Vec::new();
        let mut v = Vec::new();
        for _ in 0 .. map.width {
            z.push(0);
            let mut tm = Vec::new();
            for _ in 0 .. 4 {
                tm.push(false);
            }
            v.push(tm);
        }
        zm.push(z);
        vm.push(v);
    }

    let mut zq = VecDeque::new(); 
    zq.push_back(start);

    while !zq.is_empty() {
        let Beam(p, d) = zq.pop_front().unwrap();
        if !map.inbounds(p.1, p.0) {
            continue;
        }
        if vm[p.1 as usize][p.0 as usize][d as usize] {
            continue;
        }
        zm[p.1 as usize][p.0 as usize] += 1;
        vm[p.1 as usize][p.0 as usize][d as usize] = true;
        let (a, b) = map.at(p.1, p.0).step(&d);
        zq.push_back(Beam(p.moveby(&a), a));
        if let Some(b) = b {
            zq.push_back(Beam(p.moveby(&b), b));
        }
    }

    let mut t = 0;
    for y in 0 .. zm.len() {
        for x in 0 .. zm[y].len() {
            t += if zm[y][x] > 0 { 1 } else { 0 }
        }
    }
    t
}

fn parta(map: &Map) -> usize {
    zap(&map, Beam(Point(0, 0), Dir::East))
}

fn partb(map: &Map) -> usize {
    let mut best = 0;
    for y in 0 .. map.height {
        let y = y as i32;
        let v = zap(&map, Beam(Point(0, y), Dir::East));
        if v > best {
            best = v;
        }

        let x = (map.width - 1) as i32;
        let v = zap(&map, Beam(Point(x, y), Dir::West));
        if v > best {
            best = v;
        }
    }

    for x in 0 .. map.width {
        let x = x as i32;
        let v = zap(&map, Beam(Point(x, 0), Dir::South));
        if v > best {
            best = v;
        }

        let y = (map.height - 1) as i32;
        let v = zap(&map, Beam(Point(x, y), Dir::North));
        if v > best {
            best = v;
        }
    }

    best
}

pub fn solve(input: &str) -> (String, String) {
    let map = parse(input);
    (parta(&map).to_string(), partb(&map).to_string())
}
