// Day 14: Parabolic Reflector Dish
// The input is a 2d grid of rocks, which are either round ('O') or fixed ('#').
// We can "tip" the grid, causing round rocks to migrate in a direction.
// Part A: tip the grid north so that all the rounded rocks roll as far north as
// they can, then compute the sum of the distances in rows from the south edge
// to each rock, starting at 1.
// t0 result is 136

use std::collections::HashMap;

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
enum Tile {
    Round,
    Cube,
    Space,
}

#[derive(Clone, Hash, Eq, PartialEq)]
struct Map {
    tiles: Vec<Vec<Tile>>,
    width: usize,
    height: usize,
}

impl Map {
    fn contains(&self, y: i32, x: i32) -> bool {
        let uy = y as usize;
        let ux = x as usize;
        y >= 0 && x >= 0 && uy < self.height && ux < self.width
    }

    fn at(&self, y: i32, x: i32) -> Tile {
        assert!(y >= 0);
        assert!(x >= 0);
        self.tiles[y as usize][x as usize]
    }

    fn put(&mut self, y: i32, x: i32, t: Tile) {
        self.tiles[y as usize][x as usize] = t;
    }

    fn hashkey(&self) -> Vec<Tile> {
        let mut r = Vec::new();
        for y in 0 .. self.height {
            for x in 0 .. self.height {
                r.push(self.tiles[y][x]);
            }
        }
        r
    }
}

fn parse_tile(c: char) -> Tile {
    match c {
        'O' => Tile::Round,
        '#' => Tile::Cube,
        '.' => Tile::Space,
        _   => panic!("bad tile {}", c),
    }
}

fn unparse_tile(t: Tile) -> char {
    match t {
        Tile::Round => 'O',
        Tile::Cube => '#',
        Tile::Space => '.',
    }
}

fn parse(input: &str) -> Map {
    let mut tiles = Vec::new();
    let mut width = 0;
    for line in input.split('\n').filter(|x| !x.is_empty()) {
        let mut row = Vec::new();
        for c in line.chars() {
            row.push(parse_tile(c));
        }
        width = row.len();
        tiles.push(row);
    }
    let height = tiles.len();
    Map { tiles, width, height }
}

fn roll(map: &mut Map, y: i32, x: i32, dy: i32, dx: i32) {
    let mut y = y;
    let mut x = x;
    while map.contains(y, x) {
        let ny = y + dy;
        let nx = x + dx;
        if map.contains(ny, nx) && map.at(ny, nx) == Tile::Space {
            map.put(ny, nx, Tile::Round);
            map.put(y, x, Tile::Space);
        } else {
            break;
        }
        y = ny;
        x = nx;
    }
}

fn tilt(m: &mut Map, dy: i32, dx: i32) {
    // We need to process the array backwards when going east or south - always
    // process rocks that are closer to the edge we're tilting towards first.
    for y in 0 .. m.height {
        for x in 0 .. m.width {
            let ty = if dy > 0 || dx > 0 { m.height - y - 1 } else { y };
            let tx = if dy > 0 || dx > 0 { m.width - x - 1 } else { x };
            if m.tiles[ty][tx] == Tile::Round {
                roll(m, ty as i32, tx as i32, dy, dx);
            }
        }
    }
}

fn raster(map: &Map) {
    for y in 0 .. map.height {
        for x in 0 .. map.width {
            print!("{}", unparse_tile(map.tiles[y][x]));
        }
        println!("");
    }
    println!("");
}

fn spin(m: &mut Map) {
    // north west south east
    tilt(m, -1, 0);
    tilt(m, 0, -1);
    tilt(m, 1, 0);
    tilt(m, 0, 1);
}

fn north_load(map: &Map) -> usize {
    let mut t = 0;
    for y in 0 .. map.height {
        for x in 0 .. map.width {
            if map.tiles[y][x] == Tile::Round {
                t += map.height - y;
            }
        }
    }
    t
}

fn parta(map: &Map) -> usize {
    let mut map = map.clone();
    tilt(&mut map, -1, 0);
    north_load(&map)
}

fn spin_period(map: &Map) -> (usize, usize) {
    // TODO: why is it the case that the spin always has a period? it doesn't
    // seem like it necessarily has to; can chaotic behavior emerge?
    let mut map = map.clone();
    let mut seen: HashMap<Vec<Tile>, usize> = HashMap::new();
    seen.insert(map.hashkey(), 0);
    for i in 1 .. 1_000_000_000 {
        spin(&mut map);
        let key = map.hashkey();
        if let Some(s) = seen.get(&key) {
            return (*s, i - *s);
        }
        seen.insert(key, i);
    }
    (0, 0)
}

fn spin_times(map: &Map, n: usize) -> Map {
    let mut map = map.clone();
    for i in 0 .. n {
        spin(&mut map);
    }
    map
}

fn partb(map: &Map) -> usize {
    let (s, p) = spin_period(&map);
    let start = spin_times(&map, s);
    let rest = (1_000_000_000 - s) % p;
    let done = spin_times(&start, rest);
    north_load(&done)
}

pub fn solve(input: &str) -> (String, String) {
    let map = parse(input);
    (parta(&map).to_string(), partb(&map).to_string())
}
