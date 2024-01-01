// Day 13: Point of Incidence
// TODO: brief description
// TODO: rewrite this to use Map2d

#[derive(Clone, Copy, Eq, PartialEq)]
enum Tile {
    Ash,
    Rock,
}

#[derive(Clone)]
struct Map {
    tiles: Vec<Vec<Tile>>,
    width: usize,
    height: usize,
}

fn parse_tile(c: char) -> Tile {
    match c {
        '.' => Tile::Ash,
        '#' => Tile::Rock,
        _   => panic!("bogus tile: {}", c),
    }
}

fn parse_map(lines: Vec<&str>) -> Map {
    let mut tiles = Vec::new();
    let mut width = 0;
    for line in lines.iter() {
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

fn parse_maps(input: &str) -> Vec<Map> {
    let mut lines: Vec<&str> = Vec::new();
    let mut maps = Vec::new();
    for line in input.split('\n') {
        if !line.is_empty() {
            lines.push(line);
        } else {
            maps.push(parse_map(lines));
            lines = Vec::new();
        }
    }
    maps
}

fn reflects_vertically_at(map: &Map, y: usize) -> bool {
    let mut d = 0;
    while d <= y && d + y + 1 < map.height {
        let cy = y - d;
        let ry = y + 1 + d;
        for x in 0 .. map.width {
            if map.tiles[cy][x] != map.tiles[ry][x] {
                return false;
            }
        }
        d += 1;
    }
    true
}

fn reflects_horizontally_at(map: &Map, x: usize) -> bool {
    let mut d = 0;
    while d <= x && d + x + 1 < map.width {
        let cx = x - d;
        let rx = x + 1 + d;
        for y in 0 .. map.height {
            if map.tiles[y][cx] != map.tiles[y][rx] {
                return false;
            }
        }
        d += 1;
    }
    true
}

fn vert_reflection(map: &Map, but: usize) -> usize {
    for y in 0 .. map.height - 1 {
        if reflects_vertically_at(map, y) && but != y + 1 {
            return y + 1;
        }
    }
    0
}

fn horiz_reflection(map: &Map, but: usize) -> usize {
    for x in 0 .. map.width - 1 {
        if reflects_horizontally_at(map, x) && but != x + 1 {
            return x + 1;
        }
    }
    0
}

fn score(map: &Map) -> usize {
    100 * vert_reflection(map, 0) + horiz_reflection(map, 0)
}

fn parta(maps: &Vec<Map>) -> usize {
    maps.iter().map(score).sum()
}

fn flip(tile: Tile) -> Tile {
    match tile {
        Tile::Ash => Tile::Rock,
        Tile::Rock => Tile::Ash,
    }
}

fn smudge_score(map: &Map) -> usize {
    let vr = vert_reflection(map, 0);
    let hr = horiz_reflection(map, 0);

    let mut map: Map = map.clone();
    for y in 0 .. map.height {
        for x in 0 .. map.width {
            map.tiles[y][x] = flip(map.tiles[y][x]);
            let nvr = vert_reflection(&map, vr);
            let nhr = horiz_reflection(&map, hr);
            if nvr != 0 {
                return 100 * nvr;
            } else if nhr != 0 {
                return nhr;
            }
            map.tiles[y][x] = flip(map.tiles[y][x]);
        }
    }

    panic!("no good");
}

fn partb(maps: &Vec<Map>) -> usize {
    maps.iter().map(smudge_score).sum()
}

pub fn solve(input: &str) -> (String, String) {
    let maps = parse_maps(input);
    (parta(&maps).to_string(), partb(&maps).to_string())
}
