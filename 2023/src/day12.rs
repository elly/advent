// Day 12: Hot Springs
//
// We're given a partly damaged map of springs ('#') and empty spaces ('.').
// Some of the spaces are damaged and replaced with '?'. The rows look like
// this:
//   .??..??...?##. 1,1,3
// The numbers at the end are the size of each contiguous group of damaged
// springs, so that line means "there's one damaged spring, then at least one
// blank space, then one damaged spring, then at least one blank space, then
// three damaged springs". All the damaged springs are accounted for by the
// numbers.
//
// For part A, we need to find out how many possible arrangements there are for
// each line, then sum all those counts. To do that, I think we can do a
// recursive search - for each '?', sum up the number of arrangements for a
// spring being there or not being there. It looks like there's never more than
// 15 or so '?'s in a line in the input, so the search space shouldn't be too
// big.

use std::collections::HashMap;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Tile {
    Spring,
    Empty,
    Unknown,
}

#[derive(Debug)]
struct Line {
    tiles: Vec<Tile>,
    runs: Vec<usize>,
}

fn parse_tile(t: char) -> Tile {
    match t {
        '#' => Tile::Spring,
        '.' => Tile::Empty,
        '?' => Tile::Unknown,
        _   => panic!("bogus tile {}", t),
    }
}

fn parse_line(input: &str) -> Line {
    let ps: Vec<_> = input.split(' ').collect();
    let tiles = ps[0].chars().map(parse_tile).collect();
    let runs = ps[1].split(',').map(|r| r.parse::<usize>().unwrap()).collect();
    Line { tiles, runs }
}

fn parse(input: &str) -> Vec<Line> {
    input.split('\n').filter(|x| !x.is_empty()).map(parse_line).collect()
}

fn decrun(runs: &[usize]) -> Vec<usize> {
    let mut c = runs.to_vec();
    c[0] -= 1;
    c
}

fn poprun(runs: &[usize]) -> &[usize] {
    if !runs.is_empty() {
        assert!(runs[0] == 0);
        &runs[1..]
    } else {
        runs
    }
}

fn swaphead(tiles: &[Tile], tile: Tile) -> Vec<Tile> {
    let mut ts = tiles.to_vec();
    ts[0] = tile;
    ts
}

type SolnCache = HashMap<(Vec<Tile>, Vec<usize>, bool), usize>;

fn solns(tiles: &[Tile], runs: &[usize], inrun: bool, cache: &mut SolnCache) -> usize {
    let hk = (tiles.to_vec(), runs.to_vec(), inrun);
    if cache.contains_key(&hk) {
        return *cache.get(&hk).unwrap();
    }

    // The recursive solution! Base cases:
    // 1) If both vectors are empty, we won - there's 1 solution.
    // 2) If tiles is empty but runs isn't, we didn't assign all the springs -
    //    there's no solutions.
    // A note about the runs slice: if there's a 0 at the head of it, that means
    // the current run must terminate (ie there must be a '.' here).
    // The recursion itself has some cases, where t is the current tile and r is
    // the head of the current run:
    // 4) t == '#':
    //     4a) r > 0: set inrun, decrement r, go next
    //     4b) r = 0: fail
    // 5) t == '.':
    //     5a) r > 0 and inrun: fail (we needed a longer run)
    //     5b) r > 0: clear inrun, go next
    //     5c) r = 0: clear inrun, remove r from runs, go next
    // 6) t == '?':
    //     try with head == '#' and head == '.', sum both
    if tiles.is_empty() {
        return if runs.is_empty() || (runs.len() == 1 && runs[0] == 0) {
            1
        } else {
            0
        };
    }

    let t = tiles[0];
    let r = runs.get(0).unwrap_or(&0);
    let res = match (t, r, inrun) {
        (Tile::Spring, 0, _) => 0,
        (Tile::Spring, _, _) => solns(&tiles[1..], &decrun(runs), true, cache),
        (Tile::Empty, 0, _) => solns(&tiles[1..], poprun(runs), false, cache),
        (Tile::Empty, _, true) => 0,
        (Tile::Empty, _, false) => solns(&tiles[1..], runs, false, cache),
        (Tile::Unknown, _, _) =>
            solns(&swaphead(tiles, Tile::Spring), runs, inrun, cache) +
            solns(&swaphead(tiles, Tile::Empty), runs, inrun, cache),
    };

    cache.insert(hk, res);
    res
}

fn parta(lines: &[Line]) -> usize {
    let mut cache = HashMap::new();
    lines.iter().map(|line| solns(&line.tiles, &line.runs, false, &mut cache)).sum()
}

fn unfold(line: &Line) -> Line {
    let mut tiles = Vec::new();
    let mut runs = Vec::new();
    for _ in 0 .. 5 {
        tiles.append(&mut line.tiles.to_vec());
        tiles.push(Tile::Unknown);
        runs.append(&mut line.runs.to_vec());
    }
    tiles.pop();
    Line { tiles, runs }
}

fn partb(lines: &[Line]) -> usize {
    let mut cache = HashMap::new();
    lines.iter().map(|line| solns(&line.tiles, &line.runs, false, &mut cache)).sum()
}

pub fn solve(input: &str) -> (String, String) {
    let input = parse(input);
    let longer: Vec<_> = input.iter().map(unfold).collect();
    (parta(&input).to_string(), partb(&longer).to_string())
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    fn t(input: &str) -> usize {
        let line = crate::day12::parse_line(input);
        crate::day12::solns(&line.tiles, &line.runs, false, &mut HashMap::new())
    }

    fn te(input: &str) -> usize {
        let line = crate::day12::parse_line(input);
        let line = crate::day12::unfold(&line);
        crate::day12::solns(&line.tiles, &line.runs, false, &mut HashMap::new())
    }

    #[test]
    fn test_solns_simple() {
        assert_eq!(t(". 0"), 1);
        assert_eq!(t("# 1"), 1);
        assert_eq!(t(". 1"), 0);
        assert_eq!(t("# 0"), 0);

        assert_eq!(t("## 2"), 1);
        assert_eq!(t("#.# 1,1"), 1);
        assert_eq!(t("..# 1"), 1);
    }

    #[test]
    fn test_solns_spring() {
        assert_eq!(t("? 0"), 1);
        assert_eq!(t("? 1"), 1);
        assert_eq!(t("?? 1"), 2);
        assert_eq!(t("#?#?#?#?# 1,1,1,1,1"), 1);
    }

    #[test]
    fn test_unfolded() {
        assert_eq!(te("# 1"), 1);
        assert_eq!(te("???.### 1,1,3"), 1);
    }
}
