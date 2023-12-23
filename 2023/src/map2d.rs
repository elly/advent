// map2d & related types

use std::collections::{HashSet, VecDeque};

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Point2d {
    pub x: i32,
    pub y: i32,
}

pub const POINT_ZERO: Point2d = Point2d { x: 0, y: 0 };

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Dir2d {
    North = 0,
    South = 1,
    West = 2,
    East = 3,
}

impl Dir2d {
    pub fn opposite(&self) -> Dir2d {
        match *self {
            Dir2d::North => Dir2d::South,
            Dir2d::South => Dir2d::North,
            Dir2d::West  => Dir2d::East,
            Dir2d::East  => Dir2d::West,
        }
    }
}

impl Point2d {
    pub fn movebyn(&self, dir: Dir2d, d: i32) -> Point2d {
        match dir {
            Dir2d::North => Point2d { x: self.x, y: self.y - d },
            Dir2d::South => Point2d { x: self.x, y: self.y + d },
            Dir2d::West  => Point2d { x: self.x - d, y: self.y },
            Dir2d::East  => Point2d { x: self.x + d, y: self.y },
        }
    }

    pub fn moveby(&self, dir: Dir2d) -> Point2d {
        self.movebyn(dir, 1)
    }
}

#[derive(Clone)]
pub struct Map2d<T> {
    pub cells: Vec<Vec<T>>,
    pub width: usize,
    pub height: usize,
}

impl<T: Clone> Map2d<T> {
    pub fn new(height: usize, width: usize, factory: impl Fn(Point2d) -> T) -> Map2d<T> {
        let mut cells = Vec::new();
        for y in 0 .. height {
            let mut row = Vec::new();
            for x in 0 .. width {
                row.push(factory(Point2d { x: x as i32, y: y as i32 }));
            }
            cells.push(row);
        }
        Map2d { cells, width, height, }
    }

    pub fn from_str(input: &str, parse: fn(char) -> T) -> Map2d<T> {
        let strs: Vec<_> = input.split('\n').collect();
        Map2d::from_strs(&strs, parse)
    }

    pub fn from_strs(input: &[&str], parse: fn(char) -> T) -> Map2d<T> {
        let mut cells = Vec::new();
        for line in input.iter().filter(|x| !x.is_empty()) {
            let mut row = Vec::new();
            for c in line.chars() {
                row.push(parse(c));
            }
            cells.push(row);
        }

        let width = cells[0].len();
        let height = cells.len();

        Map2d { cells, width, height }
    }

    pub fn at(&self, y: i32, x: i32) -> &T {
        assert!(y >= 0);
        assert!(x >= 0);
        &self.cells[y as usize][x as usize]
    }

    pub fn atu(&self, y: usize, x: usize) -> &T {
        &self.cells[y][x]
    }

    pub fn atp(&self, p: Point2d) -> &T {
        self.at(p.y, p.x)
    }

    pub fn transform<U>(&self, mapping: impl Fn(Point2d) -> U) -> Map2d<U> {
        let mut cells = Vec::new();
        for y in 0 .. self.height {
            let mut row = Vec::new();
            for x in 0 .. self.width {
                row.push(mapping(Point2d { x: x as i32, y: y as i32 }));
            }
            cells.push(row);
        }

        let width = cells[0].len();
        let height = cells.len();
        Map2d { cells, width, height }
    }

    pub fn sum(&self, mapping: fn(&T) -> usize) -> usize {
        let mut t = 0;
        for y in 0 .. self.height {
            for x in 0 .. self.width {
                t += mapping(&self.cells[y][x]);
            }
        }
        t
    }

    pub fn inbounds(&self, y: i32, x: i32) -> bool {
        if x < 0 || y < 0 {
            return false;
        }
        if x as usize >= self.width || y as usize >= self.height {
            return false;
        }
        true
    }

    pub fn inboundsp(&self, p: Point2d) -> bool {
        self.inbounds(p.y, p.x)
    }

    pub fn topleft(&self) -> Point2d {
        Point2d { x: 0, y: 0 }
    }

    pub fn bottomright(&self) -> Point2d {
        Point2d { x: self.width as i32 - 1, y: self.height as i32 - 1 }
    }

    pub fn points(&self) -> Vec<Point2d> {
        let mut r = Vec::new();
        for y in 0 .. self.height {
            for x in 0 .. self.width {
                r.push(Point2d { x: x as i32, y: y as i32 });
            }
        }
        r
    }

    pub fn reachability(&self, start: Point2d,
                        passable: impl Fn(Point2d) -> bool) -> Map2d<bool> {
        let mut q = VecDeque::new();
        let mut m = HashSet::new();
        q.push_back(start);
        while !q.is_empty() {
            let p = q.pop_front().unwrap();
            if m.contains(&p) {
                continue;
            }
            m.insert(p);
            for d in [Dir2d::North, Dir2d::South, Dir2d::West, Dir2d::East] {
                let np = p.moveby(d);
                if self.inboundsp(np) && passable(np) {
                    q.push_back(np);
                }
            }
        }
        Map2d::<bool>::new(self.height, self.width, |p| m.contains(&p))
    }
}

#[cfg(test)]
mod tests {
    use crate::map2d::Map2d;
    fn pdigit(c: char) -> usize {
        c.to_digit(10).unwrap() as usize
    }

    const MAP: &str = "123\n456\n789\n012\n";

    #[test]
    fn test_parse() {
        let m = Map2d::from_str(MAP, pdigit);
        assert_eq!(m.width, 3);
        assert_eq!(m.height, 4);
        assert_eq!(*m.at(0, 0), 1);
        assert_eq!(*m.at(0, 2), 3);
        assert_eq!(*m.at(3, 0), 0);
        assert_eq!(*m.at(3, 2), 2);
    }

    #[test]
    fn test_xform() {
        let m = Map2d::from_str(MAP, pdigit);
        let q = m.transform(|p| m.atp(p) * 10);
        assert_eq!(*m.at(0, 0), 1);
        assert_eq!(*q.at(0, 0), 10);
    }

    #[test]
    fn test_sum() {
        let m = Map2d::from_str(MAP, pdigit);
        assert_eq!(m.sum(|x| *x), 48);
    }

    #[test]
    fn test_bounds() {
        let m = Map2d::from_str(MAP, pdigit);
        assert_eq!(m.inbounds(0, 0), true);
        assert_eq!(m.inbounds(-1, 0), false);
        assert_eq!(m.inbounds(4, 0), false);
        assert_eq!(m.inbounds(0, -1), false);
        assert_eq!(m.inbounds(0, 3), false);
    }
}
