// map2d & related types

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Point2d {
    pub x: i32,
    pub y: i32,
}

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
    pub fn from_size(width: usize, height: usize,
                     factory: impl Fn(Point2d) -> T) -> Map2d<T> {
        let mut cells = Vec::new();
        for y in 0..height {
            let mut row = Vec::new();
            for x in 0..width {
                row.push(factory(Point2d { x: x as i32, y: y as i32 }));
            }
            cells.push(row);
        }
        Map2d { cells, width, height }
    }

    pub fn from_str(input: &str, parse: fn(char) -> T) -> Map2d<T> {
        let input: Vec<_> = input.split('\n').collect();
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

    pub fn atp(&self, p: Point2d) -> &T {
        assert!(p.y >= 0);
        assert!(p.x >= 0);
        &self.cells[p.y as usize][p.x as usize]
    }

    pub fn update(&mut self, p: Point2d, f: impl Fn(&T) -> T) {
        let ux = p.x as usize;
        let uy = p.y as usize;
        self.cells[uy][ux] = f(&self.cells[uy][ux]);
    }

    pub fn sum(&self, f: impl Fn(&T) -> usize) -> usize {
        let mut t = 0;
        for y in 0..self.height {
            for x in 0..self.width {
                t += f(&self.cells[y][x]);
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

    pub fn topright(&self) -> Point2d {
        Point2d { x: self.width as i32 - 1, y: 0 }
    }

    pub fn bottomleft(&self) -> Point2d {
        Point2d { x: 0, y: self.height as i32 - 1 }
    }

    pub fn bottomright(&self) -> Point2d {
        Point2d { x: self.width as i32 - 1, y: self.height as i32 - 1 }
    }

    pub fn centerleft(&self) -> Point2d {
        assert!(self.height % 2 == 1);
        Point2d { x: 0, y: (self.height as i32 - 1) / 2 }
    }

    pub fn centerright(&self) -> Point2d {
        assert!(self.height % 2 == 1);
        Point2d { x: self.width as i32 - 1, y: (self.height as i32 - 1) / 2 }
    }

    pub fn centertop(&self) -> Point2d {
        assert!(self.width % 2 == 1);
        Point2d { x: (self.width as i32 - 1) / 2, y: 0 }
    }

    pub fn centerbottom(&self) -> Point2d {
        assert!(self.width % 2 == 1);
        Point2d { x: (self.width as i32 - 1) / 2, y: self.height as i32 - 1 }
    }
}

#[cfg(test)]
mod tests {
    use crate::map2d::{Map2d,Point2d};
    fn pdigit(c: char) -> usize {
        c.to_digit(10).unwrap() as usize
    }

    const MAP: &str = "123\n456\n789\n012\n";

    #[test]
    fn test_parse() {
        let m = Map2d::from_str(MAP, pdigit);
        assert_eq!(m.width, 3);
        assert_eq!(m.height, 4);
        assert_eq!(*m.atp(Point2d { x: 0, y: 0 }), 1);
        assert_eq!(*m.atp(Point2d { x: 2, y: 0 }), 3);
        assert_eq!(*m.atp(Point2d { x: 0, y: 3 }), 0);
        assert_eq!(*m.atp(Point2d { x: 2, y: 3 }), 2);
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
