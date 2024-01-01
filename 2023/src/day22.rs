// Day 22: Sand Slabs
// TODO: add a brief description of what this is & how it works
// TODO: factor out a 3d geometry library?

use std::collections::HashMap;

type Point3 = (i32, i32, i32);

#[derive(Clone, Debug)]
struct Brick {
    x: (i32, i32),
    y: (i32, i32),
    z: (i32, i32),
    k: bool,
}

#[derive(Clone, Debug)]
struct Slice {
    x: (i32, i32),
    y: (i32, i32),
    z: i32,
}

impl Brick {
    fn footprint(&self) -> Slice {
        Slice { x: self.x, y: self.y, z: self.z.0 - 1 }
    }
}

#[derive(Clone)]
struct World {
    bricks: Vec<Brick>,
    space: HashMap<Point3,usize>,
}

impl World {
    fn with_killed(&self, bi: usize) -> World {
        let mut w = self.clone();
        w.bricks[bi].k = true;
        w
    }

    fn movedown(&mut self, bi: usize) {
        let b = &mut self.bricks[bi];
        let nz = b.z.0 - 1;
        for x in b.x.0..=b.x.1 {
            for y in b.y.0..=b.y.1 {
                self.space.remove(&(x, y, b.z.1));
                self.space.insert((x, y, nz), bi);
            }
        }
        b.z.1 -= 1;
        b.z.0 -= 1;
    }

    fn bricks_in(&self, slice: &Slice) -> Vec<usize> {
        let mut r = Vec::new();
        for i in 0..self.bricks.len() {
            let b = &self.bricks[i];
            if b.k {
                continue;
            }
            if slice.z < b.z.0 || slice.z > b.z.1 {
                continue;
            }
            if slice.x.0 > b.x.1 || slice.x.1 < b.x.0 {
                continue;
            }
            if slice.y.0 > b.y.1 || slice.y.1 < b.y.0 {
                continue;
            }

            let found = true;
//            for x in b.x.0..=b.x.1 {
//                for y in b.y.0..=b.y.1 {
//                    let inx = x >= slice.x.0 && x <= slice.x.1;
//                    let iny = y >= slice.y.0 && y <= slice.y.1;
//                    if inx && iny {
//                        found = true;
//                        break;
//                    }
//                }
//                if found {
//                    break;
//                }
//            }

            if found {
                r.push(i);
            }
        }
        r
    }

    fn supporters(&self, bi: usize) -> Vec<usize> {
        let fp = self.bricks[bi].footprint();
        self.bricks_in(&fp)
    }

    fn onfloor(&self, bi: usize) -> bool {
        self.bricks[bi].z.0 == 1
    }

    fn nmoved(&self, other: &Self) -> usize {
        assert!(self.bricks.len() == other.bricks.len());
        let mut d = 0;
        for bi in 0..self.bricks.len() {
            let mb = &self.bricks[bi];
            let ob = &other.bricks[bi];
            if mb.x != ob.x || mb.y != ob.y || mb.z != ob.z {
                d += 1;
            }
        }
        d
    }
}

fn parse_brick(brick: &str) -> Brick {
    let parts: Vec<_> = brick.split('~').collect();
    let mins: Vec<_> = parts[0].split(',').map(|x| x.parse::<i32>().unwrap()).collect();
    let maxs: Vec<_> = parts[1].split(',').map(|x| x.parse::<i32>().unwrap()).collect();

    Brick {
        x: (mins[0], maxs[0]),
        y: (mins[1], maxs[1]),
        z: (mins[2], maxs[2]),
        k: false,
    }
}

fn spacemap(bricks: &Vec<Brick>) -> HashMap<Point3,usize> {
    let mut r = HashMap::new();
    for i in 0..bricks.len() {
        let b = &bricks[i];
        for x in b.x.0..=b.x.1 {
            for y in b.y.0..=b.y.1 {
                for z in b.z.0..=b.z.1 {
                    r.insert((x, y, z), i);
                }
            }
        }
    }
    r
}

fn parse(input: &str) -> World {
    let bricks = input.split('\n')
                      .filter(|x| !x.is_empty())
                      .map(parse_brick).collect();
    let space = spacemap(&bricks);
    World { bricks, space }
}

fn settleone(world: &mut World, bi: usize) -> bool {
    let mut supps = world.supporters(bi);
    let mut moved = false;
    while supps.is_empty() && !world.onfloor(bi) {
        moved = true;
        world.movedown(bi);
        supps = world.supporters(bi);
    }
    moved
}

fn settle(world: &mut World) {
    let mut moved = true;
    while moved {
        moved = false;
        for i in 0..world.bricks.len() {
            moved = moved || settleone(world, i);
        }
    }
}

// Cleverness note: we don't actually need to simulate the physics at all; we
// just need to build a tree of which bricks will eventually support which other
// bricks, then operate on the tree.
// TODO: do that instead of the sim
fn parta(world: &World) -> usize {
    let mut ss = Vec::new();
    for bi in 0..world.bricks.len() {
        let supps = world.supporters(bi);
        if supps.len() == 1 && !ss.contains(&supps[0]) {
            ss.push(supps[0]);
        }
    }
    world.bricks.len() - ss.len()
}

fn partb(world: &World) -> usize {
    let mut t = 0;
    for bi in 0..world.bricks.len() {
        let mut w = world.with_killed(bi);
        settle(&mut w);
        t += world.nmoved(&w);
    }
    t
}

pub fn solve(input: &str) -> (String, String) {
    let mut world = parse(input);
    settle(&mut world);
    (parta(&world).to_string(), partb(&world).to_string())
}
