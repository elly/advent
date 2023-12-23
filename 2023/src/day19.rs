// Day 19: Aplenty
// We have a big set of parts. Each part is rated in four categories: x, m, a,
// s. We are given a bunch of workflows, which look like:
//   ex{x>10:one,m<20:two,a>30:R,A}
// where the conditions are like: variable <|> const : workflow, and there are
// two hardcoded workflows R and A. We always start at the workflow called "in".

// Part B: if each rating can range from 1 to 4000, how many distinct
// combinations of ratings will be accepted? It seems like we can do that
// recursively - a workflow takes in a structure specifying ranges of ratings
// (like "x in 1 - 4000, m in 1 - 4000, a in 1 - 4000, s in 1 - 4000") and
// produces new structures describing where sub-ranges of those ranges end up.
// We then need to accumulate all the sub-ranges that eventually end up in "A"
// and we're good.
//
// So for example, if we had this workflow:
//   px{a<2006:qkq,m>2090:A,rfg}
// with input ranges:
//   x:[1,4000],m:[1,4000],a:[1,4000],s:[1,4000]
// then we'd get outputs like this:
//   x:[1,4000],m:[1,4000],a:[1,2005],s:[1,4000] -> qkq
//   x:[1,4000],m:[1,2089],a:[2006,4000],s:[1,4000] -> A
//   x:[1,4000],m:[2090,4000],a:[2006,4000],s:[1,4000] -> rfg
// then we'd feed those ranges back into those workflows, summing in the size of
// any range that maps to A.
//
// We can check that the output is well-formed: the size of all the output
// ranges, added together, has to be equal to the size of the input range.
//
// And... that just worked, yay! Solved :)

use std::collections::{HashMap, VecDeque};
use std::str::FromStr;

#[derive(Debug)]
struct Part {
    x: u32,
    m: u32,
    a: u32,
    s: u32,
}

impl FromStr for Part {
    type Err = ();
    fn from_str(input: &str) -> Result<Part, Self::Err> {
        let input = input.replace("{", "").replace("}", "")
                         .replace("x=", "").replace("m=", "")
                         .replace("a=", "").replace("s=", "");
        let parts: Vec<_> = input.split(',').map(|x| x.parse::<u32>().unwrap())
                                 .collect();
        Ok(Part { x: parts[0], m: parts[1], a: parts[2], s: parts[3] })
    }
}

#[derive(Clone, Copy, Debug)]
struct PartRange {
    // All of these are closed at both ends - i.e., x == (0,10) means both 0 and
    // 10 are included.
    x: (u32,u32),
    m: (u32,u32),
    a: (u32,u32),
    s: (u32,u32),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Field {
    X = 0,
    M = 1,
    A = 2,
    S = 3,
}

impl Part {
    fn field(&self, f: Field) -> u32 {
        match f {
            Field::X => self.x,
            Field::M => self.m,
            Field::A => self.a,
            Field::S => self.s,
        }
    }
}

impl PartRange {
    fn size(&self) -> usize {
        (self.x.1 + 1 - self.x.0) as usize *
        (self.m.1 + 1 - self.m.0) as usize *
        (self.a.1 + 1 - self.a.0) as usize *
        (self.s.1 + 1 - self.s.0) as usize
    }

    fn empty(&self) -> bool {
        self.x.1 < self.x.0 || self.m.1 < self.m.0
        || self.a.1 < self.a.0 || self.s.1 < self.s.0
    }

    fn field(&self, f: Field) -> (u32, u32) {
        match f {
            Field::X => self.x,
            Field::M => self.m,
            Field::A => self.a,
            Field::S => self.s,
        }
    }

    fn with_field(&self, f: Field, fv: (u32, u32)) -> PartRange {
        let x = if f == Field::X { fv } else { self.x };
        let m = if f == Field::M { fv } else { self.m };
        let a = if f == Field::A { fv } else { self.a };
        let s = if f == Field::S { fv } else { self.s };
        PartRange { x, m, a, s }
    }

    fn constrain_lt(&self, f: Field, val: u32) -> (PartRange, PartRange) {
        assert!(val != 0);
        let r = self.field(f);
        let inrange = (r.0, val - 1);
        let outrange = (val, r.1);
        (self.with_field(f, inrange), self.with_field(f, outrange))
    }

    fn constrain_gt(&self, f: Field, val: u32) -> (PartRange, PartRange) {
        let r = self.field(f);
        let inrange = (val + 1, r.1);
        let outrange = (r.0, val);
        (self.with_field(f, inrange), self.with_field(f, outrange))
    }
}

impl FromStr for Field {
    type Err = ();
    fn from_str(input: &str) -> Result<Field, Self::Err> {
        match input {
            "x" => Ok(Field::X),
            "m" => Ok(Field::M),
            "a" => Ok(Field::A),
            "s" => Ok(Field::S),
            _   => Err(()),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Insn {
    Lt(Field, u32, String),
    Gt(Field, u32, String),
    Jm(String),
}

impl FromStr for Insn {
    // field < const : target | field > const : target | target
    type Err = ();
    fn from_str(input: &str) -> Result<Insn, Self::Err> {
        let mut parts = input.split(':');
        if input.contains(":") {
            let cond = parts.next().unwrap();
            let target = String::from(parts.next().unwrap());
            if cond.contains("<") {
                let mut cp = cond.split('<');
                Ok(
                    Insn::Lt(Field::from_str(cp.next().unwrap()).unwrap(),
                             u32::from_str(cp.next().unwrap()).unwrap(),
                             target))
            } else if cond.contains(">") {
                let mut cp = cond.split('>');
                Ok(
                    Insn::Gt(Field::from_str(cp.next().unwrap()).unwrap(),
                             u32::from_str(cp.next().unwrap()).unwrap(),
                             target))

            } else {
                Err(())
            }
        } else {
            Ok(Insn::Jm(String::from(parts.next().unwrap())))
        }
    }
}

struct Workflow {
    label: String,
    insns: Vec<Insn>,
}

impl FromStr for Workflow {
    // name { insn , insn , insn }
    type Err = ();
    fn from_str(input: &str) -> Result<Workflow, Self::Err> {
        let input = input.replace("}", "");
        let parts: Vec<_> = input.split('{').collect();
        let insns = parts[1].split(',')
                            .map(|s| Insn::from_str(s).unwrap())
                            .collect();
        Ok(Workflow { label: String::from(parts[0]), insns })
    }
}

impl Workflow {
    fn handle(&self, p: &Part) -> &str {
        for insn in &self.insns {
            match insn {
                Insn::Lt(f, v, t) => if p.field(*f) < *v { return t; }
                Insn::Gt(f, v, t) => if p.field(*f) > *v { return t; }
                Insn::Jm(t) => { return t; }
            }
        }
        panic!("no match?");
    }
}

type Workflows = HashMap<String, Workflow>;
type Parts = Vec<Part>;

fn parse(input: &str) -> (Workflows, Parts) {
    let mut workflows: Workflows = HashMap::new();
    let mut parts: Parts = Vec::new();

    let mut partsec = false;
    for line in input.split('\n') {
        if line.is_empty() {
            partsec = true;
        } else if !partsec {
            let f = Workflow::from_str(line).unwrap();
            workflows.insert(f.label.clone(), f);
        } else {
            parts.push(Part::from_str(line).unwrap());
        }
    }

    (workflows, parts)
}

fn dest<'a>(workflows: &'a Workflows, part: &'a Part) -> &'a str {
    let mut s = "in";
    while s != "A" && s != "R" {
        let f = workflows.get(s).unwrap();
        s = f.handle(part);
    }
    s
}

fn parta(workflows: &Workflows, parts: &Parts) -> usize {
    let mut t = 0;
    for p in parts {
        let d = dest(workflows, p);
        if d == "A" {
            let pt = p.x + p.m + p.a + p.s;
            t += pt as usize;
        }
    }
    t
}

fn sim(flow: &Workflow, r: PartRange) -> Vec<(PartRange, String)> {
    let mut out = Vec::new();
    let mut r = r;

    for insn in &flow.insns {
        if let Insn::Lt(f, v, t) = insn {
            let (inr, outr) = r.constrain_lt(*f, *v);
            out.push((inr, String::from(t)));
            r = outr;
        } else if let Insn::Gt(f, v, t) = insn {
            let (inr, outr) = r.constrain_gt(*f, *v);
            out.push((inr, String::from(t)));
            r = outr;
        } else if let Insn::Jm(t) = insn {
            out.push((r, String::from(t)));
        }
    }

    out
}

fn partb(workflows: &Workflows) -> usize {
    let mut q = VecDeque::new();
    let all = (1, 4000);
    let start = String::from("in");
    let mut total = 0;
    q.push_back((PartRange { x: all, m: all, a: all, s: all }, start));

    while !q.is_empty() {
        let (range, flow) = q.pop_front().unwrap();
        if flow == "A" {
            total += range.size();
        } else if flow == "R" {
            /* nothing, discard it */
        } else {
            let flow = workflows.get(&flow).unwrap();
            let outs = sim(flow, range);
            for out in outs {
                q.push_back(out);
            }
        }
    }

    total
}

pub fn solve(input: &str) -> (String, String) {
    let (workflows, parts) = parse(input);
    (parta(&workflows, &parts).to_string(), partb(&workflows).to_string())
}

#[test]
fn test_parse_insn() {
    assert_eq!(Insn::from_str("a<2006:qkq").unwrap(),
               Insn::Lt(Field::A, 2006, String::from("qkq")));
    assert_eq!(Insn::from_str("m>2090:A").unwrap(),
               Insn::Gt(Field::M, 2090, String::from("A")));
}

#[test]
fn test_range_size() {
    let all = (0, 4000);
    let r = PartRange { x: all, m: all, a: all, s: all };
    assert_eq!(r.size(), 4001 * 4001 * 4001 * 4001);

    let r = PartRange { x: (1,50), m: all, a: all, s: all };
    assert_eq!(r.size(), 50 * 4001 * 4001 * 4001);
}

#[test]
fn test_range_constrain() {
    let all = (0, 4000);
    let r = PartRange { x: all, m: all, a: all, s: all };
    let (inr, outr) = r.constrain_lt(Field::X, 2000);
    assert_eq!(inr.x, (0, 1999));
    assert_eq!(outr.x, (2000, 4000));

    let (inr, outr) = r.constrain_gt(Field::X, 2000);
    assert_eq!(inr.x, (2001, 4000));
    assert_eq!(outr.x, (0, 2000));
}
