// Day 19: Aplenty
// We have a big set of parts. Each part is rated in four categories: x, m, a,
// s. We are given a bunch of workflows, which look like:
//   ex{x>10:one,m<20:two,a>30:R,A}
// where the conditions are like: variable <|> const : workflow, and there are
// two hardcoded workflows R and A. We always start at the workflow called "in".

use std::collections::HashMap;
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

pub fn solve(input: &str) -> (String, String) {
    let (workflows, parts) = parse(input);
    (parta(&workflows, &parts).to_string(), "".to_string())
}

#[test]
fn test_parse_insn() {
    assert_eq!(Insn::from_str("a<2006:qkq").unwrap(),
               Insn::Lt(Field::A, 2006, String::from("qkq")));
    assert_eq!(Insn::from_str("m>2090:A").unwrap(),
               Insn::Gt(Field::M, 2090, String::from("A")));
}
