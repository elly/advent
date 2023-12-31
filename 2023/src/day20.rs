// Day 20: Pulse Propagation

use std::collections::{HashMap,VecDeque};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ModKind {
    FlipFlop,
    Conj,
    Broadcast,
}

#[derive(Clone, Debug)]
struct Module {
    kind: ModKind,
    name: String,
    outs: Vec<String>,
}

fn parse_mod(input: &str) -> Module {
    let input = input.replace(",", "");
    let parts: Vec<_> = input.split(' ').collect();
    let sigil = parts[0].chars().next().unwrap();

    let (kind, name) = match sigil {
        '%' => (ModKind::FlipFlop, &parts[0][1..]),
        '&' => (ModKind::Conj, &parts[0][1..]),
        _   => (ModKind::Broadcast, &parts[0][0..]),
    };
    let outs = &parts[2..];

    Module {
        kind,
        name: String::from(name),
        outs: outs.iter().map(|x| String::from(*x)).collect(),
    }
}

fn parse(input: &str) -> HashMap<String, Module> {
    let mut r = HashMap::new();
    for line in input.split('\n').filter(|x| !x.is_empty()) {
        let m = parse_mod(line);
        r.insert(m.name.clone(), m);
    }

    r
}

type Circuit = HashMap<String, Module>;

// To simulate a circuit, we operate in a series of steps; each step moves
// pulses through one set of modules. The step function maps a set of module
// states (what inputs they are receiving, plus their own states) to a new set
// of module states.
#[derive(Clone, Debug)]
struct ModState {
    // The last input (defaulting to off) received for each one of this module's
    // wired inputs.
    last_inputs: HashMap<String, bool>,

    // For flipflops only, whether the module is on or off.
    onoff: bool,
}

fn new_modstates(circuit: &Circuit) -> HashMap<String, ModState> {
    let mut r = HashMap::new();
    for k in circuit.keys() {
        let mut last_inputs = HashMap::new();
        for (ik, iv) in circuit.iter() {
            if iv.outs.contains(k) {
                last_inputs.insert(ik.clone(), false);
            }
        }
        r.insert(k.clone(), ModState { last_inputs, onoff: false });
    }
    r
}

fn sim_mod(src: &str, dm: &Module, ds: &mut ModState, pulse: bool) ->
Option<bool> {
    if dm.kind == ModKind::Conj {
        ds.last_inputs.insert(src.to_string(), pulse);
        Some(!ds.last_inputs.values().all(|k| *k))
    } else if dm.kind == ModKind::FlipFlop {
        if !pulse {
            ds.onoff = !ds.onoff;
            Some(ds.onoff)
        } else {
            None
        }
    } else if dm.kind == ModKind::Broadcast {
        Some(pulse)
    } else {
        None
    }
}

fn sim(circuit: &Circuit, presses: usize, target: &str) -> (usize, usize, usize) {
    let mut states: HashMap<String, ModState> = new_modstates(circuit);
    let (mut highs, mut lows) = (0, 0);

    for press in 0 .. presses {
        let mut q = VecDeque::new();
        q.push_back(("button", "broadcaster", false));
        while !q.is_empty() {
            let Some((src, dst, pulse)) = q.pop_front() else { panic!("...") };
            let dm = circuit.get(dst);

            if pulse {
                highs += 1;
            } else {
                lows += 1;
            }

            if src == target && pulse {
                return (highs, lows, press + 1);
            }

            if let Some(dm) = dm {
                let mut ds = states.get_mut(dst).unwrap();
                let p = sim_mod(src, dm, &mut ds, pulse);
                if let Some(p) = p {
                    for o in dm.outs.iter() {
                        q.push_back((dst, &o, p));
                    }
                }
            } else {
//              dbg!((src, dst, pulse));
            }
        }
    }

    (highs, lows, 0)
}

fn partb(circuit: &Circuit) -> usize {
    // This is hardcoded off my input - a bit gross.
    // TODO: generalize?
    let (_, _, rsx) = sim(circuit, 10000000, "sx");
    let (_, _, rjt) = sim(circuit, 10000000, "jt");
    let (_, _, rkb) = sim(circuit, 10000000, "kb");
    let (_, _, rks) = sim(circuit, 10000000, "ks");
    rsx * rjt * rkb * rks
}

pub fn solve(input: &str) -> (String, String) {
    let circuit = parse(input);
    let (h, l, _) = sim(&circuit, 1000, "");
    ((h * l).to_string(), partb(&circuit).to_string())
}
