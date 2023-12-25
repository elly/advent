// Day 20: Pulse Propagation

use std::collections::HashMap;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ModKind {
    FlipFlop,
    Conj,
    Broadcast,
    Dummy,
}

#[derive(Clone, Debug)]
struct Module {
    kind: ModKind,
    name: String,
    outs: Vec<String>,
    ins: Vec<String>,
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
        ins: Vec::new(),
    }
}

fn parse(input: &str) -> HashMap<String, Module> {
    let mut tr = HashMap::new();
    for line in input.split('\n').filter(|x| !x.is_empty()) {
        let m = parse_mod(line);
        tr.insert(m.name.clone(), m);
    }

    // Compute all the inputs for each module and fill those in too.
    let mut r = tr.clone();
    for (k, v) in tr.iter() {
        for i in v.outs.iter() {
            if let Some(m) = r.get_mut(i) {
                m.ins.push(k.clone());
            }
        }
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
    // The last input received for each wired input, or None if no input was
    // received from this module this cycle.
    inputs: Vec<Option<bool>>,

    // The last input (defaulting to off) received for each one of this module's
    // wired inputs. This vector's length is equal to the corresponding module's
    // number of connected inputs.
    last_inputs: Vec<bool>,

    // For flipflops only, whether the module is on or off.
    onoff: bool,

    // Last output, if there was one.
    out: Option<bool>,

    // Number of button presses left, if this is a broadcast module. Grim hack.
    presses: usize,
}

type CircuitState = HashMap<String, ModState>;

fn new_circuitstate(circuit: &Circuit) -> CircuitState {
    let mut r = HashMap::new();

    for (k, v) in circuit.iter() {
        let len = v.ins.len();

        let mut inputs = Vec::new();
        inputs.resize(len, None);

        let mut last_inputs = Vec::new();
        last_inputs.resize(len, false);

        r.insert(k.clone(), ModState {
            inputs,
            last_inputs,
            onoff: false,
            out: None,
            presses: 0,
        });
    }

    r
}

fn step_conj(s: &ModState) -> ModState {
    let has_input = s.inputs.iter().any(|x| x.is_some());
    let out = if has_input {
        Some(!s.last_inputs.iter().all(|i| *i))
    } else {
        None
    };
    ModState {
        out, .. s.clone()
    }
}

fn step_flip(s: &ModState) -> ModState {
    let has_low = s.inputs.iter().any(|x| x.is_some() && !x.unwrap());
    let onoff = if has_low { !s.onoff } else { s.onoff };
    let out = if has_low { Some(onoff) } else { None };
    ModState {
        onoff,
        out,
        .. s.clone()
    }
}

fn step_special(s: &ModState) -> ModState {
    ModState {
        out: if s.presses > 0 { Some(false) } else { None },
        presses: if s.presses > 0 { s.presses - 1 } else { 0 },
        .. s.clone()
    }
}

fn step(circuit: &Circuit, states: CircuitState) -> CircuitState {
    let mut r = HashMap::new();
    // To simulate the circuit we make two passes; the first pass fills each
    // module's inputs and last_inputs fields from its wired inputs, and the
    // second pass computes its new output state.

    for (k, v) in circuit.iter() {
        let ms = states.get(k).unwrap();
        let ins: Vec<Option<bool>> = v.ins.iter().map(|m| {
            states.get(m).unwrap().out
        }).collect();
        let mut ir = Vec::new();
        let mut lir = Vec::new();
        for i in 0 .. ins.len() {
            ir.push(ins[i]);
            if let Some(iv) = ins[i] {
                lir.push(iv);
            } else {
                lir.push(ms.last_inputs[i]);
            }
        }

        let s = ModState {
            inputs: ir,
            last_inputs: lir,
            .. ms.clone()
        };

        let s = match v.kind {
            ModKind::Conj => step_conj(&s),
            ModKind::FlipFlop => step_flip(&s),
            ModKind::Broadcast => step_special(&s),
            ModKind::Dummy => s.clone(),
        };

        r.insert(k.clone(), s);
    }

    r
}

fn run(circuit: &Circuit, states: CircuitState, presses: usize) -> (usize, usize) {
    let (mut h, mut l) = (0, 0);
    let mut th = 0;
    let mut tl = 0;
    let mut presses = presses;
    let mut states = states;
    while h > 0 || l > 0 || presses > 0 {
        if h == 0 && l == 0 && presses > 0 {
            let mut s = states.get_mut(&String::from("broadcaster")).unwrap();
            s.presses = 1;
            presses -= 1;
            tl += 1;
        }

        h = 0;
        l = 0;


        states = step(circuit, states);
        for (k, v) in states.iter() {
            let outs = circuit.get(k).unwrap().outs.len();
            if let Some(s) = v.out {
                if s {
                    h += outs;
                } else {
                    l += outs;
                }
            }
        }
        th += h;
        tl += l;
    }
    (th, tl)
}

// 720502125 is too low

pub fn solve(input: &str) -> (String, String) {
    let circuit = parse(input);
    let mut cs = new_circuitstate(&circuit);
    let (h, l) = run(&circuit, cs, 1000);
    ((h * l).to_string(), "".to_string())
}
