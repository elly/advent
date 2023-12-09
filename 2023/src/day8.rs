// Day 8:

use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Dir {
    Left,
    Right,
}

#[derive(Debug)]
struct Node {
    id: String,
    left: String,
    right: String,
}

#[derive(Debug)]
struct Graph {
    route: Vec<Dir>,
    nodes: HashMap<String, Node>,
}

fn parse_dir(input: char) -> Dir {
    match input {
        'L' => Dir::Left,
        'R' => Dir::Right,
        _   => panic!("dir? {}", input),
    }
}

fn parse_node(input: &str) -> Node {
    let input = input.replace("(", "").replace(")", "").replace(",", "")
                     .replace(" =", "");
    let parts: Vec<_> = input.split(' ').collect();
    Node {
        id: parts[0].to_string(),
        left: parts[1].to_string(),
        right: parts[2].to_string()
    }
}

fn parse(input: &str) -> Graph {
    let mut nodes = HashMap::new();
    let mut lines = input.split('\n').filter(|x| !x.is_empty());
    let route = lines.next().unwrap().chars().map(parse_dir).collect();

    lines.for_each(|line| {
        let n = parse_node(&line);
        nodes.insert(n.id.clone(), n);
    });

    Graph { route, nodes }
}

fn step<'a>(graph: &'a Graph, dir: Dir, node: &'a str) -> &'a str {
    let n = &graph.nodes[node];
    if dir == Dir::Left {
        &n.left
    } else {
        &n.right
    }
}

fn follow(graph: &Graph, start: &str, endp: fn(&str) -> bool) -> u64 {
    let mut steps = 0;
    let mut cur = start;
    let mut dirs = graph.route.iter().cycle();
    while !endp(cur) {
        cur = step(graph, *dirs.next().unwrap(), &cur);
        steps += 1;
    }
    steps
}

fn parta(graph: &Graph) -> u64 {
    if graph.nodes.contains_key("AAA") {
        follow(&graph, &"AAA", |x| { x == "ZZZ" })
    } else {
        0
    }
}

fn partb(graph: &Graph) -> u64 {
    let mut starts = Vec::new();
    graph.nodes.iter().for_each(|(k, _v)| {
        if k.ends_with("A") {
            starts.push(k.as_str());
        }
    });

    starts.iter().map(|s| {
        follow(&graph, s, |x| { x.ends_with("Z") })
    }).reduce(|a, e| num::integer::lcm(a, e)).unwrap()
}

pub fn solve(input: &str) -> (String, String) {
    let g = parse(&input);
    (parta(&g).to_string(), partb(&g).to_string())
}
