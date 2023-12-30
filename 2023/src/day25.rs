// Day 25: Snowverload
// We are given a graph (the input) with connections written like:
//   jqt: rhn xhk nvd
// where the edges are undirected. We want to find 3 edges we can cut to
// partition the graph, then compute the product of the sizes of the two halves
// of the partition.

use std::collections::{HashMap,HashSet};

type EdgeId = usize;
type NodeId = usize;

struct Edge {
    n0: NodeId,
    n1: NodeId,
}

struct Node {
    id: NodeId,
    tag: String,       // for debugging
}

struct Graph {
    nodes: Vec<Node>,
    edges: Vec<Edge>,
}

fn parse(input: &str) -> Graph {
    let mut nodes: Vec<Node> = Vec::new();
    let mut edges: Vec<Edge> = Vec::new();

    fn addnode(nodes: &mut Vec<Node>, name: &str) -> usize {
        for i in 0..nodes.len() {
            if nodes[i].tag == name { return i; }
        }
        let n = Node {
            id: nodes.len(),
            tag: String::from(name),
        };
        nodes.push(n);
        nodes.len() - 1
    }

    fn hasedge(edges: &Vec<Edge>, n0: usize, n1: usize) -> bool {
        edges.iter().find(|e| e.n0 == n0 && e.n1 == n1).is_some()
    }

    fn addedge(edges: &mut Vec<Edge>, in0: usize, in1: usize) {
        let n0 = if in0 < in1 { in0 } else { in1 };
        let n1 = if in0 < in1 { in1 } else { in0 };
        if !hasedge(edges, n0, n1) {
            edges.push(Edge { n0, n1 });
        }
    }

    for line in input.split('\n').filter(|x| !x.is_empty()) {
        let line = line.replace(":", "");
        let parts: Vec<_> = line.split(' ').collect();
        let n0 = addnode(&mut nodes, parts[0]);
        for p in &parts[1..] {
            let n1 = addnode(&mut nodes, p);
            addedge(&mut edges, n0, n1);
        }
    }

    Graph { nodes, edges }
}

type PathMap = HashMap<(NodeId,NodeId),Vec<EdgeId>>;

fn floyd_warshall(g: &Graph, cuts: &Vec<EdgeId>) -> PathMap {
    let sz = g.nodes.len();
    let mut dists: Vec<usize> = Vec::new();
    let mut prevs: Vec<usize> = Vec::new();

    const INFINITY: usize = 9999999999;

    dists.resize(sz * sz, INFINITY);
    prevs.resize(sz * sz, INFINITY);

    for ei in 0..g.edges.len() {
        if cuts.contains(&ei) { continue; }
        let e = &g.edges[ei];
        dists[e.n0 * sz + e.n1] = 1;
        prevs[e.n0 * sz + e.n1] = e.n0;
        dists[e.n1 * sz + e.n0] = 1;
        prevs[e.n1 * sz + e.n0] = e.n1;
    }

    for n in &g.nodes {
        dists[n.id * sz + n.id] = 0;
        prevs[n.id * sz + n.id] = n.id;
    }

    for k in 0..g.nodes.len() {
        for i in 0..g.nodes.len() {
            for j in 0..g.nodes.len() {
                let dij = dists[i * sz + j];
                let dik = dists[i * sz + k];
                let dkj = dists[k * sz + j];
                if dij > dik + dkj {
                    dists[i * sz + j] = dik + dkj;
                    prevs[i * sz + j] = prevs[k * sz + j];
                }
            }
        }
    }

    fn findedge(g: &Graph, in0: NodeId, in1: NodeId) -> EdgeId {
        let n0 = if in0 < in1 { in0 } else { in1 };
        let n1 = if in0 < in1 { in1 } else { in0 };
        g.edges.iter().position(|e| e.n0 == n0 && e.n1 == n1).unwrap()
    }

    let mut pm: PathMap = HashMap::new();
    for u in 0..g.nodes.len() {
        for iv in 0..g.nodes.len() {
            if prevs[u * sz + iv] == INFINITY { continue; }
            let mut path: Vec<EdgeId> = Vec::new();
            let mut v = iv;
            while u != v {
                path.push(findedge(g, prevs[u * sz + v], v));
                v = prevs[u * sz + v];
            }
            pm.insert((u, iv), path);
        }
    }
    pm
}

fn popular_edge(g: &Graph, m: &PathMap) -> EdgeId {
    let mut counts: Vec<usize> = Vec::new();
    counts.resize(g.edges.len(), 0);

    for p in m.values() {
        for e in p.iter() {
            counts[*e] += 1;
        }
    }

    let mut mi = 0;
    for i in 0..counts.len() {
        if counts[i] > counts[mi] {
            mi = i;
        }
    }
    mi
}

fn connected(g: &Graph, m: &PathMap) -> bool {
    for i in 0..g.nodes.len() {
        for j in 0..g.nodes.len() {
            if !m.contains_key(&(i, j)) { return false; }
        }
    }
    true
}

fn partitions(g: &Graph, m: &PathMap) -> (HashSet<NodeId>, HashSet<NodeId>) {
    let mut s1 = HashSet::new();
    let mut s2 = HashSet::new();
    s1.insert(0);
    for i in 1..g.nodes.len() {
        if m.contains_key(&(0, i)) {
            s1.insert(i);
        } else {
            s2.insert(i);
        }
    }
    (s1, s2)
}

fn parta(g: &Graph) -> usize {
    let mut c = Vec::new();
    let mut m = floyd_warshall(g, &c);

    while connected(&g, &m) {
        let e = popular_edge(g, &m);
        c.push(e);
        m = floyd_warshall(g, &c);
    }

    let (p1, p2) = partitions(g, &m);

    p1.len() * p2.len()
}

pub fn solve(input: &str) -> (String, String) {
    let g = parse(input);
    (parta(&g).to_string(), "".to_string())
}
