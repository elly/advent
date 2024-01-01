// Day 9: Mirage Maintenance
// TODO: brief description

fn parse_line(input: &str) -> Vec<i64> {
    input.split(' ').map(|x| x.parse::<i64>().unwrap()).collect()
}

fn parse(input: &str) -> Vec<Vec<i64>> {
    input.split('\n').filter(|x| !x.is_empty()).map(parse_line).collect()
}

fn diffs(vec: &Vec<i64>) -> Vec<i64> {
    let s1 = vec.iter();
    let s2 = vec.iter().skip(1);
    s1.zip(s2).map(|(a, b)| b - a).collect()
}

fn predict(vec: &Vec<i64>) -> i64 {
    if vec.iter().all(|x| *x == 0) {
        0
    } else {
        let ds = diffs(vec);
        let p = predict(&ds);
        vec.iter().last().unwrap() + p
    }
}

fn rpredict(vec: &Vec<i64>) -> i64 {
    let mut v = vec.clone();
    v.reverse();
    predict(&v)
}

fn parta(input: &Vec<Vec<i64>>) -> i64 {
    input.iter().map(predict).sum()
}

fn partb(input: &Vec<Vec<i64>>) -> i64 {
    input.iter().map(rpredict).sum()
}

pub fn solve(input: &str) -> (String, String) {
    let vecs = parse(input);
    (parta(&vecs).to_string(), partb(&vecs).to_string())
}

#[test]
fn test_diffs() {
    assert_eq!(diffs(&vec![1, 2, 4, 8]), vec![1, 2, 4]);
    assert_eq!(diffs(&vec![1, 3, 7, 15]), vec![2, 4, 8]);
}

#[test]
fn test_predict() {
    assert_eq!(predict(&vec![3, 3, 3, 3]), 3);
    assert_eq!(predict(&vec![0, 3, 6, 9, 12, 15]), 18);
    assert_eq!(predict(&vec![1, 3, 6, 10, 15, 21]), 28);
    assert_eq!(predict(&vec![10, 13, 16, 21, 30, 45]), 68);
}

#[test]
fn test_rpredict() {
    assert_eq!(rpredict(&vec![3, 3, 3, 3]), 3);
    assert_eq!(rpredict(&vec![10, 13, 16, 21, 30, 45]), 5);
}
