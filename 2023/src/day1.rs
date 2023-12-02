// Day 1: Trebuchet!?
// We have a file with a bunch of lines. Each line contains a calibration value,
// which is the two-digit number formed from the first number and the last
// number.
// Part a: Sum all the calibration values.

fn alldigits(s: &str) -> Vec<u32> {
    let digits = ["zero", "one", "two", "three", "four", "five", "six", "seven",
                  "eight", "nine"];
    let mut i = 0;
    let mut result: Vec<u32> = Vec::new();
    while i < s.len() {
        let c = s.chars().nth(i).expect("index");
        if c.is_digit(10) {
            result.push(c.to_digit(10).expect("digit"));
            i += 1;
        } else {
            let z = &s[i..];
            let mut found = false;
            for x in 0..digits.len() {
                if z.starts_with(digits[x]) {
                    result.push(x as u32);
                    i += 1;
                    found = true;
                    break;
                }
            }
            if !found {
                i += 1;
            }
        }
    }
    dbg!(result)
}

fn digits(s: &str) -> Vec<u32> {
    s.chars().filter(|x| x.is_digit(10))
             .map(|x| x.to_digit(10).expect("digit"))
             .collect()
}

fn calibration(digits: Vec<u32>) -> u32 {
    if digits.len() > 0 {
        digits[0] * 10 + digits[digits.len() - 1]
    } else {
        0
    }
}

fn parta(lines: &[&str]) -> u32 {
    lines.iter().map(|line| calibration(digits(line))).sum()
}

fn partb(lines: &[&str]) -> u32 {
    // 54506 is too high here
    lines.iter().map(|line| calibration(alldigits(line))).sum()
}

pub fn solve(input: &str) -> (String, String) {
    let lines: Vec<&str> = input.split('\n').collect();
    (parta(&lines[..]).to_string(), partb(&lines[..]).to_string())
}
