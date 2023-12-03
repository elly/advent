// Day 3: Gear Ratios
// The input is a 2d grid, containing symbols and numbers; the numbers occupy
// multiple consecutive cells in the grid while the symbols occupy one. Any
// number adjacent to a symbol (in any of its digits) is a "part number" and the
// others are not.
//
// To do this, we'll parse the input into two sets: a set of numbers with their
// coordinates, and a set of symbols with their coordinates. The part numbers
// are then those that are adjacent to any symbol.

struct Number {
    x: usize,
    y: usize,
    width: usize,
    val: u32,
}

struct Symbol {
    x: usize,
    y: usize,
    g: char,
}

fn parse_number(row: &[char], x: usize, y: usize) -> Option<Number> {
    if row[x].is_digit(10) {
        let mut nx = x;
        let mut v = 0;
        while nx < row.len() && row[nx].is_digit(10) {
            v = v * 10 + row[nx].to_digit(10).unwrap();
            nx += 1;
        }
        Some(Number { x, y, width: nx - x, val: v })
    } else {
        None
    }
}

fn parse(input: &str) -> (Vec<Number>, Vec<Symbol>) {
    let lines: Vec<&str> = input.split('\n').filter(|x| !x.is_empty()).collect();
    let mut numbers: Vec<Number> = Vec::new();
    let mut symbols: Vec<Symbol> = Vec::new();

    for y in 0..lines.len() {
        let chars: Vec<char> = lines[y].chars().collect();
        let mut x = 0;
        while x < chars.len() {
            if let Some(n) = parse_number(&chars, x, y) {
                x = n.x + n.width;
                numbers.push(n);
            } else if chars[x] != '.' {
                symbols.push(Symbol { x, y, g: chars[x] });
                x += 1;
            } else {
                x += 1;
            }
        }
    }

    (numbers, symbols)
}

fn adjacent_to(number: &Number, symbol: &Symbol) -> bool {
    let minx = (number.x as isize) - 1;
    let maxx = (number.x + number.width) as isize;
    let miny = (number.y as isize) - 1;
    let maxy = (number.y as isize) + 1;

    let sx = symbol.x as isize;
    let sy = symbol.y as isize;

    sx >= minx && sx <= maxx && sy >= miny && sy <= maxy
}

fn adjacent_to_any(number: &Number, symbols: &[Symbol]) -> bool {
    symbols.iter().any(|s| adjacent_to(number, s))
}

fn parta(numbers: &[Number], symbols: &[Symbol]) -> u32 {
    numbers.iter().filter(|n| adjacent_to_any(n, symbols))
                  .map(|n| n.val)
                  .sum()
}

fn gearing_ratio(symbol: &Symbol, numbers: &[Number]) -> u32 {
    let adjs: Vec<&Number> = numbers.iter().filter(|n| adjacent_to(n, symbol)).collect();
    if symbol.g == '*' && adjs.len() == 2 {
        adjs[0].val * adjs[1].val
    } else {
        0
    }
}

fn partb(numbers: &[Number], symbols: &[Symbol]) -> u32 {
    symbols.iter().map(|s| gearing_ratio(s, numbers)).sum()
}

pub fn solve(input: &str) -> (String, String) {
    let (numbers, symbols) = parse(input);
    (parta(&numbers, &symbols).to_string(), partb(&numbers, &symbols).to_string())
}
