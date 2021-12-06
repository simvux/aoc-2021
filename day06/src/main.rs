use std::io::Read;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();

    let mut counts = [0; 9];
    for age in input.split(',').map(|n| n.trim().parse::<usize>().unwrap()) {
        counts[age] += 1;
    }

    let mut part1 = counts;
    for _ in 0..80 {
        next_generation(&mut part1);
    }

    println!("part 1: {}", part1.iter().copied().sum::<u64>());

    let mut part2 = counts;
    for _ in 0..256 {
        next_generation(&mut part2);
    }
    println!("part 2: {}", part2.iter().copied().sum::<u64>());
}

fn next_generation(counts: &mut [u64; 9]) {
    let reset = counts[0];

    for age in 1..9 {
        counts[age - 1] = counts[age];
        counts[age] = 0;
    }

    counts[6] += reset;
    assert_eq!(counts[8], 0);
    counts[8] = reset;
}
