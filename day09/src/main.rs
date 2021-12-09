use std::collections::HashSet;
use std::io::prelude::*;

fn main() {
    let lines = std::io::stdin()
        .lock()
        .lines()
        .map(|line| {
            line.unwrap()
                .chars()
                .map(|c| c.to_digit(10).unwrap() as usize)
                .collect()
        })
        .collect::<Vec<Vec<usize>>>();

    let mut task = Cave::new(lines);
    task.explore();

    println!(
        "part 1: {}",
        task.risk_levels.iter().copied().sum::<usize>()
    );

    task.basins.sort_by(|a, b| b.cmp(a));

    println!(
        "part 2: {}",
        task.basins.iter().take(3).copied().product::<usize>()
    );
}

struct Cave {
    lines: Vec<Vec<usize>>,
    risk_levels: Vec<usize>,
    basins: Vec<usize>,
}

impl Cave {
    fn new(lines: Vec<Vec<usize>>) -> Self {
        Self {
            lines,
            risk_levels: Vec::new(),
            basins: Vec::new(),
        }
    }

    fn explore(&mut self) {
        for (iline, line) in self.lines.iter().enumerate() {
            for (inum, num) in line.iter().enumerate() {
                let mut position = Position {
                    lines: &self.lines,
                    iline,
                    inum,
                    basin_blacklist: &mut HashSet::new(),
                };

                if position.is_risk_level() {
                    let size = position.seek_basin();
                    self.risk_levels.push(num + 1);
                    self.basins.push(size);
                }
            }
        }
    }
}

struct Position<'a> {
    lines: &'a [Vec<usize>],
    iline: usize,
    inum: usize,

    basin_blacklist: &'a mut HashSet<(usize, usize)>,
}

impl<'a> Position<'a> {
    fn is_risk_level(&self) -> bool {
        [self.left(), self.over(), self.under(), self.right()]
            .iter()
            .all(|&n| self.num() < n)
    }

    fn num(&self) -> usize {
        self.lines[self.iline][self.inum]
    }

    fn left(&self) -> usize {
        non_zero(self.inum, || self.line().get(self.inum - 1).copied())
    }
    fn right(&self) -> usize {
        self.line().get(self.inum + 1).copied().unwrap_or(100)
    }
    fn under(&self) -> usize {
        self.lines
            .get(self.iline + 1)
            .map(|line_over| line_over[self.inum])
            .unwrap_or(100)
    }
    fn over(&self) -> usize {
        non_zero(self.iline, || {
            self.lines
                .get(self.iline - 1)
                .map(|line_under| line_under[self.inum])
        })
    }

    fn line(&self) -> &[usize] {
        &self.lines[self.iline]
    }

    fn seek_basin(&mut self) -> usize {
        let mut sum = 1;

        if self.basin_blacklist.contains(&(self.iline, self.inum)) {
            return 0;
        } else {
            self.basin_blacklist.insert((self.iline, self.inum));
        }

        if self.flows(self.left()) {
            sum += self.fork(|line| line, |num| num - 1).seek_basin();
        }
        if self.flows(self.right()) {
            sum += self.fork(|line| line, |num| num + 1).seek_basin();
        }
        if self.flows(self.under()) {
            sum += self.fork(|line| line + 1, |num| num).seek_basin();
        }
        if self.flows(self.over()) {
            sum += self.fork(|line| line - 1, |num| num).seek_basin();
        }

        sum
    }

    fn flows(&self, n: usize) -> bool {
        n > self.num() && n != 9 && n != 100
    }

    fn fork(
        &mut self,
        on_line: impl Fn(usize) -> usize,
        on_num: impl Fn(usize) -> usize,
    ) -> Position<'_> {
        Position {
            lines: self.lines,
            iline: on_line(self.iline),
            inum: on_num(self.inum),
            basin_blacklist: self.basin_blacklist,
        }
    }
}

fn non_zero<F: Fn() -> Option<usize>>(n: usize, f: F) -> usize {
    if n == 0 {
        100
    } else {
        f().unwrap_or(100)
    }
}
