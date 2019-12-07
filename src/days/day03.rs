use std::collections::HashSet;
use std::hash::{Hash, Hasher};

pub fn solve03a(input: &String) -> String {
    let mut lines = input.lines();
    let w1 = lines.next().unwrap();
    let w2 = lines.next().unwrap();

    dist_manhattan(
        closest_cross_manhattan(w1.to_string(), w2.to_string()),
        Point::zero(),
    )
    .to_string()
}

pub fn solve03b(input: &String) -> String {
    let mut lines = input.lines();
    let w1 = lines.next().unwrap();
    let w2 = lines.next().unwrap();

    dist_signal(
        closest_cross_signal(w1.to_string(), w2.to_string()),
        Point::zero(),
    )
    .to_string()
}

fn closest_cross_manhattan(wire1: String, wire2: String) -> Point {
    closest_to_origin_by(
        dist_manhattan,
        parse(wire1).intersection(&parse(wire2)).cloned().collect(),
    )
}

fn closest_cross_signal(wire1: String, wire2: String) -> Point {
    let mut points_in_common = HashSet::new();
    let wpoints1 = parse(wire1);
    let wpoints2 = parse(wire2);

    for wpoint1 in wpoints1 {
        if wpoints2.contains(&wpoint1) {
            let wpoint2 = wpoints2.get(&wpoint1).unwrap();
            points_in_common.insert(Point::new(wpoint1.x, wpoint1.y, wpoint1.w + wpoint2.w));
        }
    }

    closest_to_origin_by(dist_signal, points_in_common)
}

#[derive(Eq, Debug, Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
    w: i32,
}

impl Point {
    fn new(x: i32, y: i32, w: i32) -> Point {
        Point { x, y, w }
    }

    fn zero() -> Point {
        Point { x: 0, y: 0, w: 0 }
    }
}

impl PartialEq for Point {
    fn eq(&self, other: &Self) -> bool {
        self.x.eq(&other.x) && self.y.eq(&other.y)
    }
}

impl Hash for Point {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.x.hash(state);
        self.y.hash(state);
    }
}

fn parse(input: String) -> HashSet<Point> {
    let mut set = HashSet::new();
    if input.len() == 0 {
        return set;
    }

    let (mut x, mut y, mut w) = (0, 0, 0);

    for word in input.split(",") {
        let (direction, steps) = word.split_at(1);
        let step = steps.parse::<i32>().unwrap();
        match direction {
            "U" => {
                for i in y + 1..=y + step {
                    w += 1;
                    set.insert(Point { x, y: i, w: w });
                }
                y += step;
            }
            "D" => {
                for i in (y - step..=y - 1).rev() {
                    w += 1;
                    set.insert(Point { x, y: i, w: w });
                }
                y -= step;
            }
            "R" => {
                for i in x + 1..=x + step {
                    w += 1;
                    set.insert(Point { x: i, y, w: w });
                }
                x += step;
            }
            "L" => {
                for i in (x - step..=x - 1).rev() {
                    w += 1;
                    set.insert(Point { x: i, y, w: w });
                }
                x -= step;
            }
            _ => panic!("Invalid direction"),
        };
    }

    set
}

fn dist_manhattan(p2: Point, p1: Point) -> i32 {
    (p2.x - p1.x).abs() + (p2.y - p1.y).abs()
}

fn dist_signal(p2: Point, p1: Point) -> i32 {
    p2.w - p1.w
}

fn closest_to_origin_by(f: fn(Point, Point) -> i32, set: HashSet<Point>) -> Point {
    *set.iter().min_by_key(|&pt| f(*pt, Point::zero())).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(parse(String::from("")), [].iter().cloned().collect());
        assert_eq!(
            parse(String::from("U2")),
            [Point::new(0, 1, 1), Point::new(0, 2, 2)]
                .iter()
                .cloned()
                .collect()
        );

        let r2d2: HashSet<Point> = [
            Point::new(1, 0, 1),
            Point::new(2, 0, 2),
            Point::new(2, -1, 3),
            Point::new(2, -2, 4),
        ]
        .iter()
        .cloned()
        .collect();
        assert_eq!(parse(String::from("R2,D2")), r2d2);

        let mut r10d2: HashSet<Point> = (1..=10)
            .map(|i| Point::new(i, 0, i))
            .collect::<HashSet<Point>>();
        r10d2.insert(Point::new(10, -1, 11));
        r10d2.insert(Point::new(10, -2, 12));
        assert_eq!(parse(String::from("R10,D2")), r10d2);
    }

    fn pt(x: i32, y: i32) -> Point {
        Point::new(x, y, 0)
    }

    #[test]
    fn test_dist_manhattan() {
        assert_eq!(dist_manhattan(pt(3, 3), pt(0, 0)), 6);
        assert_eq!(dist_manhattan(pt(0, 0), pt(3, 3)), 6);
        assert_eq!(dist_manhattan(pt(0, 10), pt(0, -10)), 20);
        assert_eq!(dist_manhattan(pt(0, -3), pt(0, -7)), 4);
        assert_eq!(dist_manhattan(pt(158, -12), pt(0, 0)), 170);
        assert_eq!(dist_manhattan(pt(-2, 1), pt(0, 0)), 3);
        assert_eq!(dist_manhattan(pt(1, 2), pt(4, -3)), 8);
    }

    #[test]
    fn test_example0() {
        let w1 = String::from("R8,U5,L5,D3");
        let w2 = String::from("U7,R6,D4,L4");
        assert_eq!(
            dist_manhattan(closest_cross_manhattan(w1, w2), Point::zero()),
            6
        );
    }

    #[test]
    fn test_example1() {
        let w1 = String::from("R75,D30,R83,U83,L12,D49,R71,U7,L72");
        let w2 = String::from("U62,R66,U55,R34,D71,R55,D58,R83");
        assert_eq!(
            dist_manhattan(closest_cross_manhattan(w1, w2), Point::zero()),
            159
        );
    }

    #[test]
    fn test_example2() {
        let w1 = String::from("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51");
        let w2 = String::from("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7");
        assert_eq!(
            dist_manhattan(closest_cross_manhattan(w1, w2), Point::zero()),
            135
        );
    }

    #[test]
    fn test_dist_signal() {
        assert_eq!(dist_signal(Point::new(2, 4, 5), Point::zero()), 5);
        assert_eq!(dist_signal(Point::new(2, 4, 5), Point::new(1, 2, -2)), 7);
    }

    #[test]
    fn test_example4() {
        let w1 = String::from("R8,U5,L5,D3");
        let w2 = String::from("U7,R6,D4,L4");
        assert_eq!(dist_signal(closest_cross_signal(w1, w2), Point::zero()), 30);
    }

    #[test]
    fn test_example5() {
        let w1 = String::from("R75,D30,R83,U83,L12,D49,R71,U7,L72");
        let w2 = String::from("U62,R66,U55,R34,D71,R55,D58,R83");
        assert_eq!(
            dist_signal(closest_cross_signal(w1, w2), Point::zero()),
            610
        );
    }

    #[test]
    fn test_example6() {
        let w1 = String::from("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51");
        let w2 = String::from("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7");
        assert_eq!(
            dist_signal(closest_cross_signal(w1, w2), Point::zero()),
            410
        );
    }
}
