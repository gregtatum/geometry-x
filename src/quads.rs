#![allow(dead_code, unused_imports, unused_variables, unused_mut)]
use cgmath::prelude::*;
use cgmath::{vec2, vec3, vec4, Vector2, Vector3, Vector4};

pub enum Axis {
    X,
    Y,
    Z,
}

pub enum Direction {
    Positive,
    Negative,
}

pub struct Facing {
    axis: Axis,
    direction: Direction,
}

type Position = Vector3<f64>;
type Positions = Vec<Position>;
type Cell = Vector4<usize>;
type Cells = Vec<Vector4<usize>>;

pub enum CreateQuadsOptions {
    /// Create a quad from the positions.
    FromPositions((Vector3<f64>, Vector3<f64>, Vector3<f64>, Vector3<f64>)),
    /// Create a quad from the width and height of the vector, and the facing direction.
    FromSize((Vector2<f64>, Facing)),
}

#[derive(PartialEq, Debug, Clone)]
pub struct QuadMesh {
    cells: Cells,
    positions: Positions,
}

impl QuadMesh {
    fn new() -> QuadMesh {
        QuadMesh {
            cells: vec![],
            positions: vec![],
        }
    }

    fn get_positions(&self, cell: &Cell) -> (&Position, &Position, &Position, &Position) {
        (
            self.positions
                .get(cell.x)
                .expect("Unable to find position from cell"),
            self.positions
                .get(cell.y)
                .expect("Unable to find position from cell"),
            self.positions
                .get(cell.z)
                .expect("Unable to find position from cell"),
            self.positions
                .get(cell.w)
                .expect("Unable to find position from cell"),
        )
    }

    fn iter_cells<'a>(&'a self) -> impl Iterator<Item = &'a Cell> + 'a {
        self.cells.iter()
    }
}

/// Compute a cell's normal regardless of it's neighboring cells.
fn get_cell_normal(mesh: &QuadMesh, cell: &Cell) -> Vector3<f64> {
    let position_a = mesh.positions.get(cell.x).expect("Unable to find position");
    let position_b = mesh.positions.get(cell.y).expect("Unable to find position");
    let position_c = mesh.positions.get(cell.z).expect("Unable to find position");
    let edge_a = position_b - position_a;
    let edge_b = position_c - position_b;
    edge_a.cross(edge_b).normalize()
}

/// Create a quad with options.
pub fn create_quad(mesh: &mut QuadMesh, options: CreateQuadsOptions) {
    let cell = Vector4::new(
        mesh.positions.len(),
        mesh.positions.len() + 1,
        mesh.positions.len() + 2,
        mesh.positions.len() + 3,
    );

    match options {
        CreateQuadsOptions::FromPositions(positions) => {
            mesh.positions.push(positions.0);
            mesh.positions.push(positions.1);
            mesh.positions.push(positions.2);
            mesh.positions.push(positions.3);
        }
        CreateQuadsOptions::FromSize((size, facing)) => {
            let (x, y) = (size.x / 2.0, size.y / 2.0);
            match facing.axis {
                Axis::X => {
                    mesh.positions.push((0.0, -x, -y).into());
                    mesh.positions.push((0.0, x, -y).into());
                    mesh.positions.push((0.0, x, y).into());
                    mesh.positions.push((0.0, -x, y).into());
                }
                Axis::Y => {
                    mesh.positions.push((-x, 0.0, -y).into());
                    mesh.positions.push((-x, 0.0, y).into());
                    mesh.positions.push((x, 0.0, y).into());
                    mesh.positions.push((x, 0.0, -y).into());
                }
                Axis::Z => {
                    mesh.positions.push((x, -y, 0.0).into());
                    mesh.positions.push((x, y, 0.0).into());
                    mesh.positions.push((-x, y, 0.0).into());
                    mesh.positions.push((-x, -y, 0.0).into());
                }
            };
        }
    };

    mesh.cells.push(cell);
    let mut normals = vec![];
    let normal = get_cell_normal(&mesh, &cell);
    normals.push(normal);
    normals.push(normal);
    normals.push(normal);
    normals.push(normal);
}

trait TextArt {
    fn as_text_art(&self, axis: Axis) -> String;
}

fn get_grid(width: usize, height: usize) -> Vec<String> {
    let mut lines = vec![];
    let iter = (width as i32 * -1)..(width as i32 + 1);
    let iter_rev = (height as i32 * -1)..(height as i32 + 1);

    {
        let mut s = String::from("   ");
        for x in iter.clone() {
            if x >= 0 {
                s.push_str(&format!(" {} ", x));
            } else {
                s.push_str(&format!("{} ", x));
            }
        }
        lines.push(s);
    }

    for y in iter_rev {
        let mut s = String::from("");
        if y >= 0 {
            s.push_str(&format!(" {} ", y));
        } else {
            s.push_str(&format!("{} ", y));
        }

        for x in iter.clone() {
            if x == 0 {
                if y == 0 {
                    s.push_str("┈┊┈");
                } else {
                    s.push_str(" ┊ ");
                }
            } else {
                if y == 0 {
                    s.push_str("┈┈┈");
                } else {
                    s.push_str(" · ");
                }
            }
        }
        lines.push(s);
    }
    lines
}

fn replace_codepoint(str: &mut String, replacement: &str, index: usize) {
    str.replace_range(
        str.char_indices()
            .nth(index)
            .map(|(pos, ch)| (pos..pos + ch.len_utf8()))
            .unwrap(),
        replacement,
    )
}

impl TextArt for QuadMesh {
    fn as_text_art(&self, axis: Axis) -> String {
        let x_margin = 4;
        let y_margin = 1;
        let col_size = 3;
        let half_w: usize = 5;
        let half_h: usize = 5;
        let mut lines = get_grid(half_w, half_h);
        for cell in self.iter_cells() {
            let (a, b, c, d) = self.get_positions(&cell);

            let mut translate = |px: f64, py: f64| -> (usize, usize) {
                (
                    (x_margin as i32 + (px as i32 + half_w as i32) * col_size as i32) as usize,
                    (y_margin as i32 + py as i32 + half_h as i32) as usize,
                )
            };

            let mut apply_orthogonal = |p: &Position| match axis {
                Axis::X => translate(p.z, p.y),
                Axis::Y => translate(p.x, p.z),
                Axis::Z => translate(p.x, p.y),
            };

            {
                let mut print_segment = |a: &Position, b: &Position| {
                    let (ax, ay) = apply_orthogonal(a);
                    let (bx, by) = apply_orthogonal(b);
                    let dy = by as i32 - ay as i32;
                    let dx = bx as i32 - ax as i32;
                    let slope = if dx == 0 {
                        100000000.0
                    } else {
                        (by as f32 - ay as f32) / (bx as f32 - ax as f32)
                    };

                    if slope.abs() > 0.5 {
                        let slope = (bx as f32 - ax as f32) / (by as f32 - ay as f32);
                        let sign = if dy > 0 { 1 } else { -1 };
                        for y in 0..dy.abs() {
                            let x = ax as f32 + (slope * dy as f32);
                            if x >= 0.0 {
                                if let Some(line) = lines.get_mut((ay as i32 + y * sign) as usize) {
                                    replace_codepoint(line, "┃", x.round() as usize);
                                }
                            }
                        }
                    } else {
                        let sign = if dx > 0 { 1 } else { -1 };
                        for x in 0..dx.abs() {
                            let y = ay as f32 + (slope * dx as f32);
                            if y >= 0.0 {
                                if let Some(line) = lines.get_mut(y.round() as usize) {
                                    replace_codepoint(line, "━", (ax as i32 + x * sign) as usize);
                                }
                            }
                        }
                    };
                };

                print_segment(&a, &b);
                print_segment(&b, &c);
                print_segment(&c, &d);
                print_segment(&d, &a);
            }

            {
                let mut print_point = |p: &Position| {
                    let (x, y) = apply_orthogonal(p);
                    if let Some(mut line) = lines.get_mut(y) {
                        for (i, ch) in line.chars().enumerate() {
                            if i == x {
                                replace_codepoint(line, "◆", i);
                                break;
                            }
                        }
                    }
                };

                print_point(&a);
                print_point(&b);
                print_point(&c);
                print_point(&d);
            }
        }
        let mut string = String::new();
        for item in lines {
            string.push_str(item.trim_end());
            string.push_str("\n");
        }
        string
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use insta::*;

    #[test]
    fn test_create_quad_from_size() {
        let mut mesh = QuadMesh::new();
        create_quad(
            &mut mesh,
            CreateQuadsOptions::FromSize((
                vec2(2.0, 4.0),
                Facing {
                    axis: Axis::Z,
                    direction: Direction::Positive,
                },
            )),
        );

        assert_snapshot!(mesh.as_text_art(Axis::Z), @r###"
           -5 -4 -3 -2 -1  0  1  2  3  4  5
        -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
        -4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
        -3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
        -2  ·  ·  ·  ·  ◆━━━━━◆  ·  ·  ·  ·
        -1  ·  ·  ·  ·  ┃  ┊  ┃  ·  ·  ·  ·
         0 ┈┈┈┈┈┈┈┈┈┈┈┈┈┃┈┈┊┈┈┃┈┈┈┈┈┈┈┈┈┈┈┈┈
         1  ·  ·  ·  ·  ┃  ┊  ┃  ·  ·  ·  ·
         2  ·  ·  ·  ·  ◆━━━━━◆  ·  ·  ·  ·
         3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
         4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
         5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
        "###);
    }

    #[test]
    fn test_create_quad_positions() {
        let mut mesh = QuadMesh::new();
        create_quad(
            &mut mesh,
            CreateQuadsOptions::FromPositions((
                vec3(-4.0, 0.0, -3.0),
                vec3(-4.0, 0.0, 3.0),
                vec3(1.0, 0.0, 3.0),
                vec3(1.0, 0.0, -3.0),
            )),
        );

        assert_snapshot!(mesh.as_text_art(Axis::Y), @r###"
           -5 -4 -3 -2 -1  0  1  2  3  4  5
        -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
        -4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
        -3  ·  ◆━━━━━━━━━━━━━━◆  ·  ·  ·  ·
        -2  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
        -1  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
         0 ┈┈┈┈┃┈┈┈┈┈┈┈┈┈┈┈┊┈┈┃┈┈┈┈┈┈┈┈┈┈┈┈┈
         1  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
         2  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
         3  ·  ◆━━━━━━━━━━━━━━◆  ·  ·  ·  ·
         4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
         5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
        "###);
    }
}
