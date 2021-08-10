//! This module is a collection of quad geometry creation and manipulation tools. Quads
//! can be easier to reason with for geometry generation compared to directly manipulating
//! triangles. In addition, they are useful for generating smoothed models using
//! Catmull-Clark subdivision.

#![allow(dead_code, unused_imports, unused_variables, unused_mut)]
#![warn(missing_docs)]
#![allow(clippy::many_single_char_names)]

use cgmath::prelude::*;
use cgmath::{vec2, vec3, vec4, Vector2, Vector3, Vector4};

/// An axis direction.
pub enum Axis {
    ///
    X,
    ///
    Y,
    ///
    Z,
}

/// The direction of an axis.
pub enum Direction {
    ///
    Positive,
    ///
    Negative,
}

/// The axis and direction to determine the facing direction of a quad. Useful in the
/// [SingleQuadOptions].
pub struct Facing {
    ///
    pub axis: Axis,
    ///
    pub direction: Direction,
}

/// A single position in a mesh. It can be indexed by multiple cells.
pub type Position = Vector3<f64>;
/// A normal for a mesh.
pub type Normal = Vector3<f64>;
/// The collection of [Position]s that are indexed by [Cells].
pub type Positions = Vec<Position>;
/// The collection of [Normal]s that match the indexes of the [Positions].
pub type Normals = Vec<Normal>;
/// A single cell in a [simplicial complex](https://en.wikipedia.org/wiki/Simplicial_complex).
pub type Cell = Vector4<usize>;
/// The list of all cells in the mesh or [simplicial complex](https://en.wikipedia.org/wiki/Simplicial_complex).
pub type Cells = Vec<Vector4<usize>>;

/// The input options for [`create_single_quad`].
pub enum SingleQuadOptions {
    /// Create a quad from a positions tuple.
    ///
    /// # Example
    ///
    /// ```
    /// # use cgmath::*;
    /// # use geometry_x::quads::*;
    /// let mut mesh = QuadMesh::new();
    /// create_single_quad(
    ///     &mut mesh,
    ///     SingleQuadOptions::FromPositions((
    ///         vec3(-4.0, 0.0, -3.0),
    ///         vec3(-4.0, 0.0, 3.0),
    ///         vec3(1.0, 0.0, 3.0),
    ///         vec3(1.0, 0.0, -3.0),
    ///     )),
    /// );
    ///
    /// assert_eq!(mesh.as_text_art(Axis::Y).as_str(), &"
    ///    -5 -4 -3 -2 -1  0  1  2  3  4  5
    /// -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    /// -4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    /// -3  ·  ◆━━━━━━━━━━━━━━◆  ·  ·  ·  ·
    /// -2  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
    /// -1  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
    ///  0 ┈┈┈┈┃┈┈┈┈┈┈┈┈┈┈┈┊┈┈┃┈┈┈┈┈┈┈┈┈┈┈┈┈
    ///  1  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
    ///  2  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
    ///  3  ·  ◆━━━━━━━━━━━━━━◆  ·  ·  ·  ·
    ///  4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    /// "[1..]);
    /// ```
    FromPositions((Vector3<f64>, Vector3<f64>, Vector3<f64>, Vector3<f64>)),
    /// Create a quad from the width and height of the vector, and the facing direction.
    ///
    /// # Example
    ///
    /// ```
    /// # use cgmath::*;
    /// # use geometry_x::quads::*;
    /// let mut mesh = QuadMesh::new();
    /// create_single_quad(
    ///     &mut mesh,
    ///     SingleQuadOptions::FromSize((
    ///         vec2(2.0, 4.0),
    ///         Facing {
    ///             axis: Axis::Z,
    ///             direction: Direction::Positive,
    ///         },
    ///     )),
    /// );
    ///
    /// assert_eq!(mesh.as_text_art(Axis::Z).as_str(), &"
    ///    -5 -4 -3 -2 -1  0  1  2  3  4  5
    /// -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    /// -4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    /// -3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    /// -2  ·  ·  ·  ·  ◆━━━━━◆  ·  ·  ·  ·
    /// -1  ·  ·  ·  ·  ┃  ┊  ┃  ·  ·  ·  ·
    ///  0 ┈┈┈┈┈┈┈┈┈┈┈┈┈┃┈┈┊┈┈┃┈┈┈┈┈┈┈┈┈┈┈┈┈
    ///  1  ·  ·  ·  ·  ┃  ┊  ┃  ·  ·  ·  ·
    ///  2  ·  ·  ·  ·  ◆━━━━━◆  ·  ·  ·  ·
    ///  3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///  4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    /// "[1..]);
    /// ```
    FromSize((Vector2<f64>, Facing)),
}

/// A mesh made up of quads.
#[derive(PartialEq, Debug, Clone)]
pub struct QuadMesh {
    /// Indexes into the positions array. A single cell represents a single quad.
    cells: Cells,
    /// The positions in an unordered Vector.
    positions: Positions,
    /// The computed normals for a quad mesh.
    normals: Option<Normals>,
}

impl QuadMesh {
    /// Create a new [QuadMesh] with no normals.
    pub fn new() -> QuadMesh {
        QuadMesh {
            cells: vec![],
            positions: vec![],
            normals: None,
        }
    }

    /// Create a new [QuadMesh] with normals.
    pub fn new_with_normals() -> QuadMesh {
        QuadMesh {
            cells: vec![],
            positions: vec![],
            normals: Some(vec![]),
        }
    }

    /// Given a [Cell], look up a [Position]s tuple.
    pub fn get_positions(&self, cell: &Cell) -> (&Position, &Position, &Position, &Position) {
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

    /// Iterate through the cells.
    ///
    /// ```
    /// # use geometry_x::quads::*;
    /// # use cgmath::*;
    /// let mut mesh = QuadMesh::new();
    ///
    /// create_single_quad(
    ///     &mut mesh,
    ///     SingleQuadOptions::FromSize((
    ///         vec2(2.0, 4.0),
    ///         Facing {
    ///             axis: Axis::Z,
    ///             direction: Direction::Positive,
    ///         },
    ///     )),
    /// );
    ///
    /// for cell in mesh.iter_cells() {
    ///    let (a, b, c, d) = mesh.get_positions(&cell);
    ///    assert_eq!(a, &Vector3::new(1.0, -2.0, 0.0));
    ///    assert_eq!(b, &Vector3::new(1.0, 2.0, 0.0));
    ///    assert_eq!(c, &Vector3::new(-1.0, 2.0, 0.0));
    ///    assert_eq!(d, &Vector3::new(-1.0, -2.0, 0.0));
    /// }
    /// ```
    pub fn iter_cells(&self) -> impl Iterator<Item = &Cell> {
        self.cells.iter()
    }
}

impl Default for QuadMesh {
    fn default() -> Self {
        Self::new()
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

/// See [SingleQuadOptions] for different inputs for creating a single quad inside
/// of a QuadMesh.
///
/// ```text
///    -5 -4 -3 -2 -1  0  1  2  3  4  5
/// -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
/// -4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
/// -3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
/// -2  ·  ·  ·  ·  ◆━━━━━◆  ·  ·  ·  ·
/// -1  ·  ·  ·  ·  ┃  ┊  ┃  ·  ·  ·  ·
///  0 ┈┈┈┈┈┈┈┈┈┈┈┈┈┃┈┈┊┈┈┃┈┈┈┈┈┈┈┈┈┈┈┈┈
///  1  ·  ·  ·  ·  ┃  ┊  ┃  ·  ·  ·  ·
///  2  ·  ·  ·  ·  ◆━━━━━◆  ·  ·  ·  ·
///  3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
///  4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
///  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
/// ```
pub fn create_single_quad(mesh: &mut QuadMesh, options: SingleQuadOptions) {
    let cell = Vector4::new(
        mesh.positions.len(),
        mesh.positions.len() + 1,
        mesh.positions.len() + 2,
        mesh.positions.len() + 3,
    );

    match options {
        SingleQuadOptions::FromPositions(positions) => {
            mesh.positions.push(positions.0);
            mesh.positions.push(positions.1);
            mesh.positions.push(positions.2);
            mesh.positions.push(positions.3);
        }
        SingleQuadOptions::FromSize((size, facing)) => {
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
    let normal = get_cell_normal(mesh, &cell);
    normals.push(normal);
    normals.push(normal);
    normals.push(normal);
    normals.push(normal);
}

/// A trait to draw some piece of geometry using text art. See https://gregtatum.com/writing/2020/ascii-physics-system/
pub trait TextArt {
    /// Given a geometry type and an axis, draw it using text and an orthogonal
    /// projection.
    fn as_text_art(&self, axis: Axis) -> String;
}

fn get_grid(width: usize, height: usize) -> Vec<String> {
    let mut lines = vec![];
    let iter = -(width as i32)..(width as i32 + 1);
    let iter_rev = -(height as i32)..(height as i32 + 1);

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
            } else if y == 0 {
                s.push_str("┈┈┈");
            } else {
                s.push_str(" · ");
            }
        }
        lines.push(s);
    }
    lines
}

/// Safely operate on a UTF-8 string to replace a codepoint.
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
    /// Draw a [QuadMesh] using text art. See https://gregtatum.com/writing/2020/ascii-physics-system/
    fn as_text_art(&self, axis: Axis) -> String {
        let x_margin = 4;
        let y_margin = 1;
        let col_size = 3;
        let half_w: usize = 5;
        let half_h: usize = 5;
        let mut lines = get_grid(half_w, half_h);
        for cell in self.iter_cells() {
            let (a, b, c, d) = self.get_positions(cell);

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

                print_segment(a, b);
                print_segment(b, c);
                print_segment(c, d);
                print_segment(d, a);
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

                print_point(a);
                print_point(b);
                print_point(c);
                print_point(d);
            }
        }
        let mut string = String::new();
        for item in lines {
            string.push_str(item.trim_end());
            string.push('\n');
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
        create_single_quad(
            &mut mesh,
            SingleQuadOptions::FromSize((
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
        create_single_quad(
            &mut mesh,
            SingleQuadOptions::FromPositions((
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
