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

/// A single position in a mesh. It can be indexed by multiple quads.
pub type Position = Vector3<f64>;
/// A normal for a mesh.
pub type Normal = Vector3<f64>;
/// The collection of [Position]s that are indexed by [Quad]s.
pub type Positions = Vec<Position>;
/// The collection of [Normal]s that match the indexes of the [Positions].
pub type Normals = Vec<Normal>;
/// A single quad that references the positions.
pub type Quad = Vector4<usize>;
/// A convience for destructuring.
pub type QuadTuple = (usize, usize, usize, usize);
/// The list of all quads in the mesh.
pub type Quads = Vec<Vector4<usize>>;

/// The input options for [`QuadMesh::create_single_quad`].
pub enum SingleQuadOptions {
    /// Create a quad from a positions tuple.
    ///
    /// # Example
    ///
    /// ```
    /// # use cgmath::*;
    /// # use geometry_x::quads::*;
    /// let mut mesh = QuadMesh::new();
    /// mesh.create_single_quad(
    ///     SingleQuadOptions::FromPositions((
    ///         vec3(-4.0, 0.0, -3.0),
    ///         vec3(-4.0, 0.0, 3.0),
    ///         vec3(1.0, 0.0, 3.0),
    ///         vec3(1.0, 0.0, -3.0),
    ///     )),
    /// );
    ///
    /// mesh.assert_art(
    ///     Axis::Y,
    ///     "
    ///     │    -5 -4 -3 -2 -1  0  1  2  3  4  5
    ///     │ -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -3  ·  ◆━━━━━━━━━━━━━━◆  ·  ·  ·  ·
    ///     │ -2  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
    ///     │ -1  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
    ///     │  0 ┈┈┈┈┃┈┈┈┈┈┈┈┈┈┈┈┊┈┈┃┈┈┈┈┈┈┈┈┈┈┈┈┈
    ///     │  1  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
    ///     │  2  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
    ///     │  3  ·  ◆━━━━━━━━━━━━━━◆  ·  ·  ·  ·
    ///     │  4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     "
    /// );
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
    /// mesh.create_single_quad(
    ///     SingleQuadOptions::FromSize((
    ///         vec2(2.0, 4.0),
    ///         Facing {
    ///             axis: Axis::Z,
    ///             direction: Direction::Positive,
    ///         },
    ///     )),
    /// );
    ///
    /// mesh.assert_art(
    ///     Axis::Z,
    ///     "
    ///     │    -5 -4 -3 -2 -1  0  1  2  3  4  5
    ///     │ -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -2  ·  ·  ·  ·  ◆━━━━━◆  ·  ·  ·  ·
    ///     │ -1  ·  ·  ·  ·  ┃  ┊  ┃  ·  ·  ·  ·
    ///     │  0 ┈┈┈┈┈┈┈┈┈┈┈┈┈┃┈┈┊┈┈┃┈┈┈┈┈┈┈┈┈┈┈┈┈
    ///     │  1  ·  ·  ·  ·  ┃  ┊  ┃  ·  ·  ·  ·
    ///     │  2  ·  ·  ·  ·  ◆━━━━━◆  ·  ·  ·  ·
    ///     │  3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │  4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     "
    /// );
    /// ```
    FromSize((Vector2<f64>, Facing)),
}

/// The quads structure is defined as a mesh. This mesh contains a list of positions in
/// space, and a list of indexes to define how the quads are to be formed. Different
/// quads can share the same point, and points can be duplicated but share the same
/// space.
///
/// Normals are optionally computed for the QuadMesh.
/// ```
/// # use geometry_x::quads::*;
/// # use cgmath::*;
/// # let mut mesh = QuadMesh::new();
/// # mesh.create_single_quad(SingleQuadOptions::FromSize((
/// #     vec2(4.0, 4.0),
/// #     Facing {
/// #         axis: Axis::Z,
/// #         direction: Direction::Positive,
/// #     },
/// # )));
/// mesh.assert_art(
///     Axis::Z, "
///     │    -5 -4 -3 -2 -1  0  1  2  3  4  5
///     │ -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
///     │ -4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
///     │ -3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
///     │ -2  ·  ·  ·  ◆━━━━━━━━━━━◆  ·  ·  ·
///     │ -1  ·  ·  ·  ┃  ·  ┊  ·  ┃  ·  ·  ·
///     │  0 ┈┈┈┈┈┈┈┈┈┈┃┈┈┈┈┈┊┈┈┈┈┈┃┈┈┈┈┈┈┈┈┈┈
///     │  1  ·  ·  ·  ┃  ·  ┊  ·  ┃  ·  ·  ·
///     │  2  ·  ·  ·  ◆━━━━━━━━━━━◆  ·  ·  ·
///     │  3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
///     │  4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
///     │  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
///     "
/// );
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct QuadMesh {
    /// Indexes into the positions array. A single quad represents a single quad.
    pub quads: Quads,
    /// The positions in an unordered Vector.
    pub positions: Positions,
    /// The computed normals for a quad mesh.
    pub normals: Option<Normals>,
}

/// A shared error for [QuadMesh] operations.
#[derive(Debug)]
pub enum QuadError {
    /// A position was not found for a given index.
    PositionIndexError,
    /// A quad was not found for a given index.
    QuadIndexError,
}

/// A [Result] with the [QuadError] applied.
pub type QuadResult<T> = Result<T, QuadError>;

impl QuadMesh {
    /// Create a new [QuadMesh] with no normals.
    pub fn new() -> QuadMesh {
        QuadMesh {
            quads: vec![],
            positions: vec![],
            normals: None,
        }
    }

    /// Create a new [QuadMesh] with normals.
    pub fn new_with_normals() -> QuadMesh {
        QuadMesh {
            quads: vec![],
            positions: vec![],
            normals: Some(vec![]),
        }
    }

    /// Get a position from the mesh by index.
    pub fn get_position(&self, index: usize) -> QuadResult<&Position> {
        match self.positions.get(index) {
            Some(position) => Ok(position),
            None => Err(QuadError::PositionIndexError),
        }
    }

    /// Get a mut position from the mesh by index.
    pub fn get_position_mut(&mut self, index: usize) -> QuadResult<&mut Position> {
        match self.positions.get_mut(index) {
            Some(position) => Ok(position),
            None => Err(QuadError::PositionIndexError),
        }
    }

    /// Get a clone of a position based on its index.
    pub fn clone_position(&self, index: usize) -> QuadResult<Position> {
        self.get_position(index).map(|p| *p)
    }

    /// Get a [Quad] from the mesh by index.
    pub fn get_quad(&self, index: usize) -> QuadResult<&Quad> {
        match self.quads.get(index) {
            Some(quad) => Ok(quad),
            None => Err(QuadError::QuadIndexError),
        }
    }

    /// Get a mutable [Quad] from the mesh by index.
    pub fn get_quad_mut(&mut self, index: usize) -> QuadResult<&mut Quad> {
        match self.quads.get_mut(index) {
            Some(quad) => Ok(quad),
            None => Err(QuadError::QuadIndexError),
        }
    }

    /// Get a clone of a position based on its index.
    pub fn clone_quad(&self, index: usize) -> QuadResult<Quad> {
        self.get_quad(index).map(|c| *c)
    }

    // Get an easy to destructure quad.
    fn get_quad_tuple(&self, index: usize) -> QuadResult<QuadTuple> {
        self.get_quad(index).map(|c| (*c).into())
    }

    /// Given a [Quad], look up a [Position]s tuple.
    pub fn get_positions(&self, quad: &Quad) -> (&Position, &Position, &Position, &Position) {
        (
            self.positions
                .get(quad.x)
                .expect("Unable to find position from quad"),
            self.positions
                .get(quad.y)
                .expect("Unable to find position from quad"),
            self.positions
                .get(quad.z)
                .expect("Unable to find position from quad"),
            self.positions
                .get(quad.w)
                .expect("Unable to find position from quad"),
        )
    }

    /// Iterate through the quads.
    ///
    /// ```
    /// # use geometry_x::quads::*;
    /// # use cgmath::*;
    /// let mut mesh = QuadMesh::new();
    ///
    /// mesh.create_single_quad(
    ///     SingleQuadOptions::FromSize((
    ///         vec2(2.0, 4.0),
    ///         Facing {
    ///             axis: Axis::Z,
    ///             direction: Direction::Positive,
    ///         },
    ///     )),
    /// );
    ///
    /// for quad in mesh.iter_quads() {
    ///    let (a, b, c, d) = mesh.get_positions(&quad);
    ///    assert_eq!(a, &Vector3::new(1.0, -2.0, 0.0));
    ///    assert_eq!(b, &Vector3::new(1.0, 2.0, 0.0));
    ///    assert_eq!(c, &Vector3::new(-1.0, 2.0, 0.0));
    ///    assert_eq!(d, &Vector3::new(-1.0, -2.0, 0.0));
    /// }
    /// ```
    pub fn iter_quads(&self) -> impl Iterator<Item = &Quad> {
        self.quads.iter()
    }

    /// See [SingleQuadOptions] for different inputs for creating a single quad inside
    /// of a QuadMesh.
    ///
    /// ```text
    /// │    -5 -4 -3 -2 -1  0  1  2  3  4  5
    /// │ -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    /// │ -4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    /// │ -3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    /// │ -2  ·  ·  ·  ·  ◆━━━━━◆  ·  ·  ·  ·
    /// │ -1  ·  ·  ·  ·  ┃  ┊  ┃  ·  ·  ·  ·
    /// │  0 ┈┈┈┈┈┈┈┈┈┈┈┈┈┃┈┈┊┈┈┃┈┈┈┈┈┈┈┈┈┈┈┈┈
    /// │  1  ·  ·  ·  ·  ┃  ┊  ┃  ·  ·  ·  ·
    /// │  2  ·  ·  ·  ·  ◆━━━━━◆  ·  ·  ·  ·
    /// │  3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    /// │  4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    /// │  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    /// ```
    pub fn create_single_quad(&mut self, options: SingleQuadOptions) {
        let mut positions = &mut self.positions;
        let quad = Vector4::new(
            positions.len(),
            positions.len() + 1,
            positions.len() + 2,
            positions.len() + 3,
        );

        match options {
            SingleQuadOptions::FromPositions((a, b, c, d)) => {
                positions.push(a);
                positions.push(b);
                positions.push(c);
                positions.push(d);
            }
            SingleQuadOptions::FromSize((size, facing)) => {
                let (x, y) = (size.x / 2.0, size.y / 2.0);
                match facing.axis {
                    Axis::X => {
                        positions.push((0.0, -x, -y).into());
                        positions.push((0.0, x, -y).into());
                        positions.push((0.0, x, y).into());
                        positions.push((0.0, -x, y).into());
                    }
                    Axis::Y => {
                        positions.push((-x, 0.0, -y).into());
                        positions.push((-x, 0.0, y).into());
                        positions.push((x, 0.0, y).into());
                        positions.push((x, 0.0, -y).into());
                    }
                    Axis::Z => {
                        positions.push((x, -y, 0.0).into());
                        positions.push((x, y, 0.0).into());
                        positions.push((-x, y, 0.0).into());
                        positions.push((-x, -y, 0.0).into());
                    }
                };
            }
        };

        self.quads.push(quad);
        let mut normals = vec![];
        let normal = self.get_normal(&quad);
        normals.push(normal);
        normals.push(normal);
        normals.push(normal);
        normals.push(normal);
    }

    /// Compute a quad's normal regardless of it's neighboring quads.
    pub fn get_normal(&self, quad: &Quad) -> Normal {
        let position_a = self.positions.get(quad.x).expect("Unable to find position");
        let position_b = self.positions.get(quad.y).expect("Unable to find position");
        let position_c = self.positions.get(quad.z).expect("Unable to find position");
        let edge_a = position_b - position_a;
        let edge_b = position_c - position_b;
        edge_a.cross(edge_b).normalize()
    }

    /// Split a quad quad vertically.
    ///
    /// ```
    /// # use geometry_x::quads::*;
    /// # use cgmath::*;
    /// let mut mesh = QuadMesh::new();
    /// mesh.create_single_quad(SingleQuadOptions::FromSize((
    ///     vec2(8.0, 8.0),
    ///     Facing {
    ///         axis: Axis::Z,
    ///         direction: Direction::Positive,
    ///     },
    /// )));
    ///
    /// mesh.assert_art(
    ///     Axis::Z, "
    ///     │    -5 -4 -3 -2 -1  0  1  2  3  4  5
    ///     │ -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -4  ·  ◆━━━━━━━━━━━━━━━━━━━━━━━◆  ·
    ///     │ -3  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │ -2  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │ -1  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  0 ┈┈┈┈┃┈┈┈┈┈┈┈┈┈┈┈┊┈┈┈┈┈┈┈┈┈┈┈┃┈┈┈┈
    ///     │  1  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  2  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  3  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  4  ·  ◆━━━━━━━━━━━━━━━━━━━━━━━◆  ·
    ///     │  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     "
    /// );
    ///
    /// let quad_a = mesh.quads.len() - 1;
    /// mesh.split_vertical(quad_a, 0.2).unwrap();
    ///
    /// mesh.assert_art(
    ///     Axis::Z,
    ///     "
    ///     │    -5 -4 -3 -2 -1  0  1  2  3  4  5
    ///     │ -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -4  ·  ◆━━━━━◆━━━━━━━━━━━━━━━━━◆  ·
    ///     │ -3  ·  ┃  ·  ┃  ·  ┊  ·  ·  ·  ┃  ·
    ///     │ -2  ·  ┃  ·  ┃  ·  ┊  ·  ·  ·  ┃  ·
    ///     │ -1  ·  ┃  ·  ┃  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  0 ┈┈┈┈┃┈┈┈┈┈┃┈┈┈┈┈┊┈┈┈┈┈┈┈┈┈┈┈┃┈┈┈┈
    ///     │  1  ·  ┃  ·  ┃  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  2  ·  ┃  ·  ┃  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  3  ·  ┃  ·  ┃  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  4  ·  ◆━━━━━◆━━━━━━━━━━━━━━━━━◆  ·
    ///     │  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     "
    /// );
    /// ```
    pub fn split_vertical(&mut self, quad_index: usize, t: f64) -> QuadResult<()> {
        //  b---bc---c
        //  |   |    |
        //  |   |    |
        //  a---ad---d
        let (pt_bc, pt_ad) = {
            let (pt_a, pt_b, pt_c, pt_d) = self.get_positions(self.get_quad(quad_index)?);
            (pt_c.lerp(*pt_b, t), pt_d.lerp(*pt_a, t))
        };
        let bc = self.positions.len();
        self.positions.push(pt_bc);

        let ad = self.positions.len();
        self.positions.push(pt_ad);

        let (_a, _b, c, d) = self.get_quad_tuple(quad_index)?;
        self.quads.push(vec4(ad, bc, c, d));

        let mut quad = self.get_quad_mut(quad_index)?;
        quad.z = bc;
        quad.w = ad;
        Ok(())
    }

    /// Split a quad quad horizontally.
    ///
    /// ```
    /// # use geometry_x::quads::*;
    /// # use cgmath::*;
    /// let mut mesh = QuadMesh::new();
    /// mesh.create_single_quad(SingleQuadOptions::FromSize((
    ///     vec2(8.0, 8.0),
    ///     Facing {
    ///         axis: Axis::Z,
    ///         direction: Direction::Positive,
    ///     },
    /// )));
    ///
    /// mesh.assert_art(
    ///     Axis::Z, "
    ///     │    -5 -4 -3 -2 -1  0  1  2  3  4  5
    ///     │ -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -4  ·  ◆━━━━━━━━━━━━━━━━━━━━━━━◆  ·
    ///     │ -3  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │ -2  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │ -1  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  0 ┈┈┈┈┃┈┈┈┈┈┈┈┈┈┈┈┊┈┈┈┈┈┈┈┈┈┈┈┃┈┈┈┈
    ///     │  1  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  2  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  3  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  4  ·  ◆━━━━━━━━━━━━━━━━━━━━━━━◆  ·
    ///     │  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     "
    /// );
    ///
    /// let quad_a = mesh.quads.len() - 1;
    /// mesh.split_horizontal(quad_a, 0.2).unwrap();
    ///
    /// mesh.assert_art(
    ///     Axis::Z,
    ///     "
    ///     │    -5 -4 -3 -2 -1  0  1  2  3  4  5
    ///     │ -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -4  ·  ◆━━━━━━━━━━━━━━━━━━━━━━━◆  ·
    ///     │ -3  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │ -2  ·  ◆━━━━━━━━━━━━━━━━━━━━━━━◆  ·
    ///     │ -1  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  0 ┈┈┈┈┃┈┈┈┈┈┈┈┈┈┈┈┊┈┈┈┈┈┈┈┈┈┈┈┃┈┈┈┈
    ///     │  1  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  2  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  3  ·  ┃  ·  ·  ·  ┊  ·  ·  ·  ┃  ·
    ///     │  4  ·  ◆━━━━━━━━━━━━━━━━━━━━━━━◆  ·
    ///     │  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     "
    /// );
    /// ```
    pub fn split_horizontal(&mut self, quad_index: usize, t: f64) -> QuadResult<()> {
        //  b--------c
        //  |        |
        //  ab------cd
        //  |        |
        //  a--------d
        let (pt_ab, pt_cd) = {
            let (pt_a, pt_b, pt_c, pt_d) = self.get_positions(self.get_quad(quad_index)?);
            (pt_a.lerp(*pt_b, t), pt_d.lerp(*pt_c, t))
        };
        let ab = self.positions.len();
        self.positions.push(pt_ab);

        let cd = self.positions.len();
        self.positions.push(pt_cd);

        let (_a, b, c, _d) = self.get_quad_tuple(quad_index)?;
        self.quads.push(vec4(ab, b, c, cd));

        let mut quad = self.get_quad_mut(quad_index)?;
        quad.y = ab;
        quad.z = cd;
        Ok(())
    }

    /// Split a quad quad horizontally, but disjoint. This will ensure the new quad
    /// quad has its own positions.
    ///
    /// ```
    /// # use geometry_x::quads::*;
    /// # use cgmath::*;
    /// let mut mesh = QuadMesh::new();
    /// mesh.create_single_quad(SingleQuadOptions::FromSize((
    ///     vec2(6.0, 6.0),
    ///     Facing {
    ///         axis: Axis::Z,
    ///         direction: Direction::Positive,
    ///     },
    /// )));
    ///
    /// mesh.assert_art(
    ///     Axis::Z, "
    ///     │    -5 -4 -3 -2 -1  0  1  2  3  4  5
    ///     │ -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -3  ·  ·  ◆━━━━━━━━━━━━━━━━━◆  ·  ·
    ///     │ -2  ·  ·  ┃  ·  ·  ┊  ·  ·  ┃  ·  ·
    ///     │ -1  ·  ·  ┃  ·  ·  ┊  ·  ·  ┃  ·  ·
    ///     │  0 ┈┈┈┈┈┈┈┃┈┈┈┈┈┈┈┈┊┈┈┈┈┈┈┈┈┃┈┈┈┈┈┈┈
    ///     │  1  ·  ·  ┃  ·  ·  ┊  ·  ·  ┃  ·  ·
    ///     │  2  ·  ·  ┃  ·  ·  ┊  ·  ·  ┃  ·  ·
    ///     │  3  ·  ·  ◆━━━━━━━━━━━━━━━━━◆  ·  ·
    ///     │  4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     "
    /// );
    ///
    /// let quad_a = mesh.quads.len() - 1;
    /// mesh.split_horizontal_disjoint(quad_a, 0.2).unwrap();
    ///
    /// mesh.assert_art(
    ///     Axis::Z,
    ///     "
    ///     │    -5 -4 -3 -2 -1  0  1  2  3  4  5
    ///     │ -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -3  ·  ·  ◆━━━━━━━━━━━━━━━━━◆  ·  ·
    ///     │ -2  ·  ·  ┃  ·  ·  ┊  ·  ·  ┃  ·  ·
    ///     │ -1  ·  ·  ◆━━━━━━━━━━━━━━━━━◆  ·  ·
    ///     │  0 ┈┈┈┈┈┈┈┃┈┈┈┈┈┈┈┈┊┈┈┈┈┈┈┈┈┃┈┈┈┈┈┈┈
    ///     │  1  ·  ·  ┃  ·  ·  ┊  ·  ·  ┃  ·  ·
    ///     │  2  ·  ·  ┃  ·  ·  ┊  ·  ·  ┃  ·  ·
    ///     │  3  ·  ·  ◆━━━━━━━━━━━━━━━━━◆  ·  ·
    ///     │  4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     "
    /// );
    ///
    /// mesh.translate(quad_a, vec3(-1.0, -2.0, 0.0)).expect("Failed to translate.");
    ///
    /// mesh.assert_art(
    ///     Axis::Z,
    ///     "
    ///     │    -5 -4 -3 -2 -1  0  1  2  3  4  5
    ///     │ -5  ·  ◆━━━━━━━━━━━━━━━━━◆  ·  ·  ·
    ///     │ -4  ·  ┃  ·  ·  ·  ┊  ·  ┃  ·  ·  ·
    ///     │ -3  ·  ◆━━━━━━━━━━━━━━━━━◆  ·  ·  ·
    ///     │ -2  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -1  ·  ·  ◆━━━━━━━━━━━━━━━━━◆  ·  ·
    ///     │  0 ┈┈┈┈┈┈┈┃┈┈┈┈┈┈┈┈┊┈┈┈┈┈┈┈┈┃┈┈┈┈┈┈┈
    ///     │  1  ·  ·  ┃  ·  ·  ┊  ·  ·  ┃  ·  ·
    ///     │  2  ·  ·  ┃  ·  ·  ┊  ·  ·  ┃  ·  ·
    ///     │  3  ·  ·  ◆━━━━━━━━━━━━━━━━━◆  ·  ·
    ///     │  4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     "
    /// );
    /// ```
    pub fn split_horizontal_disjoint(&mut self, quad_index: usize, t: f64) -> QuadResult<()> {
        //  b--------c
        //  |        |
        //  ab1----cd1
        //  ab2----cd2
        //  | target |
        //  a--------d
        let (pt_ab, pt_cd) = {
            let (pt_a, pt_b, pt_c, pt_d) = self.get_positions(self.get_quad(quad_index)?);
            (pt_a.lerp(*pt_b, t), pt_d.lerp(*pt_c, t))
        };
        let ab1 = self.positions.len();
        self.positions.push(pt_ab);
        let ab2 = self.positions.len();
        self.positions.push(pt_ab);

        let cd1 = self.positions.len();
        self.positions.push(pt_cd);
        let cd2 = self.positions.len();
        self.positions.push(pt_cd);

        let (a, b, c, d) = self.get_quad_tuple(quad_index)?;
        self.quads.push(vec4(ab2, b, c, cd2));

        let mut quad = self.get_quad_mut(quad_index)?;
        quad.y = ab1;
        quad.z = cd1;
        Ok(())
    }

    /// Translate a single quad quad by a vector.
    ///
    /// ```
    /// # use geometry_x::quads::*;
    /// # use cgmath::*;
    /// # let mut mesh = QuadMesh::new();
    /// # mesh.create_single_quad(SingleQuadOptions::FromSize((
    /// #     vec2(2.0, 2.0),
    /// #     Facing {
    /// #         axis: Axis::Z,
    /// #         direction: Direction::Positive,
    /// #     },
    /// # )));
    /// mesh.assert_art(
    ///     Axis::Z, "
    ///     │    -5 -4 -3 -2 -1  0  1  2  3  4  5
    ///     │ -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -2  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -1  ·  ·  ·  ·  ◆━━━━━◆  ·  ·  ·  ·
    ///     │  0 ┈┈┈┈┈┈┈┈┈┈┈┈┈┃┈┈┊┈┈┃┈┈┈┈┈┈┈┈┈┈┈┈┈
    ///     │  1  ·  ·  ·  ·  ◆━━━━━◆  ·  ·  ·  ·
    ///     │  2  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │  3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │  4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     "
    /// );
    ///
    /// let quad = mesh.quads.len() - 1;
    ///
    /// mesh.translate(quad, vec3(2.0, 4.0, 0.0)).expect("Failed to translate.");
    ///
    /// mesh.assert_art(
    ///     Axis::Z,
    ///     "
    ///     │    -5 -4 -3 -2 -1  0  1  2  3  4  5
    ///     │ -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -2  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │ -1  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │  0 ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┊┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈
    ///     │  1  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │  2  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
    ///     │  3  ·  ·  ·  ·  ·  ┊  ◆━━━━━◆  ·  ·
    ///     │  4  ·  ·  ·  ·  ·  ┊  ┃  ·  ┃  ·  ·
    ///     │  5  ·  ·  ·  ·  ·  ┊  ◆━━━━━◆  ·  ·
    ///     "
    /// );
    /// ```
    pub fn translate(&mut self, quad_index: usize, translate: Vector3<f64>) -> QuadResult<()> {
        let (a, b, c, d) = self.get_quad_tuple(quad_index)?;
        *(self.get_position_mut(a))? += translate;
        *(self.get_position_mut(b))? += translate;
        *(self.get_position_mut(c))? += translate;
        *(self.get_position_mut(d))? += translate;
        Ok(())
    }
}

impl Default for QuadMesh {
    fn default() -> Self {
        Self::new()
    }
}

/// A trait to draw some piece of geometry using text art. See <https://gregtatum.com/writing/2020/ascii-physics-system/>
pub trait TextArt {
    /// Given a geometry type and an axis, draw it using text and an orthogonal
    /// projection.
    fn as_text_art(&self, axis: Axis) -> String;

    /// Assert that the geometry matches some kind of art. Ignores leading and trailing
    /// whitespace.
    fn assert_art(&self, axis: Axis, expect: &str);
}

fn get_grid(width: usize, height: usize) -> Vec<String> {
    let mut lines = vec![];
    let iter = -(width as i32)..(width as i32 + 1);
    let iter_rev = -(height as i32)..(height as i32 + 1);
    {
        let mut s = String::from("│    ");
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
        let mut s = String::from("│ ");
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
    /// Draw a [QuadMesh] using text art. See <https://gregtatum.com/writing/2020/ascii-physics-system/>
    fn as_text_art(&self, axis: Axis) -> String {
        let x_margin = 6;
        let y_margin = 1;
        let col_size = 3;
        let half_w: usize = 5;
        let half_h: usize = 5;
        let mut lines = get_grid(half_w, half_h);
        for quad in self.iter_quads() {
            let (a, b, c, d) = self.get_positions(quad);

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

    fn assert_art(&self, axis: Axis, expect: &str) {
        let actual = self.as_text_art(axis);
        let mut actual_iter = actual.lines();
        let mut expect_iter = expect.lines().peekable();
        // Skip leading whitespace
        skip_whitespace_lines(&mut expect_iter);

        let report_is_different = || {
            let mut actual_iter = actual.lines();
            let mut expect_iter = expect.lines().peekable();
            let mut msg = String::from("Art does not match.\n");
            msg.push_str("┌─────────────────────────────────────\n");
            msg.push_str("│ Expected:\n");
            msg.push_str("├─────────────────────────────────────\n");
            for line in expect.lines() {
                let line = line.trim();
                if !line.is_empty() {
                    msg.push_str(line);
                    msg.push('\n');
                }
            }
            msg.push_str("├─────────────────────────────────────\n");
            msg.push_str("│ Actual:\n");
            msg.push_str("├─────────────────────────────────────\n");
            for line in actual.lines() {
                let line = line.trim();
                if !line.is_empty() {
                    msg.push_str(line);
                    msg.push('\n');
                }
            }
            msg.push_str("└─────────────────────────────────────\n");
            panic!("{}", msg);
        };

        let is_different = false;
        loop {
            if let Some(actual_line) = actual_iter.next() {
                match expect_iter.next() {
                    Some(expect_line) => {
                        if actual_line.trim() != expect_line.trim() {
                            report_is_different();
                        }
                    }
                    None => report_is_different(),
                }
            } else {
                break;
            }
        }
        skip_whitespace_lines(&mut expect_iter);
        if expect_iter.peek().is_some() {
            report_is_different();
        }
    }
}

fn skip_whitespace_lines(lines: &mut std::iter::Peekable<std::str::Lines>) {
    loop {
        if let Some(line) = lines.peek() {
            if line.trim().is_empty() {
                lines.next();
                continue;
            }
        }
        return;
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_create_quad_from_size() {
        let mut mesh = QuadMesh::new();
        mesh.create_single_quad(SingleQuadOptions::FromSize((
            vec2(2.0, 4.0),
            Facing {
                axis: Axis::Z,
                direction: Direction::Positive,
            },
        )));

        mesh.assert_art(
            Axis::Z,
            "
            │    -5 -4 -3 -2 -1  0  1  2  3  4  5
            │ -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
            │ -4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
            │ -3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
            │ -2  ·  ·  ·  ·  ◆━━━━━◆  ·  ·  ·  ·
            │ -1  ·  ·  ·  ·  ┃  ┊  ┃  ·  ·  ·  ·
            │  0 ┈┈┈┈┈┈┈┈┈┈┈┈┈┃┈┈┊┈┈┃┈┈┈┈┈┈┈┈┈┈┈┈┈
            │  1  ·  ·  ·  ·  ┃  ┊  ┃  ·  ·  ·  ·
            │  2  ·  ·  ·  ·  ◆━━━━━◆  ·  ·  ·  ·
            │  3  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
            │  4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
            │  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
            ",
        );
    }

    #[test]
    fn test_create_quad_positions() {
        let mut mesh = QuadMesh::new();
        mesh.create_single_quad(SingleQuadOptions::FromPositions((
            vec3(-4.0, 0.0, -3.0),
            vec3(-4.0, 0.0, 3.0),
            vec3(1.0, 0.0, 3.0),
            vec3(1.0, 0.0, -3.0),
        )));

        mesh.assert_art(
            Axis::Y,
            "
            │    -5 -4 -3 -2 -1  0  1  2  3  4  5
            │ -5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
            │ -4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
            │ -3  ·  ◆━━━━━━━━━━━━━━◆  ·  ·  ·  ·
            │ -2  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
            │ -1  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
            │  0 ┈┈┈┈┃┈┈┈┈┈┈┈┈┈┈┈┊┈┈┃┈┈┈┈┈┈┈┈┈┈┈┈┈
            │  1  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
            │  2  ·  ┃  ·  ·  ·  ┊  ┃  ·  ·  ·  ·
            │  3  ·  ◆━━━━━━━━━━━━━━◆  ·  ·  ·  ·
            │  4  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
            │  5  ·  ·  ·  ·  ·  ┊  ·  ·  ·  ·  ·
            ",
        );
    }
}
