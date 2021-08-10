#![allow(clippy::new_without_default)]

pub mod quads;

#[diplomat::bridge]
pub mod ffi {
    use super::quads;

    #[diplomat::opaque]
    pub struct Mesh(pub quads::QuadMesh);

    impl Mesh {
        pub fn new() -> Box<Mesh> {
            Box::new(Mesh(quads::QuadMesh::new()))
        }
    }
}
