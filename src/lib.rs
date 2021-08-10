pub mod quads;

#[diplomat::bridge]
pub mod ffi {
    pub struct MyFFIType {
        pub a: i32,
    }

    impl MyFFIType {
        pub fn new() -> MyFFIType {
            MyFFIType { a: 42 }
        }
    }
}
