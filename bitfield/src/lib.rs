// Crates that have the "proc-macro" crate type are only allowed to export
// procedural macros. So we cannot have one crate that defines procedural macros
// alongside other types of public APIs like traits and structs.
//
// For this project we are going to need a #[bitfield] macro but also a trait
// and some structs. We solve this by defining the trait and structs in this
// crate, defining the attribute macro in a separate bitfield-impl crate, and
// then re-exporting the macro from this crate so that users only have one crate
// that they need to import.
//
// From the perspective of a user of this crate, they get all the necessary APIs
// (macro, trait, struct) through the one bitfield crate.
pub use bitfield_impl::{bitfield, BitfieldSpecifier};

use bitfield_impl::define_and_specify_types;

pub mod checks;

pub trait Specifier {
    const BITS: usize;
    type Access;
    type Raw;

    // Optimize since https://github.com/occar421/my-proc-macro-workshop/blob/ef287ccb5e445989f29ceec55914839c6ee5140b/bitfield/impl/src/lib.rs
    fn to_access(raw: Self::Raw) -> Self::Access;
}

define_and_specify_types!();

impl Specifier for bool {
    const BITS: usize = 1;
    type Access = bool;
    type Raw = u8;
    
    fn to_access(raw: Self::Raw) -> Self::Access {
        raw != 0
    }
}

