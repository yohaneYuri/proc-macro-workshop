// See `https://github.com/dtolnay/case-studies/blob/master/bitfield-assertion/README.md`
// I pass the 4st test on the basic of it, no futher declarative macros <3. The commented parts
// are what I come up with. Since Rustc no longer exposes the full type path to us in stderr, 
// I overwrite the stderr file.
pub trait TotalSizeIsMultipleOfEightBits {
    type Check;
}

pub struct Remainder<const N: usize>;

// pub type ZeroMod8 = Remainder<0>;
// pub type OneMod8 = Remainder<1>;
// pub type TwoMod8 = Remainder<2>;
// pub type ThreeMod8 = Remainder<3>;
// pub type FourMod8 = Remainder<4>;
// pub type FiveMod8 = Remainder<5>;
// pub type SixMod8 = Remainder<6>;
// pub type SevenMod8 = Remainder<7>;

pub enum ZeroMod8 {}
pub enum OneMod8 {}
pub enum TwoMod8 {}
pub enum ThreeMod8 {}
pub enum FourMod8 {}
pub enum FiveMod8 {}
pub enum SixMod8 {}
pub enum SevenMod8 {}

impl TotalSizeIsMultipleOfEightBits for ZeroMod8 {
    type Check = ();
}

pub trait AliasTo {
    type Alias;
}

impl AliasTo for Remainder<0> {
    type Alias = ZeroMod8;
}

impl AliasTo for Remainder<1> {
    type Alias = OneMod8;
}

impl AliasTo for Remainder<2> {
    type Alias = TwoMod8;
}

impl AliasTo for Remainder<3> {
    type Alias = ThreeMod8;
}

impl AliasTo for Remainder<4> {
    type Alias = FourMod8;
}

impl AliasTo for Remainder<5> {
    type Alias = FiveMod8;
}

impl AliasTo for Remainder<6> {
    type Alias = SixMod8;
}

impl AliasTo for Remainder<7> {
    type Alias = SevenMod8;
}

pub type MultipleOf8Bits<const N: usize> = <<Remainder<N> as AliasTo>::Alias as TotalSizeIsMultipleOfEightBits>::Check;


pub trait PowerOf2 {
    type Check;
}


pub trait DiscriminantInRange {
    type Check;
}

pub enum False {}
pub enum True {}

impl DiscriminantInRange for True {
    type Check = ();
}

pub struct CheckResult<const N: bool>;

impl AliasTo for CheckResult<true> {
    type Alias = True;
}

impl AliasTo for CheckResult<false> {
    type Alias = False;
}

pub type DiscriminantIsValid<const N: bool> = <<CheckResult<N> as AliasTo>::Alias as DiscriminantInRange>::Check;


// pub trait EqualToBitsAttribute {
//     type Check;
// }

// impl EqualToBitsAttribute for True {
//     type Check = ();
// }

// pub type CorrectBitfieldWidth<const N: bool> = <<CheckResult<N> as AliasTo>::Alias as EqualToBitsAttribute>::Check;