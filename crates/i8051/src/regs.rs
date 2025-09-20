use std::cmp::PartialEq;
use std::fmt;
use std::ops::{
    Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not, Shl, Shr,
    Sub, SubAssign,
};

use bytemuck::TransparentWrapper;

pub trait U16Equivalent: Copy + Clone {
    fn to_u16(self) -> u16;
    fn from_u16(value: u16) -> Self;
    fn to_u8(self) -> Reg8 {
        Reg8(self.to_u16() as u8)
    }
    fn sext(self) -> RegI16 {
        RegI16(self.to_u16() as i8 as i16)
    }
}

#[derive(Copy, Clone, TransparentWrapper)]
#[repr(transparent)]
pub struct Reg8(pub u8);

impl PartialOrd for Reg8 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.0.cmp(&other.0))
    }
}

impl Ord for Reg8 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl From<u8> for Reg8 {
    fn from(value: u8) -> Self {
        Self(value)
    }
}

impl U16Equivalent for Reg8 {
    fn to_u16(self) -> u16 {
        self.0 as u16
    }
    fn from_u16(value: u16) -> Self {
        Self(value as u8)
    }
}

#[derive(Copy, Clone, TransparentWrapper)]
#[repr(transparent)]
pub struct RegI16(pub i16);

impl U16Equivalent for RegI16 {
    fn to_u16(self) -> u16 {
        self.0 as u16
    }
    fn from_u16(value: u16) -> Self {
        Self(value as i16)
    }
}

#[derive(Copy, Clone, TransparentWrapper)]
#[repr(transparent)]
pub struct Reg16(pub u16);

impl U16Equivalent for Reg16 {
    fn to_u16(self) -> u16 {
        self.0
    }
    fn from_u16(value: u16) -> Self {
        Self(value)
    }
}

impl U16Equivalent for u16 {
    fn to_u16(self) -> u16 {
        self
    }
    fn from_u16(value: u16) -> Self {
        value
    }
}

impl U16Equivalent for u8 {
    fn to_u16(self) -> u16 {
        self as u16
    }
    fn from_u16(value: u16) -> Self {
        value as u8
    }
}

impl U16Equivalent for i32 {
    fn to_u16(self) -> u16 {
        self as u16
    }
    fn from_u16(value: u16) -> Self {
        value as i32
    }
}

macro_rules! derive_ops {
    ($type:ident) => {
        impl<T> Add<T> for $type
        where
            T: U16Equivalent,
        {
            type Output = $type;
            fn add(self, other: T) -> Self::Output {
                $type::from_u16(self.to_u16().wrapping_add(other.to_u16()))
            }
        }
        impl<T> AddAssign<T> for $type
        where
            T: U16Equivalent,
        {
            fn add_assign(&mut self, other: T) {
                *self = $type::from_u16(self.to_u16().wrapping_add(other.to_u16()))
            }
        }
        impl<T> Sub<T> for $type
        where
            T: U16Equivalent,
        {
            type Output = $type;
            fn sub(self, other: T) -> Self::Output {
                $type::from_u16(self.to_u16().wrapping_sub(other.to_u16()))
            }
        }
        impl<T> SubAssign<T> for $type
        where
            T: U16Equivalent,
        {
            fn sub_assign(&mut self, other: T) {
                *self = $type::from_u16(self.to_u16().wrapping_sub(other.to_u16()))
            }
        }
        impl<T> Shl<T> for $type
        where
            T: U16Equivalent,
        {
            type Output = Reg16;
            fn shl(self, other: T) -> Self::Output {
                Reg16(self.to_u16() << other.to_u16())
            }
        }
        impl<T> Shr<T> for $type
        where
            T: U16Equivalent,
        {
            type Output = $type;
            fn shr(self, other: T) -> Self::Output {
                $type::from_u16(self.to_u16() >> other.to_u16())
            }
        }
        impl<T> BitAnd<T> for $type
        where
            T: U16Equivalent,
        {
            type Output = $type;
            fn bitand(self, other: T) -> Self::Output {
                $type::from_u16(self.to_u16() & other.to_u16())
            }
        }
        impl<T> BitAndAssign<T> for $type
        where
            T: U16Equivalent,
        {
            fn bitand_assign(&mut self, other: T) {
                *self = $type::from_u16(self.to_u16() & other.to_u16())
            }
        }
        impl<T> BitOr<T> for $type
        where
            T: U16Equivalent,
        {
            type Output = $type;
            fn bitor(self, other: T) -> Self::Output {
                $type::from_u16(self.to_u16() | other.to_u16())
            }
        }
        impl<T> BitOrAssign<T> for $type
        where
            T: U16Equivalent,
        {
            fn bitor_assign(&mut self, other: T) {
                *self = $type::from_u16(self.to_u16() | other.to_u16())
            }
        }
        impl<T> BitXor<T> for $type
        where
            T: U16Equivalent,
        {
            type Output = $type;
            fn bitxor(self, other: T) -> Self::Output {
                $type::from_u16(self.to_u16() ^ other.to_u16())
            }
        }
        impl<T> BitXorAssign<T> for $type
        where
            T: U16Equivalent,
        {
            fn bitxor_assign(&mut self, other: T) {
                *self = $type::from_u16(self.to_u16() ^ other.to_u16())
            }
        }
        impl<T> PartialEq<T> for $type
        where
            T: U16Equivalent,
        {
            fn eq(&self, other: &T) -> bool {
                self.to_u16() == other.to_u16()
            }
        }
        impl Eq for $type {}
        impl Not for $type {
            type Output = $type;
            fn not(self) -> Self::Output {
                $type::from_u16(!self.to_u16())
            }
        }
        impl fmt::Display for $type {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }
        impl fmt::UpperHex for $type {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }
    };
}

derive_ops!(Reg8);
derive_ops!(Reg16);
derive_ops!(RegI16);
