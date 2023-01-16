use std::num::ParseIntError;

pub fn u8_from_hex_slice(hex_slice: &str) -> Result<u8, ParseIntError> {
    u8::from_str_radix(hex_slice, 16)
}

#[test]
fn test_u8_from_hex_slice() {
    assert_eq!(u8_from_hex_slice("A"), Ok(10));
    assert_eq!(u8_from_hex_slice("0A"), Ok(10));
    assert!(u8_from_hex_slice("").is_err());
    assert!(u8_from_hex_slice("x").is_err());
    assert!(u8_from_hex_slice("AAA").is_err());
}

pub fn u16_from_hex_slice(hex_slice: &str) -> Result<u16, ParseIntError> {
    u16::from_str_radix(hex_slice, 16)
}

#[test]
fn test_u16_from_hex_slice() {
    assert_eq!(u16_from_hex_slice("A"), Ok(10));
    assert_eq!(u16_from_hex_slice("0A"), Ok(10));
    assert_eq!(u16_from_hex_slice("AAA"), Ok(2730));
    assert!(u16_from_hex_slice("").is_err());
    assert!(u16_from_hex_slice("x").is_err());
}

pub fn u32_from_hex_slice(hex_slice: &str) -> Result<u32, ParseIntError> {
    u32::from_str_radix(hex_slice, 16)
}

#[test]
fn test_u32_from_hex_slice() {
    assert_eq!(u32_from_hex_slice("A"), Ok(10));
    assert_eq!(u32_from_hex_slice("0A"), Ok(10));
    assert_eq!(u32_from_hex_slice("AAA"), Ok(2730));
    assert!(u32_from_hex_slice("").is_err());
    assert!(u32_from_hex_slice("x").is_err());
}
