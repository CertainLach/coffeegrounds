use std::fmt::Display;

use byteorder::{BigEndian, WriteBytesExt};
use enumflags2::{BitFlags, RawBitFlags};
use nom::number::streaming::be_u16;

use crate::{version::Version, ClassParse, SerResult};

#[repr(u16)]
#[derive(Clone, Copy, BitFlags, Debug)]
pub enum ClassAccessFlags {
	Public = 0x0001,
	Final = 0x0010,
	Super = 0x0020,
	Interface = 0x0200,
	Abstract = 0x0400,
	Synthetic = 0x1000,
	Annotation = 0x2000,
	Enum = 0x4000,
}

impl Display for ClassAccessFlags {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use ClassAccessFlags::*;
		write!(
			f,
			"{}",
			match self {
				Public => "public",
				Final => "final",
				Super => "super",
				Interface => "interface",
				Abstract => "abstract",
				Synthetic => "synthetic",
				Annotation => "annotation",
				Enum => "enum",
			}
		)
	}
}
#[repr(u16)]
#[derive(Clone, Copy, BitFlags, Debug)]
pub enum FieldAccessFlags {
	Public = 0x0001,
	Private = 0x0002,
	Protected = 0x0004,
	Static = 0x0008,
	Final = 0x0010,
	Volatile = 0x0040,
	Transient = 0x0080,
	Synthetic = 0x1000,
	Enum = 0x4000,
}

impl Display for FieldAccessFlags {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use FieldAccessFlags::*;
		write!(
			f,
			"{}",
			match self {
				Public => "public",
				Private => "private",
				Protected => "protected",
				Static => "static",
				Final => "final",
				Volatile => "volatile",
				Transient => "transient",
				Synthetic => "synthetic",
				Enum => "enum",
			}
		)
	}
}

#[repr(u16)]
#[derive(Clone, Copy, BitFlags, Debug)]
pub enum MethodAccessFlags {
	Public = 0x0001,
	Private = 0x0002,
	Protected = 0x0004,
	Static = 0x0008,
	Final = 0x0010,
	Synchronized = 0x0020,
	Bridge = 0x0040,
	Varargs = 0x0080,
	Native = 0x0100,
	Abstract = 0x0400,
	Strictfp = 0x0800,
	Synthetic = 0x1000,
}

impl Display for MethodAccessFlags {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use MethodAccessFlags::*;
		write!(
			f,
			"{}",
			match self {
				Public => "public",
				Private => "private",
				Protected => "protected",
				Static => "static",
				Final => "final",
				Synchronized => "synchronized",
				Bridge => "bridge",
				Varargs => "varargs",
				Native => "native",
				Abstract => "abstract",
				Strictfp => "strictfp",
				Synthetic => "synthetic",
			}
		)
	}
}

#[derive(Debug)]
pub struct AccessFlags<F: RawBitFlags> {
	valid: BitFlags<F>,
	invalid: u16,
}
impl<F: RawBitFlags<Type = u16>> ClassParse<'_> for AccessFlags<F> {
	fn parse(input: &[u8], _ver: Version) -> nom::IResult<&[u8], Self> {
		let (input, access_flags) = be_u16(input)?;
		let flags = match BitFlags::from_bits(access_flags) {
			Ok(valid) => AccessFlags { valid, invalid: 0 },
			Err(e) => AccessFlags {
				valid: e.truncate(),
				invalid: e.invalid_bits(),
			},
		};
		Ok((input, flags))
	}

	fn serialize(&self, output: &mut Vec<u8>, _ver: Version) -> SerResult {
		let flags = BitFlags::bits(self.valid) | self.invalid;
		output.write_u16::<BigEndian>(flags)?;
		Ok(())
	}
}

impl<F: RawBitFlags + Display> Display for AccessFlags<F> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for flag in self.valid.iter() {
			write!(f, "{} + ", flag)?;
		}
		write!(f, "{}", self.invalid)?;
		Ok(())
	}
}
