use std::fmt::Display;

use byteorder::{BigEndian, WriteBytesExt};
use enumflags2::BitFlags;
use nom::{
	error::ErrorKind,
	number::streaming::{be_u16, be_u32},
};

use crate::{ClassParse, SerResult};

use super::{constant_pool::List, version::Version};

#[repr(u16)]
#[derive(Clone, Copy, BitFlags, Debug)]
pub enum ValidAccessFlags {
	Public = 0x0001,
	Final = 0x0010,
	Super = 0x0020,
	Interface = 0x0200,
	Abstract = 0x0400,
	Synthetic = 0x1000,
	Annotation = 0x2000,
	Enum = 0x4000,
}

impl Display for ValidAccessFlags {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use ValidAccessFlags::*;
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

#[derive(Debug)]
pub struct AccessFlags {
	valid: BitFlags<ValidAccessFlags>,
	invalid: u16,
}
impl ClassParse<'_> for AccessFlags {
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

impl Display for AccessFlags {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for flag in self.valid.iter() {
			write!(f, "{} + ", flag)?;
		}
		write!(f, "{}", self.invalid)?;
		Ok(())
	}
}

#[derive(Debug)]
pub struct ClassFile<'i> {
	magic: u32,
	version: Version,

	constant_pool: List<'i>,
	access_flags: AccessFlags,

	// interfaces: Vec<Interface>,
	// fields: Vec<Field>,
	// methods: Vec<Method>,
	// attributes: Vec<Attribute>
}
impl<'i> ClassFile<'i> {
	pub fn parse(input: &'i [u8]) -> nom::IResult<&'i [u8], Self> {
		let (input, magic) = be_u32(input)?;
		let (input, version) = Version::parse(input)?;
		let (input, constant_pool) = List::parse(input, version)?;
		let (input, access_flags) = AccessFlags::parse(input, version)?;

		Ok((
			input,
			ClassFile {
				magic,
				version,
				constant_pool,
				access_flags,

			},
		))
	}

	pub fn serialize(&self, output: &mut Vec<u8>) -> SerResult {
		todo!()
	}
}
impl Display for ClassFile<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		writeln!(f, "class {} version {}", self.magic, self.version)?;
		write!(f, "{}", self.constant_pool)?;

		Ok(())
	}
}
