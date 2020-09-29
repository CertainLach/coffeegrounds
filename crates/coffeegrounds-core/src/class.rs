use std::fmt::Display;

use nom::number::streaming::be_u32;

use crate::{ClassParse, SerResult};

use super::{constant_pool::List, version::Version};

#[derive(Debug)]
pub struct ClassFile<'i> {
	magic: u32,
	version: Version,

	constant_pool: List<'i>,
	// access_flags: u2,
	// this_class: u2,
	// super_class: u2,

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

		Ok((
			input,
			ClassFile {
				magic,
				version,
				constant_pool,
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
