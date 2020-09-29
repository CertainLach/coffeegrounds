use std::fmt::Display;

use byteorder::{BigEndian, WriteBytesExt};
use nom::{do_parse, number::streaming::be_u16, IResult};

use crate::SerResult;

#[derive(Clone, Copy)]
pub enum VersionFeature {
	/// General support in this library
	Supported,
	Preview,

	HalfSizedCodeFields,
	StackMap,
	Invokedynamic,
	RelaxAccessCheck,
	DynamicConstant,
}
impl VersionFeature {
	fn supported(&self, ver: Version) -> bool {
		use VersionFeature::*;
		match self {
			Supported => ver >= Version(45, 0) && ver <= Version(60, 0),
			Preview => ver.minor() == 65535,
			HalfSizedCodeFields => ver <= Version(45, 2),
			StackMap => ver >= Version(50, 0),
			Invokedynamic => ver >= Version(51, 0),
			RelaxAccessCheck => ver < Version(52, 0),
			DynamicConstant => ver >= Version(55, 0),
		}
	}
}

// Packing forced by default to fit into 4 bytes
#[repr(packed)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Version(u16, u16);

impl Display for Version {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if let Some(v) = self.major_name() {
			write!(f, "jdk{}", v)?;
		} else {
			write!(f, "unknown{}", self.major())?;
		}
		if self.minor() != 0 {
			write!(f, "-")?;
			if self.supports(VersionFeature::Preview) {
				write!(f, "preview")?;
			} else {
				write!(f, "{}", self.minor())?;
			}
		}
		Ok(())
	}
}

impl Version {
	pub fn major(self) -> u16 {
		self.0
	}
	pub fn minor(self) -> u16 {
		self.1
	}

	pub fn major_name(self) -> Option<&'static str> {
		Some(match self.major() {
			49 => "1.5",
			50 => "6",
			51 => "7",
			52 => "8",
			53 => "9",
			54 => "10",
			55 => "11",
			56 => "12",
			57 => "13",
			58 => "14",
			59 => "15",
			60 => "16",
			_ => return None,
		})
	}
	pub fn supports(self, feature: VersionFeature) -> bool {
		feature.supported(self)
	}
}

impl Version {
	pub fn parse<'i>(input: &'i [u8]) -> IResult<&'i [u8], Self> {
		do_parse!(
			input,
			minor: be_u16 >> major: be_u16 >> (Self(major, minor))
		)
	}

	pub fn serialize(&self, output: &mut Vec<u8>) -> SerResult {
		output.write_u16::<BigEndian>(self.1)?;
		output.write_u16::<BigEndian>(self.0)?;
		Ok(())
	}
}

#[test]
fn versions() {
	assert!(Version(1, 2) == Version(1, 2));
	assert!(Version(1, 2) < Version(1, 3));
	assert!(Version(1, 3) < Version(2, 1));
}
