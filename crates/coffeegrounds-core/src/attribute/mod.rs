use std::{borrow::Cow, fmt::Display, io::Write, ops::Deref};

use byteorder::{BigEndian, WriteBytesExt};
use nom::{
	bytes::streaming::take,
	number::streaming::{be_u16, be_u32},
};

use crate::{
	constant_pool::{self, PrintWithCp},
	version::Version,
	ClassParse,
};

use self::runtime_visible_annotations::RuntimeVisibleAnnotations;

pub mod code;
pub mod runtime_visible_annotations;

use code::Code;

pub trait Attribute<'i>: ClassParse<'i> {
	const NAME: &'static [u8];
}

#[derive(Debug)]
pub struct ParsedAttribute<'i, A: Attribute<'i>> {
	attribute: A,
	rest_data: Cow<'i, [u8]>,
}
impl<'i, A: Attribute<'i> + PrintWithCp> PrintWithCp for ParsedAttribute<'i, A> {
	fn print(
		&self,
		cp: &constant_pool::List<'_>,
		f: &mut std::fmt::Formatter<'_>,
	) -> std::fmt::Result {
		self.attribute.print(cp, f)?;
		if !self.rest_data.is_empty() {
			write!(f, "raw {:?}", &self.rest_data as &[u8])?;
		}
		Ok(())
	}

	fn has_comment(&self) -> bool {
		todo!()
	}
}
impl<'i, A: Attribute<'i>> Deref for ParsedAttribute<'i, A> {
	type Target = A;

	fn deref(&self) -> &A {
		&self.attribute
	}
}

#[derive(Debug)]
pub struct RawAttribute<'i> {
	version: Version,
	name_index: u16,
	data: Cow<'i, [u8]>,
}

impl<'i> RawAttribute<'i> {
	fn parse_as<A: Attribute<'i>>(
		&'i self,
		cp: &'i constant_pool::List<'i>,
		version: Version,
	) -> Option<ParsedAttribute<'i, A>> {
		match cp.get_str(self.name_index as usize) {
			Some(v) if v as &[u8] == A::NAME => {
				if let Ok((rest_data, attribute)) = A::parse(&self.data, version) {
					Some(ParsedAttribute {
						attribute,
						rest_data: Cow::Borrowed(rest_data),
					})
				} else {
					None
				}
			}
			_ => None,
		}
	}
}

impl<'i> ClassParse<'i> for RawAttribute<'i> {
	fn parse(input: &'i [u8], version: crate::version::Version) -> nom::IResult<&'i [u8], Self> {
		let (input, name_index) = be_u16(input)?;
		let (input, data_len) = be_u32(input)?;

		let (input, data) = take(data_len)(input)?;

		Ok((
			input,
			RawAttribute {
				version,
				name_index,
				data: Cow::Borrowed(data),
			},
		))
	}

	fn serialize(
		&self,
		output: &mut Vec<u8>,
		version: crate::version::Version,
	) -> crate::SerResult {
		assert_eq!(self.version, version);

		output.write_u16::<BigEndian>(self.name_index)?;
		output.write_u32::<BigEndian>(self.data.len() as u32)?;

		output.write_all(&self.data)?;
		Ok(())
	}
}

impl Display for RawAttribute<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "#{}", self.name_index)
	}
}

impl PrintWithCp for RawAttribute<'_> {
	fn print(
		&self,
		cp: &crate::constant_pool::List<'_>,
		f: &mut std::fmt::Formatter<'_>,
	) -> std::fmt::Result {
		write!(f, "#{} // ", self.name_index)?;
		cp.get(self.name_index as usize).print(cp, f)?;
		writeln!(f)?;
		if let Some(runtime) = self.parse_as::<RuntimeVisibleAnnotations>(cp, self.version) {
			runtime.print(cp, f)?;
		} else if let Some(code) = self.parse_as::<Code>(cp, self.version) {
			code.print(cp, f)?;
		} else {
			write!(f, "raw {:?}", self.data)?;
		}
		Ok(())
	}

	fn has_comment(&self) -> bool {
		todo!()
	}
}

#[derive(Debug)]
pub struct List<'i>(Vec<RawAttribute<'i>>);

impl<'i> Deref for List<'i> {
	type Target = Vec<RawAttribute<'i>>;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl<'i> ClassParse<'i> for List<'i> {
	fn parse(input: &'i [u8], ver: crate::version::Version) -> nom::IResult<&'i [u8], Self> {
		let (input, count) = be_u16(input)?;
		let mut attributes = Vec::with_capacity(count as usize);
		let mut input = input;
		for _ in 0..count {
			let (new_input, attribute) = RawAttribute::parse(input, ver)?;
			input = new_input;
			attributes.push(attribute);
		}
		Ok((input, List(attributes)))
	}

	fn serialize(&self, output: &mut Vec<u8>, ver: crate::version::Version) -> crate::SerResult {
		output.write_u16::<BigEndian>(self.0.len() as u16)?;
		for attr in self.0.iter() {
			attr.serialize(output, ver)?;
		}
		Ok(())
	}
}

impl PrintWithCp for List<'_> {
	fn print(
		&self,
		cp: &crate::constant_pool::List<'_>,
		f: &mut std::fmt::Formatter<'_>,
	) -> std::fmt::Result {
		for (i, attr) in self.0.iter().enumerate() {
			if i != 0 {
				writeln!(f)?;
			}
			write!(f, "attribute ")?;
			attr.print(cp, f)?;
		}
		Ok(())
	}

	fn has_comment(&self) -> bool {
		todo!()
	}
}
