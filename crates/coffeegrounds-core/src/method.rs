use nom::number::streaming::be_u16;

use crate::{
	access::AccessFlags, access::MethodAccessFlags, attribute, constant_pool::PrintWithCp,
	ClassParse,
};

#[derive(Debug)]
pub struct Method<'i> {
	access_flags: AccessFlags<MethodAccessFlags>,
	name_index: u16,
	descriptor_index: u16,
	attributes: attribute::List<'i>,
}

impl PrintWithCp for Method<'_> {
	fn print(
		&self,
		cp: &crate::constant_pool::List<'_>,
		f: &mut std::fmt::Formatter<'_>,
	) -> std::fmt::Result {
		write!(
			f,
			"method {} #{} #{} // ",
			self.access_flags, self.name_index, self.descriptor_index
		)?;
		cp.get(self.name_index as usize).print(cp, f)?;
		write!(f, " ")?;
		cp.get(self.descriptor_index as usize).print(cp, f)?;
		if !self.attributes.is_empty() {
			writeln!(f)?;
			self.attributes.print(cp, f)?;
		}
		Ok(())
	}

	fn has_comment(&self) -> bool {
		todo!()
	}
}

impl<'i> ClassParse<'i> for Method<'i> {
	fn parse(input: &'i [u8], ver: crate::version::Version) -> nom::IResult<&'i [u8], Self> {
		let (input, access_flags) = AccessFlags::parse(input, ver)?;
		let (input, name_index) = be_u16(input)?;
		let (input, descriptor_index) = be_u16(input)?;
		let (input, attributes) = attribute::List::parse(input, ver)?;
		Ok((
			input,
			Self {
				access_flags,
				name_index,
				descriptor_index,
				attributes,
			},
		))
	}

	fn serialize(&self, output: &mut Vec<u8>, ver: crate::version::Version) -> crate::SerResult {
		todo!()
	}
}
