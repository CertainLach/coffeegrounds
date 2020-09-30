use nom::number::streaming::be_u16;

use crate::{
	access::AccessFlags, access::FieldAccessFlags, attribute, constant_pool::PrintWithCp,
	ClassParse,
};

#[derive(Debug)]
pub struct Field<'i> {
	access: AccessFlags<FieldAccessFlags>,
	name_index: u16,
	descriptor_index: u16,
	attributes: attribute::List<'i>,
}

impl<'i> ClassParse<'i> for Field<'i> {
	fn parse(input: &'i [u8], ver: crate::version::Version) -> nom::IResult<&'i [u8], Self> {
		let (input, access) = AccessFlags::parse(input, ver)?;
		let (input, name_index) = be_u16(input)?;
		let (input, descriptor_index) = be_u16(input)?;
		let (input, attributes) = attribute::List::parse(input, ver)?;

		Ok((
			input,
			Field {
				access,
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

impl PrintWithCp for Field<'_> {
	fn print(
		&self,
		cp: &crate::constant_pool::List<'_>,
		f: &mut std::fmt::Formatter<'_>,
	) -> std::fmt::Result {
		write!(
			f,
			"field {} #{} #{} // ",
			self.access, self.name_index, self.descriptor_index
		)?;

		cp.get(self.descriptor_index as usize).print(cp, f)?;
		write!(f, " ")?;
		cp.get(self.name_index as usize).print(cp, f)?;
		if self.attributes.len() != 0 {
			writeln!(f)?;
			self.attributes.print(cp, f)?;
		}

		Ok(())
	}

	fn has_comment(&self) -> bool {
		false
	}
}
