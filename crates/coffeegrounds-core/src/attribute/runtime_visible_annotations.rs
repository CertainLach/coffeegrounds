use nom::{
	error::ErrorKind,
	number::streaming::{be_u16, be_u8},
};

use crate::{constant_pool::PrintWithCp, ClassParse};

use super::Attribute;

#[derive(Debug)]
pub enum ElementValue {
	Const(u8, u16),
	EnumConst {
		type_name_index: u16,
		const_name_index: u16,
	},
	ClassInfo(u16),
	Annotation(Box<Annotation>),
	Array(Vec<ElementValue>),
}
impl ClassParse<'_> for ElementValue {
	fn parse(input: &[u8], ver: crate::version::Version) -> nom::IResult<&[u8], Self> {
		let (mut input, tag) = be_u8(input)?;
		let value = match tag {
			b'B' | b'C' | b'D' | b'F' | b'I' | b'J' | b'S' | b'Z' | b's' => {
				let (new_input, value) = be_u16(input)?;
				input = new_input;
				Self::Const(tag, value)
			}
			b'e' => {
				let (new_input, type_name_index) = be_u16(input)?;
				let (new_input, const_name_index) = be_u16(new_input)?;
				input = new_input;
				Self::EnumConst {
					type_name_index,
					const_name_index,
				}
			}
			b'c' => {
				let (new_input, index) = be_u16(input)?;
				input = new_input;
				Self::ClassInfo(index)
			}
			b'@' => {
				let (new_input, annotation) = Annotation::parse(input, ver)?;
				input = new_input;
				Self::Annotation(Box::new(annotation))
			}
			b'[' => {
				let (new_input, num_values) = be_u16(input)?;
				input = new_input;
				let mut elements = Vec::with_capacity(num_values as usize);
				for _ in 0..num_values {
					let (new_input, value) = Self::parse(input, ver)?;
					input = new_input;
					elements.push(value);
				}
				Self::Array(elements)
			}
			_ => return Err(nom::Err::Failure((input, ErrorKind::Tag))),
		};
		Ok((input, value))
	}

	fn serialize(&self, output: &mut Vec<u8>, ver: crate::version::Version) -> crate::SerResult {
		todo!()
	}
}
impl PrintWithCp for ElementValue {
	fn print(
		&self,
		cp: &crate::constant_pool::List<'_>,
		f: &mut std::fmt::Formatter<'_>,
	) -> std::fmt::Result {
		match self {
			ElementValue::Const(tag, value) => {
				write!(f, "const {} #{} // ", tag, value)?;
				cp.get(*value as usize).print(cp, f)?;
			}
			ElementValue::EnumConst {
				type_name_index,
				const_name_index,
			} => {
				write!(
					f,
					"enumconst #{} #{} // ",
					type_name_index, const_name_index
				)?;
				cp.get(*type_name_index as usize).print(cp, f)?;
				write!(f, " ")?;
				cp.get(*const_name_index as usize).print(cp, f)?;
			}
			ElementValue::ClassInfo(cl) => {
				write!(f, "classinfo #{} // ", cl)?;
				cp.get(*cl as usize).print(cp, f)?;
			}
			ElementValue::Annotation(annotation) => {
				annotation.print(cp, f)?;
			}
			ElementValue::Array(els) => {
				writeln!(f, "array [")?;
				for el in els {
					el.print(cp, f)?;
					writeln!(f)?;
				}
				write!(f, "]")?;
			}
		};
		Ok(())
	}

	fn has_comment(&self) -> bool {
		todo!()
	}
}

#[derive(Debug)]
pub struct Annotation {
	type_index: u16,
	pairs: Vec<(u16, ElementValue)>,
}

impl PrintWithCp for Annotation {
	fn print(
		&self,
		cp: &crate::constant_pool::List<'_>,
		f: &mut std::fmt::Formatter<'_>,
	) -> std::fmt::Result {
		write!(f, "annotation #{} // ", self.type_index)?;
		cp.get(self.type_index as usize).print(cp, f)?;
		for (k, v) in self.pairs.iter() {
			write!(f, "el #{} {{ // ", k)?;
			cp.get(*k as usize).print(cp, f)?;
			v.print(cp, f)?;
			write!(f, "}}")?;
		}
		Ok(())
	}

	fn has_comment(&self) -> bool {
		todo!()
	}
}

impl ClassParse<'_> for Annotation {
	fn parse(input: &[u8], ver: crate::version::Version) -> nom::IResult<&[u8], Self> {
		let (input, type_index) = be_u16(input)?;
		let (input, count) = be_u16(input)?;
		let mut pairs = Vec::with_capacity(count as usize);
		let mut input = input;
		for _ in 0..count {
			let (new_input, element_name) = be_u16(input)?;
			let (new_input, element_value) = ElementValue::parse(new_input, ver)?;
			input = new_input;
			pairs.push((element_name, element_value))
		}
		Ok((input, Annotation { type_index, pairs }))
	}

	fn serialize(&self, output: &mut Vec<u8>, ver: crate::version::Version) -> crate::SerResult {
		todo!()
	}
}

#[derive(Debug)]
pub struct RuntimeVisibleAnnotations {
	annotations: Vec<Annotation>,
}

impl ClassParse<'_> for RuntimeVisibleAnnotations {
	fn parse(input: &[u8], ver: crate::version::Version) -> nom::IResult<&[u8], Self> {
		let (input, annotation_count) = be_u16(input)?;
		let mut annotations = Vec::with_capacity(annotation_count as usize);
		let mut input = input;
		for _ in 0..annotation_count {
			let (new_input, annotation) = Annotation::parse(input, ver)?;
			input = new_input;
			annotations.push(annotation);
		}
		Ok((input, Self { annotations }))
	}

	fn serialize(&self, output: &mut Vec<u8>, ver: crate::version::Version) -> crate::SerResult {
		todo!()
	}
}

impl PrintWithCp for RuntimeVisibleAnnotations {
	fn print(
		&self,
		cp: &crate::constant_pool::List<'_>,
		f: &mut std::fmt::Formatter<'_>,
	) -> std::fmt::Result {
		writeln!(f, "runtimevisibleannotations")?;
		for (i, a) in self.annotations.iter().enumerate() {
			if i != 0 {
				writeln!(f)?;
			}
			a.print(cp, f)?;
		}
		Ok(())
	}

	fn has_comment(&self) -> bool {
		todo!()
	}
}

impl Attribute<'_> for RuntimeVisibleAnnotations {
	const NAME: &'static [u8] = b"RuntimeVisibleAnnotations";
}
