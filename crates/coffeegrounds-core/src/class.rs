use std::fmt::Display;

use nom::{
	error::ErrorKind,
	number::streaming::{be_u16, be_u32},
};

use super::{constant_pool::List, version::Version};
use crate::{
	access::AccessFlags, access::ClassAccessFlags, attribute, constant_pool::PrintWithCp,
	field::Field, method::Method, ClassParse, SerResult,
};

#[derive(Debug)]
pub struct ClassFile<'i> {
	version: Version,

	constant_pool: List<'i>,
	access_flags: AccessFlags<ClassAccessFlags>,

	this_class: u16,
	super_class: u16,

	interfaces: Vec<u16>,

	fields: Vec<Field<'i>>,
	methods: Vec<Method<'i>>,
	attributes: attribute::List<'i>,
}
impl<'i> ClassFile<'i> {
	pub fn parse(input: &'i [u8]) -> nom::IResult<&'i [u8], Self> {
		let (input, magic) = be_u32(input)?;
		if magic != 0xcafebabe {
			return Err(nom::Err::Failure((input, ErrorKind::Tag)));
		}
		let (input, version) = Version::parse(input)?;
		let (input, constant_pool) = List::parse(input, version)?;
		let (input, access_flags) = AccessFlags::parse(input, version)?;

		let (input, this_class) = be_u16(input)?;
		let (input, super_class) = be_u16(input)?;

		let (input, interface_count) = be_u16(input)?;
		let mut interfaces = Vec::with_capacity(interface_count as usize);
		let mut input = input;
		for _ in 0..interface_count {
			let (new_input, iface) = be_u16(input)?;
			input = new_input;
			interfaces.push(iface);
		}

		let (input, field_count) = be_u16(input)?;
		let mut fields = Vec::with_capacity(field_count as usize);
		let mut input = input;
		for _ in 0..field_count {
			let (new_input, field) = Field::parse(input, version)?;
			input = new_input;
			fields.push(field);
		}

		let (input, method_count) = be_u16(input)?;
		let mut methods = Vec::with_capacity(method_count as usize);
		let mut input = input;
		for _ in 0..method_count {
			let (new_input, method) = Method::parse(input, version)?;
			input = new_input;
			methods.push(method);
		}

		let (input, attributes) = attribute::List::parse(input, version)?;

		Ok((
			input,
			ClassFile {
				version,
				constant_pool,
				access_flags,

				this_class,
				super_class,

				interfaces,
				fields,
				methods,

				attributes,
			},
		))
	}

	pub fn serialize(&self, output: &mut Vec<u8>) -> SerResult {
		todo!()
	}
}
impl Display for ClassFile<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.constant_pool)?;

		write!(
			f,
			"class {} #{} #{} for {} // ",
			self.access_flags, self.this_class, self.super_class, self.version,
		)?;
		self.constant_pool
			.get(self.this_class as usize)
			.print(&self.constant_pool, f)?;
		write!(f, " extends ")?;
		self.constant_pool
			.get(self.super_class as usize)
			.print(&self.constant_pool, f)?;
		writeln!(f)?;

		if !self.interfaces.is_empty() {
			write!(
				f,
				"implements {} // ",
				self.interfaces
					.iter()
					.map(|i| format!("#{}", i))
					.collect::<Vec<_>>()
					.join(", ")
			)?;
			for (i, iface) in self.interfaces.iter().enumerate() {
				if i != 0 {
					write!(f, ", ")?;
				}
				self.constant_pool
					.get(*iface as usize)
					.print(&self.constant_pool, f)?;
			}
			writeln!(f)?;
		}

		if !self.fields.is_empty() {
			for (i, field) in self.fields.iter().enumerate() {
				if i != 0 {
					writeln!(f)?;
				}
				field.print(&self.constant_pool, f)?;
			}
			writeln!(f)?;
		}

		if !self.methods.is_empty() {
			for (i, method) in self.methods.iter().enumerate() {
				if i != 0 {
					writeln!(f)?;
				}
				method.print(&self.constant_pool, f)?;
			}
			writeln!(f)?;
		}

		Ok(())
	}
}
