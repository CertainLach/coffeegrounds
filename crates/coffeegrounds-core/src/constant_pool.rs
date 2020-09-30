use std::{
	fmt::{self, Display, Formatter},
	io::Write,
	ops::Index,
};

use byteorder::{BigEndian, WriteBytesExt};
use nom::{
	do_parse,
	error::ErrorKind,
	number::streaming::{be_f32, be_f64, be_i32, be_i64, be_u16, be_u8},
	take,
	Err::Failure,
	IResult,
};
use num_traits::FromPrimitive;

use crate::{ClassParse, SerResult};

use super::{string::JavaString, version::Version};

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, enum_primitive_derive::Primitive)]
pub enum Tag {
	Class = 7,
	Fieldref = 9,
	Methodref = 10,
	InterfaceMethodref = 11,
	String = 8,
	Integer = 3,
	Float = 4,
	Long = 5,
	Double = 6,
	NameAndType = 12,
	Utf8 = 1,
	MethodHandle = 15,
	MethodType = 16,
	InvokeDynamic = 18,

	Module = 19,
	Package = 20,
}
impl Tag {
	pub fn occupies_two_slots(self) -> bool {
		self == Self::Long || self == Self::Double
	}
}

impl ClassParse<'_> for Tag {
	fn parse(input: &[u8], _: Version) -> IResult<&[u8], Self> {
		let (rest, tag) = be_u8(input)?;
		let tag = Self::from_u8(tag).ok_or_else(|| Failure((input, ErrorKind::Tag)))?;
		Ok((rest, tag))
	}

	fn serialize(&self, output: &mut Vec<u8>, _: Version) -> SerResult {
		output.push(*self as u8);
		Ok(())
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ReferenceKind {
	GetField,
	GetStatic,
	PutField,
	PutStatic,
	InvokeVirtual,
	InvokeStatic,
	InvokeSpecial,
	NewInvokeSpecial,
	InvokeInterface,
	Other(u8),
}
impl From<u8> for ReferenceKind {
	fn from(v: u8) -> Self {
		use ReferenceKind::*;
		match v {
			1 => GetField,
			2 => GetStatic,
			3 => PutField,
			4 => PutStatic,
			5 => InvokeVirtual,
			6 => InvokeStatic,
			7 => InvokeSpecial,
			8 => NewInvokeSpecial,
			9 => InvokeInterface,
			v => Other(v),
		}
	}
}
impl Into<u8> for &ReferenceKind {
	fn into(self) -> u8 {
		use ReferenceKind::*;
		match self {
			GetField => 1,
			GetStatic => 2,
			PutField => 3,
			PutStatic => 4,
			InvokeVirtual => 5,
			InvokeStatic => 6,
			InvokeSpecial => 7,
			NewInvokeSpecial => 8,
			InvokeInterface => 9,
			Other(v) => *v,
		}
	}
}
impl Display for ReferenceKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use ReferenceKind::*;
		write!(
			f,
			"{}",
			match self {
				GetField => "getfield",
				GetStatic => "getstatic",
				PutField => "putfield",
				PutStatic => "putstatic",
				InvokeVirtual => "invokevirtual",
				InvokeStatic => "invokestatic",
				InvokeSpecial => "invokespecial",
				NewInvokeSpecial => "newinvokespecial",
				InvokeInterface => "invokeinterface",
				Other(v) => return write!(f, "other #{}", v),
			}
		)?;
		Ok(())
	}
}

#[derive(PartialEq, Debug)]
pub enum Item<'input> {
	Class {
		name_index: u16,
	},
	Fieldref {
		class_index: u16,
		name_and_type_index: u16,
	},
	Methodref {
		class_index: u16,
		name_and_type_index: u16,
	},
	InterfaceMethodref {
		class_index: u16,
		name_and_type_index: u16,
	},
	String {
		string_index: u16,
	},
	Integer(i32),
	Float(f32),
	Long(i64),
	Double(f64),
	NameAndType {
		name_index: u16,
		descriptor_index: u16,
	},
	Utf8(JavaString<'input>),
	MethodHandle {
		reference_kind: ReferenceKind,
		reference_index: u16,
	},
	MethodType {
		descriptor_index: u16,
	},
	InvokeDynamic {
		bootstrap_method_attr_index: u16,
		name_and_type_index: u16,
	},

	Module {
		name_index: u16,
	},
	Package {
		name_index: u16,
	},

	/// Can be rewritten
	UserNop,
	/// Reserves slot for double/long
	Nop,
}

pub trait PrintWithCp {
	fn print(&self, cp: &List<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result;
	fn has_comment(&self) -> bool;
}
impl<T> PrintWithCp for Option<T>
where
	T: PrintWithCp,
{
	fn print(&self, cp: &List<'_>, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Some(v) => v.print(cp, f),
			None => write!(f, "!"),
		}
	}

	fn has_comment(&self) -> bool {
		if let Some(v) = self {
			v.has_comment()
		} else {
			true
		}
	}
}

impl<'i> Item<'i> {
	fn tag(&self) -> Tag {
		use Tag::*;
		match self {
			Item::Class { .. } => Class,
			Item::Fieldref { .. } => Fieldref,
			Item::Methodref { .. } => Methodref,
			Item::InterfaceMethodref { .. } => InterfaceMethodref,
			Item::String { .. } => String,
			Item::Integer(_) => Integer,
			Item::Float(_) => Float,
			Item::Long(_) => Long,
			Item::Double(_) => Double,
			Item::NameAndType { .. } => NameAndType,
			Item::Utf8(_) => Utf8,
			Item::MethodHandle { .. } => MethodHandle,
			Item::MethodType { .. } => MethodType,
			Item::InvokeDynamic { .. } => InvokeDynamic,

			Item::Module { .. } => Module,
			Item::Package { .. } => Package,

			// User nops should be rewritten before output
			Item::UserNop => panic!("user nop tag"),
			Item::Nop => panic!("nop tag"),
		}
	}
	pub fn occupies_two_slots(&self) -> bool {
		self.tag().occupies_two_slots()
	}
}
impl PrintWithCp for &Item<'_> {
	fn print(&self, list: &List, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Item::Class { name_index } => {
				list.get(*name_index as usize).print(list, f)?;
			}
			Item::Fieldref {
				class_index,
				name_and_type_index,
			} => {
				list.get(*name_and_type_index as usize).print(list, f)?;
				write!(f, " in ")?;
				list.get(*class_index as usize).print(list, f)?;
			}
			Item::Methodref {
				class_index,
				name_and_type_index,
			} => {
				list.get(*name_and_type_index as usize).print(list, f)?;
				write!(f, " in ")?;
				list.get(*class_index as usize).print(list, f)?;
			}
			Item::InterfaceMethodref {
				class_index,
				name_and_type_index,
			} => {
				list.get(*name_and_type_index as usize).print(list, f)?;
				write!(f, " in ")?;
				list.get(*class_index as usize).print(list, f)?;
			}
			Item::String { string_index } => {
				list.get(*string_index as usize).print(list, f)?;
			}
			Item::NameAndType {
				name_index,
				descriptor_index,
			} => {
				list.get(*name_index as usize).print(list, f)?;
				write!(f, " of type ")?;
				list.get(*descriptor_index as usize).print(list, f)?;
			}
			Item::MethodHandle {
				reference_index, ..
			} => {
				list.get(*reference_index as usize).print(list, f)?;
			}
			Item::MethodType { descriptor_index } => {
				list.get(*descriptor_index as usize).print(list, f)?;
			}
			Item::InvokeDynamic {
				name_and_type_index,
				..
			} => {
				list.get(*name_and_type_index as usize).print(list, f)?;
			}
			Item::Module { name_index } => {
				list.get(*name_index as usize).print(list, f)?;
			}
			Item::Package { name_index } => {
				list.get(*name_index as usize).print(list, f)?;
			}
			Item::Utf8(v) => write!(f, "{}", v)?,
			_ => {}
		}
		Ok(())
	}

	fn has_comment(&self) -> bool {
		match self {
			Item::Integer(_) => false,
			Item::Float(_) => false,
			Item::Long(_) => false,
			Item::Double(_) => false,
			Item::Utf8(_) => false,
			Item::UserNop => false,
			Item::Nop => false,
			_ => true,
		}
	}
}
impl<'i> ClassParse<'i> for Item<'i> {
	#[rustfmt::skip]
	fn parse(input: &'i [u8], ver: Version) -> IResult<&[u8], Item<'i>> {
		let (input, tag) = Tag::parse(input, ver)?;
		use Item::*;
		Ok(match tag {
			Tag::Class => do_parse!(input, name_index: be_u16 >> (Class { name_index }))?,
			Tag::Fieldref => do_parse!(
				input,
				class_index: be_u16 >>
				name_and_type_index: be_u16 >>
				(Fieldref {
					class_index,
					name_and_type_index
				})
			)?,
			Tag::Methodref => do_parse!(
				input,
				class_index: be_u16 >>
				name_and_type_index: be_u16 >>
				(Methodref {
					class_index,
					name_and_type_index
				})
			)?,
			Tag::InterfaceMethodref => do_parse!(
				input,
				class_index: be_u16 >>
				name_and_type_index: be_u16	>>
				(InterfaceMethodref {
					class_index,
					name_and_type_index
				})
			)?,
			Tag::String => do_parse!(
				input,
				string_index: be_u16 >> (Item::String { string_index })
			)?,
			Tag::Integer => do_parse!(input, value: be_i32 >> (Integer(value)))?,
			Tag::Float => do_parse!(input, value: be_f32 >> (Float(value)))?,
			Tag::Long => do_parse!(input, value: be_i64 >> (Long(value)))?,
			Tag::Double => do_parse!(input, value: be_f64 >> (Double(value)))?,
			Tag::NameAndType => do_parse!(
				input,
				name_index: be_u16 >>
				descriptor_index: be_u16 >>
				(NameAndType {
					name_index,
					descriptor_index,
				})
			)?,
			Tag::Utf8 => do_parse!(input, length: be_u16 >> data: take!(length) >> (Utf8(JavaString(data))))?,
			Tag::MethodHandle => do_parse!(
				input,
				reference_kind: be_u8 >>
				reference_index: be_u16 >>
				(MethodHandle {
					reference_kind: ReferenceKind::from(reference_kind),
					reference_index,
				})
			)?,
			Tag::MethodType => do_parse!(
				input,
				descriptor_index: be_u16 >> (MethodType { descriptor_index })
			)?,
			Tag::InvokeDynamic => do_parse!(
				input,
				bootstrap_method_attr_index: be_u16 >>
				name_and_type_index: be_u16	>>
				(InvokeDynamic {
					bootstrap_method_attr_index,
					name_and_type_index
				})
			)?,

			Tag::Module => do_parse!(
				input,
				name_index: be_u16 >>
				(Module { name_index })
			)?,
			Tag::Package => do_parse!(
				input,
				name_index: be_u16 >>
				(Package { name_index })
			)?,
		})
	}

	fn serialize(&self, output: &mut Vec<u8>, _ver: Version) -> SerResult {
		output.push(self.tag() as u8);
		match self {
			Item::Class { name_index } => output.write_u16::<BigEndian>(*name_index)?,
			Item::Fieldref {
				class_index,
				name_and_type_index,
			} => {
				output.write_u16::<BigEndian>(*class_index)?;
				output.write_u16::<BigEndian>(*name_and_type_index)?;
			}
			Item::Methodref {
				class_index,
				name_and_type_index,
			} => {
				output.write_u16::<BigEndian>(*class_index)?;
				output.write_u16::<BigEndian>(*name_and_type_index)?;
			}
			Item::InterfaceMethodref {
				class_index,
				name_and_type_index,
			} => {
				output.write_u16::<BigEndian>(*class_index)?;
				output.write_u16::<BigEndian>(*name_and_type_index)?;
			}
			Item::String { string_index } => {
				output.write_u16::<BigEndian>(*string_index)?;
			}
			Item::Integer(v) => {
				output.write_i32::<BigEndian>(*v)?;
			}
			Item::Float(v) => {
				output.write_f32::<BigEndian>(*v)?;
			}
			Item::Long(v) => {
				output.write_i64::<BigEndian>(*v)?;
			}
			Item::Double(v) => {
				output.write_f64::<BigEndian>(*v)?;
			}
			Item::NameAndType {
				name_index,
				descriptor_index,
			} => {
				output.write_u16::<BigEndian>(*name_index)?;
				output.write_u16::<BigEndian>(*descriptor_index)?;
			}
			Item::Utf8(s) => {
				output.write_u16::<BigEndian>(s.0.len() as u16)?;
				output.write_all(&s.0)?;
			}
			Item::MethodHandle {
				reference_kind,
				reference_index,
			} => {
				output.write_u8(reference_kind.into())?;
				output.write_u16::<BigEndian>(*reference_index)?;
			}
			Item::MethodType { descriptor_index } => {
				output.write_u16::<BigEndian>(*descriptor_index)?;
			}
			Item::InvokeDynamic {
				bootstrap_method_attr_index,
				name_and_type_index,
			} => {
				output.write_u16::<BigEndian>(*bootstrap_method_attr_index)?;
				output.write_u16::<BigEndian>(*name_and_type_index)?;
			}

			Item::Module { name_index } => {
				output.write_u16::<BigEndian>(*name_index)?;
			}
			Item::Package { name_index } => {
				output.write_u16::<BigEndian>(*name_index)?;
			}

			Item::UserNop => panic!("user nops should be rewritten before output"),
			Item::Nop => panic!("nop serialize"),
		};
		Ok(())
	}
}

impl Display for Item<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Item::Class { name_index } => write!(f, "class #{}", name_index),
			Item::Fieldref {
				class_index,
				name_and_type_index,
			} => write!(f, "fieldref #{} in #{}", name_and_type_index, class_index),
			Item::Methodref {
				class_index,
				name_and_type_index,
			} => write!(f, "methodref #{} in #{}", name_and_type_index, class_index),
			Item::InterfaceMethodref {
				class_index,
				name_and_type_index,
			} => write!(
				f,
				"interfacemethodref #{} in #{}",
				name_and_type_index, class_index
			),
			Item::String { string_index } => write!(f, "string #{}", string_index),
			Item::Integer(v) => write!(f, "int {}", v),
			Item::Float(v) => write!(f, "float {}", v),
			Item::Long(v) => write!(f, "long {}", v),
			Item::Double(v) => write!(f, "double {}", v),
			Item::NameAndType {
				name_index,
				descriptor_index,
			} => write!(f, "nameandtype #{} #{}", descriptor_index, name_index),
			Item::Utf8(v) => write!(f, "utf8 {}", v),
			Item::MethodHandle {
				reference_kind,
				reference_index,
			} => write!(f, "methodhandle {} #{}", reference_kind, reference_index),
			Item::MethodType { descriptor_index } => write!(f, "methodtype #{}", descriptor_index),
			Item::InvokeDynamic {
				bootstrap_method_attr_index,
				name_and_type_index,
			} => write!(
				f,
				"invokedynamic {} {}",
				bootstrap_method_attr_index, name_and_type_index
			),
			Item::Module { name_index } => write!(f, "module #{}", name_index),
			Item::Package { name_index } => write!(f, "package #{}", name_index),
			Item::UserNop => write!(f, "user nop"),
			Item::Nop => write!(f, "reserved slot"),
		}?;
		Ok(())
	}
}

#[derive(Debug)]
pub struct List<'i>(Vec<Item<'i>>);
impl<'i> List<'i> {
	pub fn new() -> Self {
		Self(vec![Item::Nop])
	}

	pub fn push(&mut self, item: Item<'i>) {
		let two_slots = item.occupies_two_slots();
		self.0.push(item);
		if two_slots {
			self.0.push(Item::Nop)
		}
	}
	pub fn replace(&mut self, old: usize, new: Item<'i>) {
		if self.0[old] == Item::Nop {
			panic!("can't put anything to placeholder")
		} else if new.occupies_two_slots() {
			// Replaced last item
			if self.0.len() - 1 == old {
				self.0.push(Item::Nop);
			} else if self.0[old + 1] == Item::UserNop {
				self.0[old + 1] = Item::Nop;
			} else if self.0[old + 1] == Item::Nop {
				// Nothing to do
			} else {
				panic!("no space for two-slot item")
			}
		} else if self.0.len() > old && self.0[old + 1] == Item::Nop {
			self.0[old + 1] = Item::UserNop
		}
		self.0[old] = new;
	}

	pub fn get_str(&self, index: usize) -> Option<&JavaString> {
		if let Some(Item::Utf8(v)) = self.0.get(index) {
			Some(&v)
		} else {
			None
		}
	}

	pub fn get(&self, index: usize) -> Option<&Item<'i>> {
		self.0.get(index)
	}
}
impl<'i> Index<usize> for List<'i> {
	type Output = Item<'i>;

	fn index(&self, index: usize) -> &Self::Output {
		&self.0[index]
	}
}

impl<'i> ClassParse<'i> for List<'i> {
	fn parse(input: &'i [u8], ver: Version) -> IResult<&'i [u8], Self> {
		let (mut input, size) = be_u16(input)?;

		let mut data = Vec::with_capacity(size as usize);
		data.push(Item::Nop);

		let mut is_nop = false;
		for _ in 1..size {
			if is_nop {
				data.push(Item::Nop);
				is_nop = false;
				continue;
			}
			let (new_input, item) = Item::parse(input, ver)?;
			input = new_input;
			is_nop = item.occupies_two_slots();
			data.push(item);
		}

		Ok((input, List(data)))
	}

	fn serialize(&self, output: &mut Vec<u8>, ver: Version) -> SerResult {
		output.write_u16::<BigEndian>(self.0.len() as u16)?;

		let items = &self.0;

		let mut is_nop = false;
		for item in items.iter().skip(1) {
			if is_nop {
				is_nop = false;
				continue;
			}
			item.serialize(output, ver)?;
			is_nop = item.occupies_two_slots();
		}

		Ok(())
	}
}

impl Display for List<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for (idx, item) in self.0.iter().enumerate() {
			write!(f, "{}. {}", idx, item)?;
			if item.has_comment() {
				write!(f, " // ")?;
				// Let item explain yourself
				item.print(self, f)?;
			}
			writeln!(f)?;
		}
		Ok(())
	}
}
