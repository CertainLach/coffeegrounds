mod disassembler;
pub mod instruction;

use std::io::{self, Cursor};

use byteorder::{BigEndian, ReadBytesExt};
use nom::number::streaming::{be_u16, be_u32, be_u8};

use crate::{constant_pool::PrintWithCp, version::VersionFeature, ClassParse};

use super::Attribute;

struct Label(u32);

fn disassemble(code: &[u8]) -> anyhow::Result<Vec<Instruction>> {
	let mut input = Cursor::new(code);
	let mut out = Vec::new();
	loop {
		use Instruction::*;
		let instruction = input.read_u8();
		if instruction.is_err() {
			println!("Read done");
			return Ok(out);
		}
		let instruction = instruction.unwrap();
		dbg!(instruction);
	}
}

pub struct Code {
	max_stack: u16,
	max_locals: u16,
	code: Vec<Instruction>,
}
impl ClassParse<'_> for Code {
	fn parse(mut input: &[u8], ver: crate::version::Version) -> nom::IResult<&[u8], Self> {
		let (max_stack, max_locals, code_length) =
			if ver.supports(VersionFeature::HalfSizedCodeFields) {
				let (new_input, max_stack) = be_u8(input)?;
				let (new_input, max_locals) = be_u8(new_input)?;

				let (new_input, code_length) = be_u16(new_input)?;
				input = new_input;

				(max_stack as u16, max_locals as u16, code_length as u32)
			} else {
				let (new_input, max_stack) = be_u16(input)?;
				let (new_input, max_locals) = be_u16(new_input)?;

				let (new_input, code_length) = be_u32(new_input)?;
				input = new_input;

				(max_stack, max_locals, code_length)
			};
		let code = disassemble(&input[0..code_length as usize]).unwrap();
		let input = &input[code_length as usize..];

		Ok((
			input,
			Code {
				max_stack,
				max_locals,
				code,
			},
		))
	}

	fn serialize(&self, output: &mut Vec<u8>, ver: crate::version::Version) -> crate::SerResult {
		todo!()
	}
}

impl PrintWithCp for Code {
	fn print(
		&self,
		cp: &crate::constant_pool::List<'_>,
		f: &mut std::fmt::Formatter<'_>,
	) -> std::fmt::Result {
		for insn in self.code.iter() {
			println!("{:?}", insn);
		}
		Ok(())
	}

	fn has_comment(&self) -> bool {
		todo!()
	}
}

impl Attribute<'_> for Code {
	const NAME: &'static [u8] = b"Code";
}
