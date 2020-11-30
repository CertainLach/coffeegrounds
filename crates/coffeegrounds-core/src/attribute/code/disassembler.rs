use super::instruction::{
	self, AAType, AType, FComparsion, IComparsion,
	Instruction::{self, *},
	IntOpOperands, IntOpType, InvokeType, LoadStoreVariant, OpOperands, OpType, ReturnType,
};
use byteorder::{BigEndian, ReadBytesExt};
use std::io::{self, Cursor};

struct Disassembler<'c>(Cursor<&'c [u8]>);
impl<'c> Disassembler<'c> {
	pub fn new(code: &'c [u8]) -> Self {
		Self(Cursor::new(code))
	}
	fn read_u8(&mut self) -> io::Result<u8> {
		self.0.read_u8()
	}
	fn read_i8(&mut self) -> io::Result<i8> {
		self.0.read_i8()
	}
	fn read_u16(&mut self) -> io::Result<u16> {
		self.0.read_u16::<BigEndian>()
	}
	fn read_i16(&mut self) -> io::Result<i16> {
		self.0.read_i16::<BigEndian>()
	}
	fn read_u32(&mut self) -> io::Result<u32> {
		self.0.read_u32::<BigEndian>()
	}
	fn read_i32(&mut self) -> io::Result<i32> {
		self.0.read_i32::<BigEndian>()
	}
	fn read_instruction(&mut self) -> io::Result<(usize, Instruction)> {
		let opcode = self.read_u8()?;
		use Instruction::*;

		Ok((
			self.0.position() as usize,
			match opcode {
				0x32 => ArrayLoad(AAType::Object),
				0x53 => ArrayStore(AAType::Object),
				0x01 => AconstNull,
				0x19 => Load {
					stack_type: AType::Object,
					index: self.read_u8()? as u16,
					variant: LoadStoreVariant::Normal,
				},
				0x2a..=0x2d => Load {
					stack_type: AType::Object,
					index: opcode as u16 - 0x2a,
					variant: LoadStoreVariant::Singlebyte,
				},
				0xbd => AnewArray {
					ty: self.read_u16()?,
				},
				0xb0 => Return(ReturnType::Object),
				0xbe => ArrayLength,
				0x3a => Store {
					stack_type: AType::Object,
					index: self.read_u8()? as u16,
					variant: LoadStoreVariant::Normal,
				},
				0x4b..=0x4e => Store {
					stack_type: AType::Object,
					index: opcode as u16 - 0x4b,
					variant: LoadStoreVariant::Singlebyte,
				},
				0xbf => Athrow,
				0x33 => ArrayLoad(AAType::Byte),
				0x54 => ArrayStore(AAType::Byte),
				0x10 => Bipush(self.read_u8()?),
				0x34 => ArrayLoad(AAType::Char),
				0x55 => ArrayStore(AAType::Char),
				0xc0 => Checkcast(self.read_u16()?),
				0x90 => Convert(AAType::Double, AAType::Float),
				0x8e => Convert(AAType::Double, AAType::Integer),
				0x8f => Convert(AAType::Double, AAType::Long),
				0x63 => Op(OpType::Add, OpOperands::Double),
				0x31 => ArrayLoad(AAType::Double),
				0x52 => ArrayStore(AAType::Double),
				0x98 => Dcmp(FComparsion::G),
				0x97 => Dcmp(FComparsion::L),
				0x0e => Dconst(0.0),
				0x0f => Dconst(1.0),
				0x6f => Op(OpType::Div, OpOperands::Double),
				0x18 => Load {
					stack_type: AType::Double,
					index: 0,
					variant: LoadStoreVariant::Normal,
				},
				0x26..=0x29 => Load {
					stack_type: AType::Double,
					index: opcode as u16 - 0x26,
					variant: LoadStoreVariant::Singlebyte,
				},
				0x6b => Op(OpType::Mul, OpOperands::Double),
				0x77 => Op(OpType::Neg, OpOperands::Double),
				0x73 => Op(OpType::Rem, OpOperands::Double),
				0xaf => Return(ReturnType::Double),
				0x39 => Store {
					stack_type: AType::Double,
					index: self.read_u8()? as u16,
					variant: LoadStoreVariant::Normal,
				},
				0x47..=0x4a => Store {
					stack_type: AType::Double,
					index: opcode as u16 - 0x47,
					variant: LoadStoreVariant::Singlebyte,
				},
				0x67 => Op(OpType::Sub, OpOperands::Double),
				0x59 => Dup,
				0x5a => DupX1,
				0x5b => DupX2,
				0x5c => Dup2,
				0x5d => Dup2X1,
				0x5e => Dup2X2,
				0x8d => Convert(AAType::Float, AAType::Double),
				0x8b => Convert(AAType::Float, AAType::Integer),
				0x8c => Convert(AAType::Float, AAType::Long),
				0x62 => Op(OpType::Add, OpOperands::Float),
				0x30 => ArrayLoad(AAType::Float),
				0x51 => ArrayStore(AAType::Float),
				0x96 => Fcmp(FComparsion::G),
				0x95 => Fcmp(FComparsion::L),
				0x0b => Instruction::Fconst(0.0),
				0x0c => Instruction::Fconst(1.0),
				0x0d => Instruction::Fconst(2.0),
				0x6e => Op(OpType::Div, OpOperands::Float),
				0x17 => Load {
					stack_type: AType::Float,
					index: 0,
					variant: LoadStoreVariant::Normal,
				},
				0x22..=0x25 => Load {
					stack_type: AType::Float,
					index: opcode as u16 - 0x22,
					variant: LoadStoreVariant::Singlebyte,
				},
				0x6a => Op(OpType::Mul, OpOperands::Float),
				0x76 => Op(OpType::Neg, OpOperands::Float),
				0x72 => Op(OpType::Rem, OpOperands::Float),
				0xae => Return(ReturnType::Float),
				0x38 => Store {
					stack_type: AType::Float,
					index: self.read_u8()? as u16,
					variant: LoadStoreVariant::Normal,
				},
				0x43..=0x46 => Store {
					stack_type: AType::Float,
					index: opcode as u16 - 0x43,
					variant: LoadStoreVariant::Singlebyte,
				},
				0x66 => Op(OpType::Sub, OpOperands::Float),
				0xb4 => Getfield(self.read_u16()?),
				0xb2 => Getstatic(self.read_u16()?),
				0xa7 | 0xc8 => {
					let wide = opcode == 0xa7;
					Goto {
						offset: if wide {
							self.read_u32()?
						} else {
							self.read_u16()? as u32
						},
						wide,
					}
				}
				0x91 => Convert(AAType::Integer, AAType::Byte),
				0x92 => Convert(AAType::Integer, AAType::Char),
				0x87 => Convert(AAType::Integer, AAType::Double),
				0x86 => Convert(AAType::Integer, AAType::Float),
				0x85 => Convert(AAType::Integer, AAType::Long),
				0x93 => Convert(AAType::Integer, AAType::Short),
				0x60 => Op(OpType::Add, OpOperands::Integer),
				0x2e => ArrayLoad(AAType::Integer),
				0x7e => IntOp(IntOpType::And, IntOpOperands::Int),
				0x4f => ArrayStore(AAType::Integer),
				0x02..=0x08 => Iconst(opcode as i32 - 3),
				0x6c => Op(OpType::Div, OpOperands::Integer),
				0xa5 | 0xa6 => IfAcmp {
					equals: opcode == 0xa5,
					target: self.read_u16()?,
				},
				0x9f..=0xa4 => {
					let condition = [
						IComparsion::Eq,
						IComparsion::Ne,
						IComparsion::Lt,
						IComparsion::Ge,
						IComparsion::Gt,
						IComparsion::Le,
					][opcode as usize - 0x9f];
					IfIcmp {
						condition,
						target: self.read_u16()?,
					}
				}
				0x99..=0x9e => {
					let condition = [
						IComparsion::Eq,
						IComparsion::Ne,
						IComparsion::Lt,
						IComparsion::Ge,
						IComparsion::Gt,
						IComparsion::Le,
					][opcode as usize - 0x99];
					If {
						condition,
						target: self.read_u16()?,
					}
				}
				0xc6 | 0xc7 => IfNull {
					equals: opcode == 0xc6,
					target: self.read_u16()?,
				},
				0x84 => Iinc {
					index: self.read_u8()? as u16,
					value: self.read_i8()? as i16,
					wide: false,
				},
				0x15 => Load {
					stack_type: AType::Integer,
					index: self.read_u8()? as u16,
					variant: LoadStoreVariant::Normal,
				},
				0x1a..=0x1d => Load {
					stack_type: AType::Integer,
					index: opcode as u16 - 0x1a,
					variant: LoadStoreVariant::Singlebyte,
				},
				0x68 => Op(OpType::Mul, OpOperands::Integer),
				0x74 => Op(OpType::Neg, OpOperands::Integer),
				0xc1 => Instanceof(0),
				0xba => {
					let index = self.read_u16()?;
					let zeros = self.read_u16()?;
					Invokedynamic { index, zeros }
				}
				0xb9 => {
					let index = self.read_u16()?;
					let count = self.read_u8()?;
					let zero = self.read_u8()?;
					Invoke(InvokeType::Interface { index, count, zero })
				}
				0xb7 => Invoke(InvokeType::Special(self.read_u16()?)),
				0xb8 => Invoke(InvokeType::Static(self.read_u16()?)),
				0xb6 => Invoke(InvokeType::Virtual(self.read_u16()?)),
				0x80 => IntOp(IntOpType::Or, IntOpOperands::Int),
				0x70 => Op(OpType::Rem, OpOperands::Integer),
				0xac => Return(ReturnType::Int),
				0x78 => IntOp(IntOpType::Shl, IntOpOperands::Int),
				0x7a => IntOp(IntOpType::Shr, IntOpOperands::Int),
				0x36 => Store {
					stack_type: AType::Integer,
					index: 0,
					variant: LoadStoreVariant::Normal,
				},
				0x3b..=0x3e => Store {
					stack_type: AType::Integer,
					index: opcode as u16 - 0x3b,
					variant: LoadStoreVariant::Singlebyte,
				},
				0x64 => Op(OpType::Sub, OpOperands::Integer),
				0x7c => IntOp(IntOpType::Ushr, IntOpOperands::Int),
				0x82 => IntOp(IntOpType::Xor, IntOpOperands::Int),
				0xa8 => Jsr {
					offset: self.read_u16()? as u32,
					wide: false,
				},
				0xc9 => Jsr {
					offset: self.read_u32()?,
					wide: true,
				},
				0x8a => Convert(AAType::Long, AAType::Double),
				0x89 => Convert(AAType::Long, AAType::Float),
				0x88 => Convert(AAType::Long, AAType::Integer),
				0x61 => Op(OpType::Add, OpOperands::Long),
				0x2f => ArrayLoad(AAType::Long),
				0x7f => IntOp(IntOpType::And, IntOpOperands::Long),
				0x50 => ArrayStore(AAType::Long),
				0x94 => Lcmp,
				0x09..=0x0a => Lconst(opcode as i64 - 0x09),
				0x12 => Ldc {
					index: self.read_u8()? as u16,
					wide: false,
				},
				0x13 => Ldc {
					index: self.read_u16()?,
					wide: true,
				},
				0x14 => Ldc2 {
					index: self.read_u16()?,
				},
				0x6d => Op(OpType::Div, OpOperands::Long),

				0x16 => Load {
					stack_type: AType::Long,
					index: 0,
					variant: LoadStoreVariant::Normal,
				},
				0x1e..=0x21 => Load {
					stack_type: AType::Long,
					index: opcode as u16 - 0x1e,
					variant: LoadStoreVariant::Singlebyte,
				},

				0x69 => Op(OpType::Mul, OpOperands::Long),
				0x75 => Op(OpType::Neg, OpOperands::Long),
				0xab => {
					let offset = self.0.position() - 1;
					let mut zeros = [0; 3];
					for i in 0..(3 - (offset & 3)) {
						zeros[i as usize] = self.read_u8()?;
					}

					let default = self.read_i32()?;
					let mut pairs = Vec::new();

					let pair_count = self.read_u32()?;
					for _ in 0..pair_count {
						pairs.push((self.read_i32()?, self.read_i32()?))
					}

					Lookupswitch {
						zeros,
						default,
						pairs,
					}
				}
				0x81 => IntOp(IntOpType::Or, IntOpOperands::Long),
				0x71 => Op(OpType::Rem, OpOperands::Long),
				0xad => Return(ReturnType::Long),
				0x79 => IntOp(IntOpType::Shl, IntOpOperands::Long),
				0x7b => IntOp(IntOpType::Shr, IntOpOperands::Long),

				0x37 => Store {
					stack_type: AType::Long,
					index: 0,
					variant: LoadStoreVariant::Normal,
				},
				0x3f..=0x42 => Store {
					stack_type: AType::Long,
					index: opcode as u16 - 0x3f,
					variant: LoadStoreVariant::Singlebyte,
				},

				0x65 => Op(OpType::Sub, OpOperands::Long),
				0x7d => IntOp(IntOpType::Ushr, IntOpOperands::Long),
				0x83 => IntOp(IntOpType::Xor, IntOpOperands::Long),
				0xc2 => Monitorenter,
				0xc3 => Monitorexit,
				0xc5 => Multianewarray {
					ty: self.read_u16()?,
					dimensions: self.read_u8()?,
				},
				0xbb => New(self.read_u16()?),
				0xbc => NewArray(self.read_u16()?),
				0x00 => Nop,
				0x57 => Pop,
				0x58 => Pop2,
				0xb5 => Putfield(self.read_u16()?),
				0xb3 => Putstatic(self.read_u16()?),
				0xa9 => Ret {
					index: self.read_u8()? as u16,
					wide: false,
				},
				0xb1 => Return(ReturnType::Void),
				0x35 => ArrayLoad(AAType::Short),
				0x56 => ArrayStore(AAType::Short),
				0x11 => Sipush(self.read_i16()?),
				0x5f => Swap,
				0xaa => {
					let offset = self.0.position() - 1;
					let mut zeros = [0; 3];
					for i in 0..(3 - (offset & 3)) {
						zeros[i as usize] = self.read_u8()?;
					}

					let default = self.read_i32()?;
					let low = self.read_i32()?;
					let high = self.read_i32()?;

					let count = high - low + 1;
					let mut offsets = Vec::with_capacity((count as usize).max(0));

					for _ in 0..count {
						offsets.push(self.read_i32()?)
					}

					Tableswitch {
						zeros,
						default,
						low,
						high,
						offsets,
					}
				}
				0xc4 => {
					let opcode: u8 = self.read_u8()?;
					match opcode {
						0x19 => Load {
							stack_type: AType::Object,
							variant: LoadStoreVariant::Wide,
							index: self.read_u16()?,
						},
						0x3a => Store {
							stack_type: AType::Object,
							variant: LoadStoreVariant::Wide,
							index: self.read_u16()?,
						},
						0x18 => Load {
							stack_type: AType::Double,
							variant: LoadStoreVariant::Wide,
							index: self.read_u16()?,
						},
						0x39 => Store {
							stack_type: AType::Double,
							variant: LoadStoreVariant::Wide,
							index: self.read_u16()?,
						},
						0x17 => Load {
							stack_type: AType::Float,
							variant: LoadStoreVariant::Wide,
							index: self.read_u16()?,
						},
						0x38 => Store {
							stack_type: AType::Float,
							variant: LoadStoreVariant::Wide,
							index: self.read_u16()?,
						},
						0x15 => Load {
							stack_type: AType::Integer,
							variant: LoadStoreVariant::Wide,
							index: self.read_u16()?,
						},
						0x36 => Store {
							stack_type: AType::Integer,
							variant: LoadStoreVariant::Wide,
							index: self.read_u16()?,
						},
						0x16 => Load {
							stack_type: AType::Long,
							variant: LoadStoreVariant::Wide,
							index: self.read_u16()?,
						},
						0x37 => Store {
							stack_type: AType::Long,
							variant: LoadStoreVariant::Wide,
							index: self.read_u16()?,
						},
						0xa9 => Ret {
							index: self.read_u16()?,
							wide: true,
						},
						0x84 => Iinc {
							index: self.read_u16()?,
							value: self.read_i16()?,
							wide: true,
						},
						_ => return Err(anyhow::anyhow!("unknown insn")),
					}
				}
				_ => return Err(anyhow::anyhow!("unknown insn")),
			},
		))
	}
}
