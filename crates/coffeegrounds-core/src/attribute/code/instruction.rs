#[derive(Clone, Copy, Debug)]
pub enum IComparsion {
	Eq,
	Ne,
	Lt,
	Ge,
	Gt,
	Le,
}

#[derive(Debug)]
pub enum FComparsion {
	G,
	L,
}

#[derive(Debug)]
pub enum LoadStoreVariant {
	// aload_x
	Singlebyte,
	// aload x
	Normal,
	// wide aload x
	Wide,
}

#[derive(Debug)]
pub enum AAType {
	Object,
	Double,
	Float,
	Integer,
	Long,
	Byte,
	Char,
	Short,
}

#[derive(Debug)]
pub enum AType {
	Integer,
	Long,
	Float,
	Double,
	Object,
}

#[derive(Debug)]
pub enum ReturnType {
	Long,
	Void,
	Object,
	Double,
	Float,
	Int,
}

#[derive(Debug)]
pub enum OpType {
	Div,
	Mul,
	Neg,
	Sub,
	Add,
	Rem,
}

#[derive(Debug)]
pub enum OpOperands {
	Long,
	Double,
	Float,
	Integer,
}

#[derive(Debug)]
pub enum IntOpType {
	And,
	Or,
	Xor,

	Shl,
	Shr,
	Ushr,
}

#[derive(Debug)]
pub enum IntOpOperands {
	Int,
	Long,
}

#[derive(Debug)]
pub enum InvokeType {
	Interface { count: u8, zero: u8, index: u16 },
	Special(u16),
	Virtual(u16),
	Static(u16),
}

#[derive(Debug)]
pub enum Instruction {
	Load {
		stack_type: AType,
		index: u16,
		variant: LoadStoreVariant,
	},
	Store {
		stack_type: AType,
		index: u16,
		variant: LoadStoreVariant,
	},

	ArrayLoad(AAType),
	ArrayStore(AAType),

	// double => float, int, long
	// float => double, int, long
	// int => byte, char, double, float, long, short
	// long => double, float, int
	Convert(AAType, AAType),

	Return(ReturnType),

	Op(OpType, OpOperands),
	IntOp(IntOpType, IntOpOperands),

	AconstNull,
	AnewArray {
		ty: u16,
	},

	ArrayLength,
	Athrow,

	Bipush(u8),

	Checkcast(u16),

	Dcmp(FComparsion),

	Dconst(f64),

	Dup,
	DupX1,
	DupX2,
	Dup2,
	Dup2X1,
	Dup2X2,

	Fcmp(FComparsion),
	Fconst(f32),

	Getfield(u16),

	Getstatic(u16),
	Goto {
		offset: u32,
		wide: bool,
	},

	Iconst(i32),
	Lconst(i64),

	IfAcmp {
		equals: bool,
		target: u16,
	},
	IfIcmp {
		condition: IComparsion,
		target: u16,
	},
	If {
		condition: IComparsion,
		target: u16,
	},
	IfNull {
		equals: bool,
		target: u16,
	},
	Iinc {
		index: u16,
		value: i16,
		wide: bool,
	},

	Instanceof(u16),

	Invokedynamic {
		index: u16,
		zeros: u16,
	},
	Invoke(InvokeType),

	Jsr {
		offset: u32,
		wide: bool,
	},
	Lcmp,

	Ldc {
		index: u16,
		wide: bool,
	},
	Ldc2 {
		index: u16,
	},

	Lookupswitch {
		zeros: [u8; 3],
		default: i32,
		pairs: Vec<(i32, i32)>,
	},
	Lor,

	Monitorenter,
	Monitorexit,
	Multianewarray {
		ty: u16,
		dimensions: u8,
	},

	New(u16),
	NewArray(u16),

	Nop,
	Pop,
	Pop2,

	Putfield(u16),
	Putstatic(u16),

	Ret {
		index: u16,
		wide: bool,
	},

	Sipush(i16),
	Swap,
	Tableswitch {
		zeros: [u8; 3],
		default: i32,
		low: i32,
		high: i32,
		offsets: Vec<i32>,
	},
}
