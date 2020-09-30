use nom::IResult;
use version::Version;

pub mod access;
pub mod attribute;
pub mod class;
pub mod constant_pool;
pub mod field;
pub mod method;
pub mod string;
pub mod version;

pub trait ClassParse<'i>: Sized {
	fn parse(input: &'i [u8], ver: Version) -> IResult<&'i [u8], Self>;
	fn serialize(&self, output: &mut Vec<u8>, ver: Version) -> SerResult;
}
#[derive(Debug, thiserror::Error)]
pub enum SerError {
	#[error("io error: {0}")]
	Io(#[from] std::io::Error),
}

pub type SerResult = Result<(), SerError>;
