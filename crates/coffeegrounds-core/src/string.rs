use std::{convert::TryInto, fmt::Display, str::Utf8Error};

#[derive(Debug, thiserror::Error)]
pub enum JavaStringError {
	#[error("invalid codepoint found")]
	InvalidCodepoint,
	#[error("utf8 error: {0}")]
	Utf8Error(#[from] Utf8Error),
}

#[derive(PartialEq, Eq, Debug)]
pub struct JavaString<'i>(pub &'i [u8]);
impl<'s> From<&'s str> for JavaString<'s> {
	fn from(v: &'s str) -> Self {
		JavaString(v.as_bytes())
	}
}
impl TryInto<String> for &JavaString<'_> {
	type Error = JavaStringError;

	fn try_into(self) -> Result<String, Self::Error> {
		let str = std::str::from_utf8(&self.0)?;
		for c in str.chars() {
			if c as usize == 0 {
				return Err(JavaStringError::InvalidCodepoint);
			}
		}
		Ok(str
			.chars()
			.map(|c| if c as u32 == 0xc080 { 0 as char } else { c })
			.collect())
	}
}

impl Display for JavaString<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if let Ok(s) = TryInto::<String>::try_into(self) {
			write!(f, "{:?}", s)?;
		} else {
			write!(f, "{:?}", self.0)?;
		}
		Ok(())
	}
}
