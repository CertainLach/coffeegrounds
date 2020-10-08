use std::{
	borrow::Cow, convert::TryFrom, convert::TryInto, fmt::Display, ops::Deref, str::Utf8Error,
};

use smallvec::{smallvec, SmallVec};

#[derive(Debug, thiserror::Error)]
pub enum JavaStringError {
	#[error("invalid codepoint found")]
	InvalidCodepoint,
	#[error("utf8 error: {0}")]
	Utf8Error(#[from] Utf8Error),
}

#[derive(PartialEq, Eq, Debug, Hash)]
pub struct JavaString<'i>(pub Cow<'i, [u8]>);

impl<'i> Deref for JavaString<'i> {
	type Target = [u8];

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

/// This conversion is fallible because MUTF-8 have different encoding for strings
impl<'s> TryFrom<&'s str> for JavaString<'s> {
	type Error = JavaStringError;

	fn try_from(v: &'s str) -> Result<Self, JavaStringError> {
		if v.chars().any(|c| c == 0 as char) {
			return Err(JavaStringError::InvalidCodepoint);
		}
		Ok(JavaString(Cow::Borrowed(v.as_bytes())))
	}
}
impl From<String> for JavaString<'static> {
	fn from(v: String) -> Self {
		JavaString(Cow::Owned(
			v.chars()
				.flat_map(|c| {
					let c = if c == 0 as char {
						std::char::from_u32(0xc080).expect("valid char")
					} else {
						c
					};
					// Should never allocate, used because of len
					let mut vec: SmallVec<[u8; 4]> = smallvec![0];
					c.encode_utf8(&mut vec);
					vec
				})
				.collect(),
		))
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
