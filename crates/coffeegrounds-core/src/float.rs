use std::{hash::Hash, hash::Hasher, ops::Deref};

#[derive(Debug)]
pub struct JavaFloatWrapper<T>(pub T);
impl<T> Deref for JavaFloatWrapper<T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl PartialEq for JavaFloatWrapper<f32> {
	fn eq(&self, other: &Self) -> bool {
		self.0.to_bits() == other.0.to_bits()
	}
}
impl Eq for JavaFloatWrapper<f32> {}
impl Hash for JavaFloatWrapper<f32> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		state.write_u32(self.0.to_bits())
	}
}
impl PartialEq for JavaFloatWrapper<f64> {
	fn eq(&self, other: &Self) -> bool {
		self.0.to_bits() == other.0.to_bits()
	}
}
impl Eq for JavaFloatWrapper<f64> {}
impl Hash for JavaFloatWrapper<f64> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		state.write_u64(self.0.to_bits())
	}
}
