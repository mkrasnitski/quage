pub trait BitExtract {
    fn bit(&self, bit: u8) -> bool;
}

macro_rules! impl_bit_extract {
    ($type:ty) => {
        impl BitExtract for $type {
            fn bit(&self, bit: u8) -> bool {
                *self & (1 << bit) != 0
            }
        }
    };
}

impl_bit_extract!(u8);
impl_bit_extract!(u16);
