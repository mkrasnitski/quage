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

pub mod keycode_serde {
    use std::fmt;

    use sdl2::keyboard::Keycode;
    use serde::{
        de::{self, Visitor},
        Deserializer,
    };

    pub struct KeycodeVisitor;

    impl<'de> Visitor<'de> for KeycodeVisitor {
        type Value = Keycode;

        fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(formatter, "a string representing a keycode")
        }

        fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Keycode::from_name(s)
                .ok_or(format!("invalid hotkey value \"{}\"", s))
                .map_err(de::Error::custom)
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Keycode, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(KeycodeVisitor)
    }
}
