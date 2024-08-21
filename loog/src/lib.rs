#![no_std]

#[cfg(feature = "alloc")]
extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

#[cfg(all(feature = "defmt", feature = "log"))]
compile_error!("`defmt` and `log` features are mutually exclusive");

mod macros;

#[cfg(feature = "defmt")]
pub use defmt;
#[cfg(feature = "log")]
pub use log;
#[doc(hidden)]
pub use loog_macros::translate;

#[doc(hidden)]
pub mod private {
    #[cfg(feature = "alloc")]
    pub use alloc::format;
    pub use core::{
        assert, assert_eq, assert_ne, debug_assert, debug_assert_eq, debug_assert_ne, panic, todo,
        unimplemented, unreachable, write, writeln,
    };
    #[cfg(feature = "std")]
    pub use std::{dbg, println};
}

#[cfg(feature = "defmt")]
pub use defmt::{Debug2Format, Display2Format};

#[cfg(not(feature = "defmt"))]
pub use self::to_format::{Debug2Format, Display2Format};
#[cfg(not(feature = "defmt"))]
mod to_format {
    use core::fmt;

    pub struct Debug2Format<T>(pub T);

    impl<T: fmt::Debug> fmt::Debug for Debug2Format<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.0.fmt(f)
        }
    }

    pub struct Display2Format<T>(pub T);

    impl<T: fmt::Display> fmt::Display for Display2Format<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.0.fmt(f)
        }
    }
}

#[macro_export]
macro_rules! switch {
    ($( $target:ident => $then:expr )+) => {{
        $( $crate::_switch_impl!($target, result, $then); )+
        result
    }};
}

#[cfg(feature = "defmt")]
#[doc(hidden)]
#[macro_export]
macro_rules! _switch_impl {
    (defmt, $result:ident, $then:expr) => {
        let $result = $then;
    };
    (log, $result:ident, $then:expr) => {};
}

#[cfg(feature = "log")]
#[doc(hidden)]
#[macro_export]
macro_rules! _switch_impl {
    (log, $result:ident, $then:expr) => {
        let $result = $then;
    };
    (defmt, $result:ident, $then:expr) => {};
}

#[cfg(test)]
mod tests {
    macro_rules! tests {
        ($($macro:ident { $( ($($arg:tt)*) $(: $type:ty)? );+ $(;)? })+) => {$(
            #[allow(unreachable_code)]
            #[test]
            fn $macro() {
                return;
                $( tests!(_call $macro ($($arg)*) $($type,)? ()); )+
            }
        )+};
        (_call $macro:ident ($($arg:tt)*) $type:ty $(, $_type:ty)?) => {
            let _: $type = crate::$macro!($($arg)*);
        }
    }

    tests! {
        dbg {
            ();
            (0): u8;
            (0,): u8;
            (0, 1): (u8, u8);
            (0, 1,): (u8, u8);
        }
        assert {
            (true);
            (true, "message");
        }
        debug_assert {
            (true);
            (true, "message");
        }
        assert_eq {
            (0, 0);
            (0, 0, "message");
        }
        debug_assert_eq {
            (0, 0);
            (0, 0, "message");
        }
        assert_ne {
            (0, 0);
            (0, 0, "message");
        }
        debug_assert_ne {
            (0, 0);
            (0, 0, "message");
        }
        unwrap {
            (Some(0)): u8;
            (Some(0), "message"): u8;
            (Some(0), "{}", 0): u8;
        }
        println {
            ("message");
            ("message {}", 0);
        }
        error {
            ("error");
            ("error: {}", 0);
        }
        warn {
            ("warn");
            ("warn: {}", 0);
        }
        info {
            ("info");
            ("info: {}", 0);
        }
        debug {
            ("debug");
            ("debug: {}", 0);
        }
        trace {
            ("trace");
            ("trace: {}", 0);
        }
    }
}
