#[cfg(feature = "defmt")]
#[macro_export]
macro_rules! dbg {
	() => {{ $crate::defmt::dbg!() }};
	($($val:expr),+ $(,)?) => {{ $crate::defmt::dbg!($($val),+) }};
}

#[cfg(all(not(feature = "defmt"), feature = "std"))]
#[macro_export]
macro_rules! dbg {
	() => {{ ::std::dbg!() }};
	($($val:expr),+ $(,)?) => {{ ::std::dbg!($($val),+) }};
}

#[cfg(not(any(feature = "defmt", feature = "std")))]
#[macro_export]
macro_rules! dbg {
	() => {{ () }};
	($val:expr $(,)?) => {{ $val }};
	($($val:expr),+ $(,)?) => {{ ($($val),+) }};
}

#[cfg(feature = "defmt")]
#[macro_export]
macro_rules! debug_assert {
	($cond:expr $(,)?) => {{ $crate::defmt::debug_assert!($cond) }};
	($cond:expr, $($arg:tt)+ $(,)?) => {{ $crate::translate!(defmt, $crate::defmt::debug_assert, [$cond], $($arg)+) }};
}

#[cfg(not(feature = "defmt"))]
#[macro_export]
macro_rules! debug_assert {
	($cond:expr $(,)?) => {{ ::core::debug_assert!($cond) }};
	($cond:expr, $($arg:tt)+ $(,)?) => {{ $crate::translate!(std, ::core::debug_assert, [$cond], $($arg)+) }};
}

#[cfg(feature = "defmt")]
#[macro_export]
macro_rules! assert {
	($cond:expr $(,)?) => {{ $crate::defmt::assert!($cond) }};
	($cond:expr, $($arg:tt)+ $(,)?) => {{ $crate::translate!(defmt, $crate::defmt::assert, [$cond], $($arg)+) }};
}

#[cfg(not(feature = "defmt"))]
#[macro_export]
macro_rules! assert {
	($cond:expr $(,)?) => {{ ::core::assert!($cond) }};
	($cond:expr, $($arg:tt)+ $(,)?) => {{ $crate::translate!(std, ::core::assert, [$cond], $($arg)+) }};
}

#[cfg(feature = "defmt")]
#[macro_export]
macro_rules! debug_assert_eq {
	($left:expr, $right:expr $(,)?) => {{ $crate::defmt::debug_assert_eq!($left, $right) }};
	($left:expr, $right:expr, $($arg:tt)+ $(,)?) => {{ $crate::translate!(defmt, $crate::defmt::debug_assert_eq, [$left, $right], $($arg)*) }};
}

#[cfg(not(feature = "defmt"))]
#[macro_export]
macro_rules! debug_assert_eq {
	($left:expr, $right:expr $(,)?) => {{ ::core::debug_assert_eq!($left, $right) }};
	($left:expr, $right:expr, $($arg:tt)+ $(,)?) => {{ $crate::translate!(std, ::core::assert_eq, [$left, $right], $($arg)*) }};
}

#[cfg(feature = "defmt")]
#[macro_export]
macro_rules! debug_assert_ne {
	($left:expr, $right:expr $(,)?) => {{ $crate::defmt::debug_assert_ne!($left, $right) }};
	($left:expr, $right:expr, $($arg:tt)+ $(,)?) => {{ $crate::translate!(defmt, $crate::defmt::debug_assert_ne, [$left, $right], $($arg)*) }};
}

#[cfg(not(feature = "defmt"))]
#[macro_export]
macro_rules! debug_assert_ne {
	($left:expr, $right:expr $(,)?) => {{ ::core::debug_assert_ne!($left, $right) }};
	($left:expr, $right:expr, $($arg:tt)+ $(,)?) => {{ $crate::translate!(std, ::core::assert_eq, [$left, $right], $($arg)*) }};
}

#[cfg(feature = "defmt")]
#[macro_export]
macro_rules! assert_eq {
	($left:expr, $right:expr $(,)?) => {{ $crate::defmt::assert_eq!($left, $right) }};
	($left:expr, $right:expr, $($arg:tt)+ $(,)?) => {{ $crate::translate!(defmt, $crate::defmt::assert_eq, [$left, $right], $($arg)*) }};
}

#[cfg(not(feature = "defmt"))]
#[macro_export]
macro_rules! assert_eq {
	($left:expr, $right:expr $(,)?) => {{ ::core::assert_eq!($left, $right) }};
	($left:expr, $right:expr, $($arg:tt)+ $(,)?) => {{ $crate::translate!(std, ::core::assert_eq, [$left, $right], $($arg)*) }};
}

#[cfg(feature = "defmt")]
#[macro_export]
macro_rules! assert_ne {
	($left:expr, $right:expr $(,)?) => {{ $crate::defmt::assert_ne!($left, $right) }};
	($left:expr, $right:expr, $($arg:tt)+ $(,)?) => {{ $crate::translate!(defmt, $crate::defmt::assert_ne, [$left, $right], $($arg)*) }};
}

#[cfg(not(feature = "defmt"))]
#[macro_export]
macro_rules! assert_ne {
	($left:expr, $right:expr $(,)?) => {{ ::core::assert_ne!($left, $right) }};
	($left:expr, $right:expr, $($arg:tt)+ $(,)?) => {{ $crate::translate!(std, ::core::assert_eq, [$left, $right], $($arg)*) }};
}

#[cfg(feature = "defmt")]
#[macro_export]
macro_rules! unwrap {
	($e:expr $(,)?) => {{ $crate::defmt::unwrap!($e) }};
	($e:expr, $format:literal $(, $arg:tt)* $(,)?) => {{ $crate::translate!(defmt, $crate::defmt::unwrap, [$e], $format $(, $arg)*) }};
}

#[cfg(all(not(any(feature = "defmt", feature = "std")), feature = "alloc"))]
#[macro_export]
macro_rules! unwrap {
	($e:expr $(,)?) => {{ ($e).unwrap() }};
	($e:expr, $format:literal $(,)?) => {{ ($e).expect($format) }};
	($e:expr, $format:literal $(, $arg:tt)* $(,)?) => {{ ($e).expect(&$crate::translate!(std, ::alloc::format, [], $format $(, $arg)*)) }};
}

#[cfg(all(not(feature = "defmt"), feature = "std"))]
#[macro_export]
macro_rules! unwrap {
	($e:expr $(,)?) => {{ ($e).unwrap() }};
	($e:expr, $format:literal $(,)?) => {{ ($e).expect($format) }};
	($e:expr, $format:literal $(, $arg:tt)* $(,)?) => {{ ($e).expect(&$crate::translate!(std, ::std::format, [], $format $(, $arg)*)) }};
}

#[cfg(not(any(feature = "defmt", feature = "alloc")))]
#[macro_export]
macro_rules! unwrap {
    ($e:expr $(,)?) => {{
        ($e).unwrap()
    }};
    ($e:expr, $format:literal $(, $arg:tt)* $(,)?) => {{
        ($e).expect($format)
    }};
}

#[cfg(feature = "defmt")]
#[macro_export]
macro_rules! println {
	() => {{ $crate::defmt::println!() }};
	($format:literal $(, $arg:tt)* $(,)?) => {{ $crate::translate!(defmt, $crate::defmt::println, [], $format, $($arg),*) }};
}

#[cfg(all(not(feature = "defmt"), feature = "std"))]
#[macro_export]
macro_rules! println {
	() => {{ ::std::println!() }};
	($format:literal $(, $arg:tt)* $(,)?) => {{ $crate::translate!(std, ::std::println, [], $format, $($arg),*) }};
}

#[cfg(not(any(feature = "defmt", feature = "std")))]
#[macro_export]
macro_rules! println {
    () => {{
        ()
    }};
    ($format:literal $(, $arg:tt)* $(,)?) => {{
        ()
    }};
}

#[cfg(feature = "defmt")]
#[macro_export]
macro_rules! error {
	($format:literal $(, $arg:expr)* $(,)?) => {{ use $crate::defmt; $crate::translate!(defmt, $crate::defmt::error, [], $format, $($arg),*) }};
}

#[cfg(feature = "log")]
#[macro_export]
macro_rules! error {
	($format:literal $(, $arg:expr)* $(,)?) => {{ $crate::translate!(std, $crate::log::error, [], $format, $($arg),*) }};
}

#[cfg(not(any(feature = "defmt", feature = "log")))]
#[macro_export]
macro_rules! error {
    ($format:literal $(, $arg:expr)* $(,)?) => {{
        ()
    }};
}

#[cfg(feature = "defmt")]
#[macro_export]
macro_rules! warn {
	($format:literal $(, $arg:expr)* $(,)?) => {{ use $crate::defmt; $crate::translate!(defmt, $crate::defmt::warn, [], $format, $($arg),*) }};
}

#[cfg(feature = "log")]
#[macro_export]
macro_rules! warn {
	($format:literal $(, $arg:expr)* $(,)?) => {{ $crate::translate!(std, $crate::log::warn, [], $format, $($arg),*) }};
}

#[cfg(not(any(feature = "defmt", feature = "log")))]
#[macro_export]
macro_rules! warn {
    ($format:literal $(, $arg:expr)* $(,)?) => {{
        ()
    }};
}

#[cfg(feature = "defmt")]
#[macro_export]
macro_rules! debug {
	($format:literal $(, $arg:expr)* $(,)?) => {{ use $crate::defmt; $crate::translate!(defmt, $crate::defmt::debug, [], $format, $($arg),*) }};
}

#[cfg(feature = "log")]
#[macro_export]
macro_rules! debug {
	($format:literal $(, $arg:expr)* $(,)?) => {{ $crate::translate!(std, $crate::log::debug, [], $format, $($arg),*) }};
}

#[cfg(not(any(feature = "defmt", feature = "log")))]
#[macro_export]
macro_rules! debug {
    ($format:literal $(, $arg:expr)* $(,)?) => {{
        ()
    }};
}

#[cfg(feature = "defmt")]
#[macro_export]
macro_rules! info {
	($format:literal $(, $arg:expr)* $(,)?) => {{ use $crate::defmt; $crate::translate!(defmt, $crate::defmt::info, [], $format, $($arg),*) }};
}

#[cfg(feature = "log")]
#[macro_export]
macro_rules! info {
	($format:literal $(, $arg:expr)* $(,)?) => {{ $crate::translate!(std, $crate::log::info, [], $format, $($arg),*) }};
}

#[cfg(not(any(feature = "defmt", feature = "log")))]
#[macro_export]
macro_rules! info {
    ($format:literal $(, $arg:expr)* $(,)?) => {{
        ()
    }};
}

#[cfg(feature = "defmt")]
#[macro_export]
macro_rules! trace {
	($format:literal $(, $arg:expr)* $(,)?) => {{ use $crate::defmt; $crate::translate!(defmt, $crate::defmt::trace, [], $format, $($arg),*) }};
}

#[cfg(feature = "log")]
#[macro_export]
macro_rules! trace {
	($format:literal $(, $arg:expr)* $(,)?) => {{ $crate::translate!(std, $crate::log::trace, [], $format, $($arg),*) }};
}

#[cfg(not(any(feature = "defmt", feature = "log")))]
#[macro_export]
macro_rules! trace {
    ($format:literal $(, $arg:expr)* $(,)?) => {{
        ()
    }};
}

#[cfg(feature = "defmt")]
#[macro_export]
macro_rules! intern {
    ($s:literal $(,)?) => {{
        $crate::defmt::intern!($s)
    }};
}

#[cfg(not(feature = "defmt"))]
#[macro_export]
macro_rules! intern {
    ($s:literal $(,)?) => {{
        $s
    }};
}
