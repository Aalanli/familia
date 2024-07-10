use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::memory_buffer::MemoryBuffer;

macro_rules! include_cstr {
    ( $path:literal $(,)? ) => {{
        // Use a constant to force the verification to run at compile time.
        const VALUE: &'static ::core::ffi::CStr = match ::core::ffi::CStr::from_bytes_with_nul(
            concat!(include_str!($path), "\0").as_bytes(),
        ) {
            Ok(value) => value,
            Err(_) => panic!(concat!("interior NUL byte(s) in `", $path, "`")),
        };
        VALUE
    }};
}

const RTS_LL: &'static ::core::ffi::CStr = include_cstr!("rts.ll");

pub fn add_rts(context: &Context) -> Module {
    let buf = MemoryBuffer::create_from_memory_range(RTS_LL.to_bytes(), "rts_ll");
    context.create_module_from_ir(buf).unwrap()
}

#[test]
fn test_add_rts() {
    let context = Context::create();
    add_rts(&context);
}
