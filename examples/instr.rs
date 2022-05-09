use xlang::Context;

const SCRIPT: &'static str = r#"
    fn detect(): bool {
        let res: i64 = get_value(0, 1);
        if res == 258 {
            return true;
        }

        return false;
    }
"#;

/// ty:
///     0: i8
///     1: i16
unsafe extern "C" fn get_value(offset: i64, ty: i64, is_be: bool) -> i64 {
    let offset: usize = offset as _;

    match ty {
        0 => PAYLOAD[offset as usize] as _,
        _ => {
            if is_be {
                i16::from_be_bytes(PAYLOAD.get(offset..offset + 2).unwrap().try_into().unwrap())
                    as _
            } else {
                i16::from_le_bytes(PAYLOAD.get(offset..offset + 2).unwrap().try_into().unwrap())
                    as _
            }
        }
    }
}

static mut PAYLOAD: &[u8] = &[];

fn main() -> Result<(), Box<dyn std::error::Error>> {
    simple_logger::init_with_level(log::Level::Trace)?;

    let mut ctx = Context::new()?;

    ctx.add_global_c_function::<(i64, i64), i64>("get_value", get_value as usize)?;

    ctx.load(SCRIPT)?;

    let res = unsafe {
        PAYLOAD = &[2, 1];
        let detect = ctx.get_function::<unsafe extern "C" fn() -> bool>("detect")?;
        detect.call()
    };

    println!("res: {}", res);

    Ok(())
}
