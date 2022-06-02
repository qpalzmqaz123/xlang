use xlang::Context;

const SCRIPT: &'static str = r#"
    fn test(): i64 {
        return add(1, 2);
    }
"#;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    simple_logger::init_with_level(log::Level::Trace)?;

    let mut ctx = Context::new()?;

    xlang::add_global_function!(ctx, "add", |a: i64, b: i64|: i64 {
        a + b
    })?;

    ctx.load(SCRIPT)?;
    let res = unsafe {
        let test = ctx.get_function::<unsafe extern "C" fn() -> i64>("test")?;
        test.call()
    };

    println!("res: {}", res);

    Ok(())
}
