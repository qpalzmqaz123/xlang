use xlang::Context;

const SCRIPT: &'static str = r#"
    fn fib(n: i64): i64 {
        if n == 0 {
            return 0;
        }

        if n == 1 || n == 2 {
            return 1;
        }

        return fib(n - 2) + fib(n - 1);
    }
"#;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    simple_logger::init_with_level(log::Level::Trace)?;

    let ctx = Context::new()?;
    ctx.load(SCRIPT)?;

    let t1 = std::time::Instant::now();
    let res = unsafe {
        let fib = ctx.get_function::<unsafe extern "C" fn(i64) -> i64>("fib")?;
        fib.call(40)
    };

    println!("res: {}, time: {}ms", res, t1.elapsed().as_millis());

    Ok(())
}
