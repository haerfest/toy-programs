extern crate time;

fn fib(n: i32) -> i32 {
    return if n < 2 {
        1
    } else {
        fib(n - 2) + fib(n - 1)
    }
}

fn main() {
    let start = time::pricse_time_ns();
    let answer = fib(40);
    println!("{}", answer);
}
