extern crate time;

fn fib(n: i32) -> i32 {
    return if n < 2 {
        1
    } else {
        fib(n - 2) + fib(n - 1)
    }
}

fn main() {
    let start = time::precise_time_s();
    let answer = fib(40);
    let elapsed = time::precise_time_s() - start;
    println!("{}, elapsed: {}", answer, elapsed);
}
