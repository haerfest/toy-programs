use std::io::stdio::println;

enum List {
    Cons(u32, ~List),
    Nil
}

fn print_list(list: List) {
    match list {
        Nil               => println("Nil"),
        Cons(head, ~tail) => {
            print!("{} -> ", head);
            print_list(tail);
        }
    }
}

fn main() {
    let list = Cons(1, ~Cons(2, ~Cons(3, ~Nil)));
    print_list(list);
}
