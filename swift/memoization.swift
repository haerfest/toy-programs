import Cocoa
import Foundation

// -----------------------------------------------------------------------------
//  Trivial recursive implementation.
// -----------------------------------------------------------------------------

func recursiveFib(n: Int) -> Int {
    if n == 0 || n == 1 {
        return 1
    }
    
    return recursiveFib(n - 1) + recursiveFib(n - 2)
}

// -----------------------------------------------------------------------------
//  Memoized version, remembering calculated values in an array.
// -----------------------------------------------------------------------------

func memoizedFib(n: Int) -> Int {
    var memory = [Int](count: n + 1, repeatedValue: 0)
    
    memory[0] = 1
    memory[1] = 1
    
    func fib(n: Int) -> Int {
        if memory[n] == 0 {
            memory[n] = fib(n - 2) + fib(n - 1)
        }
        
        return memory[n]
    }
    
    return fib(n)
}

// -----------------------------------------------------------------------------
//  Small function to return the execution time of an expression in msecs.
// -----------------------------------------------------------------------------

func timeIt(expression: () -> ()) -> NSTimeInterval {
    let startTime = NSDate()

    expression()
    
    return 1e3 * NSDate().timeIntervalSinceDate(startTime)
}

// -----------------------------------------------------------------------------
//  Main
// -----------------------------------------------------------------------------

let recursiveTime = timeIt { recursiveFib(40) }
let memoizedTime  = timeIt { memoizedFib(40) }

// Example output:
//   recursive ...  632.764 msec
//   memoized ....    0.381 msec

print(NSString(format: "recursive ... %8.3f msec", recursiveTime))
print(NSString(format: "memoized .... %8.3f msec", memoizedTime))
