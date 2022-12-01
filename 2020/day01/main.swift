import Foundation

// Find two values in the array with sum
func two_with_sum(values: [Int], sum: Int) -> (Int, Int)? {
    for x in values {
        let y = sum - x
        if (values.contains { $0 == y }) {
            return (x, y)
        }
    }
    return nil
}

// Find three values in the array with sum
func three_with_sum(values: [Int], sum: Int) -> (Int, Int, Int)? {
    for x in values {
        let remainder = sum - x
        let ret = two_with_sum(values: values, sum: remainder)
        if ret != nil {
            let (y, z) = ret!
            return (x, y, z)
        }
    }
    return nil
}


let file_name = "data.txt"

let content =
    try! String(
        contentsOfFile: file_name,
        encoding: String.Encoding.utf8
    )

// seperate at new line -> trim -> convert to int
var values = content.components(separatedBy: "\n")
    .map() { $0.trimmingCharacters(in: .whitespacesAndNewlines) }
    .map() { Int($0)! }

values.sort()

// Puzzle 01: Two Values with sum 2020
let ret_p1 = two_with_sum(values: values, sum: 2020)
if ret_p1 != nil {
    let (x, y) = ret_p1!
    print(String(format: "Puzzle 01: %d * %d = %d", x, y, x*y))
} else {
    print("Puzzle 01: No such values")
}

// Puzzle 02: Three Values with sum 2020
let ret_p2 = three_with_sum(values: values, sum: 2020)
if ret_p2 != nil {
    let (x, y, z) = ret_p2!
    print(String(format: "Puzzle 02: %d * %d * %d = %d", x, y, z, x*y*z))
} else {
    print("Puzzle 02: No such values")
}