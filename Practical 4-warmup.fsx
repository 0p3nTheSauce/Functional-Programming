type MyList<'data> = 
| Empty
| Element of 'data * MyList<'data>

let listString list =
    let rec toString = 
        function
            | Empty -> " "
            | Element (v, Empty) -> string v
            | Element (v, nested) -> string v + ", " + toString nested

    "[" + toString list + "]"

let push v l = Element (v, l)

let reverse list = 
    let rec reversal output = 
        function 
            | Empty ->
                output
            | Element (x, remaining) -> 
                reversal (push x output) remaining
        
    reversal Empty list

let sequence rule start n = 
    let rec generate count previous = 
        if count = 0 then
            Empty
        else
            push (rule previous) (generate (count - 1) (rule previous))
    generate n start

let doubles = sequence (fun x -> x * 2) 1
let banana = sequence (fun k -> k + "na") "ba"

