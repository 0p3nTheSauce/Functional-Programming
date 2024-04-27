//Basics
let max a b =
    if a >= b then 
        a
    else 
        b

let abs a =
    if a < 0 then 
        a * -1
    else 
        a

// (fun x -> x < 3)

//Recursion

let rec gcd x y = //greatest common diviser
    if y = (max x y) then
        gcd y x
    elif y = 0 then 
        x
    else
        gcd y (x % y) 

let pow x y = //power of
    let rec inPow acc x y = 
        if y = 0 then 
            1
        elif y = 1 then 
            acc
        else
            inPow (acc * x) x (y-1)
    inPow x x y

let fib n = //stairs/fibonacci n
    let rec inFib acc1 acc2 n =
        if n = 1 then 
            acc1
        elif n = 2 then 
            acc2 
        else 
            inFib acc2 (acc1+acc2) (n-1)
    if n = 0 then 
        0
    else
        inFib 1 2 n

let sqrt n = //square root
    let rec inSqrt x n i =
        if i = 0 then
            x 
        else
            inSqrt ( (x + n / x) / 2.0 ) n (i-1)
    if n = 0.0 then
        0.0
    else
        inSqrt (n/2.0) n 20

// Higher Order Functions

let sumFunOddEven fn n = // takes an integer n, and n int -> int function. Sums the even and odd results placed in a tuple
    let rec funOE (even, odd) fn n =
        let result = fn n
        if n = 0 then 
            (even, odd)
        elif result % 2 = 0 then
            funOE (even + result, odd) fn (n-1) 
        else
            funOE (even, odd + result) fn (n-1)
    funOE (0,0) fn n

let fold fn state n =
    let rec bfold fn state n = 
        if n < 0 then
            state 
        elif n < 10 then 
            fn state n 
        else 
            bfold fn (fn state (n % 10)) (n/10)
    if n < 0  then 
        bfold fn state (n * -1)
    else 
        bfold fn state n

let first n = 
    fold (fun x y -> y) 0 n

let funpredtup funct pred number = 
//function takes a predicate, another function and an integer n
//applies the function to 1 to n and checks if the result satisfies predicate
//outputs a tuple (count predicate satisfied, sum result)
    let rec predTup (satisfied, result) funct pred n =
        if n = 0 then 
            (satisfied, result)
        else
            if (funct n) |> pred  then 
                predTup (satisfied+1,result+funct n) funct pred (n-1) 
            else
                predTup (satisfied, result) funct pred (n-1)
    predTup (0,0) funct pred number

// let wtf x f =
//     let rec bod x f tup count= 
//         if (fun (x,_) -> x = 1) tup then 
//             (count, tup)
//         else
//             bod 
        
