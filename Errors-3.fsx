let andThen nextStep = 
    function
        | Sorry -> Sorry
        | Item v -> Item (nextStep v)

let addBag = 
    andThen (fun p -> p + 0.28) 

let discount =
    andThen (fun p ->
        if p > 1000.0 then
            p * 0.95
        else
            p
    )

let checkout rate = 
    calculateTax rate >> discount >> addBag

checkout 1.15 1698.1