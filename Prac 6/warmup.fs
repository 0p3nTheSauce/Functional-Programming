let stringFold f initial (s : string) =
    Seq.fold (fun state ch -> f state (string ch)) initial s

type PartOfSentence =
| Word of string
| Comma
| Period
| Space

type List<'a> =
| Empty
| V of 'a * List<'a>


type Input =
| Valid of List<PartOfSentence>
| Invalid of string

let stringLength = stringFold (fun n _ -> n + 1) 0

let isInSet set s =
    stringFold (fun state item -> state || item = s) false set

let onlyValidChars =
    stringFold (fun state item ->
        state && isInSet "abcdefghijklmnopqrstuvwxyz ,." item
    ) true

let stringReverse = stringFold (fun state item -> item + state) ""

let stringToList =
    stringReverse
    >> stringFold (fun state item -> V (item, state)) Empty

let listFold f initial =
    let rec listFold state =
        function
        | Empty -> state 
        | V (item, rest) -> listFold (f state item) rest
    listFold initial

let listReverse list = 
    listFold (fun state item -> V (item, state)) Empty list

let listMap f list = 
    listReverse list 
    |> listFold (fun state item -> V (f item,state)) Empty

let toPartOfSentence s =
    if s = " " then 
        Space
    elif s = "." then 
        Period
    elif s ="," then 
        Comma
    else
        Word s

let rec merge =
    function
    | Empty -> Empty
    | V (Word a, V (Word b, rest)) ->
        merge (V (Word (a + b), rest))
    | V (x, rest) ->
        V (x, merge rest)

let rec checkValidOrdering = 
    function
    | Empty -> true
    | V (Word _, (V (Space, (V (Word x, rest))))) ->
        checkValidOrdering (V (Word x, rest))
    | V (Word _, (V (Comma, V (Space, (V (Word x, rest)))))) ->
        checkValidOrdering (V (Word x, rest)) 
    | V (Word _, (V (Period, V (Space, (V (Word x, rest)))))) -> 
        checkValidOrdering (V (Word x, rest))
    | V (Word _, (V (Period, Empty))) -> true
    | _ -> false

let inGoodOrder = 
    stringToList
    >> listMap toPartOfSentence
    >> merge
    >> checkValidOrdering

let classifyInput s = 
    if stringLength s >= 2 && onlyValidChars s && inGoodOrder s then
        Valid (stringToList s |> listMap toPartOfSentence |> merge)
    else 
        Invalid s

let rec precededByComma =
    function
    | V (Comma, V (Space, V (Word w, rest))) -> 
        V (w, precededByComma rest)
    | V (_, rest) ->
        precededByComma rest 
    | Empty -> Empty

let rec succeededByComma = 
    function
    | V (Word w, V (Comma, V (Space, rest))) -> 
        V (w, succeededByComma rest)
    | V (_, rest) -> 
        succeededByComma rest
    | Empty -> Empty

let rec placeCommaBefore w =
    function
    | V (Period, V (Space, rest)) ->
        V (Period, V (Space, placeCommaBefore w rest))
    | V (Comma, V (Space, rest)) -> 
        V (Comma, V(Space, placeCommaBefore w rest))
    | V (Space, V (Word x, rest)) ->
        if x = w then
            V (Comma, V (Space, V (Word x, placeCommaBefore w rest)))
        else
            V (Space, V (Word x, placeCommaBefore w rest))
    | V (x, rest) -> 
        V (x, placeCommaBefore w rest)
    | Empty -> Empty

let rec placeCommaAfter w =
    function
    | V (Word x, Empty) ->
        V (Word x, Empty)
    | V (Word x, V (Comma, V (Space, rest))) ->
        V (Word x, V (Comma, V (Space, placeCommaAfter w rest))) 
    | V (Word x, V (Space, rest)) -> 
        if x = w then 
            V (Word x, V (Comma, V (Space, placeCommaAfter w rest))) 
        else 
            V (Word x, V (Space, placeCommaAfter w rest))
    | V (x, rest) ->
        V (x, placeCommaAfter w rest)
    | Empty -> Empty

let listToString = 
    listFold (fun state -> 
        function
        | Word w -> state + w
        | Comma -> state + ","
        | Period -> state + "." 
        | Space -> state + " " 
    ) ""

let rec sprinkleCommas = 
    function
    | Invalid s -> 
        s
    | Valid list ->
        let sprinkledBefore = 
            listFold 
                (fun state word -> placeCommaBefore word state)
                list
                (precededByComma list)
        let sprinkledAfter =
            listFold
                (fun state word -> placeCommaAfter word state)
                sprinkledBefore
                (succeededByComma list)
        if sprinkledBefore = list then 
            listToString list // no changes have been made
        else 
            sprinkleCommas (Valid sprinkledAfter)

let sprinkle = classifyInput >> sprinkleCommas


