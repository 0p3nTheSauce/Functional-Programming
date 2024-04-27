type Digit =
| One 
| Two
| Three
| Four
| Five
| Six
| Seven
| Eight
| Nine

type Cell =
| EmptyCell
| FixedValue of Digit
| Definitely of Digit
| MaybeTwo of Digit * Digit
| MaybeThree of Digit * Digit * Digit
| MaybeFour of Digit * Digit * Digit * Digit
| MaybeFive of Digit * Digit * Digit * Digit * Digit
| MaybeSix of Digit * Digit * Digit * Digit * Digit * Digit
| MaybeSeven of Digit * Digit * Digit * Digit * Digit * Digit * Digit
| MaybeEight of Digit * Digit * Digit * Digit * Digit * Digit * Digit * Digit
| MaybeNine of Digit * Digit * Digit * Digit * Digit * Digit * Digit * Digit * Digit

type Cluster = 
| Cluster of Cell * Cell * Cell * Cell * Cell * Cell * Cell * Cell * Cell

type GameView = 
| Blocks of Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster
| Rows of Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster
| Columns of Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster

type UserAction =
| EnterDigit of Digit
| Right
| Left
| Up
| Down
| Quit
| StartGame

type Game = 
| SettingUp of (int * int) * GameView
| Solving of (int * int) * GameView
| Solved of GameView

let first (Cluster (x, _, _, _, _, _, _, _, _)) = x
let second (Cluster (_, x, _, _, _, _, _, _, _)) = x
let third (Cluster (_, _, x, _, _, _, _, _, _)) = x
let fourth (Cluster (_, _, _, x, _, _, _, _, _)) = x
let fifth (Cluster (_, _, _, _, x, _, _, _, _)) = x
let sixth (Cluster (_, _, _, _, _, x, _, _, _)) = x
let seventh (Cluster (_, _, _, _, _, _, x, _, _)) = x
let eighth (Cluster (_, _, _, _, _, _, _, x, _)) = x
let ninth (Cluster (_, _, _, _, _, _, _, _, x)) = x

let mapNine fn (a,b,c,d,e,f,g,h,i) = 
    (fn a, fn b, fn c, fn d, fn e, fn f, fn g, fn h, fn i) 

let straightenBlocks x y z f0 f1 f2 = 
    Cluster 
        ( f0 x, f1 x, f2 x
        , f0 y, f1 y, f2 y
        , f0 z, f1 z, f2 z
        )

let viewAsRows board = 
    (function
    | Rows _ -> board
    | Blocks (a,b,c,d,e,f,g,h,i) ->
        Rows 
            ( straightenBlocks a b c first second third
            , straightenBlocks a b c fourth fifth sixth
            , straightenBlocks a b c seventh eighth ninth
            , straightenBlocks d e f first second third
            , straightenBlocks d e f fourth fifth sixth
            , straightenBlocks d e f seventh eighth ninth
            , straightenBlocks g h i first second third
            , straightenBlocks g h i fourth fifth sixth
            , straightenBlocks g h i seventh eighth ninth
            )
    | Columns (a,b,c,d,e,f,g,h,i) ->
        Rows
            ( Cluster <| mapNine first (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine second (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine third (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine fourth (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine fifth (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine sixth (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine seventh (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine eighth (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine ninth (a,b,c,d,e,f,g,h,i)
            )
    ) board 

let testBoard = 
    let newCluster =
        Cluster 
            ( Definitely One, Definitely Two, Definitely Three
            , Definitely Four, Definitely Five, Definitely Six
            , Definitely Seven, Definitely Eight, Definitely Nine
            )
    Blocks 
        ( newCluster, newCluster, newCluster
        , newCluster, newCluster, newCluster
        , newCluster, newCluster, newCluster
        )

let newBoard = 
    let newCluster = 
        Cluster
            ( EmptyCell, EmptyCell, EmptyCell
            , EmptyCell, EmptyCell, EmptyCell
            , EmptyCell, EmptyCell, EmptyCell
            )
    Blocks 
        ( newCluster, newCluster, newCluster
        , newCluster, newCluster, newCluster
        , newCluster, newCluster, newCluster
        )
let viewAsColumns board = 
    (function
    | Columns _ -> board
    | Blocks (a,b,c,d,e,f,g,h,i) ->
        Columns
            ( straightenBlocks a d g first fourth seventh
            , straightenBlocks a d g second fifth eighth
            , straightenBlocks a d g third sixth ninth
            , straightenBlocks b e h first fourth seventh 
            , straightenBlocks b e h second fifth eighth
            , straightenBlocks b e h third sixth ninth
            , straightenBlocks c f i first fourth seventh
            , straightenBlocks c f i second fifth eighth
            , straightenBlocks c f i third sixth ninth
            )
    | Rows (a,b,c,d,e,f,g,h,i) ->
        Columns
            ( Cluster <| mapNine first (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine second (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine third (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine fourth (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine fifth (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine sixth (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine seventh (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine eighth (a,b,c,d,e,f,g,h,i)
            , Cluster <| mapNine ninth (a,b,c,d,e,f,g,h,i)
            )
    ) board

let blockColumns x y z f0 f1 f2 =
    Cluster 
        ( f0 x, f0 y, f0 z 
        , f1 x, f1 y, f1 z
        , f2 x, f2 y, f2 z 
        )

let blockRows x y z f0 f1 f2 =
    Cluster 
        ( f0 x, f1 x, f2 x
        , f0 y, f1 y, f2 y
        , f0 z, f1 z, f2 z
        )

let viewAsBlocks board = 
    (function
    | Blocks _ -> board
    | Columns (a,b,c,d,e,f,g,h,i) ->
        Blocks
            ( blockColumns a b c first second third
            , blockColumns d e f first second third 
            , blockColumns g h i first second third 
            , blockColumns a b c fourth fifth sixth 
            , blockColumns d e f fourth fifth sixth
            , blockColumns g h i fourth fifth sixth
            , blockColumns a b c seventh eighth ninth
            , blockColumns d e f seventh eighth ninth
            , blockColumns g h i seventh eighth ninth
            )
    | Rows (a,b,c,d,e,f,g,h,i) ->
        Blocks
            ( blockRows a b c first second third
            , blockRows a b c fourth fifth sixth
            , blockRows a b c seventh eighth ninth
            , blockRows d e f first second third
            , blockRows d e f fourth fifth sixth
            , blockRows d e f seventh eighth ninth
            , blockRows g h i first second third
            , blockRows g h i fourth fifth sixth 
            , blockRows g h i seventh eighth ninth
            )
    ) board

let addLeft digit cell = 
    (function
    | FixedValue a -> cell
    | EmptyCell -> Definitely digit
    | Definitely a -> MaybeTwo (digit, a)
    | MaybeTwo (a,b) -> MaybeThree (digit, a,b)
    | MaybeThree (a,b,c) -> MaybeFour (digit, a,b,c)
    | MaybeFour (a,b,c,d) -> MaybeFive (digit, a,b,c,d)
    | MaybeFive (a,b,c,d,e) -> MaybeSix (digit, a,b,c,d,e)
    | MaybeSix (a,b,c,d,e,f) -> MaybeSeven (digit, a,b,c,d,e,f)
    | MaybeSeven (a,b,c,d,e,f,g) -> MaybeEight (digit, a,b,c,d,e,f,g)
    | MaybeEight (a,b,c,d,e,f,g,h) -> MaybeNine (digit,a,b,c,d,e,f,g,h)
    | MaybeNine _ -> cell
    ) cell

let trimLeft cell = 
    (function
    | EmptyCell -> cell
    | FixedValue _ -> cell
    | Definitely _ -> EmptyCell
    | MaybeTwo (_,a) -> Definitely a
    | MaybeThree (_,a,b) -> MaybeTwo (a,b)
    | MaybeFour (_,a,b,c) -> MaybeThree (a,b,c)
    | MaybeFive (_,a,b,c,d) -> MaybeFour (a,b,c,d)
    | MaybeSix (_,a,b,c,d,e) -> MaybeFive (a,b,c,d,e)
    | MaybeSeven (_,a,b,c,d,e,f) -> MaybeSix (a,b,c,d,e,f)
    | MaybeEight (_,a,b,c,d,e,f,g) -> MaybeSeven (a,b,c,d,e,f,g)
    | MaybeNine (_,a,b,c,d,e,f,g,h) -> MaybeEight (a,b,c,d,e,f,g,h)
    ) cell

type CouldExist<'something> = 
| Value of 'something 
| Nothing

let peek cell = 
    (function
    | EmptyCell -> Nothing
    | FixedValue x -> Value x
    | Definitely x -> Value x
    | MaybeTwo (x,_) -> Value x
    | MaybeThree (x,_,_) -> Value x
    | MaybeFour (x,_,_,_) -> Value x
    | MaybeFive (x,_,_,_,_) -> Value x
    | MaybeSix (x,_,_,_,_,_) -> Value x
    | MaybeSeven (x,_,_,_,_,_,_) -> Value x
    | MaybeEight (x,_,_,_,_,_,_,_) -> Value x
    | MaybeNine (x,_,_,_,_,_,_,_,_) -> Value x
    ) cell

let andThen f = 
    function 
    | Nothing -> Nothing
    | Value something -> Value (f something)

let orElse defaultValue = 
    function 
    | Nothing -> defaultValue
    | Value whatever -> whatever

let exists x cell = 
    let rec realExists remaining =
        peek remaining
        |> andThen 
            (fun v -> v = x || realExists (trimLeft remaining))
        |> orElse false
    (function 
    | FixedValue v -> v = x
    | _ -> realExists cell
    ) cell

let insert x cell =
    let rec doInsert remaining = 
        peek remaining
        |> andThen
            (fun item ->
                if item < x then 
                    addLeft item (doInsert (trimLeft remaining))
                elif item = x then 
                    remaining 
                else
                    addLeft x remaining
            )
        |> orElse (Definitely x)
    (function
    | FixedValue _ -> cell
    | _ -> doInsert cell
    ) cell

let remove digit cell =
    let rec removeMatch remaining = 
        peek remaining
        |> andThen
            (fun item ->
                if item = digit then // skip it
                    trimLeft remaining
                else 
                    addLeft item (removeMatch (trimLeft remaining))
            )
        |> orElse cell
    (function
    | FixedValue _ -> cell
    | _ -> removeMatch cell
    ) cell

let toggle digit cell = 
    if exists digit cell then 
        remove digit cell
    else
        insert digit cell

let rec withCell board col row func = 
    let withCluster (Cluster (a,b,c,d,e,f,g,h,i)) = 
        if row = 0 then     Cluster (func a, b, c, d, e, f, g, h, i)
        elif row = 1 then   Cluster (a, func b, c, d, e, f, g, h, i)
        elif row = 2 then   Cluster (a, b, func c, d, e, f, g, h, i)
        elif row = 3 then   Cluster (a, b, c, func d, e, f, g, h, i)
        elif row = 4 then   Cluster (a, b, c, d, func e, f, g, h, i)
        elif row = 5 then   Cluster (a, b, c, d, e, func f, g, h, i)
        elif row = 6 then   Cluster (a, b, c, d, e, f, func g, h, i)
        elif row = 7 then   Cluster (a, b, c, d, e, f, g, func h, i)
        elif row = 8 then   Cluster (a, b, c, d, e, f, g, h, func i)
        else Cluster (a, b, c, d, e, f, g, h, i)
    (function 
    | Rows _ | Blocks _ -> withCell (viewAsColumns board) col row func
    | Columns (a,b,c,d,e,f,g,h,i) -> 
        if col = 0 then     Columns (withCluster a, b, c, d, e, f, g, h, i)
        elif col = 1 then   Columns (a, withCluster b, c, d, e, f, g, h, i)
        elif col = 2 then   Columns (a, b, withCluster c, d, e, f, g, h, i)
        elif col = 3 then   Columns (a, b, c, withCluster d, e, f, g, h, i)
        elif col = 4 then   Columns (a, b, c, d, withCluster e, f, g, h, i)
        elif col = 5 then   Columns (a, b, c, d, e, withCluster f, g, h, i)
        elif col = 6 then   Columns (a, b, c, d, e, f, withCluster g, h, i)
        elif col = 7 then   Columns (a, b, c, d, e, f, g, withCluster h, i)
        elif col = 8 then   Columns (a, b, c, d, e, f, g, h, withCluster i)
        else board
    ) board 

let handleDirection (col,row) board action =
    let min a b = if a < b then a else b
    let max a b = if a > b then a else b
    (function
    | Left ->   ((max 0 (col-1) , row)           , board)
    | Right ->  ((min 8 (col+1) , row)           , board)
    | Up ->     ((col           , max 0 (row-1)) , board)
    | Down ->   ((col           , max 8 (row+1)) , board)
    | _ ->      ((col           , row)           , board)
    ) action

let fixDigit n col row board = 
    // withCell board col row (fun _ -> FixedValue n)
    withCell board col row (fun c -> if c = FixedValue n then EmptyCell else FixedValue n)

let considerDigit n col row board = 
    withCell board col row (fun c -> toggle n c)

let clustersOfBoard board = 
    (function
    | Rows (a,b,c,d,e,f,g,h,i) -> (a,b,c,d,e,f,g,h,i)
    | Columns (a,b,c,d,e,f,g,h,i) -> (a,b,c,d,e,f,g,h,i)
    | Blocks (a,b,c,d,e,f,g,h,i) -> (a,b,c,d,e,f,g,h,i)
    ) board

let foldNine fn start (a,b,c,d,e,f,g,h,i) = 
    let rec inFold fn state index (a,b,c,d,e,f,g,h,i) =
        if index = 0 then 
            inFold fn (fn a state) (index+1) (a,b,c,d,e,f,g,h,i)
        elif index = 1 then 
            inFold fn (fn b state) (index+1) (a,b,c,d,e,f,g,h,i)
        elif index = 2 then
            inFold fn (fn c state) (index+1) (a,b,c,d,e,f,g,h,i)
        elif index = 3 then 
            inFold fn (fn d state) (index+1) (a,b,c,d,e,f,g,h,i)
        elif index = 4 then 
            inFold fn (fn e state) (index+1) (a,b,c,d,e,f,g,h,i)
        elif index = 5 then 
            inFold fn (fn f state) (index+1) (a,b,c,d,e,f,g,h,i)
        elif index = 6 then 
            inFold fn (fn g state) (index+1) (a,b,c,d,e,f,g,h,i)
        elif index = 7 then 
            inFold fn (fn h state) (index+1) (a,b,c,d,e,f,g,h,i)
        else 
            fn i state
    inFold fn start 0 (a,b,c,d,e,f,g,h,i)

let checkCluster (Cluster (a,b,c,d,e,f,g,h,i)) = 
    let isDefiniteCell =
        function 
        | Definitely _ | FixedValue _ -> true
        | _ -> false
    let addToHistogram (a,b,c,d,e,f,g,h,i) = 
        function
        | Nothing -> (a,b,c,d,e,f,g,h,i)
        | Value One -> (a+1,b,c,d,e,f,g,h,i)
        | Value Two -> (a,b+1,c,d,e,f,g,h,i)
        | Value Three -> (a,b,c+1,d,e,f,g,h,i)
        | Value Four -> (a,b,c,d+1,e,f,g,h,i)
        | Value Five -> (a,b,c,d,e+1,f,g,h,i)
        | Value Six -> (a,b,c,d,e,f+1,g,h,i)
        | Value Seven -> (a,b,c,d,e,f,g+1,h,i)
        | Value Eight -> (a,b,c,d,e,f,g,h+1,i)
        | Value Nine -> (a,b,c,d,e,f,g,h,i+1)
        
    foldNine 
        (fun item (definiteCells, counts) ->
            (definiteCells && isDefiniteCell item, addToHistogram counts (peek item))
        )
        (true, (0,0,0,0,0,0,0,0,0))
        (a,b,c,d,e,f,g,h,i)

    = (true, (1,1,1,1,1,1,1,1,1))

let checkGame board = 
    clustersOfBoard board
    |> foldNine (fun item state -> if checkCluster item then state+1 else state) 0
        = 9
    
let gameCompleter board = 
    checkGame (viewAsBlocks board)
        && checkGame (viewAsRows board)
        && checkGame (viewAsColumns board)


let rec evaluate io game = 
    let userAction = io game
    let g = 
        (function
        | (StartGame, SettingUp (position, board)) ->
            Solving (position, board)
        | (StartGame, Solving (position, board)) -> //If we are told to "Start game" but game is being solved
            if gameCompleter board then             //check if game is solved
                Solved (board)                      //move to solved state
            else
                Solving (position, board)           //keep solving
        | (StartGame, Solved _) -> 
            SettingUp ((0, 0), newBoard)
        | (_, Solved _) -> 
            game
        | (EnterDigit n, SettingUp ((col, row), board)) -> 
            SettingUp ((col, row), fixDigit n col row board)
        | (EnterDigit n, Solving ((col, row), board)) ->
            Solving ((col, row), considerDigit n col row board)
        | (direction, SettingUp (position, board)) -> 
            SettingUp (handleDirection position board direction)
        | (direction, Solving (position, board)) -> 
            Solving (handleDirection position board direction)
        ) (userAction, game)
    if userAction = Quit then 
        ()
    else
        evaluate io g

// and now, the least exciting part of the whole thing.

let printAt column row digit =
    System.Console.CursorLeft <- column
    System.Console.CursorTop <- row
    (function
    | Nothing -> ()
    | Value One -> printf "1"
    | Value Two -> printf "2"
    | Value Three -> printf "3"
    | Value Four -> printf "4"
    | Value Five -> printf "5"
    | Value Six -> printf "6"
    | Value Seven -> printf "7"
    | Value Eight -> printf "8"
    | Value Nine -> printf "9"
    ) digit

let printBlock cell col row =
    (function
    | Definitely v ->
        printAt (col + 2) (row + 1) (Value v)
    | FixedValue v ->
        let previous = System.Console.ForegroundColor
        System.Console.ForegroundColor <- System.ConsoleColor.Red
        printAt (col + 2) (row + 1) (Value v)
        System.Console.ForegroundColor <- previous
    | _ ->
        let rec printDigits count remaining =
            printAt (col + (count % 3) * 2) (row + (count / 3)) (peek remaining)
            (function
            | EmptyCell ->
                ()
            | Definitely _ ->
                ()
            | _ ->
                printDigits (count+1) (trimLeft remaining)
            ) remaining
        printDigits 0 cell
    ) cell

let printRow (Cluster (a,b,c,d,e,f,g,h,i)) rowStart =
    printBlock a 2 rowStart
    printBlock b 10 rowStart
    printBlock c 18 rowStart
    printBlock d 26 rowStart
    printBlock e 34 rowStart
    printBlock f 42 rowStart
    printBlock g 50 rowStart
    printBlock h 58 rowStart
    printBlock i 66 rowStart

let printBoard board message =
    System.Console.Clear () // 4, 12, 20
    printfn $"""┏━━━━━━━┯━━━━━━━┯━━━━━━━┳━━━━━━━┯━━━━━━━┯━━━━━━━┳━━━━━━━┯━━━━━━━┯━━━━━━━┓
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┣━━━━━━━┿━━━━━━━┿━━━━━━━╋━━━━━━━┿━━━━━━━┿━━━━━━━╋━━━━━━━┿━━━━━━━┿━━━━━━━┫
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┣━━━━━━━┿━━━━━━━┿━━━━━━━╋━━━━━━━┿━━━━━━━┿━━━━━━━╋━━━━━━━┿━━━━━━━┿━━━━━━━┫
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┃       │       │       ┃       │       │       ┃       │       │       ┃
┗━━━━━━━┷━━━━━━━┷━━━━━━━┻━━━━━━━┷━━━━━━━┷━━━━━━━┻━━━━━━━┷━━━━━━━┷━━━━━━━┛
{message}
"""
    let (Rows (a,b,c,d,e,f,g,h,i)) = viewAsRows board
    printRow a 1
    printRow b 5
    printRow c 9
    printRow d 13
    printRow e 17
    printRow f 21
    printRow g 25
    printRow h 29
    printRow i 33

let showCursor (boardColumn, boardRow) =
    System.Console.CursorVisible <- true
    //System.Console.CursorSize <- 100
    System.Console.CursorLeft <- 4 + 8 * boardColumn
    System.Console.CursorTop <- 2 + 4 * boardRow

let output game =
    // this entire function ... is a side-effect!!
    (function
    | Solved board ->
        System.Console.ForegroundColor <- System.ConsoleColor.Green
        printBoard board "Congratulations!  The puzzle is solved.  Press <Enter> to set up a new one."
        System.Console.ResetColor ()
    | SettingUp (position, board) ->
        printBoard board """Use  ← → ↑ ↓  keys to move around.  Type a digit to set up the puzzle.
Press <Enter> to begin solving, and <Q> to quit."""
        showCursor position
    | Solving (position, board) ->
        printBoard board """Use  ← → ↑ ↓  keys to move around.  Type a digit to add or remove it from consideration.
Press <Q> to quit."""
        showCursor position
    ) game
    game

let rec input game =
    let read = System.Console.ReadKey true
    if read.Key = System.ConsoleKey.Enter then
        StartGame
    elif read.Key = System.ConsoleKey.LeftArrow then
        Left
    elif read.Key = System.ConsoleKey.RightArrow then
        Right
    elif read.Key = System.ConsoleKey.UpArrow then
        Up
    elif read.Key = System.ConsoleKey.DownArrow then
        Down
    elif read.KeyChar = '1' then
        EnterDigit One
    elif read.KeyChar = '2' then
        EnterDigit Two
    elif read.KeyChar = '3' then
        EnterDigit Three
    elif read.KeyChar = '4' then
        EnterDigit Four
    elif read.KeyChar = '5' then
        EnterDigit Five
    elif read.KeyChar = '6' then
        EnterDigit Six
    elif read.KeyChar = '7' then
        EnterDigit Seven
    elif read.KeyChar = '8' then
        EnterDigit Eight
    elif read.KeyChar = '9' then
        EnterDigit Nine
    elif read.Key = System.ConsoleKey.Q then
        Quit
    else
        (output >> input) game

[<EntryPoint>]
let main _ =
    evaluate (output >> input) (SettingUp ((0,0), newBoard))
    0




