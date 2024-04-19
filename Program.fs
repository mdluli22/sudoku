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
// Not integers so we don't have to check really as the type is determined and not able to be mixed up
// 7
type Cell =
    | EmptyCell
    | FixedValue     of Digit
    | Definitely     of Digit
    | MaybeTwo       of Digit * Digit
    | MaybeThree     of Digit * Digit * Digit
    | MaybeFour      of Digit * Digit * Digit * Digit
    | MaybeFive      of Digit * Digit * Digit * Digit * Digit
    | MaybeSix       of Digit * Digit * Digit * Digit * Digit * Digit
    | MaybeSeven     of Digit * Digit * Digit * Digit * Digit * Digit * Digit
    | MaybeEight     of Digit * Digit * Digit * Digit * Digit * Digit * Digit * Digit
    | MaybeNine      of Digit * Digit * Digit * Digit * Digit * Digit * Digit * Digit * Digit
// 8
type Cluster = 
    | Cluster of Cell * Cell * Cell * Cell * Cell * Cell * Cell * Cell * Cell

// 9
type GameView =
    | Blocks    of Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster
    | Rows      of Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster
    | Columns   of Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster * Cluster

// 11
type UserAction =
    | EnterDigit of Digit
    | Right
    | Left
    | Up
    | Down
    | Quit
    | StartGame

// 12
type Game =
    | SettingUp of (int * int) * GameView
    | Solving   of (int * int) * GameView
    | Solved    of GameView

// 15 & 20
let first   (Cluster (x,_,_,_,_,_,_,_,_)) = x
let second  (Cluster (_,x,_,_,_,_,_,_,_)) = x
let third   (Cluster (_,_,x,_,_,_,_,_,_)) = x
let fourth  (Cluster (_,_,_,x,_,_,_,_,_)) = x
let fifth   (Cluster (_,_,_,_,x,_,_,_,_)) = x
let sixth   (Cluster (_,_,_,_,_,x,_,_,_)) = x
let seventh (Cluster (_,_,_,_,_,_,x,_,_)) = x
let eighth  (Cluster (_,_,_,_,_,_,_,x,_)) = x
let ninth   (Cluster (_,_,_,_,_,_,_,_,x)) = x

let mapNine fn (a, b, c, d, e, f, g, h, i) =
    (fn a, fn b, fn c, fn d, fn e, fn f, fn g, fn h, fn i)

let straightenBlocks x y z f0 f1 f2 =
    Cluster
        (   f0 x, f1 x, f2 x
        ,   f0 y, f1 y, f2 y
        ,   f0 z, f1 z, f2 z
        )
// 14
let viewAsRows board =
    (function
        |Rows _ -> board
        |Blocks (a, b, c, d, e, f, g, h, i) -> (*1*)
            Rows
                (   straightenBlocks a b c  first    second  third
                ,   straightenBlocks a b c  fourth   fifth   sixth
                ,   straightenBlocks a b c  seventh  eighth  ninth

                ,   straightenBlocks d e f  first    second  third
                ,   straightenBlocks d e f  fourth   fifth   sixth
                ,   straightenBlocks d e f  seventh  eighth  ninth

                ,   straightenBlocks g h i  first    second  third
                ,   straightenBlocks g h i  fourth   fifth   sixth
                ,   straightenBlocks g h i  seventh  eighth  ninth
                )
        |Columns (a, b, c, d, e, f, g, h, i) ->
            Rows
                (   Cluster <| mapNine first    (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine second   (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine third    (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine fourth   (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine fifth    (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine sixth    (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine seventh  (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine eighth   (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine ninth    (a, b, c, d, e, f, g, h, i)
                )
    )board


// 21
let testBoard =

    let newCluster =
        Cluster
            (   Definitely One,     Definitely Two,     Definitely Three
            ,   Definitely Four,    Definitely Five,    Definitely Six
            ,   Definitely Seven,   Definitely Eight,   Definitely Nine
            )

    Blocks
        (   newCluster, newCluster, newCluster
        ,   newCluster, newCluster, newCluster
        ,   newCluster, newCluster, newCluster
        )

// 22
// testBoard;;
// viewAsRows testBoard;;

// 23
let newBoard =

    let newCluster =
        Cluster
            (   EmptyCell,  EmptyCell,  EmptyCell
            ,   EmptyCell,  EmptyCell,  EmptyCell
            ,   EmptyCell,  EmptyCell,  EmptyCell
            )
    
    Blocks
        (   newCluster, newCluster, newCluster
        ,   newCluster, newCluster, newCluster
        ,   newCluster, newCluster, newCluster
        )

// 24
let viewAsColumns board =
    (function
        |Columns _ -> board
        |Blocks (a, b, c, d, e, f, g, h, i) -> (*2*)
            Columns
                (   straightenBlocks a d g first    fourth  seventh
                ,   straightenBlocks a d g second   fifth   eighth
                ,   straightenBlocks a d g third    sixth   ninth

                ,   straightenBlocks b e h first    fourth  seventh
                ,   straightenBlocks b e h second   fifth   eighth
                ,   straightenBlocks b e h third    sixth   ninth

                ,   straightenBlocks c f i first    fourth  seventh
                ,   straightenBlocks c f i second   fifth   eighth
                ,   straightenBlocks c f i third    sixth   ninth
                )
        |Rows (a, b, c, d, e, f, g, h, i) ->
            Columns
                (   Cluster <| mapNine first    (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine second   (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine third    (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine fourth   (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine fifth    (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine sixth    (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine seventh  (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine eighth   (a, b, c, d, e, f, g, h, i)
                ,   Cluster <| mapNine ninth    (a, b, c, d, e, f, g, h, i)
                )
    )board

// 25
let viewAsBlocks board = 
    let rec viewAsBlocksReal boardReal = 
        (function
        |Blocks _ -> board
        |Columns _ -> viewAsBlocksReal (viewAsRows boardReal)
        |Rows (a, b, c, d, e, f, g, h, i) ->
            Blocks
                (   straightenBlocks a b c first    second  third
                ,   straightenBlocks a b c fourth   fifth   sixth      
                ,   straightenBlocks a b c seventh  eighth  ninth

                ,   straightenBlocks d e f first    second  third
                ,   straightenBlocks d e f fourth   fifth   sixth
                ,   straightenBlocks d e f seventh  eighth  ninth

                ,   straightenBlocks g h i first    second  third
                ,   straightenBlocks g h i fourth   fifth   sixth
                ,   straightenBlocks g h i seventh  eighth  ninth
                )
        )boardReal
    viewAsBlocksReal board
// 26
let addLeft digit cell = 
    (function
        |EmptyCell                              -> Definitely   digit
        |FixedValue a                           -> cell
        |Definitely a                           -> MaybeTwo     (digit, a)
        |MaybeTwo   (a, b)                      -> MaybeThree   (digit, a, b)
        |MaybeThree (a, b, c)                   -> MaybeFour    (digit, a, b, c)
        |MaybeFour  (a, b, c, d)                -> MaybeFive    (digit, a, b, c, d)
        |MaybeFive  (a, b, c, d, e)             -> MaybeSix     (digit, a, b, c, d, e)
        |MaybeSix   (a, b, c, d, e, f)          -> MaybeSeven   (digit, a, b, c, d, e, f)
        |MaybeSeven (a, b, c, d, e, f, g)       -> MaybeEight   (digit, a, b, c, d, e, f, g)
        |MaybeEight (a, b, c, d, e, f, g, h)    -> MaybeNine    (digit, a, b, c, d, e, f, g, h)
        |MaybeNine _ -> cell
    )cell

// 27
let trimLeft cell =
    (function
        |EmptyCell                              -> cell
        |FixedValue _                           -> cell
        |Definitely _                           -> EmptyCell
        |MaybeTwo   (_, a)                      -> Definitely   a
        |MaybeThree (_, a, b)                   -> MaybeTwo     (a, b)
        |MaybeFour  (_, a, b, c)                -> MaybeThree   (a, b, c)
        |MaybeFive  (_, a, b, c, d)             -> MaybeFour    (a, b, c, d)
        |MaybeSix   (_, a, b, c, d, e)          -> MaybeFive    (a, b, c, d, e)
        |MaybeSeven (_, a, b, c, d, e, f)       -> MaybeSix     (a, b, c, d, e, f)
        |MaybeEight (_, a, b, c, d, e, f, g)    -> MaybeSeven   (a, b, c, d, e, f, g)
        |MaybeNine  (_, a, b, c, d, e, f, g, h) -> MaybeEight   (a, b, c, d, e, f, g, h)
    )cell

// 28
type CouldExist<'something> =
    | Value of 'something
    | Nothing

let peek cell =
    (function
        |EmptyCell                              -> Nothing
        |FixedValue x                           -> Value x
        |Definitely x                           -> Value x
        |MaybeTwo   (x, _)                      -> Value x
        |MaybeThree (x, _, _)                   -> Value x
        |MaybeFour  (x, _, _, _)                -> Value x
        |MaybeFive  (x, _, _, _, _)             -> Value x
        |MaybeSix   (x, _, _, _, _, _)          -> Value x
        |MaybeSeven (x, _, _, _, _, _, _)       -> Value x
        |MaybeEight (x, _, _, _, _, _, _, _)    -> Value x
        |MaybeNine  (x, _, _, _, _, _, _, _, _) -> Value x
    )cell

// 29
let andThen f =
    function
    | Nothing           -> Nothing
    | Value something   -> Value (f something)

// 30
let orElse defaultValue =
    function
    | Nothing           -> defaultValue
    | Value whatever    -> whatever


// 31
let exists x cell =
    let rec realExists remaining =
        peek remaining
        |> andThen (fun v -> v = x || realExists (trimLeft remaining))
        |> orElse false
    (function
        | FixedValue v  -> v = x
        | _             -> realExists cell
    )cell

// 32
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
        | FixedValue _  -> cell
        | _             -> doInsert cell
    )cell

// 33
let remove digit cell =
    let rec removeMatch remaining = 
        peek remaining
        |> andThen
            (fun item -> 
                if item = digit then//skip it
                    trimLeft remaining
                else
                    addLeft item (removeMatch (trimLeft remaining))
            )
        |> orElse cell
    (function
        | FixedValue _      -> cell
        | _                 -> removeMatch cell
    )cell

// 34
let toggle digit cell =
    if exists digit cell then
        remove digit cell
    else
        insert digit cell

// 35
let rec withCell board col row func =
    let withCluster (Cluster(a, b, c, d, e, f, g, h, i)) =
        if      row = 0 then    Cluster (func a, b, c, d, e, f, g, h, i)
        elif    row = 1 then    Cluster (a, func b, c, d, e, f, g, h, i)
        elif    row = 2 then    Cluster (a, b, func c, d, e, f, g, h, i)
        elif    row = 3 then    Cluster (a, b, c, func d, e, f, g, h, i)
        elif    row = 4 then    Cluster (a, b, c, d, func e, f, g, h, i)
        elif    row = 5 then    Cluster (a, b, c, d, e, func f, g, h, i)
        elif    row = 6 then    Cluster (a, b, c, d, e, f, func g, h, i)
        elif    row = 7 then    Cluster (a, b, c, d, e, f, g, func h, i)
        elif    row = 8 then    Cluster (a, b, c, d, e, f, g, h, func i)
        else    Cluster (a, b, c, d, e, f, g, h, i)
    (function
    |Rows _                                 -> withCell (viewAsColumns board) col row func
    |Blocks _                               -> withCell (viewAsColumns board) col row func
    |Columns (a, b, c, d, e, f, g, h, i)    ->
        if      col = 0 then    Columns (withCluster a, b, c, d, e, f, g, h, i)
        elif    col = 1 then    Columns (a, withCluster b, c, d, e, f, g, h, i)
        elif    col = 2 then    Columns (a, b, withCluster c, d, e, f, g, h, i)
        elif    col = 3 then    Columns (a, b, c, withCluster d, e, f, g, h, i)
        elif    col = 4 then    Columns (a, b, c, d, withCluster e, f, g, h, i)
        elif    col = 5 then    Columns (a, b, c, d, e, withCluster f, g, h, i)
        elif    col = 6 then    Columns (a, b, c, d, e, f, withCluster g, h, i)
        elif    col = 7 then    Columns (a, b, c, d, e, f, g, withCluster h, i)
        elif    col = 8 then    Columns (a, b, c, d, e, f, g, h, withCluster i)
        else board
    )board
// 37
let handleDirection (col, row) board action =
    let min a b = if a < b then a else b
    let max a b = if a > b then a else b
    (function
    |Left           -> ((max 0 (col - 1)    , row)              , board)
    |Right          -> ((min 8 (col + 1)    , row)              , board)
    |Up             -> ((col                , max 0 (row - 1))  , board)
    |Down           -> ((col                , min 8 (row + 1))  , board)
    |_              -> ((col                , row)              , board)
    )action
// 38
let fixDigit n col row board =
    withCell board col row (fun cellThing -> if exists n (cellThing) then EmptyCell else FixedValue n)

// 39
let considerDigit n col row board =
    withCell board col row (fun c -> toggle n c)

(*
    HOMEWORK
*)
// the next two functions are used to test the individual functions necessary for the homework
let testCluster = 
    Cluster
            (   Definitely One,     Definitely Two,     Definitely Three
            ,   Definitely Four,    Definitely Five,    Definitely Six
            ,   Definitely Seven,   Definitely Eight,   Definitely Nine
            )

let correctBoard = 
    Blocks
        (   
        // Top row of Blocks
            Cluster (   FixedValue Six      , FixedValue One    , FixedValue Eight  
                    ,   FixedValue Nine     , FixedValue Five   , FixedValue Two    
                    ,   FixedValue Four     , FixedValue Seven  , FixedValue Three  
                    )
        ,   Cluster (   FixedValue Five     , FixedValue Nine   , FixedValue Three  
                    ,   FixedValue Four     , FixedValue One    , FixedValue Seven  
                    ,   FixedValue Two      , FixedValue Six    , FixedValue Eight  
                    )
        ,   Cluster (   FixedValue Four     , FixedValue Two    , FixedValue Seven  
                    ,   FixedValue Eight    , FixedValue Six    , FixedValue Three  
                    ,   FixedValue One      , FixedValue Nine   , FixedValue Five   
                    )
        // Middle row of Blocks
        ,   Cluster (   FixedValue Eight    , FixedValue Six    , FixedValue One    
                    ,   FixedValue Five     , FixedValue Two    , FixedValue Four
                    ,   FixedValue Three    , FixedValue Nine   , FixedValue Seven  
                    )
        ,   Cluster (   FixedValue Three    , FixedValue Five   , FixedValue Nine   
                    ,   FixedValue Six      , FixedValue Seven  , FixedValue One
                    ,   FixedValue Eight    , FixedValue Two    , FixedValue Four   
                    )
        ,   Cluster (   FixedValue Seven    , FixedValue Four   , FixedValue Two    
                    ,   FixedValue Three    , FixedValue Eight  , FixedValue Nine
                    ,   FixedValue Five     , FixedValue One    , FixedValue Six    
                    )
        // Last row of Blocks
        ,   Cluster (   FixedValue Two      , FixedValue Eight  , FixedValue Six
                    ,   FixedValue One      , FixedValue Three  , FixedValue Five
                    ,   FixedValue Seven    , FixedValue Four   , FixedValue Nine
                    )
        ,   Cluster (   FixedValue Seven    , FixedValue Four   , FixedValue Five
                    ,   FixedValue Nine     , FixedValue Eight  , FixedValue Two
                    ,   FixedValue One      , FixedValue Three  , FixedValue Six
                    )
        ,   Cluster (   FixedValue Nine     , FixedValue Three  , FixedValue One
                    ,   FixedValue Six      , FixedValue Seven  , FixedValue Four
                    ,   FixedValue Two      , FixedValue Five   , EmptyCell
                    )
        )

let clustersOfBoard board = 
    // this will return just the data and not the wrapper of the type sent to it.
    // converts rows (whatever) to just (whatever)
    (function
    | Rows      (a, b, c, d, e, f, g, h, i) -> (a, b, c, d, e, f, g, h, i)
    | Columns   (a, b, c, d, e, f, g, h, i) -> (a, b, c, d, e, f, g, h, i)
    | Blocks    (a, b, c, d, e, f, g, h, i) -> (a, b, c, d, e, f, g, h, i)
    )board

(*
    V1
        (function
        | (1, 1, 1, 1, 1, 1, 1, 1, 1)   -> (a, b, c, d, e, f, g, h, i)
        | (0, _, _, _, _, _, _, _, _)   -> fold (fn a, b, c, d, e, f, g, h, i)
        | (1, 0, _, _, _, _, _, _, _)   -> fold (a, fn b, c, d, e, f, g, h, i)
        | (1, 1, 0, _, _, _, _, _, _)   -> fold (a, b, fn c, d, e, f, g, h, i)
        | (1, 1, 1, 0, _, _, _, _, _)   -> fold (a, b, c, fn d, e, f, g, h, i)
        | (1, 1, 1, 1, 0, _, _, _, _)   -> fold (a, b, c, d, fn e, f, g, h, i)
        | (1, 1, 1, 1, 1, 0, _, _, _)   -> fold (a, b, c, d, e, fn f, g, h, i)
        | (1, 1, 1, 1, 1, 1, 0, _, _)   -> fold (a, b, c, d, e, f, fn g, h, i)
        | (1, 1, 1, 1, 1, 1, 1, 0, _)   -> fold (a, b, c, d, e, f, g, fn h, i)
        | (1, 1, 1, 1, 1, 1, 1, 1, 0)   -> fold (a, b, c, d, e, f, g, h, fn i)
        | (_, _, _, _, _, _, _, _, _)   -> (a, b, c, d, e, f, g, h, i)
        )start
    V2
        (function
        | (1, 1, 1, 1, 1, 1, 1, 1, 1)   -> (a, b, c, d, e, f, g, h, i)
        | (0, _, _, _, _, _, _, _, _)   -> fold (fn a (a, b, c, d, e, f, g, h, i))
        | (1, 0, _, _, _, _, _, _, _)   -> fold (fn b (a, b, c, d, e, f, g, h, i))
        | (1, 1, 0, _, _, _, _, _, _)   -> fold (fn c (a, b, c, d, e, f, g, h, i))
        | (1, 1, 1, 0, _, _, _, _, _)   -> fold (fn d (a, b, c, d, e, f, g, h, i))
        | (1, 1, 1, 1, 0, _, _, _, _)   -> fold (fn e (a, b, c, d, e, f, g, h, i))
        | (1, 1, 1, 1, 1, 0, _, _, _)   -> fold (fn f (a, b, c, d, e, f, g, h, i))
        | (1, 1, 1, 1, 1, 1, 0, _, _)   -> fold (fn g (a, b, c, d, e, f, g, h, i))
        | (1, 1, 1, 1, 1, 1, 1, 0, _)   -> fold (fn h (a, b, c, d, e, f, g, h, i))
        | (1, 1, 1, 1, 1, 1, 1, 1, 0)   -> fold (fn i (a, b, c, d, e, f, g, h, i))
        //| (_, _, _, _, _, _, _, _, _)   -> (a, b, c, d, e, f, g, h, i)
        )inputCluster
    V3
        let fold inputCluster accum =
            if ((getFirstOfTwoTuple) accum) = false && (getSecondOfTwoTuple accum) |> fun (a, b, c, d, e, f, g, h, i) -> if a>1||b>1||c>1||d>1||e>1||f>1||g>1||h>1||i>1 then false else true then
                start
            else
                accum = 
    V4
        // let getFirstOfTwoTuple  = fun (a, _) -> a
        // let getSecondOfTwoTuple = fun (_, b) -> b
        // start is the value that is the initial that than is rebound to a new value in the recursive calls that indicate its validity
        // the 9-tuple in the input is 
        // (((fn a start), (fn b start), (fn c start), (fn d start), (fn e start), (fn f start), (fn g start), (fn h start), (fn i start)))
        // |>fun ( 
        //         (aPred, (aOne, _     , _     , _     , _     , _     , _     , _     , _))
        //     ,   (bPred, (_   , bTwo  , _     , _     , _     , _     , _     , _     , _))
        //     ,   (cPred, (_   , _     , cThree,    _  , _     , _     , _     , _     , _))
        //     ,   (dPred, (_   , _     , _     , dFour , _     , _     , _     , _     , _))
        //     ,   (ePred, (_   , _     , _     , _     , eFive , _     , _     , _     , _))
        //     ,   (fPred, (_   , _     , _     , _     , _     , fSix  , _     , _     , _))
        //     ,   (gPred, (_   , _     , _     , _     , _     , _     , gSeven, _     , _))
        //     ,   (hPred, (_   , _     , _     , _     , _     , _     , _     , hEight, _))
        //     ,   (iPred, (_   , _     , _     , _     , _     , _     , _     , _     , iNine))) -> 
        //     (aPred&&bPred&&cPred&&dPred&&ePred&&fPred&&gPred&&hPred&&iPred, (aOne, bTwo, cThree, dFour, eFive , fSix, gSeven, hEight, iNine))
        //need to build the answer up from the individual ones we get
        // put your fold code here mate
        // fun item (definiteCells, counts) -> (definiteCells && isDefiniteCell item, addToHistogram counts ( peek item ))
            // this function takes in an item (cell) and a 
            // tuple of (a boolean that confirms all the entered things are indeed cells, 
            // and the second part is a nine-tuple counts the instances of each number in the cluster)
            // this function returns a tuple that is equal to (true, (1, 1, 1, 1, 1, 1, 1, 1, 1)) or not
            // 
*)
let foldNine fn start (a, b, c, d, e, f, g, h, i) =
    (fn a (fn b (fn c (fn d (fn e (fn f (fn g (fn h (fn i start)))))))))
    (*
        fn a start |> fn b |> 
    *)
let checkCluster (Cluster (a, b, c, d, e, f, g, h, i)) =
    let isDefiniteCell =
        function
        | Definitely _  -> true
        | FixedValue _  -> true
        | _             -> false
    let addToHistogram (a, b, c, d, e, f, g, h, i) =
        (function
        | Nothing       -> (a   , b     , c     , d     , e     , f     , g     , h     , i   )(*SEE NOTHING, DO NOTHING*)
        | Value One     -> (a+1 , b     , c     , d     , e     , f     , g     , h     , i   )
        | Value Two     -> (a   , b+1   , c     , d     , e     , f     , g     , h     , i   )
        | Value Three   -> (a   , b     , c+1   , d     , e     , f     , g     , h     , i   )
        | Value Four    -> (a   , b     , c     , d+1   , e     , f     , g     , h     , i   )
        | Value Five    -> (a   , b     , c     , d     , e+1   , f     , g     , h     , i   )
        | Value Six     -> (a   , b     , c     , d     , e     , f+1   , g     , h     , i   )
        | Value Seven   -> (a   , b     , c     , d     , e     , f     , g+1   , h     , i   )
        | Value Eight   -> (a   , b     , c     , d     , e     , f     , g     , h+1   , i   )
        | Value Nine    -> (a   , b     , c     , d     , e     , f     , g     , h     , i+1 )
        )
    (*LOOK HERE*)
    (foldNine 
        (fun item (definiteCells, counts) -> (definiteCells && isDefiniteCell item, addToHistogram counts ( peek item ))) // fn
        (true, (0, 0, 0, 0, 0, 0, 0, 0, 0)) // start
        (a, b, c, d, e, f, g, h, i) // (nine-tuple)
    ) 
    = (true, (1, 1, 1, 1, 1, 1, 1, 1, 1))

(*
    This function was made in an attempt to solve the fold in the checkGame function without actually itterating
    It contribute to the logic formed in the current fold function but was an important step to solving the problem
    let checkGameFold fn start (a, b, c, d, e, f, g, h, i) =
        // need to slip the item which is a cluster into its individual cells
        (*
            |>fun (
                (aPred, (   aOne    , bOne    , cOne    , dOne    , eOne    , fOne    , gOne    , hOne      , iOne   ))
            ,   (bPred, (   aTwo    , bTwo    , cTwo    , dTwo    , eTwo    , fTwo    , gTwo    , hTwo      , iTwo   ))
            ,   (cPred, (   aThree  , bThree  , cThree  , dThree  , eThree  , fThree  , gThree  , hThree    , iThree ))
            ,   (dPred, (   aFour   , bFour   , cFour   , dFour   , eFour   , fFour   , gFour   , hFour     , iFour  ))
            ,   (ePred, (   aFive   , bFive   , cFive   , dFive   , eFive   , fFive   , gFive   , hFive     , iFive  ))
            ,   (fPred, (   aSix    , bSix    , cSix    , dSix    , eSix    , fSix    , gSix    , hSix      , iSix   ))
            ,   (gPred, (   aSeven  , bSeven  , cSeven  , dSeven  , eSeven  , fSeven  , gSeven  , hSeven    , iSeven ))
            ,   (hPred, (   aEight  , bEight  , cEight  , dEight  , eEight  , fEight  , gEight  , hEight    , iEight ))
            ,   (iPred, (   aNine   , bNine   , cNine   , dNine   , eNine   , fNine   , gNine   , hNine     , iNine  ))) -> 
        *)    
        if ((checkCluster a && checkCluster b && checkCluster c && checkCluster d && checkCluster e && checkCluster f && checkCluster g && checkCluster h && checkCluster i)) then
            start + 9
        else
            start
*)
let checkGame board =
    // clustersOfBoard board
    (*|>*) (foldNine (fun item state -> if checkCluster item then state+1 else state) 0 (clustersOfBoard board)) = 9

let gameComplete board =
    checkGame (viewAsBlocks board) && checkGame (viewAsRows board) && checkGame (viewAsColumns board)

// 36 
let rec evaluate io game =
    let UserAction = io game
    let g =
        (function
        |(StartGame , SettingUp (position, board))          -> Solving (position, board)
        |(StartGame, Solved _)                              -> SettingUp ((0, 0), newBoard)
        |(_, Solved _)                                      -> game
        |(EnterDigit n , SettingUp ((col, row), board))     -> SettingUp ((col, row) , fixDigit n col row board)     
        |(EnterDigit n , Solving ((col, row), board))       -> if (gameComplete board) then Solved board else Solving ((col, row), considerDigit n col row board)
        |(direction, SettingUp (position, board))           -> SettingUp (handleDirection position board direction)
        |(direction, Solving (position, board))             -> if (gameComplete board) then Solved board else Solving (handleDirection position board direction)
        ) (UserAction, game)
    if UserAction = Quit then
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
    let (Rows (a, b, c, d, e, f, g, h, i)) = viewAsRows board
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
    // evaluate (output >> input) (SettingUp ((0,0), newBoard))
    evaluate (output >> input) (SettingUp ((0,0), correctBoard)) // this done in order to test the program easier
    // the correctBoard is missing an Eight in the bottom right block
    0
// Notes:
(*1*)
        (*
            Rows
                (   Cluster
                        (   first a, second a, third a
                        ,   first b, second b, third b
                        ,   first c, second c, third c
                        )
                ,Cluster
                        (   fourth a, fifth a, sixth a
                        ,   fourth b, fifth b, sixth b
                        ,   fourth c, fifth c, sixth c
                        )
            )//the above clusters are to make the large squares from different rows but we noticed a pattern and can create a change
            // this was replaced as the pattern was discovered and we therefore created a straightenBlocks function that handles this repetition for us
        *)
