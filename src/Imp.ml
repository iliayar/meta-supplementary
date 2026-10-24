open Ostap
open GT

module Value =
  struct
    
    @type t =
    | Int   of int
    | Array of t array
    | Dyn
    with show

    let toPrintable v =
      let buf   = Buffer.create 256 in
      let put s = Buffer.add_string buf s in
      let rec inner = function
      | Int   n -> put @@ string_of_int n
      | Array a ->
         put "[";
         ignore @@ Array.fold_left (fun first v -> if first then inner v else (put ", "; inner v); false) true a;
         put "]"

      | Dyn -> put "?"
      in
      inner v;
      Buffer.contents buf
      
    module Printable =
      struct
        @type t = string with show
      end
    
    let rec join v1 v2 =
      match v1, v2 with
      | Int x , Int y when x = y ->
         Int x
      | Array xs, Array ys when Array.length xs = Array.length ys ->
         Array (Array.init (Array.length xs) (fun i -> join xs.(i) ys.(i)))
      | _ -> Dyn

    let (!?) = function
    | Int n -> n
    | v     -> invalid_arg (Printf.sprintf "integer value expected in evalBinop but %s is provided" (show(Printable.t) @@ toPrintable v))
        
    let int   n = Int n 
    let array a = Array a
    
    let checkArray a cont =
      match a with
      | Array v -> cont v
      | _       -> invalid_arg (Printf.sprintf "array value expected in length but %s is provided" (show(Printable.t) @@ toPrintable a))

    let checkIndex a i cont =
      let len = Array.length a in
      if i >= 0 && i < len
      then cont ()
      else invalid_arg (Printf.sprintf "array index %d is out of bounds [0..%d)" i len)
    
    let length a     = int @@ checkArray a Array.length    
    let elem   a i   = checkArray a (fun a -> a.(!? i))
    let subst  a ivs =
      checkArray a
        (fun orig ->
           let orig' = Array.copy orig in
           List.iter
             (fun (i, v) ->
                let i = !? i in               
                checkIndex orig' i (fun _ -> orig'.(i) <- v);
             ) ivs;
           Array orig'
        )
    
  end

module State =
  struct
    
    module M = Map.Make (String)
      
    type t = Value.t M.t
    
    let toPrintable s = List.map (fun (name, v) -> name, Value.toPrintable v)(M.bindings s)
  
    let empty              = M.empty
    let set   m name value = M.add name value m
    let unset m name       = M.remove name m
    
    let peep state x =
      match M.find_opt x state with
      | Some v -> Printf.printf "%s=%s\n" x (show(Value.Printable.t) @@ Value.toPrintable v)
      | None   -> Printf.printf "%s undefined\n" x

    let residualize r z (state : t) =
      List.fold_left
        (fun acc (x, v) -> r acc x v)
        z
        @@ List.filter (function (_, Value.Dyn) -> false | _ -> true) (M.bindings state)

    let join lstate rstate =
      M.merge
        (fun key l r -> match l, r with
         | Some lv, Some rv -> Some (Value.join lv rv)
         | None   , None    -> None
         | _, _             -> Some Value.Dyn
        ) lstate rstate

    let diff lstate rstate =
      M.merge
        (fun key l r -> match l, r with
         | None, _        -> None
         | l, None        -> l
         | Some (Value.Array l), Some (Value.Array r) when Array.length l = Array.length r ->
              Some (Value.Array (Array.map2 
                  (fun l r -> match l, r with
                              | l', Value.Dyn -> l'
                              | _, _ -> Value.Dyn) l r))
         | Some _, Some _ -> Some Value.Dyn
        ) lstate rstate
    
    exception Undefined_variable of string
    
    let get m name =
      match M.find_opt name m with
      | Some x -> x
      | None   -> raise (Undefined_variable name)

    let equal (l : t) (r : t) = M.equal (=) l r
    
    let dynamic m name =
      match M.find_opt name m with
      | None           -> true
      | Some Value.Dyn -> true
      | Some _         -> false
    
    module Printable =
      struct
        @type t = (string * Value.Printable.t) list with show
      end

    let parse s =
      let ostap (
        value:
          n:DECIMAL                      {Value.Int n}
        | "?"                            {Value.Dyn}
        | "[" vs:!(Util.list)[value] "]" {Value.Array (Array.of_list vs)}
      )
      in      
      Util.parse
        (object (self: 'self)
           inherit Matcher.t s
           inherit Util.Lexers.decimal s
           inherit Util.Lexers.lident [] s 
           inherit Util.Lexers.skip [
                       Matcher.Skip.whitespaces " \t\n\r";
                   ] s
         end) 
        (ostap (
             bs:!(Util.list)[ostap (LIDENT -"=" value)] EOF {
                 List.fold_left (fun st (x, v) -> set st x v) empty bs 
             }      
        ))
  
  end

module Expr =
  struct
  
    @type t =
      | Var    of string
      | Const  of int    
      | Binop  of string * t * t
      | Array  of t array
      | Elem   of t * t
      | Length of t
      | Subst  of t * (t * t) list
    with show
    
    @type assoc = No | Left | Right with show
    
    let prio = function
    | "!!" -> 1
    | "&&" -> 2
    | "==" -> 3
    | "!=" -> 3
    | "<=" -> 3
    | "<"  -> 3
    | ">=" -> 3
    | ">"  -> 3
    | "+"  -> 4
    | "-"  -> 4
    | "*"  -> 5
    | "/"  -> 5
    | "%"  -> 5
    | _    -> 0
    
    let assoc = function
    | "!!" -> Left
    | "&&" -> Left
    | "==" -> No
    | "!=" -> No
    | "<=" -> No
    | "<"  -> No
    | ">=" -> No
    | ">"  -> No
    | "+"  -> Left
    | "-"  -> Left
    | "*"  -> Left
    | "/"  -> Left
    | "%"  -> Left
    | _    -> No
  
    let pretty expr =
      let open Formeat in
      let rec pretty (context, position) = function 
      | Var    x         -> string x
      | Const  n         -> string @@ string_of_int n
      | Binop (op, l, r) ->
         let lp = pretty (op, Left ) l in
         let rp = pretty (op, Right) r in
         let base = hovbox @@
                      listBy space [lp; string op; rp]
         in
         if prio op < prio context then rbr base
         else if prio op = prio context && assoc op <> position then rbr base
         else base       
      | Array arr -> listBy (string "")
        [ string "[";
          listBy (string ", ") @@ Array.to_list @@ Array.map (pretty ("", No)) arr;
          string "]" 
        ]
      | Subst (x, substs) -> 
        listBy (string "") @@
          [ string "[";
            pretty ("", No) x;
            string " with ";
            listBy (string ", ") @@ List.map 
             (fun (i, v) -> listBy (string "") 
               [ pretty ("", No) i;
                 string "=";
                 pretty ("", No) v
               ]) substs;
            string "]"
          ]
      | Elem (x, i) ->
        listBy (string "") @@
          [ pretty ("", No) x;
            string "[";
            pretty ("", No) i;
            string "]"
          ]
      | Length x -> listBy (string "") [pretty ("", No) x; string ".length"]
      in
      pretty ("", No) expr
    
    let evalBinop xv yv =
      let open Value in
      let xv, yv = !? xv, !? yv in
      let i2b x = if x <> 0 then 1 else 0     in
      let b2i   = function true -> 1 | _ -> 0 in
      function
      | "!!" -> int @@ i2b (i2b xv + i2b yv)
      | "&&" -> int @@ i2b (i2b xv * i2b yv)
      | "==" -> int @@ b2i (xv =  yv)
      | "!=" -> int @@ b2i (xv <> yv)
      | "<=" -> int @@ b2i (xv <= yv)
      | "<"  -> int @@ b2i (xv <  yv)
      | ">=" -> int @@ b2i (xv >= yv)
      | ">"  -> int @@ b2i (xv >  yv)
      | "+"  -> int @@ xv + yv
      | "-"  -> int @@ xv - yv
      | "*"  -> int @@ xv * yv
      | "/"  -> int @@ xv / yv
      | "%"  -> int @@ xv mod yv
    
    let rec eval state = function
    | Var   x -> State.get state x
    | Const x -> Value.Int x
    | Binop (op, x, y) ->
       evalBinop (eval state x) (eval state y) op
    | Length a        -> Value.length @@ eval state a
    | Elem  (a, i)    -> Value.elem  (eval state a) (eval state i)
    | Subst (a, ivs)  -> Value.subst (eval state a) (List.map (fun (i, v) -> eval state i, eval state v) ivs)
    | Array es        -> Value.Array (Array.map (eval state) es)
       
    let rec spec (state : State.t) =
      let open Value in
      function
      | Var x as expr    ->
        if State.dynamic state x then expr
        else
          (match State.get state x with
           | Value.Int n   -> Const n
           | Value.Array l ->
              Array (Array.mapi
                      (fun i -> function
                                | Value.Array _ -> failwith "Multidemsional arrays no supported"
                                | Value.Int n   -> Const n
                                | Value.Dyn     -> Elem (expr, Const i))
                       l)
           | _ -> failwith "unreachable"
          )
      | Const x as expr  -> expr
      | Binop (op, x, y) ->
        let x', y' = spec state x, spec state y in
        (match x', y' with
         | Const xv, Const yv -> Const !?(evalBinop (Value.Int xv) (Value.Int yv) op)
         | _, _               -> Binop (op, x', y')
        )
      | Subst (Var a, substs) -> 
        let substs' = List.map (fun (ie, ve) -> (spec state ie, spec state ve)) substs in
        let substs_static = List.filter_map (fun (i, v) -> match i with Const iv -> Some (iv, v) | _ -> None ) substs' in
        if List.length substs' <> List.length substs_static || State.dynamic state a then Subst (Var a, substs')
        else (match State.get state a with
              | Value.Int _     -> failwith "unreachable"
              | Value.Dyn       -> Subst (Var a, substs')
              | Value.Array arr -> 
                Array (Array.mapi
                        (fun i e -> match List.assoc_opt i substs_static with
                                    | Some v -> v
                                    | None   ->
                                      match e with
                                      | Value.Dyn     -> Elem (Var a, Const i)
                                      | Value.Int v   -> Const v
                                      | Value.Array _ -> failwith "unreachable")
                        arr)
             )
      | Subst _ -> failwith "only Subst with specific var is supported"
      | Elem (Var a, i) -> 
        (match spec state i with
         | Const i' ->
           if State.dynamic state a then Elem (Var a, Const i')
           else
             (match State.get state a with
              | Value.Dyn       -> Elem (Var a, Const i')
              | Value.Int _     -> failwith "unreachable"
              | Value.Array arr ->
                match arr.(i') with
                | Value.Dyn     -> Elem (Var a, Const i')
                | Value.Array _ -> failwith "Multidemsional arrays no supported"
                | Value.Int v   -> Const v
             )
         | i' -> Elem (Var a, i)
        )
      | Elem _ -> failwith "only Elem with specific var is supported"
      | Length (Var a) ->
        if State.dynamic state a then Length (Var a)
        else
          (match State.get state a with
           | Value.Dyn       -> Length (Var a)
           | Value.Int _     -> failwith "unreachable"
           | Value.Array arr -> Const (Array.length arr)
          )
      | Length _ -> failwith "only Length with specific var is supported"
  end

module Stmt =
  struct

    @type t =
    | Assn  of string * Expr.t
    | Seq   of t * t
    | Skip
    | If    of Expr.t * t * t
    | While of Expr.t * t
    | Do    of t * Expr.t
    with show

    let rec pretty stmt =
      let open Formeat in
      match stmt with
      | Assn (x, e) ->
         hovbox @@
           listBy space [string x; string ":="; Expr.pretty e]
      | Seq (l, r) ->
         vbox @@
           listBy
             (string ";" |> space)
             [pretty l; pretty r]  
      | Skip -> string "skip"
      | If (c, t, e) ->
         hovbox @@
           listBy space
             [string "if";
              rbr @@ Expr.pretty c;
              cvbr @@ pretty t;
              string "else";
              cvbr @@ pretty e]
      | While (c, b) ->
         hovbox @@
           listBy space
             [string "while";
              rbr @@ Expr.pretty c;
              cvbr @@ pretty b]            
      | Do (b, c) ->
         hovbox @@
           listBy space
             [string "do";
              pretty b;
              string "while";
              rbr @@ Expr.pretty c]
    
    let rec eval state =
      let open Value in
      function
      | Assn (x, e)  -> State.set state x (Expr.eval state e)
      | Seq  (l, r)  -> eval (eval state l) r
      | Skip         -> state
      | If (c, t, e) -> eval state (if !?(Expr.eval state c) <> 0 then t else e)
      | While (c, b) as w ->
         if !?(Expr.eval state c) = 0
         then state
         else eval (eval state b) w                 
      | Do (b, c) as w -> eval state (Seq (b, While (c, b)))      

    let residualize ?(before=false) state stmt =
      let single = fun x -> function
        | Value.Int v     -> Assn (x, Const v)
        | Value.Array arr ->
          let assigns = List.filter_map (fun x -> x) @@ Array.to_list @@ Array.mapi
            (fun i -> function
             | Value.Int v   -> Some (Expr.Const i, Expr.Const v)
             | Value.Array _ -> failwith "Multidemsional arrays not supported"
             | Value.Dyn     -> None
            ) arr
          in if List.length assigns = 0 then Skip else Assn (x, Subst (Expr.Var x, assigns))
        | Value.Dyn -> failwith "unreachable"
      in
      State.residualize
        (function
         | Skip -> single
         | acc  -> fun x v ->
           (match single x v with
            | Skip -> acc
            | stmt -> if before then Seq (stmt, acc) else Seq (acc, stmt)
           )
           
        ) stmt state

    let rec fixpoint eq f init =
        let init' = f init in
        if eq init init' 
        then init'
        else fixpoint eq f init'
                       
    let rec spec (state : State.t) = 
      function
      | Skip        -> Skip, state
      | Assn (x, e) ->
        let e' = Expr.spec state e in
        (match e' with
         | Expr.Const ve  -> Skip, State.set state x (Value.Int ve)
         | Expr.Array arr -> 
            let arr' = Array.map (function Expr.Const v -> Value.Int v | _ -> Value.Dyn) arr in
            let substs = Array.mapi (fun i -> function 
                                             | Expr.Const _ -> None 
                                             | Expr.Elem (Var x', Expr.Const j) when x = x' && i = j -> None
                                             | Expr.Elem (Var _, e) -> Some (Expr.Const i, e)
                                             | Expr.Elem _ -> failwith "only Elem with specific var is supported"
                                             | e -> Some (Expr.Const i, e)) arr in
            let substs' = List.filter_map (fun x -> x) @@ Array.to_list substs in
            let stmt = 
              if List.length substs' = 0 then Skip
              else Assn (x, Expr.Subst (Expr.Var x, substs'))
            in
            stmt, State.set state x (Value.Array arr')
         | _             -> Assn (x, e'), State.unset state x
        )
      | Seq (s1, s2) ->
        (match spec state s1 with
         | Skip, state' -> spec state' s2
         | s1' , state' ->
            (match spec state' s2 with
             | Skip, state'' -> s1', state''
             | s2' , state'' -> Seq (s1', s2'), state''
            )
        )
      | If (c, t, e) ->
        (match Expr.spec state c with
         | Expr.Const cv when cv <> 0 -> spec state t
         | Expr.Const _               -> spec state e
         | c'                         ->
           let t', t_state = spec state t in
           let e', e_state = spec state e in
           let state' = State.join t_state e_state in
           let t'' = residualize (State.diff t_state state') t' in
           let e'' = residualize (State.diff e_state state') e' in
           (If (c', t'', e'')), state'
        )
      | While (c, s) as w ->
        (match Expr.spec state c with
         | Expr.Const cv when cv <> 0 -> spec state (Seq (s, w))
         | Expr.Const cv              -> (Skip, state)
         | _                          -> 
           let state' = fixpoint State.equal
              (fun state -> 
                let _, state' = spec state s in
                State.join state state'
              ) state
           in
           let s', state_s = spec state' s in
           let c' = Expr.spec state' c in
           let res = While (c', residualize (State.diff state_s state') s') in
           let res = residualize ~before:true (State.diff state state') res in
           (res, state')
        )
      | Do (s, c) -> spec state (Seq (s, While (c, s)))
  end

module Parser =
  struct

    let binop =
      ostap (
          s:("!!" | "&&" | "==" | "!=" | "<=" | "<" | ">=" | ">" |
             "+"  | "-"  | "*"  | "/"  | "%") {Matcher.Token.repr s} 
        )
      
    let rec expression s =
      let binop op x y = Expr.Binop (op, x, y) in
      let ops = Array.map
                  (fun (assoc, list) ->
                    assoc,           
                    List.map (fun op -> ostap ($(op)), binop op) list
                  )
                  [|
                    `Lefta , ["!!"]; 
                    `Lefta , ["&&"];
                    `Nona  , ["=="; "!="; "<="; "<"; ">="; ">"];
                    `Lefta , ["+" ; "-"];
                    `Lefta , ["*" ; "/"; "%"]; 
                  |]
      in
      let rec postfix s = ostap (
                            base:primary ops:("." %"length" {`Length} | "[" i:expression "]" {`Elem i})* {
                              List.fold_left (fun acc -> function
                                                         | `Length -> Expr.Length acc
                                                         | `Elem i -> Expr.Elem (acc, i)
                                             ) base ops
                            }
                          ) s     
      and primary s = ostap (
                          x:LIDENT                                                                          {Expr.Var   x}
                      |   c:DECIMAL                                                                         {Expr.Const c}
                      | "[" a:!(Util.list)[expression] "]"                                                  {Expr.Array (Array.of_list a)}
                      | "[" base:expression "with" ivs:!(Util.list)[ostap (expression -"=" expression)] "]" {Expr.Subst (base, ivs)}
                      | -"(" expression -")" 
                      ) s
      in  
      Util.expr (fun x -> x) ops postfix s
    
    let rec stmt s =
      let ostap (
        primary:
          %"skip"                       ";"? {Stmt.Skip   }    
        | d:LIDENT "=" s:expression     ";"? {Stmt.Assn (d, s)}
        | d:LIDENT op:binop "=" s:expression ";"?         {Stmt.Assn (d, Expr.Binop (op, Expr.Var (d), s))}
        | %"while" "(" c:expression ")" s:stmt            {Stmt.While (c, s)}
        | %"do" s:stmt %"while" "(" c:expression ")" ";"? {Stmt.Do (s, c)}
        | %"for" "(" i:stmt c:expression ";" s:stmt ")" b:stmt {
            Stmt.Seq (i, Stmt.While (c, Stmt.Seq (b, s)))
          }
        | %"if" s:ifPart {s}      
        | -"{" seq -"}";
        ifPart: "(" c:expression ")" t:stmt e:elsePart? {
          Stmt.If (c, t, match e with None -> Stmt.Skip | Some s -> s)
        };
        elsePart: %"else" s:stmt {s} | %"elif" s:ifPart {s};
        seq: ss:primary+ {let rec fold = function
                                         | [h]     -> h
                                         | h :: tl -> Stmt.Seq (h, fold tl)
                          in
                          fold ss
                         }
      )
      in
      primary s
    
    let parse s =
      Util.parse
        (object (self: 'self)
           inherit Matcher.t s
           inherit Util.Lexers.decimal s
           inherit Util.Lexers.lident ["if"; "else"; "elif"; "while"; "do"; "for"; "skip"; "with"; "length"] s 
           inherit Util.Lexers.skip [
                       Matcher.Skip.whitespaces " \t\n\r";
                       Matcher.Skip.lineComment "--";
                       Matcher.Skip.nestedComment "(*" "*)"
                   ] s
       end)
      (ostap (stmt -EOF))
    
  end

module Setup =
  struct

    exception Invalid_arg of string
    
    type 'a ref = 'a Stdlib.ref

    let ref = Stdlib.ref
    
    let mode  : [`Run | `Spec | `None] ref = ref `None
    let infile: string ref  = ref ""
    let input : State.t ref = ref State.empty

    let init () =
      let n = Array.length Sys.argv - 1 in
      let rec parse i =
        if i <= n
        then 
          match Sys.argv.(i) with
          | "--run"   -> mode := `Run; parse (i+1)
          | "--spec"  -> mode := `Spec; parse (i+1)
          | "--input" ->
             if i+1 <= n then (input := (match State.parse Sys.argv.(i+1) with
                                         | `Ok st     -> st
                                         | `Fail errs -> raise (Invalid_arg (Printf.sprintf "error parsing input state: %s" errs))
                                        );
                               parse (i+2))
             else raise (Invalid_arg "input specification expected after --input")
          | other -> infile := other; parse (i+1)
      in
      parse 1

    let getFile () =
      match !infile with
      | ""   -> raise (Invalid_arg "input file not specified")
      | file -> file
      
    let getMode () =
      match !mode with
      | `None -> raise (Invalid_arg "running mode not specified")
      | mode  -> mode
    
    let getInput () = !input
      
  end

let _ =
  try
    Setup.init ();
    (match Parser.parse @@ Util.read (Setup.getFile ()) with
     | `Ok stmt ->
        (match Setup.getMode () with
         | `Run  -> Printf.printf "%s\n" @@ show (State.Printable.t) (State.toPrintable @@ Stmt.eval (Setup.getInput ()) stmt)
         | `Spec ->
            let (stmt', state') = Stmt.spec (Setup.getInput ()) stmt in
            let stmt'' = Stmt.residualize state' stmt' in
            Printf.printf "%s\n" @@ Formeat.to_string (Stmt.pretty stmt'')
        )
     | `Fail errs -> Printf.eprintf "ERROR: %s\n" errs
    )
  with
  | Setup.Invalid_arg        err -> Printf.eprintf "ERROR: %s\n" err
  | State.Undefined_variable x   -> Printf.eprintf "ERROR: undefined vriable \"%s\"\n" x
  | exn                          -> Printf.eprintf "%s\n" @@ Printexc.to_string exn
