open Ostap
open GT

module Value = struct
    @type t = Dynamic | Int of int | Array of t list
    with show

    let rec join l r = match l, r with
        | Array lv, Array rv when List.length lv == List.length rv
                                      -> Array (List.map2 join lv rv)
        | Int lv, Int rv when lv = rv -> Int lv
        | _, _                        -> Dynamic
end

module State =
struct
  module M = Map.Make (String)
      
  type t = Value.t M.t

  let toPrintable s = M.bindings s

  let empty = M.empty
  let set (m : t) (name : string) (value : Value.t) = M.add name value m
  let unset m name = M.remove name m

  exception Undefined_variable of string
      
  let get m name = M.find_opt name m

  let equal (l : t) (r : t) = M.equal (=) l r

  let residualize r z (state : t) =
    List.fold_left
      (fun acc (x, v) -> r acc x v)
      z
      @@ List.filter (function (_, Value.Dynamic) -> false | _ -> true) (M.bindings state)

  let join lstate rstate =
    M.merge
      (fun key l r -> match l, r with
       | Some lv, Some rv -> Some (Value.join lv rv)
       | None   , None    -> None
       | _, _             -> Some Value.Dynamic
      ) lstate rstate

  let subtract (lstate : t) (rstate : t) : t =
    M.merge
      (fun key l r -> match l, r with
       | None, _        -> None
       | l, None        -> l
       | Some (Value.Array l), Some (Value.Array r) when List.length l = List.length r ->
            Some (Value.Array (List.map2 
                (fun l r -> match l, r with
                            | l', Value.Dynamic -> l'
                            | _, _ -> Value.Dynamic) l r))
       | Some _, Some _ -> Some Value.Dynamic
      ) lstate rstate
  
  module Printable =
  struct
    @type t = (string * Value.t) list with show
  end

  let init_value s = 
      let ostap (
        expr       : vars:!(Util.list)[ostap (LIDENT -"=" value)] EOF {
                       List.fold_left (fun st (x, v) -> set st x v) empty vars
                     };
        atom       : x:DECIMAL { Value.Int x }
                   | "?" { Value.Dynamic };
        value      : atom
                   | "{" elems:!(Util.list)[atom] "}" { Value.Array elems }
      ) in expr s

  let parse s =
    Util.parse
      (object (self: 'self)
         inherit Matcher.t s
         inherit Util.Lexers.decimal s
         inherit Util.Lexers.lident [] s 
         inherit Util.Lexers.skip [
           Matcher.Skip.whitespaces " \t\n\r";
         ] s
       end) 
     init_value
  
end

module Expr =
struct
  
  @type t =
  | Var    of string
  | Const  of int    
  | Binop  of string * t * t
  | Elem   of string * t
  | Length of string
  | Subst  of string * (t * t) list
  | Array  of t list
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
    in
    pretty ("", No) expr
  
  let evalBinop xv yv =
    let i2b x = if x <> 0 then 1 else 0     in
    let b2i   = function true -> 1 | _ -> 0 in
    function
    | "!!" -> i2b (i2b xv + i2b yv)
    | "&&" -> i2b (i2b xv * i2b yv)
    | "==" -> b2i (xv =  yv)
    | "!=" -> b2i (xv <> yv)
    | "<=" -> b2i (xv <= yv)
    | "<"  -> b2i (xv <  yv)
    | ">=" -> b2i (xv >= yv)
    | ">"  -> b2i (xv >  yv)
    | "+"  -> xv + yv
    | "-"  -> xv - yv
    | "*"  -> xv * yv
    | "/"  -> xv / yv
    | "%"  -> xv mod yv

  (* let rec eval state = function *)
  (* | Var   x -> State.get state x *)
  (* | Const x -> x *)
  (* | Binop (op, x, y) -> *)
  (*   evalBinop (eval state x) (eval state y) op *)

  let rec to_string = function
  | Var x -> x
  | Const x -> Int.to_string x
  | Binop (op, x, y) -> Printf.sprintf "(%s %s %s)" (to_string x) op (to_string y)
  | Subst (x, substs) -> String.concat " " @@ List.map (fun (i, v) -> x ^ "[" ^ to_string i ^ "] = " ^ to_string v ^ ";") substs
  | Array l -> "{" ^ (String.concat ", " @@ List.map to_string l) ^ "}"
  | Length a -> "length(" ^ a ^ ")"
  | Elem (a, i) -> a ^ "[" ^ to_string i ^ "]"

  let rec spec (state : State.t) = function
  | Var x as expr    -> 
    (match State.get state x with
     | Some (Value.Int n)   -> Const n
     | Some (Value.Array l) ->
        Array (List.mapi
                (fun i -> function
                          | Value.Array _ -> failwith "Multidemsional arrays no supported"
                          | Value.Int n   -> Const n
                          | Value.Dynamic -> Elem (x, Const i))
                 l)
     | _                    -> expr
    )
  | Const x as expr  -> expr
  | Binop (op, x, y) ->
    let x', y' = spec state x, spec state y in
    (match x', y' with
     | Const xv, Const yv -> Const (evalBinop xv yv op)
     | _, _               -> Binop (op, x', y')
    )
  | Subst (a, substs) -> 
    let substs' = List.map (fun (ie, ve) -> (spec state ie, spec state ve)) substs in
    let substs_static = List.filter_map (fun (i, v) -> match i with Const iv -> Some (iv, v) | _ -> None ) substs' in
    if List.length substs' <> List.length substs_static then Subst (a, substs')
    else (match State.get state a with
          | None                   -> Subst (a, substs')
          | Some (Value.Int _)     -> failwith "unreachable"
          | Some Value.Dynamic     -> Subst (a, substs')
          | Some (Value.Array arr) -> 
            (* List.iter (fun (i, _) -> if i >= List.length arr then failwith "Out of bound" else ()) substs_static; *)
            Array (List.mapi
                    (fun i e -> match List.assoc_opt i substs_static with
                                | Some v -> v
                                | None   ->
                                  match e with
                                  | Value.Dynamic -> Elem (a, Const i)
                                  | Value.Int v   -> Const v
                                  | Value.Array _ -> failwith "unreachable")
                    arr)
         )
  | Elem (a, i) -> 
    (match spec state i with
     | Const i' ->
       (match State.get state a with
        | None -> Elem (a, Const i')
        | Some Value.Dynamic     -> Elem (a, Const i')
        | Some (Value.Int _)     -> failwith "unreachable"
        | Some (Value.Array arr) ->
          match List.nth arr i' with
          | Value.Dynamic -> Elem (a, Const i')
          | Value.Array _ -> failwith "Multidemsional arrays no supported"
          | Value.Int v   -> Const v
       )
     | i' -> Elem (a, i)
    )
  | Length a ->
    (match State.get state a with
     | None -> Length a
     | Some Value.Dynamic -> Length a
     | Some (Value.Int _) -> failwith "unreachable"
     | Some (Value.Array arr) -> Const (List.length arr)
    )
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
  
  (* let rec eval state = function *)
  (* | Assn (x, e)  -> State.set state x (Expr.eval state e) *)
  (* | Seq  (l, r)  -> eval (eval state l) r *)
  (* | Skip         -> state *)
  (* | If (c, t, e) -> eval state (if Expr.eval state c <> 0 then t else e) *)
  (* | While (c, b) as w -> *)
  (*    if Expr.eval state c = 0 *)
  (*    then state *)
  (*    else eval (eval state b) w                  *)
  (* | Do (b, c) as w -> eval state (Seq (b, While (c, b)))       *)

  let residualize ?(before=false) state stmt =
    let single = fun x -> function
      | Value.Int v   -> Assn (x, Const v)
      | Value.Array l ->
        let assigns = List.filter_map (fun x -> x) @@ List.mapi
          (fun i -> function
           | Value.Int v   -> Some (Expr.Const i, Expr.Const v)
           | Value.Array _ -> failwith "Multidemsional arrays not supported"
           | Value.Dynamic -> None
          ) l
        in if List.length assigns = 0 then Skip else Assn (x, Subst (x, assigns))
      | Value.Dynamic -> failwith "unreachable"
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
       | Expr.Const ve -> Skip, State.set state x (Value.Int ve)
       | Expr.Array l  -> 
          let arr' = List.map (function Expr.Const v -> Value.Int v | _ -> Value.Dynamic) l in
          let substs = List.mapi (fun i -> function 
                                           | Expr.Const _ -> None 
                                           | Expr.Elem (x', Expr.Const j) when x = x' && i = j -> None
                                           | e -> Some (Expr.Const i, e)) l in
          let substs' = List.filter_map (fun x -> x) substs in
          let stmt = 
            if List.length substs' = 0 then Skip
            else Assn (x, Expr.Subst (x, substs'))
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
         let t'' = residualize (State.subtract t_state state') t' in
         let e'' = residualize (State.subtract e_state state') e' in
         (If (c', t'', e'')), state'
      )
    | While (c, s) as w ->
      (match Expr.spec state c with
       | Expr.Const cv when cv <> 0 -> spec state (Seq (s, w))
       | Expr.Const cv              -> (Skip, state)
       | __                         -> 
         let state' = fixpoint State.equal
            (fun state -> 
              let _, state' = spec state s in
              State.join state state'
            ) state
         in
         let s', state_s = spec state' s in
         let c' = Expr.spec state' c in
         let res = While (c', residualize (State.subtract state_s state') s') in
         let res = residualize ~before:true (State.subtract state state') res in
         (res, state')
      )
    | Do (s, c) -> spec state (Seq (s, While (c, s)))

  let indent_line depth stmt = Printf.sprintf "%s%s" (String.init (depth * 2) (fun _ -> ' ')) stmt

  let rec to_string ?(depth=0) = function
  | Assn (x, (Expr.Subst (x', _) as e)) when x = x' -> indent_line depth @@ Expr.to_string e
  | Assn (x, e) -> indent_line depth @@ Printf.sprintf "%s = %s;" x (Expr.to_string e)
  | Seq (l, r) -> Printf.sprintf "%s\n%s" (to_string ~depth l) (to_string ~depth r)
  | Skip -> indent_line depth "skip;"
  | If (c, t, e) -> 
     (indent_line depth @@ Printf.sprintf "if (%s) {\n" (Expr.to_string c))
     ^ (to_string ~depth:(depth + 1) t)
     ^ (if e = Skip then "\n" ^ (indent_line depth "}") else
         "\n" ^ (indent_line depth "} else {\n")
         ^ (to_string ~depth:(depth + 1) e)
         ^ "\n" ^ (indent_line depth "}"))
  | While (c, b) ->
     (indent_line depth @@ Printf.sprintf "while (%s) {\n" (Expr.to_string c))
     ^ (to_string ~depth:(depth + 1) b)
     ^ "\n" ^ (indent_line depth "}")
  | Do (b, c) ->
     (indent_line depth "do {\n")
     ^ (to_string ~depth:(depth + 1) b)
     ^ "\n" ^ (indent_line depth @@ Printf.sprintf "} while (%s)" (Expr.to_string c))
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
    let primary = ostap (
        x:LIDENT  {Expr.Var   x}
    |   c:DECIMAL {Expr.Const c}          
    |   a:LIDENT "[" i:expression "]" { Expr.Elem (a, i) }
    |   "#" a:LIDENT { Expr.Length a }
    | -"(" expression -")" 
    )
    in  
    Util.expr (fun x -> x) ops primary s

  let rec stmt s =
    let ostap (
      primary:
        %"skip"                       ";"? {Stmt.Skip   }    
      | d:LIDENT "=" s:expression     ";"? {Stmt.Assn (d, s)}
      | d:LIDENT op:binop "=" s:expression ";"?         {Stmt.Assn (d, Expr.Binop (op, Expr.Var (d), s))}
      | d:LIDENT "[" idx:expression "]" "=" v:expression ";"? { Stmt.Assn(d, Expr.Subst (d, [idx, v])) }
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
         inherit Util.Lexers.lident ["if"; "else"; "elif"; "while"; "do"; "for"; "skip"] s 
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
         (* | `Run  -> Printf.printf "%s\n" @@ show (State.Printable.t) (State.toPrintable @@ Stmt.eval (Setup.getInput ()) stmt) *)
         | `Run  -> failwith "todo"
         | `Spec ->
            let (stmt', state') = Stmt.spec (Setup.getInput ()) stmt in
            let stmt'' = Stmt.residualize state' stmt' in
            (* Printf.printf "%s\n" @@ Formeat.to_string (Stmt.pretty stmt'') *)
            Printf.printf "{\n%s\n}\n" @@ Stmt.to_string ~depth:1 stmt''
        )
     | `Fail errs -> Printf.eprintf "ERROR: %s\n" errs
    )
  with
  | Setup.Invalid_arg        err -> Printf.eprintf "ERROR: %s\n" err
  | State.Undefined_variable x   -> Printf.eprintf "ERROR: undefined variable \"%s\"\n" x
  | exn                          -> Printf.eprintf "%s\n" @@ Printexc.to_string exn
