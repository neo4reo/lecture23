open Printf
open Expr
open Instruction
open Misc

type 'a envt = (string * 'a) list

let count = ref 0
let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count

let find_decl (ds : decl list) (name : string) : decl option =
  find_f (fun (DFun(fname,_,_)) -> fname = name) ds

let type_mask ty = HexConst
  (match ty with
   | TNum   -> 0x1
   | TBool  -> 0x3
   | TTuple -> 0x3
  )

let type_tag ty = HexConst
  (match ty with
   | TNum   -> 0x0
   | TBool  -> 0x3
   | TTuple -> 0x1
  )

let typecheck ty =
  [ IAnd (Reg EAX, type_mask ty)
  ; ICmp (Reg EAX, type_tag ty)
  ]

let const_true  = HexConst(0xffffffff)
let const_false = HexConst(0x7fffffff)

let throw_err code =
  [ IPush (Sized(DWORD_PTR, Const code))
  ; ICall "error"
  ]

let error_overflow  = "overflow_check"
let error_non_int   = "error_non_int"
let error_non_bool  = "error_non_bool"
let error_non_tuple = "error_non_tuple"
let error_too_small = "error_too_small"
let error_too_large = "error_too_large"

let check_overflow  = IJo(error_overflow)

let check_eax_num = typecheck TNum @ [ IJne(error_non_int) ]

let stackloc      i =   RegOffset (-4 * i, ESP)
let save_to_stack i = [ IMov (stackloc i, Reg(EAX)) ]
let restore_stack i = [ IMov (Reg(EAX), stackloc i) ]

let c_call si f =
  [ ISub (Reg ESP, Const (4 * si))
  ; IPush (Sized (DWORD_PTR, Reg EAX))
  ; ICall f
  ; IAdd (Reg ESP, Const (4 * (si +1)))
  ]

let compare cmp =
  let jump_of_cmp l = match cmp with
    | Less -> IJl l | Equal -> IJe l | Greater -> IJg l
    | _ -> failwith "impossible"
  in
  let label_true = gen_temp "true" in
  let label_done = gen_temp "done" in
  [ jump_of_cmp label_true
  ; IMov (Reg EAX, const_false)
  ; IJmp label_done
  ; ILabel label_true
  ; IMov (Reg EAX, const_true)
  ; ILabel label_done
  ]

let is_type ty = typecheck ty @ compare Equal

let rec compile_prim1 (o : prim1) (e : expr)
                      (istail : bool) (si : int) (env : int envt) : instruction list =
  let prelude = compile_expr e false si env in
  let isnum   = save_to_stack si @ check_eax_num @ restore_stack si in
  let op_is   = match o with
    | Add1    -> isnum @ [ IAdd(Reg(EAX), Const(2)) ]
    | Sub1    -> isnum @ [ IAdd(Reg(EAX), Const(-2)) ]
    | IsNum   -> is_type TNum
    | IsBool  -> is_type TBool
    | IsTuple -> is_type TTuple
    | Print   -> c_call si "print"
    | Input   -> isnum @ c_call si "input"

  in prelude @ op_is

and compile_prim2 (o : prim2) (e1 : expr) (e2 : expr)
                  (istail : bool) (si : int) (env : int envt) : instruction list =
  match o with
  | Equal -> compile_equals e1 e2 false si env
  | _     -> compile_prim2_helper o e1 e2 false si env

and compile_equals (e1 : expr) (e2 : expr)
                   (istail : bool) (si : int) (env : int envt) : instruction list =
    compile_expr e2 false si env
  @ save_to_stack si
  @ compile_expr e1 false (si+1) env
  @ [ ICmp (Reg EAX, stackloc si) ]
  @ compare Equal

and compile_prim2_helper (o : prim2) (e1 : expr) (e2 : expr)
                         (istail : bool) (si : int) (env : int envt) : instruction list =
  let rhs_loc = stackloc (si+1) in
  let e1_is   = compile_expr e1 false si env     @ save_to_stack si     @ check_eax_num in
  let e2_is   = compile_expr e2 false (si+1) env @ save_to_stack (si+1) @ check_eax_num in
  let op_is   = match o with
    | Plus    -> [ IAdd (Reg(EAX), rhs_loc); check_overflow ]
    | Minus   -> [ ISub (Reg(EAX), rhs_loc); check_overflow ]
    | Times   -> [ ISar (Reg(EAX), Const(1))
                 ; IMul (Reg(EAX), rhs_loc)
                 ; check_overflow
                 ]
    | Less
    | Greater -> ICmp (Reg(EAX), rhs_loc) :: compare o
    | Equal     -> []
  in     e1_is
       @ e2_is
       @ restore_stack si
       @ op_is

and compile_let (bs : (string * expr) list) (body : expr)
                (istail : bool) (si : int) (env : int envt) : instruction list =
  match bs with
  | []         -> compile_expr body false si env
  | (x,e)::bs' ->
     (compile_expr e false si env)
     @ save_to_stack si
     @ compile_let bs' body false (si+1) ((x,si)::env)

and compile_if (cond : expr) (e_then : expr) (e_else : expr)
               (istail : bool) (si : int) (env : int envt) : instruction list =
  let cond_is    = compile_expr cond   false si env in
  let then_is    = compile_expr e_then false si env in
  let else_is    = compile_expr e_else false si env in
  let label_then = gen_temp "then" in
  let label_else = gen_temp "else" in
  let label_end  = gen_temp "end"  in
    cond_is
  @ [ ICmp(Reg(EAX), const_true)
    ; IJe(label_then)
    ; ICmp(Reg(EAX), const_false)
    ; IJe(label_else)
    ; IJmp(error_non_bool)
    ; ILabel(label_then)
    ]
  @ then_is
  @ [ IJmp(label_end); ILabel(label_else) ]
  @ else_is
  @ [ ILabel(label_end) ]

and compile_app (f : string) (args : expr list)
                (istail : bool) (si : int) (env : int envt) : instruction list =
  let put_args es offset env =
    let put1 i e = compile_expr e false (offset + i) env
                @ [ IMov (Sized(DWORD_PTR,stackloc (offset + i)), Reg EAX) ]
    in flatmapi put1 es
  in
  let lbl = gen_temp "after_call" in
  [ IMov(Sized(DWORD_PTR, stackloc si), Label lbl)
  ; IMov(stackloc (si+1), Reg ESP) ]
  @ put_args args (si+2) env
  @ begin match find env f with
      | None ->
        [ ISub(Reg ESP, Const (si*4))
        ; IJmp f]
      | Some(i) ->
        [ IMov(Reg EAX, stackloc i)
        ; ISub(Reg ESP, Const (si*4))
        ; IJmpReg(EAX)]
    end
  @ [ ILabel lbl
    ; IMov(Reg ESP, stackloc 2) ]

and compile_tuple (es : expr list)
                  (istail : bool) (si : int) (env : int envt) : instruction list =
  let numargs  = List.length es in
  let to_store = ENumber(numargs)::es in
    (flatmapi (fun i e -> compile_expr e false (si+i) env
                        @ [ IMov (Sized (DWORD_PTR, stackloc (si+i)), Sized (DWORD_PTR, Reg EAX))])
              to_store)
  @ (flatmapi (fun i _ -> [ IMov (Reg EAX, Sized (DWORD_PTR, stackloc (si+i)))
                          ; IMov (Sized (DWORD_PTR, RegOffset(4*i, EBX)), Reg EAX)])
              to_store)
  @ [ IMov (Reg EAX, Reg EBX)
    ; IOr  (Reg EAX, type_tag TTuple)
    ; IAdd (Reg EBX, Const(4 * (numargs +1)))
    ]

and compile_get_item (t : expr) (i : expr)
                     (istail : bool) (si : int) (env : int envt) : instruction list =
  compile_expr t false si env @ save_to_stack si
  @ typecheck TTuple @ [ IJne error_non_tuple ]
  @ compile_expr i false (si+1) env @ save_to_stack (si+1)
  @ check_eax_num
  @ [ IMov (Reg ECX, stackloc si)
    ; IXor (Reg ECX, type_tag TTuple)
    ; IMov (Reg EAX, stackloc (si+1))
    ; ICmp (Reg EAX, Const 0)
    ; IJl error_too_small
    ; ICmp (Reg EAX, Sized (DWORD_PTR, RegOffset(0, ECX)))
    ; IJge error_too_large
    ; ISar (Reg EAX, Const 1)
    ; IMov (Reg EAX, RegOffsetReg (ECX, EAX, 4, 4))
    ]


and compile_expr (e : expr)
                 (istail : bool) (si : int) (env : int envt) : instruction list =
  match e with
  | ENumber (n ) -> [ IMov (Reg(EAX), Const((n lsl 1))) ]
  | EBool   (b ) -> [ IMov (Reg(EAX), if b then const_true else const_false) ]
  | EId     (rx) ->
     let arg = begin match find env rx with
               | Some(i) -> stackloc i
               | None    -> Label(rx)
               end
     in [ IMov (Reg(EAX), arg) ]
  | EPrim1 (op, e')          -> compile_prim1    op e'         false si env
  | EPrim2 (op, e1, e2)      -> compile_prim2    op e1 e2      false si env
  | ELet   (bs, body)        -> compile_let      bs body       false si env
  | EIf    (c, ethen, eelse) -> compile_if       c ethen eelse false si env
  | EApp   (f, args)         -> compile_app      f args        false si env
  | ETuple (args)            -> compile_tuple    args          false si env
  | EGetItem(tuple,index)    -> compile_get_item tuple index   false si env

let compile_decl (d : decl) : instruction list =
  let DFun (fname, args, body) = d in
  let env = List.rev (List.mapi (fun i s -> (s, i+2)) args) in
  let si = List.length args + 2 in
  [ IAlign(8); ILabel fname ] @ compile_expr body false si env @ [ IRet ]

let rec well_formed_e (e : expr) (ds : decl list) (env : bool envt) =
  let go xs = flatmap (fun x -> well_formed_e x ds env) xs in
  match e with
    | ENumber _ | EBool _ -> []
    | EGetItem(l, r) -> go [l;r]
    | ETuple(elts)   -> go elts
    | EId(x) ->
       begin match find env x with
       | None -> begin match find_decl ds x with
          | None -> ["Unbound id " ^ x]
          | Some(_) -> []
         end
       | Some(_) -> []
       end
    | EPrim1(op, e) -> go [e]
    | EPrim2(op, left, right) -> go [left; right]
    | EIf(cond, thn, els) -> go [cond; thn; els]

    | EApp(name, args) ->
       begin match find_decl ds name with
       | None ->
         begin match find env name with
           | None -> ["Unknown function/id " ^ name]
           | Some(_) -> []
         end
       | Some(DFun(_,params,_)) ->
          if List.length params = List.length args
          then []
          else ["Arity mismatch: " ^ name]
       end
       @ go args

    | ELet(binds, body) ->
      let names          = List.map fst binds in
      let env_from_binds = List.map (fun a -> (a, true)) names in
      let from_body      = well_formed_e body ds (env_from_binds @ env) in
      let dup_binds =
        begin match find_dup names with
        | None       -> []
        | Some(name) -> ["Multiple bindings for variable identifier " ^ name]
        end
      in
      let (from_binds, _) =
        let f (errs, env) (name, be) =
          let errs' = well_formed_e be ds env in
          let env'  = (name,true)::env in
          (errs @ errs', env') in
        List.fold_left f ([], env) binds
      in
      dup_binds @ from_binds @ from_body


let well_formed_d (d : decl) (ds : decl list) : string list =
  match d with
  | DFun(name, args, body) ->
     let env = List.map (fun a -> (a, true)) args in
     let from_body = well_formed_e body ds env in
     begin match find_dup args with
     | None -> from_body
     | Some(v) -> ("Duplicate parameter " ^ v)::from_body
     end

let well_formed_p (p : program) : string list =
  match p with
  | Program(ds, maine) ->
     let names = List.map (fun (DFun(name, _, _)) -> name) ds in
     let subexpr_errs = (well_formed_e maine ds [])
                      @ (flatmap (fun d -> well_formed_d d ds) ds)
     in
     begin match find_dup names with
     | None -> subexpr_errs
     | Some(v) -> ("Duplicate function " ^ v)::subexpr_errs
     end

let compile_to_string prog =
  match well_formed_p prog with
  | _::_ as errs ->
     failwith (unlines errs ^ "\n")
  | [] ->
     match prog with
     | Program(decls, main) ->
        let compiled_decls = flatmap compile_decl decls in
        let compiled_main = compile_expr main false 1 [] in
        let prelude = unlines
          [ "section .text"
          ; "extern error"
          ; "extern equal"
          ; "extern print"
          ; "extern input"
          ; "global our_code_starts_here"
          ]
        in
        let main_start =
          [ ILabel("our_code_starts_here")
          ; IMov(Reg(EBX), RegOffset(4, ESP))
          ; IAdd(Reg(EBX), Const(4))
          ; IAnd(Reg(EBX), HexConst(0xFFFFFFFC))
          ]
        in
        let postlude
          = [ IRet ]
          @ [ ILabel error_non_int   ] @ (throw_err 1)
          @ [ ILabel error_non_bool  ] @ (throw_err 2)
          @ [ ILabel error_overflow  ] @ (throw_err 3)
          @ [ ILabel error_non_tuple ] @ (throw_err 4)
          @ [ ILabel error_too_small ] @ (throw_err 5)
          @ [ ILabel error_too_large ] @ (throw_err 6)
        in
        let as_assembly_string =
          [ compiled_decls
          ; main_start
          ; compiled_main
          ; postlude
          ] |> List.flatten |> to_asm
        in
        unlines [prelude; as_assembly_string; "\n"]
