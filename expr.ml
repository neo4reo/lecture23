type prim1 =
  | Add1
  | Sub1
  | Print
  | IsNum
  | IsBool
  | IsTuple
  | Input

type prim2 =
  | Plus
  | Minus
  | Times
  | Less
  | Greater
  | Equal

type expr =
  | ELet of (string * expr) list * expr
  | EPrim1 of prim1 * expr
  | EPrim2 of prim2 * expr * expr
  | EApp of string * expr list
  | ETuple of expr list
  | EGetItem of expr * expr
  | EIf of expr * expr * expr
  | ENumber of int
  | EBool of bool
  | EId of string

type type_tag =
  | TBool
  | TNum
  | TTuple

type decl =
  | DFun of string * string list * expr

type program =
  | Program of decl list * expr
