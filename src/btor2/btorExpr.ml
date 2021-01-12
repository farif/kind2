
type uop =
  (* boolean *)
    Not
  | Neg
  (* reduce *)
  | Redand
  | Redor
  | Redxor

(* Add missing operators later *)

(* boolean *)
type bop =
    And
  | Nand
  | Nor
  | Or
  | Xor
  | Xnor
  | Implies
  | Iff
  (* Equality *)
  | Eq
  | Neq
  (* Comparison *)
  | Ugt
  | Sgt
  | Ugte
  | Sgte
  | Ult
  | Slt
  | Ulte
  | Slte
  (* Rorate/shift *)
  | Rol
  | Ror
  | Sll
  | Sra
  | Srl
  (* Arithmatic*)
  | Add
  | Sub
  | Mul
  | Udiv
  | Sdiv
  | Smod
  | Urem
  | Srem
  (* Overflow *)
  | Uaddo
  | Saddo
  | Udivo
  | Sdivo
  | Umulo
  | Smulo
  | Usubo
  | Ssubo
  (* Array *)
  | Read
  | Concat

type top = 
    Ite 
  | Write

type opidx = 
    Slice 
  | Uext 
  | Sext
