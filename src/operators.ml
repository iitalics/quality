open Batteries

type t
  = BO_Add | BO_Sub | BO_Mul | BO_Div
    | BO_Eql | BO_Neq | BO_Grt | BO_Lst
    | BO_Gte | BO_Lte | BO_And | BO_Or
    | UO_Neg | UO_Ref | UO_Mov | UO_Not


let signatures =
  Hashtbl.of_list
    [BO_Add, ["int"; "int"; "int"];
     BO_Sub, ["int"; "int"; "int"];
     BO_Mul, ["int"; "int"; "int"];
     BO_Div, ["int"; "int"; "int"];
     BO_Eql, ["bool"; "int"; "int"];
     BO_Neq, ["bool"; "int"; "int"];
     BO_Grt, ["bool"; "int"; "int"];
     BO_Lst, ["bool"; "int"; "int"];
     BO_Gte, ["bool"; "int"; "int"];
     BO_Lte, ["bool"; "int"; "int"];
     BO_And, ["bool"; "int"; "int"];
     BO_Or, ["bool"; "int"; "int"];
     UO_Neg, ["int"; "int"];
     UO_Not, ["bool"; "bool"]]


let c_implementations =
  Hashtbl.of_list
    [BO_Add, "+";
     BO_Sub, "-";
     BO_Mul, "*";
     BO_Div, "/";
     BO_Eql, "==";
     BO_Neq, "!=";
     BO_Grt, ">";
     BO_Lst, "<";
     BO_Gte, ">=";
     BO_Lte, "<=";
     BO_And, "&&";
     BO_Or, "||";
     UO_Neg, "-";
     UO_Not, "!"]
