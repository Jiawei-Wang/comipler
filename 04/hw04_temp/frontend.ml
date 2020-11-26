open Ll
open Llutil
open Ast

(* instruction streams ------------------------------------------------------ *)

(* As in the last project, we'll be working with a flattened representation
   of LLVMlite programs to make emitting code easier. This version
   additionally makes it possible to emit elements will be gathered up and
   "hoisted" to specific parts of the constructed CFG
   - G of gid * Ll.gdecl: allows you to output global definitions in the middle
     of the instruction stream. You will find this useful for compiling string
     literals
   - E of uid * insn: allows you to emit an instruction that will be moved up
     to the entry block of the current function. This will be useful for 
     compiling local variable declarations
*)

type elt = 
  | L of Ll.lbl             (* block labels *)
  | I of uid * Ll.insn      (* instruction *)
  | T of Ll.terminator      (* block terminators *)
  | G of gid * Ll.gdecl     (* hoisted globals (usually strings) *)
  | E of uid * Ll.insn      (* hoisted entry block instructions *)

type stream = elt list
let ( >@ ) x y = y @ x
let ( >:: ) x y = y :: x
let lift : (uid * insn) list -> stream = List.rev_map (fun (x,i) -> I (x,i))

(* Build a CFG and collection of global variable definitions from a stream *)
let cfg_of_stream (code:stream) : Ll.cfg * (Ll.gid * Ll.gdecl) list  =
    let gs, einsns, insns, term_opt, blks = List.fold_left
      (fun (gs, einsns, insns, term_opt, blks) e ->
        match e with
        | L l ->
           begin match term_opt with
           | None -> 
              if (List.length insns) = 0 then (gs, einsns, [], None, blks)
              else failwith @@ Printf.sprintf "build_cfg: block labeled %s has\
                                               no terminator" l
           | Some terminator ->
              (gs, einsns, [], None, (l, {insns; terminator})::blks)
           end
        | T t  -> (gs, einsns, [], Some t, blks)
        | I (uid,insn)  -> (gs, einsns, (uid,insn)::insns, term_opt, blks)
        | G (gid,gdecl) ->  ((gid,gdecl)::gs, einsns, insns, term_opt, blks)
        | E (uid,i) -> (gs, (uid, i)::einsns, insns, term_opt, blks)
      ) ([], [], [], None, []) code
    in
    match term_opt with
    | None -> failwith "build_cfg: entry block has no terminator" 
    | Some terminator -> 
       let insns = einsns @ insns in
       ({insns; terminator}, blks), gs


(* compilation contexts ----------------------------------------------------- *)

(* To compile OAT variables, we maintain a mapping of source identifiers to the
   corresponding LLVMlite operands. Bindings are added for global OAT variables
   and local variables that are in scope. *)

module Ctxt = struct

  type t = (Ast.id * (Ll.ty * Ll.operand)) list
  let empty = []

  (* Add a binding to the context *)
  let add (c:t) (id:id) (bnd:Ll.ty * Ll.operand) : t = (id,bnd)::c

  (* Lookup a binding in the context *)
  let lookup (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    (* Printf.printf "look up %s\n\n\n" id; *)
    let tmp = List.assoc id c in
    (* Printf.printf "FOUND %s\n\n\n" id; *)
    tmp

  (* Lookup a function, fail otherwise *)
  let lookup_function (id:Ast.id) (c:t) : Ll.fty * Ll.operand =
    (* Printf.printf "function lookup %s\n\n" id; *)
    match List.assoc id c with
    | Fun ft, g -> (* Printf.printf "FOUND FUNCTION %s\n\n\n" id; *)ft, g
    | _ -> failwith @@ id ^ " not bound to a function"

end


(* Helper function *)
let bool_to_int64 (b:bool) : int64 =
  if b then 1L else 0L 


(* compiling OAT types ------------------------------------------------------ *)

(* The mapping of source types onto LLVMlite is straightforward. Booleans and ints
   are represented as the corresponding integer types. OAT strings are
   pointers to bytes (I8). Arrays are the most interesting type: they are
   represented as pointers to structs where the first component is the number
   of elements in the following array.

   The trickiest part of this project will be satisfying LLVM's rudimentary type
   system. Recall that global arrays in LLVMlite need to be declared with their
   length in the type to statically allocate the right amount of memory. The 
   global strings and arrays you emit will therefore have a more specific type
   annotation than the output of cmp_rty. You will have to carefully bitcast
   gids to satisfy the LLVM type checker.
*)

let rec get_arr_el_t (t:Ll.ty) : Ll.ty =
  begin match t with
  | Ll.Struct (_::[Ll.Array(_, typ)]) -> typ
  | Ll.Ptr (Ll.Struct (_::[Ll.Array(_, typ)])) -> typ
  | _ -> t
  end


let rec cmp_ty : Ast.ty -> Ll.ty = function
  | Ast.TVoid  -> Void
  | Ast.TBool  -> I1
  | Ast.TInt   -> I64
  | Ast.TRef r -> Ptr (cmp_rty r)
  | Ast.TFun f -> Fun (cmp_fty f)

and cmp_fty (ts,r:Ast.fty) : Ll.fty =
  List.map cmp_ty ts, cmp_ty r

and cmp_rty : Ast.rty -> Ll.ty = function
  | Ast.RString  -> I8
  | Ast.RArray u -> Struct [I64; Array(0, cmp_ty u)]

let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Eq | Neq | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)

let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* Compiler Invariants

   The LLVM IR type of a variable (whether global or local) that stores an Oat
   array value (or any other reference type, like "string") will always be a
   double pointer.  In general, any Oat variable of Oat-type t will be
   represented by an LLVM IR value of type Ptr (cmp_ty t).  So the Oat variable
   x : int will be represented by an LLVM IR value of type i64*, y : string will
   be represented by a value of type i8**, and arr : int[] will be represented
   by a value of type {i64, [0 x i64]}**.  Whether the LLVM IR type is a
   "single" or "double" pointer depends on whether t is a reference type.

   We can think of the compiler as paying careful attention to whether a piece
   of Oat syntax denotes the "value" of an expression or a pointer to the
   "storage space associated with it".  This is the distinction between an
   "expression" and the "left-hand-side" of an assignment statement.  Compiling
   an Oat variable identifier as an expression ("value") does the load, so
   cmp_exp called on an Oat variable of type t returns (code that) generates a
   LLVM IR value of type cmp_ty t.  Compiling an identifier as a left-hand-side
   does not do the load, so cmp_lhs called on an Oat variable of type t returns
   and operand of type (cmp_ty t)*.  Extending these invariants to account for
   array accesses: the assignment e1[e2] = e3; treats e1[e2] as a
   left-hand-side, so we compile it as follows: compile e1 as an expression to
   obtain an array value (which is of pointer of type {i64, [0 x s]}* ).
   compile e2 as an expression to obtain an operand of type i64, generate code
   that uses getelementptr to compute the offset from the array value, which is
   a pointer to the "storage space associated with e1[e2]".

   On the other hand, compiling e1[e2] as an expression (to obtain the value of
   the array), we can simply compile e1[e2] as a left-hand-side and then do the
   load.  So cmp_exp and cmp_lhs are mutually recursive.  [[Actually, as I am
   writing this, I think it could make sense to factor the Oat grammar in this
   way, which would make things clearer, I may do that for next time around.]]

 
   Consider globals7.oat

   /--------------- globals7.oat ------------------ 
   global arr = int[] null;

   int foo() { 
     var x = new int[3]; 
     arr = x; 
     x[2] = 3; 
     return arr[2]; 
   }
   /------------------------------------------------

   The translation (given by cmp_ty) of the type int[] is {i64, [0 x i64}* so
   the corresponding LLVM IR declaration will look like:

   @arr = global { i64, [0 x i64] }* null

   This means that the type of the LLVM IR identifier @arr is {i64, [0 x i64]}**
   which is consistent with the type of a locally-declared array variable.

   The local variable x would be allocated and initialized by (something like)
   the following code snippet.  Here %_x7 is the LLVM IR uid containing the
   pointer to the "storage space" for the Oat variable x.

   %_x7 = alloca { i64, [0 x i64] }*                              ;; (1)
   %_raw_array5 = call i64*  @oat_alloc_array(i64 3)              ;; (2)
   %_array6 = bitcast i64* %_raw_array5 to { i64, [0 x i64] }*    ;; (3)
   store { i64, [0 x i64]}* %_array6, { i64, [0 x i64] }** %_x7   ;; (4)

   (1) note that alloca uses cmp_ty (int[]) to find the type, so %_x7 has 
       the same type as @arr 

   (2) @oat_alloc_array allocates len+1 i64's 

   (3) we have to bitcast the result of @oat_alloc_array so we can store it
        in %_x7 

   (4) stores the resulting array value (itself a pointer) into %_x7 

  The assignment arr = x; gets compiled to (something like):

  %_x8 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_x7     ;; (5)
  store {i64, [0 x i64] }* %_x8, { i64, [0 x i64] }** @arr       ;; (6)

  (5) load the array value (a pointer) that is stored in the address pointed 
      to by %_x7 

  (6) store the array value (a pointer) into @arr 

  The assignment x[2] = 3; gets compiled to (something like):

  %_x9 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_x7      ;; (7)
  %_index_ptr11 = getelementptr { i64, [0 x  i64] }, 
                  { i64, [0 x i64] }* %_x9, i32 0, i32 1, i32 2   ;; (8)
  store i64 3, i64* %_index_ptr11                                 ;; (9)

  (7) as above, load the array value that is stored %_x7 

  (8) calculate the offset from the array using GEP

  (9) store 3 into the array

  Finally, return arr[2]; gets compiled to (something like) the following.
  Note that the way arr is treated is identical to x.  (Once we set up the
  translation, there is no difference between Oat globals and locals, except
  how their storage space is initially allocated.)

  %_arr12 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** @arr    ;; (10)
  %_index_ptr14 = getelementptr { i64, [0 x i64] },                
                 { i64, [0 x i64] }* %_arr12, i32 0, i32 1, i32 2  ;; (11)
  %_index15 = load i64, i64* %_index_ptr14                         ;; (12)
  ret i64 %_index15

  (10) just like for %_x9, load the array value that is stored in @arr 

  (11)  calculate the array index offset

  (12) load the array value at the index 

*)

(* Global initialized arrays:

  There is another wrinkle: To compile global initialized arrays like in the
  globals4.oat, it is helpful to do a bitcast once at the global scope to
  convert the "precise type" required by the LLVM initializer to the actual
  translation type (which sets the array length to 0).  So for globals4.oat,
  the arr global would compile to (something like):

  @arr = global { i64, [0 x i64] }* bitcast 
           ({ i64, [4 x i64] }* @_global_arr5 to { i64, [0 x i64] }* ) 
  @_global_arr5 = global { i64, [4 x i64] } 
                  { i64 4, [4 x i64] [ i64 1, i64 2, i64 3, i64 4 ] }

*) 



(* Some useful helper functions *)

(* Generate a fresh temporary identifier. Since OAT identifiers cannot begin
   with an underscore, these should not clash with any source variables *)
let gensym : string -> string =
  let c = ref 0 in
  fun (s:string) -> incr c; Printf.sprintf "_%s%d" s (!c)

(* Amount of space an Oat type takes when stored in the satck, in bytes.  
   Note that since structured values are manipulated by reference, all
   Oat values take 8 bytes on the stack.
*)
let size_oat_ty (t : Ast.ty) = 8L

(* Generate code to allocate a zero-initialized array of source type TRef (RArray t) of the
   given size. Note "size" is an operand whose value can be computed at
   runtime *)
let oat_alloc_array (t:Ast.ty) (size:Ll.operand) : Ll.ty * operand * stream =
  let ans_id, arr_id = gensym "array", gensym "raw_array" in
  let ans_ty = cmp_ty @@ TRef (RArray t) in
  let arr_ty = cmp_ty @@ TRef (RArray TInt) in
  ans_ty, Id ans_id, lift
    [ arr_id, Call(arr_ty, Gid "oat_alloc_array", [I64, size])
    ; ans_id, Bitcast(arr_ty, Id arr_id, ans_ty) ]


(* 
  typing of result expression 
  | CNull of ty                         (* null literal for any TRef *)
  | CBool of bool                       (* bool literal *)
  | CInt of int64                       (* int literal *)
  | CStr of string                      (* string literal *)
  | CArr of ty * exp node list          (* array literal *)
  | NewArr of ty * exp node             (* zero-initialized arrays *)
  | Id of id                            (* identifiers *)
  | Index of exp node * exp node        (* index into an array *)
  | Call of id * exp node list          (* function call *)
  | Bop of binop * exp node * exp node  (* operations of two arguments *)
  | Uop of unop * exp node 
*)


let rec expr_type (exp: Ast.exp) : Ast.ty = 
  begin match exp with 
  | CNull _ -> TVoid
  | CBool _ -> TBool
  | CInt _ -> TInt
  | CStr s -> TRef (RString)
  | NewArr (t, _) -> TRef (RArray t)
  | Id _ -> TRef (RString) (* string ??? *)
  | Index (e1, _) -> expr_type e1.elt (* e1[e2] e1 should be type? *)
  | Bop (b, _, _) ->
    begin match b with 
    | Add | Sub | Mul | IAnd | IOr | Shl | Shr | Sar -> TInt
    | _ -> TBool
    end
  | Uop (u, _) ->
    begin match u with
    | Neg | Bitnot -> TInt
    | _ -> TBool
    end
  | _ -> failwith "unmatched"
  end

let rec global_expr_type (exp:Ast.exp) : Ll.ty =
  begin match exp with
  | CStr s -> Ll.Array ((String.length s) + 1, Ll.I8)
  | CArr (t, exp_list) -> 
    let el_ty = global_expr_type (List.hd exp_list).elt in
    Ll.Ptr (Ll.Struct [I64; Ll.Array(List.length exp_list, el_ty)])
  | _ -> cmp_ty @@ expr_type exp
  end

let cmp_bop (b: Ast.binop) (op1: Ll.operand) (op2: Ll.operand) (t: Ll.ty) : Ll.insn =
  begin match b with 
  | Add -> Ll.Binop (Ll.Add, t, op1, op2)
  | Sub -> Ll.Binop (Ll.Sub, t, op1, op2)
  | Mul -> Ll.Binop (Ll.Mul, t, op1, op2)
  | Eq -> Ll.Icmp (Ll.Eq, t, op1, op2)
  | Neq -> Ll.Icmp (Ll.Ne, t, op1, op2)
  | Lt -> Ll.Icmp (Ll.Slt, t, op1, op2)
  | Lte -> Ll.Icmp (Ll.Sle, t, op1, op2)
  | Gt -> Ll.Icmp (Ll.Sgt, t, op1, op2)
  | Gte -> Ll.Icmp (Ll.Sge, t, op1, op2)
  | And -> Ll.Binop (Ll.And, t, op1, op2)
  | Or -> Ll.Binop (Ll.Or, t, op1, op2)
  | IAnd -> Ll.Binop (Ll.And, t, op1, op2)
  | IOr -> Ll.Binop (Ll.Or, t, op1, op2)
  | Shl -> Ll.Binop (Ll.Shl, t, op1, op2)
  | Shr -> Ll.Binop (Ll.Lshr, t, op1, op2)
  | Sar -> Ll.Binop (Ll.Ashr, t, op1, op2)
  end

let cmp_uop (b: Ast.unop) (op: Ll.operand) (t: Ll.ty) : Ll.insn =
  begin match b with 
  | Neg -> Ll.Binop (Ll.Mul, t, op, Ll.Const (-1L))
  | Lognot -> Ll.Binop (Ll.Xor, t, op, Ll.Const (1L))
  | Bitnot -> Ll.Binop (Ll.Xor, t, op, Ll.Const (-1L))
  end


(* Compiles an expression exp in context c, outputting the Ll operand that will
   recieve the value of the expression, and the stream of instructions
   implementing the expression. 

   Tips:
   - use the provided cmp_ty function!

   - string literals (CStr s) should be hoisted. You'll need to make sure
     either that the resulting gid has type (Ptr I8), or, if the gid has type
     [n x i8] (where n is the length of the string), convert the gid to a 
     (Ptr I8), e.g., by using getelementptr.

   - use the provided "oat_alloc_array" function to implement literal arrays
     (CArr) and the (NewArr) expressions

*)

type arg_list = (Ll.ty * Ll.operand) list


let rec cmp_exp (c:Ctxt.t) (exp:Ast.exp node) : Ll.ty * Ll.operand * stream =
  begin match exp.elt with 
  | Bop (b, e1, e2) -> 
    let t = expr_type (Bop (b, e1, e2)) in 
    let id = gensym "bop" in
    let t1, op1, strm1 = load_helper @@ cmp_exp c e1 in
    let _, op2, strm2 = load_helper @@ cmp_exp c e2 in
    (cmp_ty t, Ll.Id id, strm1 >@ strm2 >@ [I (id, cmp_bop b op1 op2 t1)])
  | CNull t -> (cmp_ty t, Ll.Null,[])
  | CBool b -> (Ll.I1, Ll.Const (bool_to_int64 b),[])
  | CInt i -> (Ll.I64, Ll.Const i,[])
  | CStr s -> 
     let str_gid = gensym "string" in
     let typ = Ll.Array ((String.length s) + 1, Ll.I8) in
     let str_ginit = Ll.GString s in
     let str_gdecl = (typ, str_ginit) in
     (typ, Ll.Gid str_gid, [G (str_gid, str_gdecl)])
  | CArr (typ, exp_node_list) ->     
    (* Printf.printf "CArr\n\n\n\n"; *)
    let arr_len = (Ll.Const (Int64.of_int (List.length exp_node_list))) in
    (* let arr_ty_constr = Ll.Struct  *)
    let arr_ty, arr_op, arr_strm = oat_alloc_array typ arr_len in 
    let streams, _ = List.fold_left (fun (s,i) el -> 
        let index_op = Ll.Const (Int64.of_int (i)) in
        let el_ty, el_op, el_st = cmp_exp c el in
        let arr_el_id = gensym "arr_el_gep" in 
        let strm = el_st 
        >@ [I (arr_el_id, Ll.Gep (arr_ty, arr_op, [Ll.Const (0L); Ll.Const (1L); index_op]))]
        >@ [I (arr_el_id, Ll.Store (el_ty, el_op, Ll.Id arr_el_id))] in
        (s >@ strm, i + 1)
      ) (arr_strm, 0) exp_node_list in
    (arr_ty, arr_op, streams)
  | NewArr (typ, exp_node) -> 
    (* Printf.printf "NEW ARR\n\n\n\n"; *)
    let exp_ty, exp_op, exp_strm = cmp_exp c exp_node in
    let arr_ty, arr_op, arr_strm = oat_alloc_array typ exp_op in
    (arr_ty, arr_op, exp_strm >@ arr_strm)
  | Id i -> 
    (* Printf.printf "Think it's id %s\n\n\n" i; *)
    let typ, opr = Ctxt.lookup i c in
    begin match opr with
    | Gid g -> (typ, Ll.Gid g, [])
    | Id id -> (typ, Ll.Id id, [])
    | Const c -> (typ, Ll.Id i, [])
    | Null -> (typ, Ll.Id i, [])
    end
  | Index (e1, e2) -> 
    let arr_t, arr_op, arr_strm = cmp_exp c e1 in 
    let arr_el_t = get_arr_el_t arr_t in
    let idx_t, idx_op, idx_strm = load_helper @@ cmp_exp c e2 in 
    let id = gensym "gep" in 
    (* let id2 = gensym "load_gep" in *)
    (Ll.Ptr arr_el_t, Ll.Id id, arr_strm 
      >@ idx_strm 
      >@ [I (id, Ll.Gep (arr_t, arr_op, [Ll.Const 0L; Ll.Const 1L; idx_op]))])

  | Call (i, exp_node_list) ->
    (* Printf.printf "test\n\n"; *)
    let (ty_list, typ), ll_op = Ctxt.lookup_function i c in
    let _, ty_op_list, streams = List.fold_left2 zip_args_w_type (c, [], []) ty_list exp_node_list in
    let lbl = gensym i in
    (typ, Ll.Id lbl, streams >@ [I (lbl, Ll.Call (typ, ll_op, ty_op_list))])
  | Uop (uop1, e1) -> 
    let t = expr_type (Uop (uop1, e1)) in 
    let id = gensym "uop" in
    let typ, op, strm = load_helper @@ cmp_exp c e1 in
    (cmp_ty t, Ll.Id id, strm >@ [I (id, cmp_uop uop1 op typ)])
  end   

and load_helper (a: Ll.ty * Ll.operand * stream) : Ll.ty * Ll.operand * stream =
  (* Printf.printf "load_helper"; *)
  let t1_ty, t1_op, t1_strm = a in
  begin match t1_ty, t1_op with
    | (Ptr p, Gid g) -> 
      Printf.printf "Hitting case 1: global '%s'\n\n\n\n\n\n" g;
      (* let loaded_t1 = gensym "gl" in *)
      begin match p with
      | _ -> (Ll.Ptr t1_ty, t1_op(* Ll.Id loaded_t1 *), t1_strm (* >@ [I (loaded_t1, Ll.Load (t1_ty, t1_op))] *))
      end
      
    | (_, Gid g) -> 
      (* Printf.printf "Hitting case 2\n\n\n"; *)
      let loaded_t1 = gensym "gl" in
      begin match t1_ty with
      | Ll.Array (int, Ll.I8) -> 
        let casted_str = gensym "casted_str" in
        let new_strm = t1_strm >@ [E (casted_str, Bitcast(Ll.Ptr t1_ty, t1_op, Ll.Ptr (Ll.I8)))] in 
        (Ll.Ptr (Ll.I8), Ll.Id casted_str, new_strm)
      | _ -> (t1_ty, Ll.Id loaded_t1, t1_strm >@ [I (loaded_t1, Ll.Load (Ll.Ptr t1_ty, t1_op))])
      end
    | (Ptr p, _) ->
      (* Printf.printf "Hitting case 3\n"; *)
      begin match p with
      | Ll.Array (int, Ll.I8) -> 
        (* Printf.printf "Matched here casted_str!!!\n\n\n\n";  *)
        let casted_str = gensym "casted_str" in
        let new_strm = t1_strm >@ [E (casted_str, Bitcast(Ll.Ptr p, t1_op, Ll.Ptr (Ll.I8)))] in 
        (Ll.Ptr (Ll.I8), Ll.Id casted_str, new_strm)
      
      | _ -> let loaded_t1 = gensym "l" in
      (p, Ll.Id loaded_t1, t1_strm >@ [I (loaded_t1, Ll.Load (t1_ty, t1_op))])
      end
      
    | _ ->  a
  end

and zip_args_w_type (c:Ctxt.t * arg_list * stream) (t:Ll.ty) (a:Ast.exp node) : Ctxt.t * arg_list * stream =
  let ctxt, args, str = c in
  let _type, _opr, _str = cmp_exp ctxt a in
  if _type <> t then
    let arg_type, arg_operand, arg_stream = load_helper @@ (_type, _opr, _str) in
    (ctxt, args @ [(arg_type, arg_operand)], str >@ arg_stream)
  else
    begin match _opr with
    | Ll.Gid g ->
      let arg_type, arg_operand, arg_stream = load_helper @@ (_type, _opr, _str) in
      (ctxt, args @ [(arg_type, arg_operand)], str >@ arg_stream)
    | _ -> (ctxt, args @ [(_type, _opr)], str >@ _str)
    end
 


(* Compile a statement in context c with return typ rt. Return a new context, 
   possibly extended with new local bindings, and the instruction stream
   implementing the statement.

   Left-hand-sides of assignment statements must either be OAT identifiers,
   or an index into some arbitrary expression of array type. Otherwise, the
   program is not well-formed and your compiler may throw an error.

   Tips:
   - for local variable declarations, you will need to emit Allocas in the
     entry block of the current function using the E() constructor.

   - don't forget to add a bindings to the context for local variable 
     declarations
   
   - you can avoid some work by translating For loops to the corresponding
     While loop, building the AST and recursively calling cmp_stmt

   - you might find it helpful to reuse the code you wrote for the Call
     expression to implement the SCall statement

   - compiling the left-hand-side of an assignment is almost exactly like
     compiling the Id or Index expression. Instead of loading the resulting
     pointer, you just need to store to it!

 *)

 let safe_ptr_usage (t:Ll.ty) (op:Ll.operand) : Ll.operand * stream =
  begin match t, op with
    | (Ptr p, _) -> let _, o, str = load_helper (t,op,[]) in (o, str)
    | (_, Gid g) -> let _, o, str = load_helper (t,op,[]) in (o, str)
    | (_,_) -> (op, [])
  end

let rec cmp_stmt (c:Ctxt.t) (rt:Ll.ty) (stmt:Ast.stmt node) : Ctxt.t * stream =
  begin match stmt.elt with
  | Ret e_opt -> 
    (* Printf.printf "Ret e_opt";   *)
    begin match e_opt with
    | Some s -> 
      let typ, opr, strm = load_helper @@ cmp_exp c s in 
      if typ = rt then
        (c, strm 
          >@ [T (Ll.Ret (typ, Some opr))])
      else 
        (
          if typ = Ll.Ptr rt then
            let loaded_val = gensym "loaded" in
            (c, strm
              >@ [I (loaded_val, Ll.Load (typ, opr))]
              >@ [T (Ll.Ret (rt, Some (Ll.Id loaded_val)))])
          else
            failwith "Incompatible return types"
        )
    | None -> 
      if rt = Void then (c, [T (Ll.Ret (Void, None))])
      else failwith "Expected a void return type"
    end
  | Decl vd ->
    let id, expr = vd in 
    (* Printf.printf "declaration %s\n" id; *)
    let typ, opr, strm = load_helper @@ cmp_exp c expr in
    let new_c = Ctxt.add c id (Ll.Ptr typ, Ll.Id id) in
    (new_c, strm >@ [E (id, Ll.Alloca typ)] >@ [I (id, Ll.Store (typ, opr, Ll.Id id))])
  | Assn (lhs_exp, rhs_exp) -> 
    begin match lhs_exp.elt with
    | Id id -> 
      let rhs_typ, rhs_opr, rhs_strm = load_helper @@ cmp_exp c rhs_exp in
      let lhs_typ, lhs_opr, lhs_strm = cmp_exp c lhs_exp in
      (c, rhs_strm >@ lhs_strm >@ [I (id, Ll.Store (rhs_typ, rhs_opr, lhs_opr))])
    | Index (arr, ind) -> 
      let arr_typ, arr_opr, arr_strm = cmp_exp c arr in
      let ind_typ, ind_opr, ind_strm = load_helper @@ cmp_exp c ind in
      let rhs_typ, rhs_opr, rhs_strm = load_helper @@ cmp_exp c rhs_exp in
      let gep_id = gensym "gep" in 
      (c, rhs_strm >@ arr_strm >@ ind_strm
      >@ [I (gep_id, Ll.Gep (arr_typ, arr_opr, [Ll.Const 0L; Ll.Const (1L); ind_opr]))]
      >@ [I (gep_id, Ll.Store (rhs_typ, rhs_opr, Ll.Id gep_id))]
      )
    | _ -> failwith "program is not well-formed"
    end
  | SCall (i, exp_node_list) -> 
    (* Printf.printf "Scall\n\n\n"; *)
    let (ty_list, typ), ll_op = Ctxt.lookup_function i c in
    let _, ty_op_list, streams = List.fold_left2 zip_args_w_type (c,[], []) ty_list exp_node_list in
    let lbl = gensym i in
    (c, streams >@ [I (lbl, Ll.Call (typ, ll_op, ty_op_list))])

  | If (e, b1, b2) -> 
    let t, op, c_strm = cmp_exp c e in
    let cmp_b1 = cmp_block c rt b1 in
    let cmp_b2 = cmp_block c rt b2 in 
    let l0 = gensym "begin_if" in 
    let l1 = gensym "then" in
    let l2 = gensym "else" in 
    let l3 = gensym "end_if" in 
    let new_op, load_strm = safe_ptr_usage t op in
    let strm = c_strm 
      >@ [T (Br (l0))]
      >@ [L (l0)]
      >@ load_strm
      >@ [T (Ll.Cbr (new_op, l1, l2))] 
      >@ [L (l1)]
      >@ cmp_b1
      >@ [T (Br (l3))]
      >@ [L (l2)]
      >@ cmp_b2
      >@ [T (Br (l3))]
      >@ [L (l3)]
    in 
    (c, strm)

  | For (vd_list, cnd_option, incr_option, body) -> 
    let new_c, vd_strm = List.fold_left for_decl_helper (c,[]) vd_list in
    
    (* conditional section *) 
    let cnd_ty, cnd_opr, cnd_strm = 
    begin match cnd_option with 
      | Some s -> cmp_exp new_c s 
      | None -> cmp_exp new_c (Ast.no_loc (Ast.CBool true))
    end in 

    (* incrementer section *)
    let _, incr_strm =
    begin match incr_option with 
    | Some s -> cmp_stmt new_c Ll.Void s 
    | None -> new_c, []
    end in 

    let loop_body = cmp_block new_c cnd_ty body in 
    let l0 = gensym "begin_for" in 
    let l1 = gensym "for_block" in 
    let l2 = gensym "end_for" in 
    let new_cnd_op, load_strm = safe_ptr_usage cnd_ty cnd_opr in 
    let strm = vd_strm 
      >@ [T (Br (l0))]
      >@ [L (l0)]
      >@ cnd_strm
      >@ load_strm
      >@ [T (Cbr (new_cnd_op, l1, l2))]
      >@ [L (l1)]
      >@ loop_body 
      >@ incr_strm
      >@ [T (Br (l0))]
      >@ [L (l2)]
    in  
    (c, strm)
  | While (while_cond_exp, while_block) -> 
    let typ, while_opr, while_strm = cmp_exp c while_cond_exp in
    begin match typ with
    | Ll.I1 -> 
      let cmp_while_block = cmp_block c typ while_block in 
      let l0 = gensym "begin_while" in 
      let l1 = gensym "while_block" in 
      let l2 = gensym "end_while" in 
      let new_op, load_strm = safe_ptr_usage typ while_opr in
      let strm = [T (Br (l0))]
        >@ [L (l0)]
        >@ while_strm
        >@ load_strm 
        >@ [T (Ll.Cbr (new_op, l1, l2))] 
        >@ [L (l1)]
        >@ cmp_while_block
        >@ [T (Br (l0))]
        >@ [L (l2)]
      in 
      (c, strm)
    | _ -> failwith "operand not a boolean"
    end
  end
  
and for_decl_helper (a: Ctxt.t * stream) (vd: vdecl) : Ctxt.t * stream =
  let c, str = a in
  cmp_stmt c Ll.Void (no_loc (Ast.Decl (vd)))


(* Compile a series of statements *)
and cmp_block (c:Ctxt.t) (rt:Ll.ty) (stmts:Ast.block) : stream =
  snd @@ List.fold_left (fun (c, code) s -> 
      let c, stmt_code = cmp_stmt c rt s in
      c, code >@ stmt_code
    ) (c,[]) stmts
  

let rec ty_cmp_helper (l: (Ast.ty * id) list) : Ll.ty list = 
  begin match l with 
  | (t, _)::tl -> [cmp_ty t] @ ty_cmp_helper tl
  | [] -> []
  end


let cmp_global_ctxt_hlpr (c:Ctxt.t) (p:Ast.decl) : Ctxt.t =
  begin match p with
  | Gvdecl g ->
    let id = g.elt.name in
    let typ = global_expr_type g.elt.init.elt in
    Ctxt.add c g.elt.name (typ, Ll.Gid id)
  | Gfdecl f -> 
    let fn = f.elt.name in
    let ret_typ = cmp_ty f.elt.rtyp in
    let args = ty_cmp_helper f.elt.args in
    Ctxt.add c fn (Ll.Fun ((args, ret_typ)), Ll.Gid fn)
  end



(* Adds each function identifer to the context at an
   appropriately translated type.  

   NOTE: The Gid of a function is just its source name
*)
let cmp_function_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
    List.fold_left (fun c -> function
      | Ast.Gfdecl { elt={ frtyp; fname; args } } ->
         let ft = TRef (RFun (List.map fst args, frtyp)) in
         Ctxt.add c fname (cmp_ty ft, Gid fname)
      | _ -> c
    ) c p 

(* Populate a context with bindings for global variables 
   mapping OAT identifiers to LLVMlite gids and their types.

   Only a small subset of OAT expressions can be used as global initializers
   in well-formed programs. (The constructors starting with C). 
*)
let cmp_global_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
  List.fold_left cmp_global_ctxt_hlpr c p

(* Compile a function declaration in global context c. Return the LLVMlite cfg
   and a list of global declarations containing the string literals appearing
   in the function.

   You will need to
   1. Allocate stack space for the function parameters using Alloca
   2. Store the function arguments in their corresponding alloca'd stack slot
   3. Extend the context with bindings for function variables
   4. Compile the body of the function using cmp_block
   5. Use cfg_of_stream to produce a LLVMlite cfg from 
 *)

type uid_l = Ll.uid list
type ty_l = Ll.ty list

let cmp_fdecl_helper (a:Ctxt.t * uid_l * ty_l * stream) (d:ty * id) : Ctxt.t * uid_l * ty_l * stream =
  let c, u_l, t_l, strm = a in
  let ast_ty, new_id = d in
  let new_ty = cmp_ty ast_ty in
  begin match new_ty with
  | I1 | I8 | I64 -> 
    let lcl = gensym "lcl" in
    let new_strm = strm >@ [I (lcl, Ll.Alloca new_ty)] >@ [I (lcl, Ll.Store (new_ty, Ll.Id new_id, Ll.Id lcl))] in
    let new_c = Ctxt.add c new_id (Ll.Ptr new_ty, Ll.Id lcl) in
    (new_c, u_l @ [new_id], t_l @ [new_ty], new_strm)
  (* | Ll.Ptr (Ll.Array (i, Ll.I8)) ->
    let casted_str = gensym "casted_str" in
    let new_strm = strm >@ [E (casted_str, Bitcast(new_ty, Ll.Gid new_id, Ll.Ptr (Ll.I8)))] in
    let new_c = Ctxt.add c new_id (Ll.Ptr (Ll.I8), Ll.Id casted_str) in
    (new_c, u_l @ [new_id], t_l @ [new_ty], new_strm) *)
  | _ -> 
    let new_c = Ctxt.add c new_id (new_ty, Ll.Id new_id) in
    (new_c, u_l @ [new_id], t_l @ [new_ty], strm)
  end
  
  

let cmp_fdecl (c:Ctxt.t) (f:Ast.fdecl node) : Ll.fdecl * (Ll.gid * Ll.gdecl) list =
  let args = f.elt.args in
  let rtyp = f.elt.rtyp in
  let _ = f.elt.name in
  let new_c, uid_list, ty_list, ll_code = List.fold_left cmp_fdecl_helper (c, [], [], []) args in
  let ll_body = cmp_block new_c (cmp_ty rtyp) f.elt.body in
  let cfg, rest = cfg_of_stream (ll_code >@ ll_body) in
  let fty = (ty_list, cmp_ty rtyp) in
  ({ fty=fty; param=uid_list; cfg=cfg }, rest)


(* Compile a global initializer, returning the resulting LLVMlite global
   declaration, and a list of additional global declarations.

   Tips:
   - Only CNull, CBool, CInt, CStr, and CArr can appear as global initializers
     in well-formed OAT programs. Your compiler may throw an error for the other
     cases

   - OAT arrays are always handled via pointers. A global array of arrays will
     be an array of pointers to arrays emitted as additional global declarations.
*)
let rec cmp_gexp (e:Ast.exp node) : Ll.gdecl * (Ll.gid * Ll.gdecl) list =
  begin match e.elt with 
  | CNull t -> ((cmp_ty t, Ll.GNull),[])
  | CBool b -> ((Ll.I1, Ll.GInt(bool_to_int64 b)),[])
  | CInt i -> ((Ll.I64, Ll.GInt i),[])
  | CStr s -> 
    let gdecl = (Ll.Array ((String.length s) + 1, Ll.I8), Ll.GString s) in
    (* let gid = gensym "g_str" in *)
    (gdecl, [(* (gid, gdecl) *)])
  | CArr (t, e_list) -> 
    let num_of_els = List.length e_list in
    let el_type = 
      begin match t with
      | Ast.TRef (Ast.RString) -> Ll.Array (num_of_els + 1, Ll.I8)
      | _ -> cmp_ty t
    end in
    let arr_type = Ll.Struct ([Ll.I64; (Ll.Array ((num_of_els), el_type))]) in
    let len_el = [ (Ll.I64, Ll.GInt (Int64.of_int @@ num_of_els)) ] in
    (* (Ll.gdecl * (Ll.gid * Ll.gdecl) list) list *)
    (* let gdecls_list = List.map cmp_gexp e_list in  *)
    let gdecls_list = List.map (fun (i,j) -> i) (List.map cmp_gexp e_list) in 
    let array_el = [ (Ll.Array (num_of_els, el_type), Ll.GArray (gdecls_list)) ] in
    let arr_gdecl = (arr_type, Ll.GStruct (len_el @ array_el) ) in
    (* let gdecls = List.map (fun (g, _) -> g) gdecls_list in *)
    (arr_gdecl, [])
  | _ -> failwith "Error for other cases"
  end


(* Oat internals function context ------------------------------------------- *)
let internals =
  [ "oat_malloc",              Ll.Fun ([I64], Ptr I64)
  ; "oat_alloc_array",         Ll.Fun ([I64], Ptr (Struct [I64; Array (0, I64)]))
  ; "oat_assert_not_null",     Ll.Fun ([Ptr I8], Void)
  ; "oat_assert_array_length", Ll.Fun ([Ptr I64; I64], Void)
  ]

(* Oat builtin function context --------------------------------------------- *)
let builtins =
  [ "array_of_string",  cmp_ty @@ TFun ([TRef RString],  TRef(RArray TInt))
  ; "string_of_array",  cmp_ty @@ TFun ([TRef(RArray TInt)], TRef RString)
  ; "length_of_string", cmp_ty @@ TFun ([TRef RString],  TInt)
  ; "string_of_int",    cmp_ty @@ TFun ([TInt],  TRef RString)
  ; "string_cat",       cmp_ty @@ TFun ([TRef RString; TRef RString], TRef RString)
  ; "print_string",     cmp_ty @@ TFun ([TRef RString],  TVoid)
  ; "print_int",        cmp_ty @@ TFun ([TInt],  TVoid)
  ; "print_bool",       cmp_ty @@ TFun ([TBool], TVoid)
  ]

(* Compile a OAT program to LLVMlite *)
let cmp_prog (p:Ast.prog) : Ll.prog =
  (* add built-in functions to context *)
  let init_ctxt = 
    List.fold_left (fun c (i, t) -> 
        Ctxt.add c i (t, Gid i)
      ) Ctxt.empty builtins in
  (* build global variable context *)
  let c = cmp_global_ctxt init_ctxt p in
  (* compile functions and global variables *)
  let fdecls, gdecls = 
    List.fold_right (fun d (fs, gs) ->
        match d with
        | Ast.Gvdecl { elt=gd } -> 
           (* Printf.printf "gd.name == %s\n" gd.name; *)
           let ll_gd, gs' = cmp_gexp gd.init in
           (fs, (gd.name, ll_gd)::gs' @ gs)
        | Ast.Gfdecl fd ->
           (* Printf.printf "fd.elt.name == %s\n" fd.elt.name; *)
           let fdecl, gs' = cmp_fdecl c fd in
           (fd.elt.name,fdecl)::fs, gs' @ gs
      ) p ([], [])
  in
  (* gather external declarations *)
  let edecls = internals @ builtins in
  { tdecls=[]; gdecls; fdecls; edecls }