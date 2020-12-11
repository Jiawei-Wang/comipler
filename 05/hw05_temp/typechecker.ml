open Ast
open Astlib
open Tctxt

(* Error Reporting ---------------------------------------------------------- *)
(* NOTE: Use type_error to report error messages for ill-typed programs. *)

exception TypeError of string

let type_error (l : 'a node) err = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))


(* initial context: G0 ------------------------------------------------------ *)
(* The Oat types of the Oat built-in functions *)
let builtins =
  [ "array_of_string",  ([TRef RString],  RetVal (TRef(RArray TInt)))
  ; "string_of_array",  ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", ([TRef RString],  RetVal TInt)
  ; "string_of_int",    ([TInt], RetVal (TRef RString))
  ; "string_cat",       ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     ([TRef RString],  RetVoid)
  ; "print_int",        ([TInt], RetVoid)
  ; "print_bool",       ([TBool], RetVoid)
  ]


(* binary operation types --------------------------------------------------- *)
let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Eq | Neq | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)

(* unary operation types ---------------------------------------------------- *)
let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* subtyping ---------------------------------------------------------------- *)
(* Decides whether H |- t1 <: t2 
    - assumes that H contains the declarations of all the possible struct types

    - you will want to introduce addition (possibly mutually recursive) 
      helper functions to implement the different judgments of the subtyping
      relation. We have included a template for subtype_ref to get you started.
      (Don't forget about OCaml's 'and' keyword.)
*)
let rec subtype (c : Tctxt.t) (t1 : Ast.ty) (t2 : Ast.ty) : bool =
  (match t1,t2 with
    | TInt, TInt -> true
    | TBool, TBool -> true
    | TNullRef(rt1), TNullRef(rt2) -> (subtype_ref c rt1 rt2)
    | TRef(rt1), TRef(rt2) -> (subtype_ref c rt1 rt2)
    | TRef(rt1), TNullRef(rt2) -> (subtype_ref c rt1 rt2)
    | _, _ -> false)

(* Decides whether H |-r ref1 <: ref2 *)
and subtype_ref (c : Tctxt.t) (t1 : Ast.rty) (t2 : Ast.rty) : bool =
  (match t1,t2 with
    | RString, RString -> true
    | RArray(a1), RArray(a2) -> a1 = a2
    | RFun(tys1,ret1), RFun(tys2,ret2) ->
      let arg_tys =
        (List.fold_left2
          (fun r e1 e2 -> (r && (subtype c e2 e1)))
          true tys1 tys2)
      in
      arg_tys && (ret_subty c ret1 ret2)
    | RStruct(id1), RStruct(id2) ->
      let struct_def1 = (Tctxt.lookup_struct id1 c) in
      let struct_def2 = (Tctxt.lookup_struct id2 c) in
      if (List.length struct_def1) >= (List.length struct_def2)
      then
        (List.fold_left
           (fun r {fieldName;ftyp} ->
              (r &&
              (is_some (Tctxt.lookup_field_option id1 fieldName c)) &&
               ftyp = (Tctxt.lookup_field id1 fieldName c)))
           true struct_def2)
      else false
    | _, _ -> false)


(* well-formed types -------------------------------------------------------- *)
(* Implement a (set of) functions that check that types are well formed according
   to the H |- t and related inference rules

    - the function should succeed by returning () if the type is well-formed
      according to the rules

    - the function should fail using the "type_error" helper function if the 
      type is not well formed

    - l is just an ast node that provides source location information for
      generating error messages (it's only needed for the type_error generation)

    - tc contains the structure definition context
 *)
let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  begin match t with
    | TBool  -> ()
    | TInt   -> ()
    | TRef r -> typecheck_rt l tc r 
  end

(* A helper function to determine whether a type allows the null value *)
let is_nullable_ty (t : Ast.ty) : bool =
  match t with
  | TNullRef _ -> true
  | _ -> false

(* typechecking expressions ------------------------------------------------- *)
(* Typechecks an expression in the typing context c, returns the type of the
   expression.  This function should implement the inference rules given in the
   oat.pdf specification.  There, they are written:

       H; G; L |- exp : t

   See tctxt.ml for the implementation of the context c, which represents the
   four typing contexts: H - for structure definitions G - for global
   identifiers L - for local identifiers

   Returns the (most precise) type for the expression, if it is type correct
   according to the inference rules.

   Uses the type_error function to indicate a (useful!) error message if the
   expression is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   Notes: - Structure values permit the programmer to write the fields in any
   order (compared with the structure definition).  This means that, given the
   declaration struct T { a:int; b:int; c:int } The expression new T {b=3; c=4;
   a=1} is well typed.  (You should sort the fields to compare them.)

*)
let rec typecheck_exp (c : Tctxt.t) (e : Ast.exp node) : Ast.ty =
  match e.elt with
  | CNull t -> t
  | CBool b -> TBool
  | CInt i -> TInt
  | CStr s -> TRef RString
  | CArr (a, l) ->
    let types_of = List.map (typecheck_exp c) l in
    if List.for_all ((=) a) types_of then TRef (RArray a)
    else type_error e "Mismatched array type"
  | NewArr (t, s) ->
      let size_type = typecheck_exp c s in
      if size_type = TInt then TRef (RArray t)
      else type_error s "Array size not an int"
  | Id i -> (match Tctxt.lookup_option i c with 
            | Some x -> x
            | None ->
              begin match Tctxt.lookup_function_option i c with
                | Some ft -> TRef (RFun ft)
                | None -> type_error e ("Unbound identifier " ^ i)
              end
            )
  | Bop (b, l, r) -> 
      let ltyp = typecheck_exp c l in
      let rtyp = typecheck_exp c r in
      let (bl, br, bres) = typ_of_binop b in
      if bl = ltyp then 
        if br = rtyp then bres
        else type_error r "Incorrect type in binary expression"       
      else type_error l "Incorrect type in binary expression"
  | Uop (u, e) ->
      let t = typecheck_exp c e in
      let (us, ures) = typ_of_unop u in
      if us = t then ures else type_error e "Incorrect type for unary operator"
  | Index (e1, e2) ->
      let arr_t = typecheck_exp c e1 in
      let ind_t = typecheck_exp c e2 in
      if ind_t = TInt then
        match arr_t with
        | TRef (RArray t) -> t
        | _ -> type_error e1 "Cannot index into non-array"
      else type_error e2 "Index of array index operator not an int"
  | Proj (s, id) ->
      let str_t = typecheck_exp c s in
      (match str_t with
      | TRef (RStruct sn) ->  
        (match Tctxt.lookup_field_option sn id c with
        | None -> type_error e (id ^ " not member of struct " ^ sn)
        | Some t -> t)
      | _ -> type_error s "Cannot project from non-struct")
  | CStruct (id, l) ->
      (match Tctxt.lookup_struct_option id c with
      | None -> type_error e (id ^ "not a struct type")
      | Some x ->
          let tc_field f = f.cfname, typecheck_exp c f.cfinit in
          let field_types = List.map tc_field l in
          let struct_names = List.sort compare (List.map (fun x -> x.fname) x) in
          let local_names = List.sort compare (List.map fst field_types) in
          if struct_names <> local_names 
          then type_error e "Mismatch of fields between struct definition and local declaration";
          List.iter (fun (id, ft) -> 
            let t = (List.find (fun i -> i.fname = id) x).ftyp in
            if t <> ft then type_error e (id ^ " field of struct incorrect")
            else ()) field_types;
          TRef (RStruct id))
  | Call (f, args) ->
      let argtyps = List.map (typecheck_exp c) args in
      match (typecheck_exp c f) with
      | TRef (RFun (l, RetVal r)) ->
          if List.length l <> List.length argtyps then type_error e "Incorrect number of arguments"
          else List.iter2 (fun arg l -> if arg <> l then type_error e "Incorrect type of argument") argtyps l;
          r
      | _ -> type_error e "Need function argument for function call"

(* statements --------------------------------------------------------------- *)

(* Typecheck a statement 
   This function should implement the statment typechecking rules from oat.pdf.  

   Inputs:
    - tc: the type context
    - s: the statement node
    - to_ret: the desired return type (from the function declaration)

   Returns:
     - the new type context (which includes newly declared variables in scope
       after this statement)

     - A boolean indicating the return behavior of a statement:
        false:  might not return
        true: definitely returns 

        in the branching statements, the return behavior of the branching 
        statement is the conjunction of the return behavior of the two 
        branches: both both branches must definitely return in order for 
        the whole statement to definitely return.

        Intuitively: if one of the two branches of a conditional does not 
        contain a return statement, then the entire conditional statement might 
        not return.
  
        looping constructs never definitely return 

   Uses the type_error function to indicate a (useful!) error message if the
   statement is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   - You will probably find it convenient to add a helper function that implements the 
     block typecheck rules.
*)
let rec typecheck_stmt (tc : Tctxt.t) (s:Ast.stmt node) (to_ret:ret_ty) : Tctxt.t * stmt_type =
  match s.elt with
  | Assn (e1, e2) ->
    let assn_to = typecheck_exp tc e1 in
    let assn_from = typecheck_exp tc e2 in
    if assn_to = assn_from then tc, NoReturn else type_error s "Mismatched types in assignment"

  | Decl (id, exp) ->
      let exp_type = typecheck_exp tc exp in
      if List.exists (fun x -> fst x = id) tc.locals then type_error s "Cannot redeclare variable"
      else Tctxt.add_local tc id exp_type, NoReturn 

  | Ret r ->
      (match r, to_ret with
      | None, RetVoid -> tc, Return
      | Some r, RetVal to_ret -> 
          let t = typecheck_exp tc r in
          if t = to_ret then tc, Return
          else type_error s "Returned incorrect type" 
      | None, RetVal to_ret -> type_error s "Returned void in non-void function"
      | Some r, RetVoid -> type_error s "Returned non-void in void function")

  | SCall (f, args) -> 
      let argtyps = List.map (typecheck_exp tc) args in
      (match (typecheck_exp tc f) with
      | TRef (RFun (l, RetVoid)) ->
          if List.length l <> List.length argtyps then type_error s "Incorrect number of arguments"
          else List.iter2 (fun arg l -> if arg <> l then type_error s "Incorrect type of argument") argtyps l;
          tc, NoReturn
      | _ -> type_error s "Need function argument for function call")

  | If (e, b1, b2) ->
      let guard_type = typecheck_exp tc e in
      if guard_type <> TBool then type_error e "Incorrect type for guard"
      else
        let (_, lft_ret) = typecheck_block tc b1 to_ret in
        let (_, rgt_ret) = typecheck_block tc b2 to_ret in
        (match lft_ret, rgt_ret with
        | Return, Return -> tc, Return
        | _ -> tc, NoReturn)

  | While (b, bl) ->
      let guard_type = typecheck_exp tc b in
      if guard_type <> TBool then type_error b "Incorrect type for guard"
      else 
        let _ = typecheck_block tc bl to_ret in
        tc, NoReturn

  | For (vs, guard, s, b) ->
    let updated_context =
      List.fold_left (fun c (id, e) ->
        let t = typecheck_exp tc e in
        Tctxt.add_local c id t) tc vs in
    let _ = (match guard with
            | None -> ()
            | Some b -> if TBool <> typecheck_exp updated_context b then type_error b "Incorrect type for guard" else ()) in
    let _ = (match s with
            | None -> ()
            | Some s -> 
                let (nc, rt) = typecheck_stmt updated_context s to_ret in
                match rt with
                | NoReturn -> ()
                | Return   -> type_error s "Cannot return in for loop increment") in
    let _ = typecheck_block updated_context b to_ret in
    tc, NoReturn


(* struct type declarations ------------------------------------------------- *)
(* Here is an example of how to implement the TYP_TDECLOK rule, which is 
   is needed elswhere in the type system.
 *)

(* Helper function to look for duplicate field names *)
let rec check_dups fs =
  match fs with
  | [] -> false
  | h :: t -> (List.exists (fun x -> x.fieldName = h.fieldName) t) || check_dups t

let typecheck_tdecl (tc : Tctxt.t) id fs  (l : 'a Ast.node) : unit =
  if check_dups fs
  then type_error l ("Repeated fields in " ^ id) 
  else List.iter (fun f -> typecheck_ty l tc f.ftyp) fs

(* function declarations ---------------------------------------------------- *)
(* typecheck a function declaration 
    - extends the local context with the types of the formal parameters to the 
      function
    - typechecks the body of the function (passing in the expected return type
    - checks that the function actually returns
*)
let typecheck_fdecl (tc : Tctxt.t) (f : Ast.fdecl) (l : 'a Ast.node)  =
  let updated = List.fold_left (fun c (t, i) -> Tctxt.add_local c i t) tc f.args in
  let _, returned = typecheck_block updated f.body f.rtyp in
  match returned with
  | NoReturn -> type_error l "Need return statement"
  | Return -> () 

(* creating the typchecking context ----------------------------------------- *)

(* The following functions correspond to the
   judgments that create the global typechecking context.

   create_struct_ctxt: - adds all the struct types to the struct 'S'
   context (checking to see that there are no duplicate fields

     H |-s prog ==> H'


   create_function_ctxt: - adds the the function identifiers and their
   types to the 'G' context (ensuring that there are no redeclared
   function identifiers)

     H ; G1 |-f prog ==> G2


   create_global_ctxt: - typechecks the global initializers and adds
   their identifiers to the 'G' global context

     H ; G1 |-g prog ==> G2    


   NOTE: global initializers may mention function identifiers as
   constants, but can mention only other global values that were declared earlier
*)

let create_struct_ctxt p =
  List.fold_left (fun c d ->
    match d with
    | Gtdecl ({elt=(id, fs)} as l) ->
        if check_dups fs then type_error l ("Repeated fields in " ^ id) 
        else if List.exists (fun (id',fs') -> check_dups (fs@fs')) c.structs
         then type_error l ("Duplicate fields in " ^ id)
        else if List.exists (fun x -> id = fst x) c.structs then
         type_error l ("Redeclaration of struct " ^ id)
        else Tctxt.add_struct c id fs
    | _ -> c) Tctxt.empty p


let create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  let builtins_context = 
    List.fold_left (fun c (id, t) -> Tctxt.add_function c id t) tc builtins
  in
    List.fold_left (fun c d ->
      match d with
      | Gfdecl ({elt=f} as l)  ->
        if List.exists (fun x -> fst x = f.name) c.functions
        then type_error l ("Redeclaration of " ^ f.name)
        else Tctxt.add_function c f.name (List.map fst f.args, f.rtyp)
      | _ -> c) builtins_context p

let create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  List.fold_left (fun c d ->
    match d with
    | Gvdecl ({elt=decl} as l) ->  
        let e = typecheck_exp tc decl.init in
        if List.exists (fun x -> fst x = decl.name) c.globals
        then type_error l ("Redeclaration of " ^ decl.name)
        else Tctxt.add_global c decl.name e
    | _ -> c) tc p


(* This function implements the |- prog and the H ; G |- prog 
   rules of the oat.pdf specification.   
*)
let typecheck_program (p:Ast.prog) : unit =
  let sc = create_struct_ctxt p in
  let fc = create_function_ctxt sc p in
  let tc = create_global_ctxt fc p in
  List.iter (fun p ->
    match p with
    | Gfdecl ({elt=f} as l) -> typecheck_fdecl tc f l
    | Gtdecl ({elt=(id, fs)} as l) -> typecheck_tdecl tc id fs l 
    | _ -> ()) p
