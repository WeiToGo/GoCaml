(*

  This file is part of GoCaml, a compiler that compiles Go-Lite (a subset of Go) to Java bytecode. 

  Copyright (C) 2015 by Deepanjan Roy, Wei Gao, Omar Gonzalez 


  GoCaml is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  GoCaml is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Foobar.  If not, see <http://www.gnu.org/licenses/>. 

  This code originated as a project for the COMP 520 class at McGill University
  in Winter 2015. Any subsequent COMP 520 student who is viewing this code must 
  follow the course rules and report any viewing and/or use of the code.

*)

open Ast
open Symtable

exception InternalError of string


(* Produces hashmap of go_structs ot class_names *)
let generate_struct_to_class_map go_ast =
  let map = Hashtbl.create 16 in
  let next = Utils.new_counter 0 in
  
  let add_to_map go_struct = 
    if Hashtbl.mem map go_struct then () 
    else Hashtbl.add map go_struct ("StructClass_" ^ (string_of_int (next ())))
  in

  let rec crawl_type go_type = match go_type with
  | GoInt | GoFloat | GoBool | GoRune | GoString -> ()
  | GoSlice(t) -> crawl_type t 
  | GoArray(_, t) -> crawl_type t
  | GoStruct(field_list) -> add_to_map field_list;
      List.iter (fun (name, t) -> crawl_type t) field_list;
  | GoCustom(_, t) -> crawl_type t
  | GoFunction(t_list, t_op) -> 
      let () = List.iter crawl_type t_list in 
      (match t_op with 
      | None -> ()
      | Some t -> crawl_type t)
  | NewType t -> crawl_type t 
  in 

  let rec crawl_program (Program(_, ltd_list)) = 
    List.iter crawl_ltd ltd_list

  and crawl_ltd (LinedTD(td, _)) = (match td with
  | FunctionDecl(id, fun_sig, stmt_list) -> 
      let () = crawl_id id in
      let () = crawl_sig fun_sig in 
      List.iter crawl_stmt stmt_list
  | TypeDeclBlock _ -> ()
  | VarDeclBlock mvd_list -> List.iter crawl_mvd mvd_list )

  and crawl_id id = match id with 
  | BlankID -> ()
  | ID(_, entry_op_ref) ->  (match !entry_op_ref with
    | None -> ()
    | Some (Entry (_, t, _, _, _)) -> crawl_type t )

  and crawl_sig (FunctionSig(f_arg_list, _)) = 
    List.iter (fun (FunctionArg(id, _)) -> crawl_id id) f_arg_list

  and crawl_stmt (LinedStatement(_, stmt)) = match stmt with 
  | EmptyStatement
  | TypeDeclBlockStatement _
  | BreakStatement
  | ContinueStatement -> ()
  | ExpressionStatement e -> crawl_expression e
  | AssignmentStatement ee_list -> 
      List.iter 
        (fun (e1, e2) -> 
          let () = crawl_expression e1 in 
          crawl_expression e2) 
      ee_list
  | ShortVarDeclStatement shvd_list -> List.iter crawl_shvd shvd_list
  | VarDeclBlockStatement mvd_list -> 
      List.iter crawl_mvd mvd_list
  | PrintStatement e_list | PrintlnStatement e_list -> 
      List.iter crawl_expression e_list 
  | IfStatement (s_op, e, stmt_list, else_stmt_list_op) -> 
      let () = match s_op with
      | None -> ()
      | Some s -> crawl_stmt s in 
      let () = crawl_expression e in 
      let () = List.iter crawl_stmt stmt_list in 
      (match else_stmt_list_op with 
      | None -> ()
      | Some else_stmt_list -> List.iter crawl_stmt else_stmt_list)
  | ReturnStatement(e_op) -> (match e_op with
      | None -> ()
      | Some e -> crawl_expression e )
  | SwitchStatement (s_op, e, case_list) -> 
      let () = match s_op with 
      | None -> ()
      | Some s -> crawl_stmt s in 
      let () = crawl_expression e in 
      List.iter crawl_switch_case case_list
  | ForStatement (init_op, e_op, post_op, stmt_list) -> 
      let () = match init_op with 
      | None -> ()
      | Some s -> crawl_stmt s in 
      let () = match e_op with
      | None -> ()
      | Some e -> crawl_expression e in 
      let () = match post_op with 
      | None -> ()
      | Some s -> crawl_stmt s in 
      List.iter crawl_stmt stmt_list
  | BlockStatement stmt_list -> List.iter crawl_stmt stmt_list

  and crawl_mvd (MultipleVarDecl(svd_list)) = 
    List.iter crawl_svd svd_list

  and crawl_svd (SingleVarDecl(id, _, e_op)) = 
    let () = crawl_id id in match e_op with 
    | None -> ()
    | Some e -> crawl_expression e

  and crawl_shvd (ShortVarDecl(id, e)) = 
    let () = crawl_id id in crawl_expression e


  and crawl_expression (Expression(exp, type_ref)) = 
    let () = match !type_ref with
    (* Hack: assume the expression was evaluated somewhere else *)
    | None -> ((* raise (InternalError("Expression information missing in AST. Did you typechck it?") *)())
    | Some t -> crawl_type t in 
    match exp with
    | IdExp id -> crawl_id id 
    | LiteralExp _ -> ()
    | UnaryExp (_, e) -> crawl_expression e
    | BinaryExp (_, e1, e2) -> let () = crawl_expression e1 in crawl_expression e2
    | FunctionCallExp (e, e_list) ->
       let () = crawl_expression e in 
       List.iter crawl_expression e_list
    | AppendExp (id, e) -> let () = crawl_id id in crawl_expression e
    | TypeCastExp (_, e) -> crawl_expression e
    | IndexExp (e1, e2) -> let () = crawl_expression e1 in crawl_expression e2
    | SelectExp (e1, id) -> let () = crawl_expression e1 in crawl_id id

  and crawl_switch_case c = match c with
  | SwitchCase(e_list, stmt_list) -> 
      let () = List.iter crawl_expression e_list in 
      List.iter crawl_stmt stmt_list
  | DefaultCase stmt_list -> List.iter crawl_stmt stmt_list

  in 
  let () = crawl_program go_ast in 
  map  

let struct_class_getter_factory_factory_JAVA struct_hashtable = 
  let map = struct_hashtable in 
  fun gostruct -> 
    try Hashtbl.find map gostruct
    with Not_found -> raise (InternalError("Cannot find struct in struct map. Might be a struct gen bug."))

let get_all_sructs struct_hashtable = 
  Hashtbl.fold (fun k n l -> k :: l) struct_hashtable []   
