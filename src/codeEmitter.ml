open Printf
open JasminAst

exception NotImplemented

let print_main_class { source; top_level_vars; methods; } out_directory = 
  let class_name = "GeneratedBytecode" in 
  let global_field_map = get_global_var_map top_level_vars in 
  let out_channel = open_out ((Filename.concat out_directory (class_name) ^ ".j")) in
  let print s = fprintf out_channel "%s" s  in 
  let println s = fprintf out_channel "%s\n" s in 
  let newline () = fprintf out_channel "\n" in 
  let () = println (".source " ^ source) in 
  let () = println (".class public " ^ class_name) in
  let () = println (".super java/lang/Object") in
  let () = newline () in 
  let print_global_field {name; jtype; _} = 
    let () = print ".field public static " in
    let () = print name in
    let () = print " " in 
    let () = print (string_of_jtype jtype) in
    newline ()
  in
  let rec print_method_statement locals = function
  | JLabel(label) -> println (label ^ ":")
  | JInst(inst) -> println ("  " ^ (string_of_jinst inst))
  | PS(pinst) -> List.iter (print_method_statement locals) (real_instructions (global_field_map, locals) pinst )
  in
  let print_method_body locals stmts = 
    let local_limit = calculate_local_limit stmts in
    let operand_stack_limit = calculate_ostack_limit stmts in
    let () = println ("  .limit locals " ^ (string_of_int local_limit)) in
    let () = println ("  .limit stack " ^ (string_of_int operand_stack_limit)) in
    List.iter (print_method_statement locals) stmts
  in
  let print_method {signature; code; local_mapping;} = 
    let () = print ".method public static " in
    let () = println (string_of_jsig signature) in
    let () = print_method_body local_mapping code in 
    let () = println ".end method" in newline ()
  in
  let print_dumb_init () = 
    let () = println ".method public <init>()V" in
    let () = println "  aload_0" in
    let () = println "  invokespecial java/lang/Object/<init>()V" in 
    let () = println "  return" in
    let () = println ".end method" in
    newline ()
  in
  let print_clinit () = 
    let () = println ".method static <clinit>()V" in
    let clinit_clause_list = List.map (fun {init_code; _ } -> init_code) top_level_vars in
    let clinit_stmts = List.flatten clinit_clause_list in 
    let () = print_method_body LocalVarMap.empty clinit_stmts in 
    let () = println "  return" in 
    let () = println ".end method" in newline ()
  in
  let () = List.iter print_global_field top_level_vars in
  let () = newline () in
  let () = print_dumb_init () in
  let () = List.iter print_method methods in
  let () = print_clinit () in
  close_out out_channel


let print_struct_class { source; top_level_vars; methods; } out_directory = raise NotImplemented
