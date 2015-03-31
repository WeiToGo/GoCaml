open Printf
open JasminAst

exception NotImplemented

let out_directory = ref (Filename.current_dir_name)

let print_class { source; class_name; field_list; method_list; clinit } = 
  let out_channel = open_out ((Filename.concat !out_directory (class_name) ^ ".j")) in
  let print s = fprintf out_channel "%s" s  in 
  let println s = fprintf out_channel "%s\n" s in 
  let newline () = fprintf out_channel "\n" in 
  let () = println (".source " ^ source) in 
  let () = println (".class public " ^ class_name) in
  let () = println (".super java/lang/Object") in
  let () = newline () in 
  let print_field (JField(name, jtyp)) = 
    let () = print ".field public static " in
    let () = print name in
    let () = print " " in 
    let () = print (string_of_jtype jtyp) in
    newline ()
  in
  let print_method_statement = function
  | JLabel(label) -> println (label ^ ":")
  | JInst(inst) -> println ("  " ^ (string_of_jinst inst))
  in
  let print_method_body stmts = 
    let local_limit = calculate_local_limit stmts in
    let operand_stack_limit = calculate_ostack_limit stmts in
    let () = println ("  .limit locals " ^ (string_of_int local_limit)) in
    let () = println ("  .limit stack " ^ (string_of_int operand_stack_limit)) in
    List.iter print_method_statement stmts
  in
  let print_method (JMethod(jsig, stmts)) = 
    let () = print ".method public static " in
    let () = println (string_of_jsig jsig) in
    let () = print_method_body stmts in 
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
  let print_clinit stmts = 
    let () = println ".method static <clinit>()V" in 
    let () = print_method_body stmts in
    let () = println ".end method" in newline ()
  in
  let () = List.iter print_field field_list in
  let () = newline () in
  let () = print_dumb_init () in
  let () = List.iter print_method method_list in
  let () = print_clinit clinit in
  close_out out_channel
