open Printf
open JasminAst

exception NotImplemented

let crawl_structs { top_level_vars; methods; _ } = 
  let map = Hashtbl.create 16 in
  let next = Utils.new_counter 0 in 
  let top_jtypes = List.map (fun {jtype; _ } -> jtype ) top_level_vars in
  let method_bindings = 
    List.flatten 
      (List.map 
        (fun {local_mapping; } -> LocalVarMap.bindings local_mapping)
        methods ) in 
  let method_jtypes = List.map (fun (x, (i, y)) -> y) method_bindings in 
  let register_struct t = match t with
  | JStruct(sfl) -> 
      if Hashtbl.mem map sfl then () else
      Hashtbl.add map sfl ("StructClass_" ^ (string_of_int (next ()))) 
  | _ -> () in 
  let () = List.iter register_struct top_jtypes in 
  let () = List.iter register_struct method_jtypes in  
  (fun x ->  Hashtbl.find map x),
  Hashtbl.fold (fun k n l -> k :: l) map []

let print_class { source; top_level_vars; methods; } struct_map out_directory class_name = 
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
    let () = print (string_of_jtype struct_map jtype) in
    newline ()
  in
  let rec print_method_statement locals = function
  | JLabel(label) -> println (label ^ ":")
  | JInst(inst) -> println ("  " ^ (string_of_jinst struct_map inst))
  | PS(pinst) -> List.iter (print_method_statement locals) (real_statements (global_field_map, locals, struct_map) pinst )
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
    let () = println (string_of_jsig struct_map signature) in
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

let print_main_class bytecode_ast struct_map out_directory = 
  print_class bytecode_ast struct_map out_directory main_class_name 

let get_struct_class jsfield_list = 
  let next_var_num = Utils.new_counter 0 in 
  { 
    source = "autogenerated_struct_class";
    top_level_vars = 
      List.map 
        (fun (name, t) -> 
            let var_number = next_var_num () in 
            { 
              name = name;
              var_number = var_number;
              jtype = t;
              init_code = [];
            } )
        jsfield_list;
    methods = [];
  }

let print_struct_classes struct_list struct_map out_directory = 
  let name_and_classes = List.map (fun x-> struct_map x, get_struct_class x) struct_list in 
  List.iter (fun (name, kls) -> print_class kls struct_map out_directory name) name_and_classes
