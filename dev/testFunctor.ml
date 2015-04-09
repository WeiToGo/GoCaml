module type StructMap = sig
  val struct_class_name: Symtable.go_struct -> string
  val struct_clases: (Symtable.go_struct * string) list 
end

module Make (X: StructMap) = 
struct 
  let gen_code () = let _, s = (List.hd X.struct_clases) in s
end
