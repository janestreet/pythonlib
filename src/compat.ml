open! Base

[%%if ocaml_version >= (4, 14, 0)]
let get_desc val_type = Types.get_desc val_type
[%%else]
let get_desc val_type = val_type.Types.desc
[%%endif]
