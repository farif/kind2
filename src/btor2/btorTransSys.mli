
val filter_nodes: BtorAst.btor -> StateVar.t list

val trans_sys_of_nodes :
          ?preserve_sig:bool ->
          ?slice_nodes:bool -> 'a -> Analysis.param -> TransSys.t * 'a

