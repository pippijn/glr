module type S = sig
  module Ptree : Sig.ConvertibleType
end

val make_lexer : 'a UserActions.t -> 'b Lexerint.lexer -> 'b Lexerint.lexer
val make_actions : 'a UserActions.t -> ParseTablesType.t -> PtreeNode.t UserActions.t
