package Giant.Parser.Tokens is


  subtype YYSType is Syntax_Node;

  Error_Message   : Unbounded_String;

  Root_Node       : Syntax_Node;
  Literal_Boolean : Gsl_Boolean;
  Literal_Natural : Gsl_Natural;
  Literal_String  : Gsl_String;
  Var_Reference   : Gsl_Var_Reference;
  Identifier      : Gsl_Identifiers.Identifier_Type;
  Script_Ref      : Gsl_Script_Reference;

    YYLVal, YYVal : YYSType; 
    type Token is
        (End_Of_Input, Error, Subgraph_T, Selection_T,
         Int_Literal_T, String_Literal_T, Identifier_T,
         Visible_Ref_T, Null_T, False_T,
         True_T, '{', '}',
         ',', '(', ')',
         ';', '[', ']',
         '.', '+' );

    Syntax_Error : exception;

end Giant.Parser.Tokens;
