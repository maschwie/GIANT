------------------------------------------------------------------------------
-- GIANT - Graphical IML Analysis and Navigation Tool
--
-- Copyright (C) 2003 Phillip Haeuser, Steffen Keul, Oliver Kopp,
-- Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
-- First Author: Steffen Keul
--
-- $RCSfile: giant-parser.y,v $
-- $Author: keulsn $
-- $Date: 2003-10-05 20:49:11 $
--
-- GSL Parser rules
-- Use ayacc to generate code, needs scanner scanner.aflex
--

-- Declarations Section
%token SUBGRAPH_T, SELECTION_T, INT_LITERAL_T,
  STRING_LITERAL_T, IDENTIFIER_T, VISIBLE_REF_T, NULL_T, FALSE_T, TRUE_T


{
  subtype YYSType is Syntax_Node;

  Error_Message   : Unbounded_String;

  Root_Node       : Syntax_Node;
  Literal_Boolean : Gsl_Boolean;
  Literal_Natural : Gsl_Natural;
  Literal_String  : Gsl_String;
  Var_Reference   : Gsl_Var_Reference;
  Identifier      : Gsl_Identifiers.Identifier_Type;
  Script_Ref      : Gsl_Script_Reference;
}

%start root 

%% 
------------------------------------------------------------------------------
-- Rules section
root               : expression
                     { Root_Node := $1; };

expression         : literal
                     { $$ := $1; }
                   | global_var
                     { $$ := $1; }
                   | visible_var
                     { $$ := $1; }
                   | global_ref
                     { $$ := $1; }
                   | visible_ref
                     { $$ := $1; }
                   | var_creation
                     { $$ := $1; }
                   | list
                     { $$ := $1; }
                   | sequence
                     { $$ := $1; }
                   | '{' list expression '}'
                     { 
                        $$ := Create_Node (Script_Decl, $2, $3);
                        Script_Ref := Create_Gsl_Script_Reference ($2, $3);
                        Set_Literal ($$, Gsl_Type (Script_Ref));
                     }
                   | expression list
                     { $$ := Create_Node (Script_Activation, $1, $2); };

list_body          : expression
                     { $$ := Create_Node (List, $1, Null_Node); }
                   | expression ',' list_body
                     { $$ := Create_Node (List, $1, $3); };

list               : '(' ')'
                     { $$ := Create_Node (List, Null_Node, Null_Node); }
                   | '(' list_body ')'
                     { $$ := $2; };

sequence_body      : expression ';'
                     { $$ := Create_Node (Sequence, $1, Null_Node); }
                   | expression ';' sequence_body
                     { $$ := Create_Node (Sequence, $1, $3); };

sequence           : '[' ']'
                     { $$ := Create_Node (Sequence, Null_Node, Null_Node); }
                   | '[' sequence_body ']'
                     { $$ := $2; };

-- Literals
literal            : FALSE_T
                     { 
                        $$ := Create_Node (Literal, Null_Node, Null_Node);
                        Literal_Boolean := Create_Gsl_Boolean (false);
                        Set_Literal ($$, Gsl_Type (Literal_Boolean));
                     }
                   | TRUE_T
                     { 
                        $$ := Create_Node (Literal, Null_Node, Null_Node);
                        Literal_Boolean := Create_Gsl_Boolean (true);
                        Set_Literal ($$, Gsl_Type (Literal_Boolean));
                     }
                   | INT_LITERAL_T
                     { 
                        $$ := Create_Node (Literal, Null_Node, Null_Node);
                        Literal_Natural := Create_Gsl_Natural;
                        Set_Value (Literal_Natural,
                                   Natural'Value(scanner_dfa.yytext));
                        Set_Literal ($$, Gsl_Type (Literal_Natural));
                     }
                   | STRING_LITERAL_T
                     { 
                        $$ := Create_Node (Literal, Null_Node, Null_Node);
                        Literal_String := Create_Gsl_String
                          (scanner_dfa.yytext (2..scanner_dfa.yylength-1));
                        Set_Literal ($$, Gsl_Type (Literal_String));
                     }
                   | NULL_T
                     { 
                        $$ := Create_Node (Literal, Null_Node, Null_Node);
                        Set_Literal ($$, Gsl_Null);
                     };

-- Identifiers
global_var         : SUBGRAPH_T '.' IDENTIFIER_T
                     {
                        $$ := Create_Node (Global_Var, Null_Node, Null_Node);
                        Identifier := Gsl_Identifiers.Get_Identifier
                          (scanner_dfa.yytext);
                        Var_Reference := Create_Gsl_Var_Reference
                          (Subgraph, Identifier);
                        Set_Literal ($$, Gsl_Type (Var_Reference));
                     }
                   | SELECTION_T '.' IDENTIFIER_T
                     { 
                        $$ := Create_Node (Global_Var, Null_Node, Null_Node);
                        Identifier := Gsl_Identifiers.Get_Identifier
                          (scanner_dfa.yytext);
                        Var_Reference := Create_Gsl_Var_Reference
                          (Selection, Identifier);
                        Set_Literal ($$, Gsl_Type (Var_Reference));
                     };
global_ref         : VISIBLE_REF_T SUBGRAPH_T '.' IDENTIFIER_T
                     { 
                        $$ := Create_Node (Global_Ref, Null_Node, Null_Node);
                        Identifier := Gsl_Identifiers.Get_Identifier
                          (scanner_dfa.yytext);
                        Var_Reference := Create_Gsl_Var_Reference
                          (Subgraph, Identifier);
                        Set_Literal ($$, Gsl_Type (Var_Reference));
                     }
                   | VISIBLE_REF_T SELECTION_T '.' IDENTIFIER_T
                     { 
                        $$ := Create_Node (Global_Ref, Null_Node, Null_Node);
                        Identifier := Gsl_Identifiers.Get_Identifier
                          (scanner_dfa.yytext);
                        Var_Reference := Create_Gsl_Var_Reference
                          (Selection, Identifier);
                        Set_Literal ($$, Gsl_Type (Var_Reference));
                     };

visible_var        : IDENTIFIER_T
                     { 
                        $$ := Create_Node (Visible_Var, Null_Node, Null_Node);
                        Identifier := Gsl_Identifiers.Get_Identifier
                          (scanner_dfa.yytext);
                        Var_Reference := Create_Gsl_Var_Reference
                          (Var, Identifier);
                        Set_Literal ($$, Gsl_Type (Var_Reference));
                     };
visible_ref        : VISIBLE_REF_T IDENTIFIER_T
                     { 
                        $$ := Create_Node (Visible_Ref, Null_Node, Null_Node);
                        Identifier := Gsl_Identifiers.Get_Identifier
                          (scanner_dfa.yytext);
                        Var_Reference := Create_Gsl_Var_Reference
                          (Var, Identifier);
                        Set_Literal ($$, Gsl_Type (Var_Reference));
                     };
var_creation       : '+' IDENTIFIER_T
                     { 
                        $$ := Create_Node (Var_Creation, Null_Node, Null_Node);
                        Identifier := Gsl_Identifiers.Get_Identifier
                          (scanner_dfa.yytext);
                        Var_Reference := Create_Gsl_Var_Reference
                          (Var, Identifier);
                        Set_Literal ($$, Gsl_Type (Var_Reference));
                     };


%% -- next section will go before package statement in SPEC file
with Giant.Gsl, Giant.Gsl.Syntax_Tree, Giant.Gsl.Types;
use  Giant.Gsl, Giant.Gsl.Syntax_Tree, Giant.Gsl.Types;
with Giant.Gsl_Identifiers;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

## -- next section will go after package statement in SPEC file  

   procedure YYParse;

   function Get_Syntax_Tree return Syntax_Node;

   function Get_Error_Message return Unbounded_String;

## -- next section will go before package statement in BODY file

with Giant.Scanner, Giant.Scanner.dfa, Giant.Scanner.IO, Text_IO;
use  Giant.Scanner, Giant.Scanner.dfa, Giant.Scanner.IO, Text_IO;

## -- next section will go after package statement in BODY file

package scanner_dfa renames scanner.dfa;
package scanner_io renames scanner.io;

   function YYLex return Token is
      Tok : Token := Scanner.YYLex;
   begin
      -- Put (Scanner_Dfa.YYText);
      return Tok;
   end YYLex;

   procedure YYError (Message : in String) is
   begin
      Error_Message := To_Unbounded_String
        (Yy_Line_Number'Img & " :" & Yy_Begin_Column'Img);
   end YYError;

   function Get_Syntax_Tree return Syntax_Node is
   begin
      return Root_Node; 
   end Get_Syntax_Tree;

   function Get_Error_Message return Unbounded_String is
   begin
      return Error_Message;
   end Get_Error_Message;
