
with Giant.Scanner, Giant.Scanner.dfa, Giant.Scanner.IO, Text_IO;
use  Giant.Scanner, Giant.Scanner.dfa, Giant.Scanner.IO, Text_IO;

with Giant.Parser.Goto_Table;
use  Giant.Parser.Goto_Table;
with Giant.Parser.Tokens;
use  Giant.Parser.Tokens;
with Giant.Parser.Shift_Reduce;
use  Giant.Parser.Shift_Reduce;

package body Giant.Parser is 
   package Parser_Goto renames Parser.Goto_Table;
   package Parser_Tokens renames Parser.Tokens;
   package Parser_Shift_Reduce  renames Parser.Shift_Reduce;

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
procedure YYParse is

   -- Rename User Defined Packages to Internal Names.
    package yy_goto_tables         renames
      Giant.Parser.Goto_Table;
    package yy_shift_reduce_tables renames
      Giant.Parser.Shift_Reduce;
    package yy_tokens              renames
      Giant.Parser.Tokens;

   use yy_tokens, yy_goto_tables, yy_shift_reduce_tables;

   procedure yyerrok;
   procedure yyclearin;


   package yy is

       -- the size of the value and state stacks
       stack_size : constant Natural := 300;

       -- subtype rule         is natural;
       subtype parse_state  is natural;
       -- subtype nonterminal  is integer;

       -- encryption constants
       default           : constant := -1;
       first_shift_entry : constant :=  0;
       accept_code       : constant := -3001;
       error_code        : constant := -3000;

       -- stack data used by the parser
       tos                : natural := 0;
       value_stack        : array(0..stack_size) of yy_tokens.yystype;
       state_stack        : array(0..stack_size) of parse_state;

       -- current input symbol and action the parser is on
       action             : integer;
       rule_id            : rule;
       input_symbol       : yy_tokens.token;


       -- error recovery flag
       error_flag : natural := 0;
          -- indicates  3 - (number of valid shifts after an error occurs)

       look_ahead : boolean := true;
       index      : integer;

       -- Is Debugging option on or off
        DEBUG : constant boolean := FALSE;

    end yy;


    function goto_state
      (state : yy.parse_state;
       sym   : nonterminal) return yy.parse_state;

    function parse_action
      (state : yy.parse_state;
       t     : yy_tokens.token) return integer;

    pragma inline(goto_state, parse_action);


    function goto_state(state : yy.parse_state;
                        sym   : nonterminal) return yy.parse_state is
        index : integer;
    begin
        index := goto_offset(state);
        while  integer(goto_matrix(index).nonterm) /= sym loop
            index := index + 1;
        end loop;
        return integer(goto_matrix(index).newstate);
    end goto_state;


    function parse_action(state : yy.parse_state;
                          t     : yy_tokens.token) return integer is
        index      : integer;
        tok_pos    : integer;
        default    : constant integer := -1;
    begin
        tok_pos := yy_tokens.token'pos(t);
        index   := shift_reduce_offset(state);
        while integer(shift_reduce_matrix(index).t) /= tok_pos and then
              integer(shift_reduce_matrix(index).t) /= default
        loop
            index := index + 1;
        end loop;
        return integer(shift_reduce_matrix(index).act);
    end parse_action;

-- error recovery stuff

    procedure handle_error is
      temp_action : integer;
    begin

      if yy.error_flag = 3 then -- no shift yet, clobber input.
      if yy.debug then
          text_io.put_line("Ayacc.YYParse: Error Recovery Clobbers " &
                   yy_tokens.token'image(yy.input_symbol));
      end if;
        if yy.input_symbol = yy_tokens.end_of_input then  -- don't discard,
        if yy.debug then
            text_io.put_line("Ayacc.YYParse: Can't discard END_OF_INPUT, quiting...");
        end if;
        raise yy_tokens.syntax_error;
        end if;

            yy.look_ahead := true;   -- get next token
        return;                  -- and try again...
    end if;

    if yy.error_flag = 0 then -- brand new error
        yyerror("Syntax Error");
    end if;

    yy.error_flag := 3;

    -- find state on stack where error is a valid shift --

    if yy.debug then
        text_io.put_line("Ayacc.YYParse: Looking for state with error as valid shift");
    end if;

    loop
        if yy.debug then
          text_io.put_line("Ayacc.YYParse: Examining State " &
               yy.parse_state'image(yy.state_stack(yy.tos)));
        end if;
        temp_action := parse_action(yy.state_stack(yy.tos), error);

            if temp_action >= yy.first_shift_entry then
                if yy.tos = yy.stack_size then
                    text_io.put_line(" Stack size exceeded on state_stack");
                    raise yy_Tokens.syntax_error;
                end if;
                yy.tos := yy.tos + 1;
                yy.state_stack(yy.tos) := temp_action;
                exit;
            end if;

        Decrement_Stack_Pointer :
        begin
          yy.tos := yy.tos - 1;
        exception
          when Constraint_Error =>
            yy.tos := 0;
        end Decrement_Stack_Pointer;

        if yy.tos = 0 then
          if yy.debug then
            text_io.put_line("Ayacc.YYParse: Error recovery popped entire stack, aborting...");
          end if;
          raise yy_tokens.syntax_error;
        end if;
    end loop;

    if yy.debug then
        text_io.put_line("Ayacc.YYParse: Shifted error token in state " &
              yy.parse_state'image(yy.state_stack(yy.tos)));
    end if;

    end handle_error;

   -- print debugging information for a shift operation
   procedure shift_debug(state_id: yy.parse_state; lexeme: yy_tokens.token) is
   begin
       text_io.put_line("Ayacc.YYParse: Shift "& yy.parse_state'image(state_id)&" on input symbol "&
               yy_tokens.token'image(lexeme) );
   end;

   -- print debugging information for a reduce operation
   procedure reduce_debug(rule_id: rule; state_id: yy.parse_state) is
   begin
       text_io.put_line("Ayacc.YYParse: Reduce by rule "&rule'image(rule_id)&" goto state "&
               yy.parse_state'image(state_id));
   end;

   -- make the parser believe that 3 valid shifts have occured.
   -- used for error recovery.
   procedure yyerrok is
   begin
       yy.error_flag := 0;
   end yyerrok;

   -- called to clear input symbol that caused an error.
   procedure yyclearin is
   begin
       -- yy.input_symbol := yylex;
       yy.look_ahead := true;
   end yyclearin;


begin
    -- initialize by pushing state 0 and getting the first input symbol
    yy.state_stack(yy.tos) := 0;


    loop

        yy.index := shift_reduce_offset(yy.state_stack(yy.tos));
        if integer(shift_reduce_matrix(yy.index).t) = yy.default then
            yy.action := integer(shift_reduce_matrix(yy.index).act);
        else
            if yy.look_ahead then
                yy.look_ahead   := false;

                yy.input_symbol := yylex;
            end if;
            yy.action :=
             parse_action(yy.state_stack(yy.tos), yy.input_symbol);
        end if;


        if yy.action >= yy.first_shift_entry then  -- SHIFT

            if yy.debug then
                shift_debug(yy.action, yy.input_symbol);
            end if;

            -- Enter new state
            if yy.tos = yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.tos := yy.tos + 1;
            yy.state_stack(yy.tos) := yy.action;
              yy.value_stack(yy.tos) := yylval;

        if yy.error_flag > 0 then  -- indicate a valid shift
            yy.error_flag := yy.error_flag - 1;
        end if;

            -- Advance lookahead
            yy.look_ahead := true;

        elsif yy.action = yy.error_code then       -- ERROR

            handle_error;

        elsif yy.action = yy.accept_code then
            if yy.debug then
                text_io.put_line("Ayacc.YYParse: Accepting Grammar...");
            end if;
            exit;

        else -- Reduce Action

            -- Convert action into a rule
            yy.rule_id  := -1 * yy.action;

            -- Execute User Action
            -- user_action(yy.rule_id);


                case yy.rule_id is

when  1 =>
--#line  56
 Root_Node := 
yy.value_stack(yy.tos); 

when  2 =>
--#line  59
 
yyval := 
yy.value_stack(yy.tos); 

when  3 =>
--#line  61
 
yyval := 
yy.value_stack(yy.tos); 

when  4 =>
--#line  63
 
yyval := 
yy.value_stack(yy.tos); 

when  5 =>
--#line  65
 
yyval := 
yy.value_stack(yy.tos); 

when  6 =>
--#line  67
 
yyval := 
yy.value_stack(yy.tos); 

when  7 =>
--#line  69
 
yyval := 
yy.value_stack(yy.tos); 

when  8 =>
--#line  71
 
yyval := 
yy.value_stack(yy.tos); 

when  9 =>
--#line  73
 
yyval := 
yy.value_stack(yy.tos); 

when  10 =>
--#line  75
 
                        
yyval := Create_Node (Script_Decl, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos-1));
                        Script_Ref := Create_Gsl_Script_Reference (
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos-1));
                        Set_Literal (
yyval, Gsl_Type (Script_Ref));
                     

when  11 =>
--#line  81
 
yyval := Create_Node (Script_Activation, 
yy.value_stack(yy.tos-1), 
yy.value_stack(yy.tos)); 

when  12 =>
--#line  84
 
yyval := Create_Node (List, 
yy.value_stack(yy.tos), Null_Node); 

when  13 =>
--#line  86
 
yyval := Create_Node (List, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos)); 

when  14 =>
--#line  89
 
yyval := Create_Node (List, Null_Node, Null_Node); 

when  15 =>
--#line  91
 
yyval := 
yy.value_stack(yy.tos-1); 

when  16 =>
--#line  94
 
yyval := Create_Node (Sequence, 
yy.value_stack(yy.tos-1), Null_Node); 

when  17 =>
--#line  96
 
yyval := Create_Node (Sequence, 
yy.value_stack(yy.tos-2), 
yy.value_stack(yy.tos)); 

when  18 =>
--#line  99
 
yyval := Create_Node (Sequence, Null_Node, Null_Node); 

when  19 =>
--#line  101
 
yyval := 
yy.value_stack(yy.tos-1); 

when  20 =>
--#line  105
 
                        
yyval := Create_Node (Literal, Null_Node, Null_Node);
                        Literal_Boolean := Create_Gsl_Boolean (false);
                        Set_Literal (
yyval, Gsl_Type (Literal_Boolean));
                     

when  21 =>
--#line  111
 
                        
yyval := Create_Node (Literal, Null_Node, Null_Node);
                        Literal_Boolean := Create_Gsl_Boolean (true);
                        Set_Literal (
yyval, Gsl_Type (Literal_Boolean));
                     

when  22 =>
--#line  117
 
                        
yyval := Create_Node (Literal, Null_Node, Null_Node);
                        Literal_Natural := Create_Gsl_Natural;
                        Set_Value (Literal_Natural,
                                   Natural'Value(scanner_dfa.yytext));
                        Set_Literal (
yyval, Gsl_Type (Literal_Natural));
                     

when  23 =>
--#line  125
 
                        
yyval := Create_Node (Literal, Null_Node, Null_Node);
                        Literal_String := Create_Gsl_String
                          (scanner_dfa.yytext (2..scanner_dfa.yylength-1));
                        Set_Literal (
yyval, Gsl_Type (Literal_String));
                     

when  24 =>
--#line  132
 
                        
yyval := Create_Node (Literal, Null_Node, Null_Node);
                        Set_Literal (
yyval, Gsl_Null);
                     

when  25 =>
--#line  139

                        
yyval := Create_Node (Global_Var, Null_Node, Null_Node);
                        Identifier := Gsl_Identifiers.Get_Identifier
                          (scanner_dfa.yytext);
                        Var_Reference := Create_Gsl_Var_Reference
                          (Subgraph, Identifier);
                        Set_Literal (
yyval, Gsl_Type (Var_Reference));
                     

when  26 =>
--#line  148
 
                        
yyval := Create_Node (Global_Var, Null_Node, Null_Node);
                        Identifier := Gsl_Identifiers.Get_Identifier
                          (scanner_dfa.yytext);
                        Var_Reference := Create_Gsl_Var_Reference
                          (Selection, Identifier);
                        Set_Literal (
yyval, Gsl_Type (Var_Reference));
                     

when  27 =>
--#line  157
 
                        
yyval := Create_Node (Global_Ref, Null_Node, Null_Node);
                        Identifier := Gsl_Identifiers.Get_Identifier
                          (scanner_dfa.yytext);
                        Var_Reference := Create_Gsl_Var_Reference
                          (Subgraph, Identifier);
                        Set_Literal (
yyval, Gsl_Type (Var_Reference));
                     

when  28 =>
--#line  166
 
                        
yyval := Create_Node (Global_Ref, Null_Node, Null_Node);
                        Identifier := Gsl_Identifiers.Get_Identifier
                          (scanner_dfa.yytext);
                        Var_Reference := Create_Gsl_Var_Reference
                          (Selection, Identifier);
                        Set_Literal (
yyval, Gsl_Type (Var_Reference));
                     

when  29 =>
--#line  176
 
                        
yyval := Create_Node (Visible_Var, Null_Node, Null_Node);
                        Identifier := Gsl_Identifiers.Get_Identifier
                          (scanner_dfa.yytext);
                        Var_Reference := Create_Gsl_Var_Reference
                          (Var, Identifier);
                        Set_Literal (
yyval, Gsl_Type (Var_Reference));
                     

when  30 =>
--#line  185
 
                        
yyval := Create_Node (Visible_Ref, Null_Node, Null_Node);
                        Identifier := Gsl_Identifiers.Get_Identifier
                          (scanner_dfa.yytext);
                        Var_Reference := Create_Gsl_Var_Reference
                          (Var, Identifier);
                        Set_Literal (
yyval, Gsl_Type (Var_Reference));
                     

when  31 =>
--#line  194
 
                        
yyval := Create_Node (Var_Creation, Null_Node, Null_Node);
                        Identifier := Gsl_Identifiers.Get_Identifier
                          (scanner_dfa.yytext);
                        Var_Reference := Create_Gsl_Var_Reference
                          (Var, Identifier);
                        Set_Literal (
yyval, Gsl_Type (Var_Reference));
                     

                    when others => null;
                end case;


            -- Pop RHS states and goto next state
            yy.tos      := yy.tos - rule_length(yy.rule_id) + 1;
            if yy.tos > yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.state_stack(yy.tos) := goto_state(yy.state_stack(yy.tos-1) ,
                                 get_lhs_rule(yy.rule_id));

              yy.value_stack(yy.tos) := yyval;

            if yy.debug then
                reduce_debug(yy.rule_id,
                    goto_state(yy.state_stack(yy.tos - 1),
                               get_lhs_rule(yy.rule_id)));
            end if;

        end if;


    end loop;


end yyparse;
end Giant.Parser;
