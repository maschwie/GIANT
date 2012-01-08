with Giant.Scanner.DFA; use Giant.Scanner.DFA; 
with Giant.Scanner.IO; use Giant.Scanner.IO; 
package body Giant.Scanner is
package Scanner_DFA renames Scanner.DFA;
package Scanner_IO renames Scanner.IO;
function YYLex return Token is
subtype short is Integer range -32768..32767;
    yy_act : Integer;
    yy_c   : short;

-- returned upon end-of-file
YY_END_TOK : constant Integer := 0;
YY_END_OF_BUFFER : constant := 23;
subtype yy_state_type is integer;
yy_current_state : yy_state_type;
INITIAL : constant := 0;
yy_accept : constant array(0..60) of short :=
    (   0,
       21,   21,   23,   22,   21,   22,   11,    2,    3,   12,
        8,   22,    9,   22,   18,   10,   20,    6,    7,   20,
       20,   20,   20,    4,    5,   21,    0,   19,    0,   18,
        0,   20,   20,   20,   20,   20,   20,    0,    1,   20,
       20,   20,   20,   20,   20,   13,   20,   20,   14,   15,
       20,   20,   20,   20,   20,   20,   20,   16,   17,    0
    ) ;

yy_ec : constant array(CHARACTER'FIRST..CHARACTER'LAST) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    2,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    1,    4,    1,    1,    1,    1,    5,    6,
        7,    1,    8,    9,   10,   11,   12,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,    1,   14,    1,
        1,    1,    1,    1,   15,   15,   15,   15,   15,   15,
       15,   15,   15,   15,   15,   15,   15,   15,   15,   15,
       15,   15,   15,   15,   15,   15,   15,   15,   15,   15,
       16,   17,   18,   19,   20,    1,   21,   22,   23,   15,

       24,   25,   26,   27,   28,   15,   15,   29,   15,   30,
       31,   32,   15,   33,   34,   35,   36,   15,   15,   15,
       15,   15,   37,    1,   38,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,

        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1
    ) ;

yy_meta : constant array(0..38) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    2,    1,    2,    1,    1,    1,    3,    2,
        2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
        2,    2,    2,    2,    2,    2,    1,    1
    ) ;

yy_base : constant array(0..63) of short :=
    (   0,
        0,    0,   86,   87,   37,   37,   87,   87,   87,   72,
       87,   71,   87,   71,   69,   87,    0,   87,   87,   60,
       44,   19,   46,   87,   87,   42,   42,   87,   59,   64,
       73,    0,   46,   45,   44,   50,   35,   67,   87,   35,
       39,   43,   40,   41,   34,    0,   34,   23,    0,    0,
       18,   31,   23,   18,   18,   21,   17,    0,    0,   87,
       59,   40,   61
    ) ;

yy_def : constant array(0..63) of short :=
    (   0,
       60,    1,   60,   60,   60,   61,   60,   60,   60,   60,
       60,   60,   60,   60,   60,   60,   62,   60,   60,   62,
       62,   62,   62,   60,   60,   60,   61,   60,   61,   60,
       63,   62,   62,   62,   62,   62,   62,   63,   60,   62,
       62,   62,   62,   62,   62,   62,   62,   62,   62,   62,
       62,   62,   62,   62,   62,   62,   62,   62,   62,    0,
       60,   60,   60
    ) ;

yy_nxt : constant array(0..125) of short :=
    (   0,
        4,    5,    5,    6,    7,    8,    9,   10,   11,   12,
       13,   14,   15,   16,   17,   18,    4,   19,    4,    4,
       17,   17,   17,   17,   20,   17,   17,   17,   17,   21,
       17,   17,   17,   22,   23,   17,   24,   25,   26,   26,
       28,   32,   35,   26,   26,   28,   59,   58,   57,   56,
       55,   54,   53,   29,   36,   52,   51,   50,   29,   27,
       27,   38,   38,   38,   49,   48,   47,   46,   45,   39,
       44,   43,   42,   41,   40,   39,   30,   27,   37,   34,
       33,   30,   31,   30,   30,   60,    3,   60,   60,   60,
       60,   60,   60,   60,   60,   60,   60,   60,   60,   60,

       60,   60,   60,   60,   60,   60,   60,   60,   60,   60,
       60,   60,   60,   60,   60,   60,   60,   60,   60,   60,
       60,   60,   60,   60,   60
    ) ;

yy_chk : constant array(0..125) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    5,    5,
        6,   62,   22,   26,   26,   27,   57,   56,   55,   54,
       53,   52,   51,    6,   22,   48,   47,   45,   27,   61,
       61,   63,   63,   63,   44,   43,   42,   41,   40,   38,
       37,   36,   35,   34,   33,   31,   30,   29,   23,   21,
       20,   15,   14,   12,   10,    3,   60,   60,   60,   60,
       60,   60,   60,   60,   60,   60,   60,   60,   60,   60,

       60,   60,   60,   60,   60,   60,   60,   60,   60,   60,
       60,   60,   60,   60,   60,   60,   60,   60,   60,   60,
       60,   60,   60,   60,   60
    ) ;


-- copy whatever the last rule matched to the standard output

procedure ECHO is
begin
   if (Text_IO.is_open(user_output_file)) then
     Text_IO.Put( user_output_file, yytext );
   else
     Text_IO.Put( yytext );
   end if;
end ECHO;
pragma Inline (ECHO);

-- enter a start condition.
-- Using procedure requires a () after the ENTER, but makes everything
-- much neater.

procedure ENTER( state : Integer ) is
begin
     yy_start := 1 + 2 * state;
end ENTER;
pragma Inline (ENTER);

-- action number for EOF rule of a given start state
function YY_STATE_EOF(state : Integer) return Integer is
begin
     return YY_END_OF_BUFFER + state + 1;
end YY_STATE_EOF;
pragma Inline (YY_STATE_EOF);

-- return all but the first 'n' matched characters back to the input stream
procedure yyless(n : Integer) is
begin
        yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
        yy_cp := yy_bp + n;
        yy_c_buf_p := yy_cp;
        YY_DO_BEFORE_ACTION; -- set up yytext again
end yyless;
pragma Inline (yyless);

-- redefine this if you have something you want each time.
procedure YY_USER_ACTION is
begin
        null;
end;
pragma Inline (YY_USER_ACTION);

-- yy_get_previous_state - get the state just before the EOB char was reached

function yy_get_previous_state return yy_state_type is
    yy_current_state : yy_state_type;
    yy_c : short;
begin
    yy_current_state := yy_start;

    for yy_cp in yytext_ptr..yy_c_buf_p - 1 loop
	yy_c := yy_ec(yy_ch_buf(yy_cp));
	if (yy_accept(yy_current_state) /= 0 ) then
	    yy_last_accepting_state := yy_current_state;
	    yy_last_accepting_cpos := yy_cp;
	end if;
	while ( yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state ) loop
	    yy_current_state := yy_def(yy_current_state);
	    if ( yy_current_state >= 61 ) then
		yy_c := yy_meta(yy_c);
	    end if;
	end loop;
	yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
    end loop;

    return yy_current_state;
end yy_get_previous_state;

procedure yyrestart( input_file : file_type ) is
begin
   open_input(Text_IO.name(input_file));
end yyrestart;
pragma Inline (yyrestart);

begin -- of YYLex
<<new_file>>
        -- this is where we enter upon encountering an end-of-file and
        -- yywrap() indicating that we should continue processing

    if ( yy_init ) then
        if ( yy_start = 0 ) then
            yy_start := 1;      -- first start state
        end if;

        -- we put in the '\n' and start reading from [1] so that an
        -- initial match-at-newline will be true.

        yy_ch_buf(0) := ASCII.LF;
        yy_n_chars := 1;

        -- we always need two end-of-buffer characters.  The first causes
        -- a transition to the end-of-buffer state.  The second causes
        -- a jam in that state.

        yy_ch_buf(yy_n_chars) := YY_END_OF_BUFFER_CHAR;
        yy_ch_buf(yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;

        yy_eof_has_been_seen := false;

        yytext_ptr := 1;
        yy_c_buf_p := yytext_ptr;
        yy_hold_char := yy_ch_buf(yy_c_buf_p);
        yy_init := false;
-- UMASS CODES :
--   Initialization
        tok_begin_line := 1;
        tok_end_line := 1;
        tok_begin_col := 0;
        tok_end_col := 0;
        token_at_end_of_line := false;
        line_number_of_saved_tok_line1 := 0;
        line_number_of_saved_tok_line2 := 0;
-- END OF UMASS CODES.
    end if; -- yy_init

    loop                -- loops until end-of-file is reached

-- UMASS CODES :
--    if last matched token is end_of_line, we must
--    update the token_end_line and reset tok_end_col.
    if Token_At_End_Of_Line then
      Tok_End_Line := Tok_End_Line + 1;
      Tok_End_Col := 0;
      Token_At_End_Of_Line := False;
    end if;
-- END OF UMASS CODES.

        yy_cp := yy_c_buf_p;

        -- support of yytext
        yy_ch_buf(yy_cp) := yy_hold_char;

        -- yy_bp points to the position in yy_ch_buf of the start of the
        -- current run.
	yy_bp := yy_cp;
	yy_current_state := yy_start;
	loop
		yy_c := yy_ec(yy_ch_buf(yy_cp));
		if (yy_accept(yy_current_state) /= 0 ) then
		    yy_last_accepting_state := yy_current_state;
		    yy_last_accepting_cpos := yy_cp;
		end if;
		while ( yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state ) loop
		    yy_current_state := yy_def(yy_current_state);
		    if ( yy_current_state >= 61 ) then
			yy_c := yy_meta(yy_c);
		    end if;
		end loop;
		yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
	    yy_cp := yy_cp + 1;
if ( yy_current_state = 60 ) then
    exit;
end if;
	end loop;
	yy_cp := yy_last_accepting_cpos;
	yy_current_state := yy_last_accepting_state;

<<next_action>>
	    yy_act := yy_accept(yy_current_state);
            YY_DO_BEFORE_ACTION;
            YY_USER_ACTION;

        if aflex_debug then  -- output acceptance info. for (-d) debug mode
            text_io.put( Standard_Error, "--accepting rule #" );
            text_io.put( Standard_Error, INTEGER'IMAGE(yy_act) );
            text_io.put_line( Standard_Error, "(""" & yytext & """)");
        end if;

-- UMASS CODES :
--   Update tok_begin_line, tok_end_line, tok_begin_col and tok_end_col
--   after matching the token.
        if yy_act /= YY_END_OF_BUFFER and then yy_act /= 0 then
-- Token are matched only when yy_act is not yy_end_of_buffer or 0.
          Tok_Begin_Line := Tok_End_Line;
          Tok_Begin_Col := Tok_End_Col + 1;
          Tok_End_Col := Tok_Begin_Col + yy_cp - yy_bp - 1;
          if yy_ch_buf ( yy_bp ) = ASCII.LF then
            Token_At_End_Of_Line := True;
          end if;
        end if;
-- END OF UMASS CODES.

<<do_action>>   -- this label is used only to access EOF actions
            case yy_act is
		when 0 => -- must backtrack
		-- undo the effects of YY_DO_BEFORE_ACTION
		yy_ch_buf(yy_cp) := yy_hold_char;
		yy_cp := yy_last_accepting_cpos;
		yy_current_state := yy_last_accepting_state;
		goto next_action;




when 1 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
yy_cp := yy_cp - 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 23 "Giant.Scanner.aflex"
null;   -- Comments are done as in C++:  // Text

when 2 => 
--# line 25 "Giant.Scanner.aflex"
return '(';

when 3 => 
--# line 26 "Giant.Scanner.aflex"
return ')';

when 4 => 
--# line 27 "Giant.Scanner.aflex"
return '{';

when 5 => 
--# line 28 "Giant.Scanner.aflex"
return '}';

when 6 => 
--# line 29 "Giant.Scanner.aflex"
return '[';

when 7 => 
--# line 30 "Giant.Scanner.aflex"
return ']';

when 8 => 
--# line 31 "Giant.Scanner.aflex"
return ',';

when 9 => 
--# line 32 "Giant.Scanner.aflex"
return '.';

when 10 => 
--# line 33 "Giant.Scanner.aflex"
return ';';

when 11 => 
--# line 34 "Giant.Scanner.aflex"
return VISIBLE_REF_T;

when 12 => 
--# line 35 "Giant.Scanner.aflex"
return '+';

when 13 => 
--# line 37 "Giant.Scanner.aflex"
return NULL_T;

when 14 => 
--# line 38 "Giant.Scanner.aflex"
return TRUE_T;

when 15 => 
--# line 39 "Giant.Scanner.aflex"
return FALSE_T;

when 16 => 
--# line 40 "Giant.Scanner.aflex"
return SUBGRAPH_T;

when 17 => 
--# line 41 "Giant.Scanner.aflex"
return SELECTION_T;

when 18 => 
--# line 43 "Giant.Scanner.aflex"
return INT_LITERAL_T;

when 19 => 
--# line 44 "Giant.Scanner.aflex"
return STRING_LITERAL_T;

when 20 => 
--# line 45 "Giant.Scanner.aflex"
return IDENTIFIER_T;

when 21 => 
--# line 47 "Giant.Scanner.aflex"
null;       -- ignore white space

when 22 => 
--# line 50 "Giant.Scanner.aflex"
ECHO;
when YY_END_OF_BUFFER + INITIAL + 1 => 
    return End_Of_Input;
                when YY_END_OF_BUFFER =>
                    -- undo the effects of YY_DO_BEFORE_ACTION
                    yy_ch_buf(yy_cp) := yy_hold_char;

                    yytext_ptr := yy_bp;

                    case yy_get_next_buffer is
                        when EOB_ACT_END_OF_FILE =>
                            begin
                            if ( yywrap ) then
                                -- note: because we've taken care in
                                -- yy_get_next_buffer() to have set up yytext,
                                -- we can now set up yy_c_buf_p so that if some
                                -- total hoser (like aflex itself) wants
                                -- to call the scanner after we return the
                                -- End_Of_Input, it'll still work - another
                                -- End_Of_Input will get returned.

                                yy_c_buf_p := yytext_ptr;

                                yy_act := YY_STATE_EOF((yy_start - 1) / 2);

                                goto do_action;
                            else
                                --  start processing a new file
                                yy_init := true;
                                goto new_file;
                            end if;
                            end;
                        when EOB_ACT_RESTART_SCAN =>
                            yy_c_buf_p := yytext_ptr;
                            yy_hold_char := yy_ch_buf(yy_c_buf_p);
                        when EOB_ACT_LAST_MATCH =>
                            yy_c_buf_p := yy_n_chars;
                            yy_current_state := yy_get_previous_state;

                            yy_cp := yy_c_buf_p;
                            yy_bp := yytext_ptr;
                            goto next_action;
                        when others => null;
                        end case; -- case yy_get_next_buffer()
                when others =>
                    text_io.put( "action # " );
                    text_io.put( INTEGER'IMAGE(yy_act) );
                    text_io.new_line;
                    raise AFLEX_INTERNAL_ERROR;
            end case; -- case (yy_act)
        end loop; -- end of loop waiting for end of file
end YYLex;
--# line 50 "Giant.Scanner.aflex"
end Giant.Scanner;
