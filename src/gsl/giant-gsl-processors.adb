------------------------------------------------------------------------------
-- GIANT - Graphical IML Analysis and Navigation Tool
--
-- Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
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
-- First Author: Gerrit Schulz
--
-- $RCSfile: giant-gsl-processors.adb,v $
-- $Author: keulsn $
-- $Date: 2003-10-05 20:49:10 $
--

with Ada.Exceptions;
with Ada.Strings.Unbounded;

with Giant.Controller;
with Giant.Default_Logger;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Graph_Lib.Subgraphs;
with Giant.Gsl.Compilers;
with Giant.Gsl.Interpreters;
with Giant.Gsl.Syntax_Tree;

package body Giant.Gsl.Processors is

   ---------------------------------------------------------------------------
   --
   procedure Execute
     (Cmd : Syntax_Node) is

      use Giant.Controller;
      use Giant.Gsl.Syntax_Tree;
      Gsl_Compiler    : Gsl.Compilers.Compiler :=
                        Gsl.Interpreters.Get_Compiler;
      Execution_Stack : Execution_Stacks.Stack :=
                        Gsl.Interpreters.Get_Execution_Stack;
      Result_Stack    : Result_Stacks.Stack :=
                        Gsl.Interpreters.Get_Result_Stack;
      AR              : Gsl.Activation_Record;
      Res1            : Gsl_Type;
      Res2            : Gsl_Type;
      Lit             : Gsl_Type;
      Res_List        : Gsl_List;
      Sub             : Graph_Lib.Subgraphs.Subgraph;
      Sel             : Graph_Lib.Selections.Selection;
   begin
      -- execute a Gsl command
      case Get_Node_Type (Cmd) is

         ---------------------------------------------------------------------
         -- literal ::= boolean_literal | int_literal | string_literal |
         --             null_literal
         when Literal =>
            -- Default_Logger.Debug ("=== GSL COMMAND: Literal");
            Lit := Get_Literal (Cmd);
            if Lit /= Gsl_Null then
               Result_Stacks.Push (Result_Stack, Copy (Lit));
            else
               Result_Stacks.Push (Result_Stack, Gsl_Null);
            end if;

         ---------------------------------------------------------------------
         -- inspection ::= visible_var | global_var
         when Visible_Var =>
            -- Default_Logger.Debug ("=== GSL COMMAND: Visible_Var");
            Lit := Get_Literal (Cmd);
            -- get value from activation record
            Lit := Gsl.Interpreters.Get_Var
              (Get_Ref_Name (Gsl_Var_Reference (Lit)));
            if Lit /= Gsl_Null then
               Result_Stacks.Push (Result_Stack, Copy (Lit));
            else
               Result_Stacks.Push (Result_Stack, Gsl_Null);
            end if;

         when Global_Var =>
            -- Default_Logger.Debug ("=== GSL COMMAND: Global_Var");
            Lit := Get_Literal (Cmd);
            if Get_Ref_Type (Gsl_Var_Reference (Lit)) = Subgraph then
               Result_Stacks.Push
                 (Result_Stack,
                  Get_Subgraph (Gsl_Identifiers.Get_Name
                                (Get_Ref_Name (Gsl_Var_Reference (Lit)))));
            elsif Get_Ref_Type (Gsl_Var_Reference (Lit)) = Selection then
               Result_Stacks.Push
                 (Result_Stack,
                  Get_Selection (Gsl_Identifiers.Get_Name
                                 (Get_Ref_Name (Gsl_Var_Reference (Lit))),
                                 Gsl.Interpreters.Get_Current_Context));
            end if;

         ---------------------------------------------------------------------
         -- reference ::= visible_ref | var_creation | global_ref
         when Visible_Ref =>
            -- Default_Logger.Debug ("=== GSL COMMAND: Visible_Ref");
            Lit := Copy (Get_Literal (Cmd));
            -- check wether the referenced variable exists
            Gsl.Interpreters.Exists_Var
              (Get_Ref_Name (Gsl_Var_Reference (Lit)));
            Result_Stacks.Push (Result_Stack, Lit);

         when Var_Creation =>
            -- Default_Logger.Debug ("=== GSL COMMAND: Var_Creation");
            Lit := Copy (Get_Literal (Cmd));
            Gsl.Interpreters.Create_Var
              (Get_Ref_Name (Gsl_Var_Reference (Lit)));
            Result_Stacks.Push (Result_Stack, Lit);

         when Global_Ref =>
            -- Default_Logger.Debug ("=== GSL COMMAND: Global_Ref");
            Lit := Copy (Get_Literal (Cmd));
            if Get_Ref_Type (Gsl_Var_Reference (Lit)) = Subgraph then
               Result_Stacks.Push (Result_Stack,
               Get_Subgraph_Reference (Gsl_Var_Reference (Lit)));
            elsif Get_Ref_Type (Gsl_Var_Reference (Lit)) = Selection then
               Result_Stacks.Push (Result_Stack,
               Get_Selection_Reference
                 (Gsl_Var_Reference (Lit),
                  Gsl.Interpreters.Get_Current_Context));
            end if;

         ---------------------------------------------------------------------
         -- script_decl ::= {list, expression}
         when Script_Decl =>
            -- Default_Logger.Debug ("=== GSL COMMAND: Script_Decl");
            Lit := Copy (Get_Literal (Cmd));
            Set_Activation_Record (Gsl_Script_Reference (Lit),
            Gsl.Interpreters.Get_Current_Activation_Record);
            Result_Stacks.Push (Result_Stack, Lit);

         ---------------------------------------------------------------------
         -- list ::= (<expression<,expression>*>?)
         when List =>
            -- Default_Logger.Debug ("=== GSL COMMAND: List");
            Res_List := Create_Gsl_List (Get_Size (Cmd));
            for i in reverse 1 .. Get_List_Size (Res_List) loop
               Result_Stacks.Pop (Result_Stack, Res1);
               Set_Value_At (Res_List, i, Res1);
            end loop;
            Result_Stacks.Push (Result_Stack, Gsl_Type (Res_List));

         ---------------------------------------------------------------------
         -- sequence ::= [<expression;>*]
         when Sequence =>
            -- Default_Logger.Debug ("=== GSL COMMAND: Sequence");
            if Gsl.Syntax_Tree.Get_Size (Cmd) = 0 then
               Result_Stacks.Push (Result_Stack, Gsl_Null);
            else
               Result_Stacks.Pop (Result_Stack, Res1);
               for i in 1 .. Get_Size (Cmd)-1 loop
                  Result_Stacks.Pop (Result_Stack, Res2);
                   -- free the memory
                  Destroy_Gsl_Type (Res2);
               end loop;
               Result_Stacks.Push (Result_Stack, Res1);
            end if;

         ---------------------------------------------------------------------
         -- script_activation ::= expression list
         when Script_Activation =>
            -- Default_Logger.Debug ("=== GSL COMMAND: Script_Activation");
            Script_Activation_Cmd;

         ---------------------------------------------------------------------
         --
         when Script_Exec =>
            -- Default_Logger.Debug ("=== GSL COMMAND: Script_Exec");
            Script_Exec_Cmd;

         ---------------------------------------------------------------------
         --
         when Script_Finish =>
            -- Default_Logger.Debug ("=== GSL COMMAND: Script_Finish");
            Gsl.Interpreters.Restore_Activation_Record;

         ---------------------------------------------------------------------
         --
         when Script_Loop =>
            -- Default_Logger.Debug ("=== GSL COMMAND: Loop");
            Result_Stacks.Pop (Result_Stack, Res1);
            if Is_Gsl_Boolean (Res1) then
               if Get_Value (Gsl_Boolean (Res1)) then
                  -- loop again
                  Execution_Stacks.Push (Execution_Stack, Copy_Node (Cmd));

                  -- set the new activation record
                  AR := Gsl.Interpreters.Create_Activation_Record
                    (Gsl.Interpreters.Get_Activation_Record_Parent
                      (Gsl.Interpreters.Get_Current_Activation_Record));
                  Gsl.Interpreters.Restore_Activation_Record;
                  Gsl.Interpreters.Set_Activation_Record (AR);

                  -- push the code of the script
                  Execution_Stacks.Push
                    (Execution_Stack, Gsl.Compilers.Get_Execution_Stack
                      (Gsl_Compiler, Get_Child1 (Cmd)));
               else
                  -- loop finished, restore the activation record
                  Gsl.Interpreters.Restore_Activation_Record;
               end if;
               -- free memory
               Destroy_Gsl_Type (Res1);
            else
               Destroy_Gsl_Type (Res1);
               Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
                 "Script 'loop': Gsl_Boolean expected.");
            end if;

         ---------------------------------------------------------------------
         --
         when Result_Pop =>
            -- Default_Logger.Debug ("=== GSL COMMAND: Result_Pop");
            Result_Stacks.Pop (Result_Stack, Res1);
            Destroy_Gsl_Type (Res1);

         ---------------------------------------------------------------------
         --
         when Param_Fetch =>
            -- Default_Logger.Debug ("=== GSL COMMAND: Param_Fetch");
            Res_List := Gsl_List (Gsl.Interpreters.Get_Params);
            Result_Stacks.Push (Result_Stack, Gsl_Type (Res_List));

      end case;
   end Execute;

   --------------------------------------------------------------------------
   -- step 1 of a Gsl Script execution
   procedure Script_Activation_Cmd is

      use Gsl.Compilers;
      use Gsl.Interpreters;
      Gsl_Compiler    : Gsl.Compilers.Compiler;
      Execution_Stack : Execution_Stacks.Stack;
      Result_Stack    : Result_Stacks.Stack;
      Script          : Gsl_Type;
      Params          : Gsl_Type;
   begin
      Gsl_Compiler    := Gsl.Interpreters.Get_Compiler;
      Execution_Stack := Gsl.Interpreters.Get_Execution_Stack;
      Result_Stack    := Gsl.Interpreters.Get_Result_Stack;

      Result_Stacks.Pop (Result_Stack, Params);
      Result_Stacks.Pop (Result_Stack, Script);
      if Is_Gsl_Script_Reference (Script) and Is_Gsl_List (Params) then
         case Get_Script_Type (Gsl_Script_Reference (Script)) is
            when Giant.Gsl.Types.Gsl_Script =>
               -- set the new activation record and push the old one
               -- to the activation record stack
               Set_Activation_Record (Create_Activation_Record
                 (Get_Activation_Record (Gsl_Script_Reference (Script))));

               Execution_Stacks.Push (Execution_Stack,
                 Get_Execution_Stack (Gsl_Compiler,
                   Gsl.Syntax_Tree.Create_Node (Script_Exec,
                     Null_Node, Null_Node)));

               -- push the code for the parameter list to the execution stack
               -- (get the Syntax_Node from the Gsl_Script_Reference)
               Execution_Stacks.Push (Execution_Stack,
                 Get_Execution_Stack (Gsl_Compiler,
                    Get_Parameter_List (Gsl_Script_Reference (Script))));

               Result_Stacks.Push (Result_Stack, Script);
               Result_Stacks.Push (Result_Stack, Params);

            when Giant.Gsl.Types.Gsl_Runtime =>
               -- call runtime function, and push the result
               Result_Stacks.Push (Result_Stack,
                 Get_Gsl_Runtime (Gsl_Script_Reference (Script))
                 (Gsl_List (Params)));
               -- free the memory
               Destroy_Gsl_Type (Params);
               Destroy_Gsl_Type (Script);
         end case;
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Runtime error: Gsl_Script_Reference and Gsl_List expected.");
      end if;
   end Script_Activation_Cmd;


   --------------------------------------------------------------------------
   -- step 2 of a Gsl Script execution
   procedure Script_Exec_Cmd is

      use Giant.Gsl.Compilers;
      Gsl_Compiler    : Gsl.Compilers.Compiler;
      Execution_Stack : Execution_Stacks.Stack;
      Result_Stack    : Result_Stacks.Stack;
      Script          : Gsl_Type;
      Params          : Gsl_Type;
      Formal          : Gsl_Type;
      Ref             : Gsl_Type;
   begin
      Gsl_Compiler    := Gsl.Interpreters.Get_Compiler;
      Execution_Stack := Gsl.Interpreters.Get_Execution_Stack;
      Result_Stack    := Gsl.Interpreters.Get_Result_Stack;

      Result_Stacks.Pop (Result_Stack, Formal);
      Result_Stacks.Pop (Result_Stack, Params);
      Result_Stacks.Pop (Result_Stack, Script);
      if Get_List_Size (Gsl_List (Formal)) /=
         Get_List_size (Gsl_List (Params)) then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Wrong number of parameters.");
      else
         -- process the parameter list
         for i in 1 .. Get_List_Size (Gsl_List (Formal)) loop
            Ref := Get_Value_At (Gsl_List (Formal), i);
            if Is_Gsl_Var_Reference (Ref) then
               Gsl.Interpreters.Set_Var
                 (Get_Ref_Name (Gsl_Var_Reference (Ref)),
                  Copy (Get_Value_At (Gsl_List (Params), i)));
             else
                Ada.Exceptions.Raise_Exception
                  (Gsl_Runtime_Error'Identity, "Gsl_Var_Reference expected.");
             end if;
         end loop;
         -- push code to destroy restore the activation record
         -- when script execution is finished
         Execution_Stacks.Push (Execution_Stack,
           Get_Execution_Stack (Gsl_Compiler,
             Giant.Gsl.Syntax_Tree.Create_Node (Script_Finish,
                                                Null_Node, Null_Node)));

         -- push the code of the script
         Execution_Stacks.Push (Execution_Stack,
           Get_Execution_Stack (Gsl_Compiler,
             Get_Script_Node (Gsl_Script_Reference (Script))));

         -- free the memory (script reference)
         Destroy_Gsl_Type (Formal);
         Destroy_Gsl_Type (Params);
         Destroy_Gsl_Type (Script);
      end if;
   end Script_Exec_Cmd;

   --------------------------------------------------------------------------
   --
   function Get_Subgraph
     (Name : String)
      return Gsl_Type is

      use Graph_Lib.Subgraphs;
      Sub      : Graph_Lib.Subgraphs.Subgraph;
      Res_List : Gsl_List;
   begin
      if Controller.Exists_Subgraph (Name) then
         Sub := Controller.Get_Subgraph (Name);
         Res_List := Create_Gsl_List (2);
         Set_Value_At (Res_List, 1, Gsl_Type
           (Create_Gsl_Node_Set (Graph_Lib.Node_Id_Sets.Copy
             (Get_All_Nodes (Sub)))));
         Set_Value_At (Res_List, 2, Gsl_Type
           (Create_Gsl_Edge_Set (Graph_Lib.Edge_Id_Sets.Copy
             (Get_All_Edges (Sub)))));
         return Gsl_Type (Res_List);
      else
         return Gsl_Null;
      end if;
   end Get_Subgraph;

   --------------------------------------------------------------------------
   --
   function Get_Subgraph_Reference
     (Ref : Gsl_Var_Reference)
      return Gsl_Type is

      Name : String := Gsl_Identifiers.Get_Name (Get_Ref_Name (Ref));
   begin
      if not Controller.Exists_Subgraph (Name) then

         Controller.Create_Subgraph (Name);
      end if;
      return Gsl_Type (Copy (Ref));
   end Get_Subgraph_Reference;

   ---------------------------------------------------------------------------
   --
   function Get_Selection
     (Name    : String;
      Context : String)
      return Gsl_Type is

      use Graph_Lib.Selections;
      Sel      : Graph_Lib.Selections.Selection;
      Res_List : Gsl_List;
   begin
      if Context = "" then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Runtime error: No context set.");
      elsif Controller.Exists_Selection (Context, Name)
      then
         Sel := Controller.Get_Selection (Context, Name);
         Res_List := Create_Gsl_List (2);
         Set_Value_At (Res_List, 1, Gsl_Type
           (Create_Gsl_Node_Set (Graph_Lib.Node_Id_Sets.Copy
             (Get_All_Nodes (Sel)))));
         Set_Value_At (Res_List, 2, Gsl_Type
           (Create_Gsl_Edge_Set (Graph_Lib.Edge_Id_Sets.Copy
             (Get_All_Edges (Sel)))));
         return Gsl_Type (Res_List);
      else
         return Gsl_Null;
      end if;
   end Get_Selection;

   --------------------------------------------------------------------------
   --
   function Get_Selection_Reference
     (Ref     : Gsl_Var_Reference;
      Context : String)
      return Gsl_Type is

      Name : String := Gsl_Identifiers.Get_Name (Get_Ref_Name (Ref));
   begin
      if Context = "" then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Runtime error: No context set.");
      elsif not Controller.Exists_Selection (Context, Name) then
         Controller.Create_Selection (Context, Name);
      end if;
      return Gsl_Type (Create_Gsl_Var_Reference
        (Selection, Get_Ref_Name (Ref), Context));
   end Get_Selection_Reference;

end Giant.Gsl.Processors;
