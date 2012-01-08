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
-- $RCSfile: giant-gsl-runtime.adb,v $
-- $Author: koppor $
-- $Date: 2003-11-06 14:17:05 $
--
-- This package implements the datatypes used in GSL.
--

with Ada.Exceptions;

with GNAT.Regpat;

with Giant.Controller;

with Giant.Config;
with Giant.Config.Class_Sets;
with Giant.Graph_Lib;
use type Giant.Graph_Lib.Edge_Id;
use type Giant.Graph_Lib.Node_Id;
with Giant.Graph_Lib.Subgraphs;
with Giant.Graph_Lib.Selections;
with Giant.Evolutions;

with Giant.GSL_Support;

------------------------------------------------------------------------------
-- Gsl Includes
with Giant.Gsl.Compilers;
with Giant.Gsl.Interpreters;
with Giant.Gsl.Processors;
with Giant.Gsl.Syntax_Tree;

package body Giant.Gsl.Runtime is

   --------------------------------------------------------------------------
   --
   function Runtime_Set
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Graph_Lib.Subgraphs;
      use Graph_Lib.Selections;
      use Gsl.Interpreters;
      Var   : Gsl_Type;
      Value : Gsl_Type;
      Sub   : Graph_Lib.Subgraphs.Subgraph;
      Sel   : Graph_Lib.Selections.Selection;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'set': Expecting 2 parameters.");
      end if;
      Var := Get_Value_At (Parameter, 1);
      Value := Copy_Gsl_Type (Get_Value_At (Parameter, 2));
      if not (Is_Gsl_Var_Reference (Var) or Is_Gsl_Global_Reference (Var))
      then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'set': Gsl_Var_Reference expected.");

      elsif Get_Ref_Type (Gsl_Var_Reference (Var)) = Gsl.Types.Var then
         if Is_Gsl_Var_Reference (Value) then
            if Get_Activation_Record_Level
              (Get_Ref_Name (Gsl_Var_Reference (Var))) >
               Get_Activation_Record_Level
              (Get_Ref_Name (Gsl_Var_Reference (Value)))
            then
               Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
                 "Script 'set': Operation not allowed. " &
                 "Gsl_Var_Reference migth lead to data corruption.");
            end if;
         end if;
         if Is_Gsl_Script_Reference (Value) then
            if Get_Script_Type (Gsl_Script_Reference (Value)) = Gsl_Script
            then
               if Get_Activation_Record_Level
                 (Get_Ref_Name (Gsl_Var_Reference (Var))) >
                  Get_Activation_Record_Level
                 (Get_Activation_Record (Gsl_Script_Reference (Value)))
               then
                  Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
                    "Script 'set': Operation not allowed. " &
                    "Gsl_Script_Reference migth lead to data corruption.");
               end if;
            end if;
         end if;
         Set_Var (Get_Ref_Name (Gsl_Var_Reference (Var)), Value);

      elsif Get_Ref_Type (Gsl_Var_Reference (Var)) = Gsl.Types.Subgraph then
         if Is_Gsl_Object_Set (Value) then
            Sub := Create (Gsl_Identifiers.Get_Name
                           (Get_Ref_Name (Gsl_Var_Reference (Var))));
            Add_Node_Set (Sub, Get_Value (Gsl_Node_Set
              (Get_Value_At (Gsl_List (Value), 1))));
            Add_Edge_Set (Sub, Get_Value (Gsl_Edge_Set
              (Get_Value_At (Gsl_List (Value), 2))));
            Controller.Add_Subgraph (Sub);
         else
           Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
             "Script 'set': Gsl_Object_Set expected.");
         end if;

      elsif Get_Ref_Type (Gsl_Var_Reference (Var)) = Gsl.Types.Selection then
         if Is_Gsl_Object_Set (Value) then
            Sel := Create (Gsl_Identifiers.Get_Name
                           (Get_Ref_Name (Gsl_Var_Reference (Var))));
            Add_Node_Set (Sel, Get_Value (Gsl_Node_Set
              (Get_Value_At (Gsl_List (Value), 1))));
            Add_Edge_Set (Sel, Get_Value (Gsl_Edge_Set
              (Get_Value_At (Gsl_List (Value), 2))));
            Controller.Add_Selection
              (Get_Ref_Context (Gsl_Var_Reference (Var)), Sel, false);
         else
           Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
             "Script 'set': Gsl_Object_Set expected.");
         end if;
      end if;
      return Gsl_Null;
   end Runtime_Set;

   --------------------------------------------------------------------------
   --
   function Runtime_Deref
     (Parameter : Gsl_List)
      return Gsl_Type is

      Ref   : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'deref': Expecting 1 parameter.");
      end if;
      Ref := Get_Value_At (Parameter, 1);
      if not (Is_Gsl_Var_Reference (Ref) or Is_Gsl_Global_Reference (Ref))
      then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'deref': Gsl_Var_Reference expected.");

      elsif Get_Ref_Type (Gsl_Var_Reference (Ref)) = Gsl.Types.Var then
         return Gsl.Interpreters.Get_Var
           (Get_Ref_Name (Gsl_Var_Reference (Ref)));

      elsif Get_Ref_Type (Gsl_Var_Reference (Ref)) = Gsl.Types.Subgraph then
         return Gsl.Processors.Get_Subgraph
           (Gsl_Identifiers.Get_Name (Get_Ref_Name (Gsl_Var_Reference (Ref))));

      elsif Get_Ref_Type (Gsl_Var_Reference (Ref)) = Gsl.Types.Selection then
         return Gsl.Processors.Get_Selection
           (Gsl_Identifiers.Get_Name (Get_Ref_Name (Gsl_Var_Reference (Ref))),
            Get_Ref_Context (Gsl_Var_Reference (Ref)));
      end if;
      return Gsl_Null;
   end Runtime_Deref;

   ---------------------------------------------------------------------------
   --
   function Runtime_If
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Gsl.Interpreters;
      use Gsl.Compilers;
      use Gsl.Syntax_Tree;
      Cond            : Gsl_Type;
      True_Branch     : Gsl_Type;
      False_Branch    : Gsl_Type;
      Param           : Gsl_List;
      Gsl_Compiler    : Gsl.Compilers.Compiler;
      Execution_Stack : Execution_Stacks.Stack;
      Result_Stack    : Result_Stacks.Stack;
   begin
      Gsl_Compiler    := Get_Compiler;
      Execution_Stack := Get_Execution_Stack;
      Result_Stack    := Get_Result_Stack;

      if Get_List_Size (Parameter) /= 3 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'if': Expecting 3 parameters.");
      end if;
      Cond := Get_Value_At (Parameter, 1);
      True_Branch := Copy_Gsl_Type (Get_Value_At (Parameter, 2));
      False_Branch := Copy_Gsl_Type (Get_Value_At (Parameter, 3));
      if Is_Gsl_Boolean (Cond) then
         if Get_Value (Gsl_Boolean (Cond)) = true then
            if Is_Gsl_Script_Reference (True_Branch) then
               -- Script_Activation to Execution Stack
               Execution_Stacks.Push (Execution_Stack, Get_Execution_Stack
                 (Gsl_Compiler, Create_Node
                   (Script_Activation, Null_Node, Null_Node)));

               Result_Stacks.Push (Result_Stack, True_Branch);
               Param := Create_Gsl_List (0);
               return Gsl_Type (Param);
            else
               return True_Branch;
            end if;
         else
            if Is_Gsl_Script_Reference (False_Branch) then
               -- Script_Activation to Execution Stack
               Execution_Stacks.Push (Execution_Stack, Get_Execution_Stack
                 (Gsl_Compiler, Create_Node
                   (Script_Activation, Null_Node, Null_Node)));

               Result_Stacks.Push (Result_Stack, False_Branch);
               Param := Create_Gsl_List (0);
               return Gsl_Type (Param);
            else
               return False_Branch;
            end if;
         end if;
      else
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'if': Gsl_Boolean expected.");
      end if;
   end Runtime_If;

   ---------------------------------------------------------------------------
   --
   function Runtime_Loop
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Gsl.Interpreters;
      use Gsl.Compilers;
      use Gsl.Syntax_Tree;
      Script   : Gsl_Type;
      Loop_Cmd : Syntax_Node;
      Comp     : Compiler;
      ES       : Execution_Stacks.Stack;
   begin
      Comp := Get_Compiler;
      ES := Get_Execution_Stack;
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'loop': Expecting 1 parameter.");
      end if;
      Script := Copy_Gsl_Type (Get_Value_At (Parameter, 1));
      if Is_Gsl_Script_Reference (Script) then
         Loop_Cmd := Create_Node (Script_Loop,
           Get_Script_Node (Gsl_Script_Reference (Script)), Null_Node);
         Execution_Stacks.Push (ES, Loop_Cmd);

         -- set the new activation record
         Set_Activation_Record (Create_Activation_Record
           (Get_Activation_Record (Gsl_Script_Reference (Script))));

         -- push the code of the script
         Execution_Stacks.Push (ES, Get_Execution_Stack (Comp,
           Get_Script_Node (Gsl_Script_Reference (Script))));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'loop': Gsl_Script_Reference expected.");
      end if;
      return Gsl_Null;
   end Runtime_Loop;

   ---------------------------------------------------------------------------
   --
   function Runtime_Error
     (Parameter : Gsl_List)
      return Gsl_Type is

      Message : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'error': Expecting 1 parameter.");
      end if;
      Message := Get_Value_At (Parameter, 1);
      if Is_Gsl_String (Message) then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           Get_Value (Gsl_String (Message)));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'error': Gsl_String expected.");
      end if;
      return Gsl_Null;
   end Runtime_Error;

   ---------------------------------------------------------------------------
   --
   function Runtime_Run
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Gsl.Interpreters;
      use Gsl.Compilers;
      use Gsl.Syntax_Tree;
      Name : Gsl_Type;
      Comp : Compiler;
      ES   : Execution_Stacks.Stack;
   begin
      Comp := Get_Compiler;
      ES := Get_Execution_Stack;
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'run': Expecting 1 parameter.");
      end if;
      Name := Get_Value_At (Parameter, 1);
      if Is_Gsl_String (Name) then
         -- push the code of the library to the execution stack
         Execution_Stacks.Push (ES, Get_Execution_Stack
           (Comp, GSL_Support.Get_GSL_Include
             (Get_Value (Gsl_String (Name)) & ".gsl")));
         -- remove the Gsl_Null result from the result stack in the next step
         Execution_Stacks.Push (ES, Get_Execution_Stack
           (Comp, Create_Node (Result_Pop, Null_Node, Null_Node)));
         return Gsl_Null;
      else
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'run': Gsl_String expected.");
      end if;

      exception
         when GSL_Support. GSL_Script_Not_Found_Exception =>
            Ada.Exceptions.Raise_Exception
              (Gsl_Runtime_Error'Identity,
               "Script 'run': Gsl library " & Get_Value (Gsl_String (Name)) &
               " not found.");
   end Runtime_Run;

------------------------------------------------------------------------------
-- arithmetic (ref. GIANT Scripting Language Specification 1.5.1.3)

   ---------------------------------------------------------------------------
   --
   function Runtime_Add
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Gsl.Interpreters;
      A        : Gsl_Type;
      B        : Gsl_Type;
      Var      : Gsl_Type;
      Node_Set : Graph_Lib.Node_Id_Set;
      Edge_Set : Graph_Lib.Edge_Id_Set;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'add': Expecting 2 parameters.");
      end if;
      A := Get_Value_At (Parameter, 1);
      B := Get_Value_At (Parameter, 2);
      if Is_Gsl_Natural (A) and Is_Gsl_Natural (B) then
         -- Gsl_Natural, normal addition
         return Gsl_Type (Create_Gsl_Natural
           (Get_Value (Gsl_Natural (A)) + Get_Value (Gsl_Natural (B))));

      elsif Is_Gsl_Var_Reference (A) then
         Var := Get_Var (Get_Ref_Name (Gsl_Var_Reference (A)));
         if Is_Gsl_Node_Set (Var) then
            Node_Set := Get_Value (Gsl_Node_Set (Var));
            if Is_Gsl_Node_Id (B) then
               Graph_Lib.Node_Id_Sets.Insert
                 (Node_Set, Get_Value (Gsl_Node_Id (B)));
            elsif Is_Gsl_Node_Set (B) then
               Graph_Lib.Node_Id_Sets.Union
                 (Node_Set, Get_Value (Gsl_Node_Set (B)));
            else
               Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
                 "Script 'add': Gsl_Node_Id or Gsl_Node_Set expected.");
            end if;
            Set_Value (Gsl_Node_Set (Var), Node_Set);
            return Gsl_Null;

         elsif Is_Gsl_Edge_Set (Var) then
            Edge_Set := Get_Value (Gsl_Edge_Set (Var));
            if Is_Gsl_Edge_Id (B) then
               Graph_Lib.Edge_Id_Sets.Insert
                 (Edge_Set, Get_Value (Gsl_Edge_Id (B)));
            elsif Is_Gsl_Edge_Set (B) then
               Graph_Lib.Edge_Id_Sets.Union
                 (Edge_Set, Get_Value (Gsl_Edge_Set (B)));
            else
               Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
                 "Script 'add': Gsl_Edge_Id or Gsl_Edge_Set expected.");
            end if;
            Set_Value (Gsl_Edge_Set (Var), Edge_Set);
            return Gsl_Null;

         else
            Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
              "Script 'add': Gsl_Node_Set or Gsl_Edge_Set expected.");
         end if;

      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'add': Gsl_Natural or Gsl_Var_Reference expected.");
      end if;
   end Runtime_Add;

   ---------------------------------------------------------------------------
   --
   function Runtime_Sub
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Gsl.Interpreters;
      A        : Gsl_Type;
      B        : Gsl_Type;
      Var      : Gsl_Type;
      Node_Set : Graph_Lib.Node_Id_Set;
      Edge_Set : Graph_Lib.Edge_Id_Set;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'sub': Expecting 2 parameters.");
      end if;
      A := Get_Value_At (Parameter, 1);
      B := Get_Value_At (Parameter, 2);
      if Is_Gsl_Natural (A) and Is_Gsl_Natural (B) then
         -- Gsl_Natural, normal substraction
         return Gsl_Type (Create_Gsl_Natural
           (Get_Value (Gsl_Natural (A)) - Get_Value (Gsl_Natural (B))));

      elsif Is_Gsl_Var_Reference (A) then
         Var := Get_Var (Get_Ref_Name (Gsl_Var_Reference (A)));
         if Is_Gsl_Node_Set (Var) then
            Node_Set := Get_Value (Gsl_Node_Set (Var));
            if Is_Gsl_Node_Id (B) then
               Graph_Lib.Node_Id_Sets.Remove_If_Exists
                 (Node_Set, Get_Value (Gsl_Node_Id (B)));
            elsif Is_Gsl_Node_Set (B) then
               Graph_Lib.Node_Id_Sets.Diff
                 (Node_Set, Get_Value (Gsl_Node_Set (B)));
            else
               Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
                 "Script 'sub': Gsl_Node_Id or Gsl_Node_Set expected.");
            end if;
            Set_Value (Gsl_Node_Set (Var), Node_Set);
            return Gsl_Null;

         elsif Is_Gsl_Edge_Set (Var) then
            Edge_Set := Get_Value (Gsl_Edge_Set (Var));
            if Is_Gsl_Edge_Id (B) then
               Graph_Lib.Edge_Id_Sets.Remove_If_Exists
                 (Edge_Set, Get_Value (Gsl_Edge_Id (B)));
            elsif Is_Gsl_Edge_Set (B) then
               Graph_Lib.Edge_Id_Sets.Diff
                 (Edge_Set, Get_Value (Gsl_Edge_Set (B)));
            else
               Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
                 "Script 'sub': Gsl_Edge_Id or Gsl_Edge_Set expected.");
            end if;
            Set_Value (Gsl_Edge_Set (Var), Edge_Set);
            return Gsl_Null;

         else
            Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
              "Script 'sub': Gsl_Node_Set or Gsl_Edge_Set expected.");
         end if;

      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'sub': Gsl_Natural or Gsl_Var_Reference expected.");
      end if;
   end Runtime_Sub;

   ---------------------------------------------------------------------------
   --
   function Runtime_Cat
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param1 : Gsl_Type;
      Param2 : Gsl_Type;
      Cat    : Gsl_String;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'cat': Expecting 2 parameters.");
      end if;
      Param1 := Get_Value_At (Parameter, 1);
      Param2 := Get_Value_At (Parameter, 2);

      if Is_Gsl_String (Param1) and Is_Gsl_String (Param2) then
         Cat := Create_Gsl_String (Get_Value (Gsl_String (Param1)) &
                                   Get_Value (Gsl_String (Param2)));
         return Gsl_Type (Cat);
      else
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'cat': Gsl_String expected.");
      end if;
   end Runtime_Cat;

------------------------------------------------------------------------------
-- compare (ref. GIANT Scripting Language Specification 1.5.1.4)

   ---------------------------------------------------------------------------
   --
   function Runtime_Less
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param1 : Gsl_Type;
      Param2 : Gsl_Type;
      Res    : Gsl_Boolean;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'less': Expecting 2 parameters.");
      end if;
      Param1 := Get_Value_At (Parameter, 1);
      Param2 := Get_Value_At (Parameter, 2);

      if Is_Gsl_Natural (Param1) and Is_Gsl_Natural (Param2) then
         if Get_Value (Gsl_Natural (Param1)) <
            Get_Value (Gsl_Natural (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;

      elsif Is_Gsl_Edge_Id (Param1) and Is_Gsl_Edge_Id (Param2) then
         if Get_Value (Gsl_Edge_Id (Param1)) <
            Get_Value (Gsl_Edge_Id (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;

      elsif Is_Gsl_Node_Id (Param1) and Is_Gsl_Node_Id (Param2) then
         if Get_Value (Gsl_Node_Id (Param1)) <
            Get_Value (Gsl_Node_Id (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;

      elsif Is_Gsl_String (Param1) and Is_Gsl_String (Param2) then
         if Get_Value (Gsl_String (Param1)) <
            Get_Value (Gsl_String (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;

      else
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'less': Gsl_Natural, " &
            "Gsl_Edge_Id, Gsl_Node_Id or Gsl_String expected.");
      end if;
      return Gsl_Type (Res);
   end Runtime_Less;

   ---------------------------------------------------------------------------
   --
   function Runtime_Equal
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param1 : Gsl_Type;
      Param2 : Gsl_Type;
      Res    : Gsl_Boolean;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'equal': Expecting 2 parameters.");
      end if;
      Param1 := Get_Value_At (Parameter, 1);
      Param2 := Get_Value_At (Parameter, 2);

      if Is_Gsl_Natural (Param1) and Is_Gsl_Natural (Param2) then
         if Get_Value (Gsl_Natural (Param1)) =
            Get_Value (Gsl_Natural (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;

      elsif Is_Gsl_Edge_Id (Param1) and Is_Gsl_Edge_Id (Param2) then
         if Get_Value (Gsl_Edge_Id (Param1)) =
            Get_Value (Gsl_Edge_Id (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;

      elsif Is_Gsl_Node_Id (Param1) and Is_Gsl_Node_Id (Param2) then
         if Get_Value (Gsl_Node_Id (Param1)) =
            Get_Value (Gsl_Node_Id (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;

      elsif Is_Gsl_String (Param1) and Is_Gsl_String (Param2) then
         if Get_Value (Gsl_String (Param1)) =
            Get_Value (Gsl_String (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;

      else
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'equal': Gsl_Natural, " &
            "Gsl_Edge_Id, Gsl_Node_Id or Gsl_String expected.");
      end if;
      return Gsl_Type (Res);
   end Runtime_Equal;

   ---------------------------------------------------------------------------
   --
   function Runtime_In_Regexp
     (Parameter : Gsl_List)
      return Gsl_Type is

      use GNAT.Regpat;

      Regexp  : Gsl_Type;
      Data    : Gsl_Type;
      Matches : Match_Array (0 .. 0);
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'in_regexp': Expecting 2 parameters.");
      end if;
      Data   := Get_Value_At (Parameter, 1);
      Regexp := Get_Value_At (Parameter, 2);

      if Is_Gsl_String (Data) and Is_Gsl_String (Regexp) then
         Match
           (Get_Value (Gsl_String (Regexp)),
            Get_Value (Gsl_String (Data)),
            Matches);

         if (Matches (0).First = 1) and
            (Matches (0).Last = Get_Value (Gsl_String (Data))'Length)
         then
            return Gsl_Type (Create_Gsl_Boolean (true));
         else
            return Gsl_Type (Create_Gsl_Boolean (false));
         end if;
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'in_regexp': Gsl_String expected.");
      end if;

      exception
         when GNAT.Regpat.Expression_Error =>
           Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
             "Script 'in_regexp': Invalid regular expression.");
   end Runtime_In_Regexp;

   ---------------------------------------------------------------------------
   function Runtime_Type_In
     (Parameter : Gsl_List)
      return Gsl_Type is
      IML_Type       : Gsl_Type;
      Class_Set      : Config.Class_Sets.Class_Set_Access;
      Class_Set_Name : Gsl_Type;

      Edge_Class     : Graph_Lib.Edge_Class_Id;
      Node_Class     : Graph_Lib.Node_Class_Id;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'type_in': Expecting 2 parameters.");
      end if;
      IML_Type       := Get_Value_At (Parameter, 1);
      Class_Set_Name := Get_Value_At (Parameter, 2);

      if Is_Gsl_String (IML_Type) and Is_Gsl_String (Class_Set_Name) then
         if Config.Class_Sets.Does_Class_Set_Exist
           (Get_Value (Gsl_String (Class_Set_Name))) then
            Class_Set := Config.Class_Sets.Get_Class_Set_Access
              (Get_Value (Gsl_String (Class_Set_Name)));

            declare
               Name : String := Get_Value (Gsl_String (IML_Type));
            begin
               if Graph_Lib.Does_Node_Class_Exist (Name) then
                  Node_Class := Graph_Lib.Convert_Node_Class_Name_To_Id (Name);
                  return Gsl_Type
                    (Create_Gsl_Boolean
                     (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
                      (Class_Set, Node_Class)));
               else
                  begin
                     Edge_Class := Graph_Lib.Convert_Edge_Class_Tag_To_Id
                       (Name);
                  exception
                     when Graph_Lib.Edge_Class_Does_Not_Exist =>
                        Ada.Exceptions.Raise_Exception
                          (Gsl_Runtime_Error'Identity,
                           "Script 'type_in': Edge_Class not found.");
                  end;
                  return Gsl_Type
                    (Create_Gsl_Boolean
                     (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
                      (Class_Set, Edge_Class)));
               end if;
            end;
         else
            Ada.Exceptions.Raise_Exception
              (Gsl_Runtime_Error'Identity,
               "Script 'type_in': Class_Set not found.");
         end if;
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'type_in': Gsl_String expected.");
      end if;

   end Runtime_Type_In;

------------------------------------------------------------------------------
-- sets and lists (ref. GIANT Scripting Language Specification 1.5.1.5)

   ---------------------------------------------------------------------------
   --
   function Runtime_Empty_Node_Set
     (Parameter : Gsl_List)
      return Gsl_Type is

      Node_Set : Gsl_Node_Set;
   begin
      if Get_List_Size (Parameter) /= 0 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'empty_node_set': Expecting no parameters.");
      end if;
      Node_Set := Create_Gsl_Node_Set (Graph_Lib.Node_Id_Sets.Empty_Set);
      return Gsl_Type (Node_Set);
   end Runtime_Empty_Node_Set;

   ---------------------------------------------------------------------------
   --
   function Runtime_Empty_Edge_Set
     (Parameter : Gsl_List)
      return Gsl_Type is

      Edge_Set : Gsl_Edge_Set;
   begin
      if Get_List_Size (Parameter) /= 0 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'empty_edge_set': Expecting no parameters.");
      end if;
      Edge_Set := Create_Gsl_Edge_Set (Graph_Lib.Edge_Id_Sets.Empty_Set);
      return Gsl_Type (Edge_Set);
   end Runtime_Empty_Edge_Set;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_In
     (Parameter : Gsl_List)
      return Gsl_Type is

      Set     : Gsl_Type;
      Element : Gsl_Type;
      Res     : Gsl_Boolean;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'is_in': Expecting 2 parameters.");
      end if;
      Set := Get_Value_At (Parameter, 1);
      Element := Get_Value_At (Parameter, 2);
      if Is_Gsl_Node_Set (Set) and Is_Gsl_Node_Id (Element) then
         Res := Create_Gsl_Boolean (Graph_Lib.Node_Id_Sets.Is_Member
                                    (Get_Value (Gsl_Node_Set (Set)),
                                     Get_Value (Gsl_Node_Id (Element))));
      elsif Is_Gsl_Edge_Set (Set) and Is_Gsl_Edge_Id (Element) then
         Res := Create_Gsl_Boolean (Graph_Lib.Edge_Id_Sets.Is_Member
                                    (Get_Value (Gsl_Edge_Set (Set)),
                                     Get_Value (Gsl_Edge_Id (Element))));

      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'is_in': Gsl_Node_Set or Gsl_Edge_Set and " &
           "Gsl_Node_Id or Gsl_Edge_Id expected.");
      end if;
      return Gsl_Type (Res);
   end Runtime_Is_In;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_First
     (Parameter : Gsl_List)
      return Gsl_Type is
      use Gsl.Interpreters;

      Obj  : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'size_of': Expecting 1 parameter.");
      end if;
      Obj := Get_Value_At (Parameter, 1);
      if Is_Gsl_Var_Reference (Obj) then
         Obj := Get_Var (Get_Ref_Name (Gsl_Var_Reference (Obj)));
      end if;
      if Is_Gsl_Node_Set (Obj) then
         if Graph_Lib.Node_Id_Sets.Is_Empty (Get_Value (Gsl_Node_Set (Obj)))
         then
            return Gsl_Null;
         else
            return Gsl_Type (Create_Gsl_Node_Id
              (Graph_Lib.Node_Id_Sets.First
                (Get_Value (Gsl_Node_Set (Obj)))));
         end if;

      elsif Is_Gsl_Edge_Set (Obj) then
         if Graph_Lib.Edge_Id_Sets.Is_Empty (Get_Value (Gsl_Edge_Set (Obj)))
         then
            Ada.Exceptions.Raise_Exception
              (Gsl_Runtime_Error'Identity,
               "Script 'get_first': non-empty set expected.");
         else
            return Gsl_Type (Create_Gsl_Edge_Id
              (Graph_Lib.Edge_Id_Sets.First
                (Get_Value (Gsl_Edge_Set (Obj)))));
         end if;

      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_first': Gsl_Node_Set or Gsl_Edge_Set expected.");
      end if;
      return Gsl_Null;
   end Runtime_Get_First;

   ---------------------------------------------------------------------------
   --
   function Runtime_Size_Of
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Gsl.Interpreters;
      Obj  : Gsl_Type;
      Size : Gsl_Natural;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'size_of': Expecting 1 parameter.");
      end if;
      Obj := Get_Value_At (Parameter, 1);
      if Is_Gsl_Var_Reference (Obj) then
         Obj := Get_Var (Get_Ref_Name (Gsl_Var_Reference (Obj)));
      end if;
      if Is_Gsl_List (Obj) then
         Size := Create_Gsl_Natural (Get_List_Size (Gsl_List (Obj)));
      elsif Is_Gsl_Node_Set (Obj) then
         Size := Create_Gsl_Natural (Graph_Lib.Node_Id_Sets.Size
                                      (Get_Value (Gsl_Node_Set (Obj))));
      elsif Is_Gsl_Edge_Set (Obj) then
         Size := Create_Gsl_Natural (Graph_Lib.Edge_Id_Sets.Size
                                      (Get_Value (Gsl_Edge_Set (Obj))));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'size_of': Gsl_List, Gsl_Node_Set or " &
           "Gsl_Edge_Set expected.");
      end if;
      return Gsl_Type (Size);
   end Runtime_Size_Of;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Entry
     (Parameter : Gsl_List)
      return Gsl_Type is

      List  : Gsl_Type;
      Index : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_entry': Expecting 2 parameters.");
      end if;
      List := Get_Value_At (Parameter, 1);
      Index := Get_Value_At (Parameter, 2);
      if Is_Gsl_List (List) and Is_Gsl_Natural (Index) then
         if Get_List_Size (Gsl_List (List)) >= Get_Value (Gsl_Natural (Index))
         then
            return Copy_Gsl_Type (Gsl_Type (Get_Value_At
              (Gsl_List (List), Get_Value (Gsl_Natural (Index)))));
         else
            return Gsl_Null;
         end if;
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_entry': Gsl_List and Gsl_Natural expected.");
      end if;
   end Runtime_Get_Entry;

------------------------------------------------------------------------------
-- types (ref. GIANT Scripting Language Specification 1.5.1.6)

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Nodeid
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_nodeid' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Node_Id (Param)));
   end Runtime_Is_Nodeid;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Edgeid
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_edgeid' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Edge_Id (Param)));
   end Runtime_Is_Edgeid;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Node_Set
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_node_set' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Node_Set (Param)));
   end Runtime_Is_Node_Set;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Edge_Set
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_edge_set' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Edge_Set (Param)));
   end Runtime_Is_Edge_Set;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_String
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_string' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_String (Param)));
   end Runtime_Is_String;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Boolean
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_boolean' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Boolean (Param)));
   end Runtime_Is_Boolean;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Natural
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_natural' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Natural (Param)));
   end Runtime_Is_Natural;

   ---------------------------------------------------------------------------
   --
   function Runtime_To_Natural
     (Parameter : Gsl_List)
      return Gsl_Type is

      Obj : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'to_natural' requires " &
             "1 parameter.");
      end if;
      Obj := Get_Value_At (Parameter, 1);
      if Is_Gsl_String (Obj) then
         return Gsl_Type (Create_Gsl_Natural (Natural'Value
           (Get_Value (Gsl_String (Obj)))));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'to_natural': Gsl_String expected.");
      end if;

      exception
         when Constraint_Error =>
            Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
              "Script 'to_natural': Bad value. Casting is not possible.");
   end Runtime_To_Natural;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_List
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_list' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_List (Param)));
   end Runtime_Is_List;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Reference
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_reference' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Var_Reference (Param)));
   end Runtime_Is_Reference;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Script
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_script' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Script_Reference (Param)));
   end Runtime_Is_Script;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Object_Set
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_object_set' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Object_Set (Param)));
   end Runtime_Is_Object_Set;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Null
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_nodeid' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      if Param = Gsl_Null then
         return Gsl_Type (Create_Gsl_Boolean (true));
      else
         return Gsl_Type (Create_Gsl_Boolean (false));
      end if;
   end Runtime_Is_Null;

------------------------------------------------------------------------------
-- IML interpreter (ref. GIANT Scripting Language Specification 1.5.2.1)

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Current_Window
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Gsl.Interpreters;
   begin
      if Gsl.Interpreters.Get_Current_Context = "" then
         return Gsl_Null;
      else
         return Gsl_Type (Create_Gsl_String (Get_Current_Context));
      end if;
   end Runtime_Get_Current_Window;

   ---------------------------------------------------------------------------
   --
   function Runtime_Set_Current_Window
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Controller;
      use Gsl.Interpreters;
      Name : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'set_current_window': Expecting 1 parameter.");
      end if;
      Name := Get_Value_At (Parameter, 1);
      if Is_Gsl_String (Name) then
         if Exists_Window (Get_Value (Gsl_String (Name))) then
            Set_Current_Context(Get_Value (Gsl_String (Name)));
            return Gsl_Type (Create_Gsl_Boolean (true));
         else
            return Gsl_Type (Create_Gsl_Boolean (false));
         end if;
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'set_current_window': Gsl_String expected.");
      end if;
      return Gsl_Null;
   end Runtime_Set_Current_Window;

------------------------------------------------------------------------------
-- IML graph (ref. GIANT Scripting Language Specification 1.5.1.7)

   ---------------------------------------------------------------------------
   --
   function Runtime_Root_Node
     (Parameter : Gsl_List)
      return Gsl_Type is

      Node : Gsl_Node_Id;
   begin
      if Get_List_Size (Parameter) /= 0 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'root_node': Expecting no parameters.");
      end if;
      Node := Create_Gsl_Node_Id;
      Set_Value (Node, Graph_Lib.Get_Root_Node);
      return Gsl_Type (Node);
   end Runtime_Root_Node;

   ---------------------------------------------------------------------------
   --
   function Runtime_All_Nodes
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      if Get_List_Size (Parameter) /= 0 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'all_nodes': Expecting no parameters.");
      end if;
      return Gsl_Type (Create_Gsl_Node_Set (Graph_Lib.Get_All_Nodes));
   end Runtime_All_Nodes;

   ---------------------------------------------------------------------------
   --
   function Runtime_All_Edges
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      if Get_List_Size (Parameter) /= 0 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'all_edges': Expecting no parameters.");
      end if;
      return Gsl_Type (Create_Gsl_Edge_Set (Graph_Lib.Get_All_Edges));
   end Runtime_All_Edges;

   ---------------------------------------------------------------------------
   --
   function Runtime_Has_Attribute
      (Parameter : Gsl_List)
      return Gsl_Type is

      use Graph_Lib;
      n      : Gsl_Type;
      attrib : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'has_attribute': Expecting 2 parameters.");
      end if;
      n := Get_Value_At (Parameter, 1);
      attrib := Get_Value_At (Parameter, 2);
      if Is_Gsl_Node_Id (n) and Is_Gsl_String (attrib) then
         return Gsl_Type (Create_Gsl_Boolean (Does_Node_Attribute_Exist
           (Get_Node_Class_Id (Get_Value (Gsl_Node_Id (n))),
            Get_Value (Gsl_String (attrib)))));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'has_attribute': Gsl_Node_Id and Gsl_String expected.");
      end if;
      return Gsl_Type (Create_Gsl_Boolean (false));
   end Runtime_Has_Attribute;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Attribute
      (Parameter : Gsl_List)
      return Gsl_Type is

      use Graph_Lib;
      N             : Gsl_Type;
      Attrib        : Gsl_Type;
      Attrib_Id     : Node_Attribute_Id;
      N_Id          : Node_Id;
      Node_Id_List  : Graph_Lib.Node_Id_List;
      Node_Iter     : Graph_Lib.Node_Id_Lists.ListIter;
      Node_List     : Gsl_List;
      Position      : Natural := 1;
      Sloc          : Gsl_List;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_attribute': Expecting 2 parameters.");
      end if;
      N := Get_Value_At (Parameter, 1);
      Attrib := Get_Value_At (Parameter, 2);
      if Is_Gsl_Node_Id (N) and Is_Gsl_String (Attrib) then
         if Does_Node_Attribute_Exist
           (Get_Node_Class_Id (Get_Value (Gsl_Node_Id (N))),
            Get_Value (Gsl_String (Attrib)))
         then
            Attrib_Id := Convert_Node_Attribute_Name_To_Id
              (Get_Node_Class_Id (Get_Value (Gsl_Node_Id (N))),
               Get_Value (Gsl_String (Attrib)));
            case Get_Node_Attribute_Class_Id (Attrib_Id) is
               when Class_Node_Id =>
                  return Gsl_Type (Create_Gsl_Node_Id
                    (Get_Node_Attribute_Node_Id_Value
                      (Get_Value (Gsl_Node_Id (N)), Attrib_Id)));

               when Class_Node_Id_List =>
                  Node_Id_List := Get_Node_Attribute_Node_Id_List_Value
                    (Get_Value (Gsl_Node_Id (N)), Attrib_Id);
                  Node_List := Create_Gsl_List
                    (Node_Id_Lists.Length (Node_Id_List));

                  Node_Iter := Node_Id_Lists.MakeListIter (Node_Id_List);
                  while Node_Id_Lists.More (Node_Iter) loop
                     Node_Id_Lists.Next (Node_Iter, N_Id);
                     Set_Value_At
                       (Node_List, Position,
                        Gsl_Type (Create_Gsl_Node_Id (N_Id)));
                     Position := Position + 1;
                  end loop;
                  return Gsl_Type (Node_List);

               when Class_Node_Id_Set =>
                  return Gsl_Type (Create_Gsl_Node_Set
                    (Get_Node_Attribute_Node_Id_Set_Value
                      (Get_Value (Gsl_Node_Id (N)), Attrib_Id)));

               when Class_Identifier =>
                  return Gsl_Type (Create_Gsl_String
                    (Get_Node_Attribute_Identifier_Value
                      (Get_Value (Gsl_Node_Id (N)), Attrib_Id)));

               when Class_String =>
                  return Gsl_Type (Create_Gsl_String
                    (Get_Node_Attribute_String_Value
                      (Get_Value (Gsl_Node_Id (N)), Attrib_Id)));

               when Class_SLoc =>
                  Sloc := Create_Gsl_List (4);
                  Set_Value_At (Sloc, 1, Gsl_Type (Create_Gsl_String
                    (Get_Node_Attribute_SLoc_Filename_Value
                      (Get_Value (Gsl_Node_Id (N)), Attrib_Id))));
                  Set_Value_At (Sloc, 2, Gsl_Type (Create_Gsl_String
                    (Get_Node_Attribute_SLoc_Path_Value
                      (Get_Value (Gsl_Node_Id (N)), Attrib_Id))));
                  Set_Value_At (Sloc, 3, Gsl_Type (Create_Gsl_Natural
                    (Get_Node_Attribute_SLoc_Line_Value
                      (Get_Value (Gsl_Node_Id (N)), Attrib_Id))));
                  Set_Value_At (Sloc, 4, Gsl_Type (Create_Gsl_Natural
                    (Get_Node_Attribute_SLoc_Column_Value
                      (Get_Value (Gsl_Node_Id (N)), Attrib_Id))));
                  return Gsl_Type (Sloc);

               when Class_Boolean =>
                  return Gsl_Type (Create_Gsl_Boolean
                    (Get_Node_Attribute_Boolean_Value
                      (Get_Value (Gsl_Node_Id (N)), Attrib_Id)));

               when Class_Natural =>
                  return Gsl_Type (Create_Gsl_Natural
                    (Get_Node_Attribute_Natural_Value
                      (Get_Value (Gsl_Node_Id (N)), Attrib_Id)));

               when Class_Invalid =>
                  return Gsl_Null;

            end case;
         else
            Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
              "Script 'get_attribute': Attribute not found.");
         end if;
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_attribute': Gsl_Node_Id and Gsl_String expected.");
      end if;
   end Runtime_Get_Attribute;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Type
      (Parameter : Gsl_List)
      return Gsl_Type is

      use Graph_Lib;
      Obj : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_type': Expecting 1 parameter.");
      end if;
      Obj := Get_Value_At (Parameter, 1);
      if Is_Gsl_Node_Id (Obj) then
         return Gsl_Type (Create_Gsl_String (Get_Node_Class_Tag
           (Get_Node_Class_Id (Get_Value (Gsl_Node_Id (Obj))))));

      elsif Is_Gsl_Edge_Id (Obj) then
         return Gsl_Type (Create_Gsl_String (Get_Edge_Class_Tag
           (Get_Edge_Class_Id (Get_Value (Gsl_Edge_Id (Obj))))));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_type': Gsl_Node_Id or Gsl_Edge_Id expected.");
      end if;
      return Gsl_Null;
   end Runtime_Get_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Instance_Of
      (Parameter : Gsl_List)
      return Gsl_Type is

      use Graph_Lib;
      Obj    : Gsl_Type;
      Class  : Gsl_Type;
      Id     : Node_Class_Id;
      Ids    : Node_Class_Id_Set;
      Result : Gsl_Boolean;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'instance_of': Expecting 2 parameters.");
      end if;
      Obj   := Get_Value_At (Parameter, 1);
      Class := Get_Value_At (Parameter, 2);
      if Is_Gsl_Node_Id (Obj) and Is_Gsl_String (Class) then
         Ids := Get_Predecessors (Get_Node_Class_Id
                                     (Get_Value (Gsl_Node_Id (Obj))), True);
         Id := Convert_Node_Class_Name_To_Id (Get_Value (Gsl_String (Class)));
         Result := Create_Gsl_Boolean
           (Node_Class_Id_Sets.Is_Member (Ids, Id));
         Node_Class_Id_Sets.Destroy (Ids);
         return Gsl_Type (Result);
      else
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity,
            "Script 'instance_of': Gsl_Node_Id and Gsl_String expected.");
      end if;
   end Runtime_Instance_Of;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Incoming
      (Parameter : Gsl_List)
      return Gsl_Type is

      Node  : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_incoming_edges': Expecting 1 parameter.");
      end if;
      Node := Get_Value_At (Parameter, 1);
      if Is_Gsl_Node_Id (Node) then
         return Gsl_Type (Create_Gsl_Edge_Set
           (Graph_Lib.Get_Incoming_Edges (Get_Value (Gsl_Node_Id (Node)))));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_incoming_edges': Gsl_Node_Id expected.");
      end if;
   end Runtime_Get_Incoming;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Outgoing
      (Parameter : Gsl_List)
      return Gsl_Type is

      Node  : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_outgoing_edges': Expecting 1 parameter.");
      end if;
      Node := Get_Value_At (Parameter, 1);
      if Is_Gsl_Node_Id (Node) then
         return Gsl_Type (Create_Gsl_Edge_Set
           (Graph_Lib.Get_Outgoing_Edges (Get_Value (Gsl_Node_Id (Node)))));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_outgoing_edges': Gsl_Node_Id expected.");
      end if;
   end Runtime_Get_Outgoing;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Source
      (Parameter : Gsl_List)
      return Gsl_Type is

      Edge : Gsl_Type;
      Node : Gsl_Node_Id;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_source': Expecting 1 parameter.");
      end if;
      Edge := Get_Value_At (Parameter, 1);
      if Is_Gsl_Edge_Id (Edge) then
         Node := Create_Gsl_Node_Id;
         Set_Value (Node, Graph_Lib.Get_Source_Node
           (Get_Value (Gsl_Edge_Id (Edge))));
         return Gsl_Type (Node);
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_source': Gsl_Edge_Id expected.");
      end if;
      return Gsl_Null;
   end Runtime_Get_Source;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Target
      (Parameter : Gsl_List)
      return Gsl_Type is

      Edge : Gsl_Type;
      Node : Gsl_Node_Id;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_target': Expecting 1 parameter.");
      end if;
      Edge := Get_Value_At (Parameter, 1);
      if Is_Gsl_Edge_Id (Edge) then
         Node := Create_Gsl_Node_Id;
         Set_Value (Node, Graph_Lib.Get_Target_Node
           (Get_Value (Gsl_Edge_Id (Edge))));
         return Gsl_Type (Node);
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_target': Gsl_Edge_Id expected.");
      end if;
      return Gsl_Null;
   end Runtime_Get_Target;

------------------------------------------------------------------------------
-- GUI (ref. GIANT Scripting Language Specification 1.5.2.3 and 1.5.2.4)

   ---------------------------------------------------------------------------
   --
   function Runtime_Input
   (Parameter : Gsl_List)
      return Gsl_Type is

      Input_Name : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
        Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'input': Expecting 1 parameter.");
      end if;
      Input_Name := Get_Value_At (Parameter, 1);
      if Is_Gsl_String (Input_Name) then
         return Gsl_Type (Create_Gsl_String (Controller.Show_Input
           (Get_Value (Gsl_String (Input_Name)))));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'input': Gsl_String expected.");
      end if;
   end Runtime_Input;

   ---------------------------------------------------------------------------
   --
   function Runtime_Exists_Window
      (Parameter : Gsl_List)
      return Gsl_Type is

      Name : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'exists_window': Expecting 1 parameter.");
      end if;
      Name := Get_Value_At (Parameter, 1);
      if Is_Gsl_String (Name) then
         return Gsl_Type (Create_Gsl_Boolean
           (Controller.Exists_Window (Get_Value (Gsl_String (Name)))));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'create_window': Gsl_String expected.");
      end if;
      return Gsl_Type (Create_Gsl_Boolean (false));
   end Runtime_Exists_Window;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Window_Content
      (Parameter : Gsl_List)
      return Gsl_Type is

      Subgraph    : Graph_Lib.Subgraphs.Subgraph;
      Window_Name : Gsl_Type;
      Object_Set  : Gsl_List;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_window_content': Expecting 1 parameter.");
      end if;
      Window_Name := Get_Value_At (Parameter, 1);
      if Is_Gsl_String (Window_Name) then
         -- check if window exist
         if not Controller.Exists_Window (Get_Value (Gsl_String (Window_Name)))
         then
            Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
              "Script 'insert_into_window': Window " &
              Get_Value (Gsl_String (Window_Name)) &  "does not exist.");
         end if;

         Subgraph := Controller.Gsl_Get_Window_Content
           (Get_Value (Gsl_String (Window_Name)));

         Object_Set := Create_Gsl_List (2);
         Set_Value_At (Object_Set, 1, Gsl_Type
           (Create_Gsl_Node_Set (Graph_Lib.Node_Id_Sets.Copy
             (Graph_Lib.Subgraphs.Get_All_Nodes (Subgraph)))));
         Set_Value_At (Object_Set, 2, Gsl_Type
           (Create_Gsl_Edge_Set (Graph_Lib.Edge_Id_Sets.Copy
             (Graph_Lib.Subgraphs.Get_All_Edges (Subgraph)))));
         return Gsl_Type (Object_Set);
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_window_content': Gsl_String expected.");
      end if;
      return Gsl_Null;
   end Runtime_Get_Window_Content;

   ---------------------------------------------------------------------------
   --
   function Runtime_Create_Window
      (Parameter : Gsl_List)
      return Gsl_Type is

      use Controller;
      Name: Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'create_window': Expecting 1 parameter.");
      end if;
      Name := Get_Value_At (Parameter, 1);
      if Is_Gsl_String (Name) then
         if Exists_Window (Get_Value (Gsl_String (Name)))
         then
            return Gsl_Type (Create_Gsl_Boolean (false));
         else
            Create_Window (Get_Value (Gsl_String (Name)));
            return Gsl_Type (Create_Gsl_Boolean (true));
         end if;
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'create_window': Gsl_String expected.");
      end if;
      return Gsl_Type (Create_Gsl_Boolean (false));
   end Runtime_Create_Window;

   ---------------------------------------------------------------------------
   --
   function Runtime_Insert_Into_Window
      (Parameter : Gsl_List)
      return Gsl_Type is

      use Graph_Lib.Selections;
      Sel            : Graph_Lib.Selections.Selection;
      Window_Name    : Gsl_Type;
      Selection_Name : Gsl_Type;
      Selection      : Gsl_Type;
      Layout_Algo    : Gsl_Type;
      Layout_Param   : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 5 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'insert_into_window': Expecting 5 parameters.");
      end if;
      Window_Name    := Get_Value_At (Parameter, 1);
      Selection_Name := Get_Value_At (Parameter, 2);
      Selection      := Copy_Gsl_Type (Get_Value_At (Parameter, 3));
      Layout_Algo    := Get_Value_At (Parameter, 4);
      Layout_Param   := Get_Value_At (Parameter, 5);
      if Is_Gsl_String (Window_Name) and Is_Gsl_String (Selection_Name) and
         Is_Gsl_Object_Set (Selection) and Is_Gsl_String (Layout_Algo) and
         Is_Gsl_String (Layout_Param)
      then
         -- check if window exist
         if not Controller.Exists_Window (Get_Value (Gsl_String (Window_Name)))
         then
            Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
              "Script 'insert_into_window': Window " &
              Get_Value (Gsl_String (Window_Name)) &  " does not exist.");
         end if;

         -- build the selection with graph_lib functions
         Sel := Create (Get_Value (Gsl_String (Selection_Name)));
         Add_Node_Set (Sel, Get_Value (Gsl_Node_Set
           (Get_Value_At (Gsl_List (Selection), 1))));
         Add_Edge_Set (Sel, Get_Value (Gsl_Edge_Set
           (Get_Value_At (Gsl_List (Selection), 2))));

         -- use controller to insert the selection
         Controller.Insert_Selection
           (Window_Name => Get_Value (Gsl_String (Window_Name)),
            Selection_Name => Get_Value (Gsl_String (Selection_Name)),
            Selection => Sel,
            Layout_Name => Get_Value (Gsl_String (Layout_Algo)),
            Additional_Parameters => Get_Value (Gsl_String (Layout_Param)),
            Parent_Evolution => Evolutions.Iterative_Evolution_Class_Access
              (Gsl.Interpreters.Get_Current_Interpreter));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'insert_into_window': Error. Check the parameters.");
      end if;
      return Gsl_Null;
   end Runtime_Insert_Into_Window;

   ---------------------------------------------------------------------------
   --
   function Runtime_Remove_From_Window
      (Parameter : Gsl_List)
      return Gsl_Type is

      use Graph_Lib.Selections;
      Window_Name    : Gsl_Type;
      Remove_Content : Gsl_Type;
      Selection      : Graph_Lib.Selections.Selection;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'remove_from_window': Expecting 2 parameters.");
      end if;
      Window_Name    := Get_Value_At (Parameter, 1);
      Remove_Content := Get_Value_At (Parameter, 2);
      if Is_Gsl_String (Window_Name) and Is_Gsl_Object_Set (Remove_Content)
      then
         -- check if window exist
         if not Controller.Exists_Window (Get_Value (Gsl_String (Window_Name)))
         then
            Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
              "Script 'remove_from_window': Window " &
              Get_Value (Gsl_String (Window_Name)) &  "does not exist.");
         end if;

         Selection := Create ("REMOVE_SELECTION_CONTENT");
         Add_Node_Set (Selection, Get_Value (Gsl_Node_Set
           (Get_Value_At (Gsl_List (Remove_Content), 1))));
         Add_Edge_Set (Selection, Get_Value (Gsl_Edge_Set
           (Get_Value_At (Gsl_List (Remove_Content), 2))));

         -- remove the selection using the giant.controller
         Controller.Remove_Selection_Content
           (Get_Value (Gsl_String (Window_Name)), Selection);

         -- destroy the selection and free the memory
         Destroy (Selection);
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'remove_from_window': Gsl_String and " &
           "Gsl_Object_Set expected.");
      end if;
      return Gsl_Null;
   end Runtime_Remove_From_Window;

end Giant.Gsl.Runtime;
