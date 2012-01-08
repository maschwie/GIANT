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
-- $RCSfile: giant-gsl-interpreters.adb,v $
-- $Author: keulsn $
-- $Date: 2003-10-05 20:49:10 $
--
-- This package implements the datatypes used in GSL.
--

with Ada.Exceptions;
with Ada.Real_Time;
with Text_IO;
with Unchecked_Deallocation;

with Giant.Gsl_Support;

with Giant.Controller;
with Giant.Default_Logger;
with Giant.Graph_Lib.Subgraphs;
with Giant.Graph_Lib.Selections;

with Giant.Gsl.Syntax_Tree;
with Giant.Gsl.Runtime;
with Giant.Gsl.Processors;

package body Giant.Gsl.Interpreters is

   ---------------------------------------------------------------------------
   -- creates a new gsl interpreter
   -- only the gsl compiler is initialized, all other fields are set to null
   function Create_Interpreter return Interpreter is

      Individual : Interpreter;
   begin
      Individual := new Interpreter_Record;
      Individual.Main_Activation_Record := null;
      Individual.Current_Activation_Record := null;
      Individual.Gsl_Compiler := Gsl.Compilers.Create_Compiler;
      return Individual;
   end Create_Interpreter;

   -------------------------------------------------------------------------
   -- returns the current interpreter
   function Get_Current_Interpreter
      return Interpreter is
   begin
      return Current_Interpreter;
   end Get_Current_Interpreter;

   -------------------------------------------------------------------------
   -- returns the current activation record
   function Get_Current_Activation_Record
      return Activation_Record is
   begin
      return Current_Interpreter.Current_Activation_Record;
   end Get_Current_Activation_Record;

   ---------------------------------------------------------------------------
   --
   function Get_Params
      return Gsl_Params is
   begin
      return Current_Interpreter.Params;
   end Get_Params;

   -------------------------------------------------------------------------
   -- returns the execution stack of the current interpreter
   function Get_Execution_Stack return Execution_Stacks.Stack is
   begin
      return Current_Interpreter.Execution_Stack;
   end Get_Execution_Stack;

   -------------------------------------------------------------------------
   -- returns the result stack of the current interpreter
   function Get_Result_Stack return Result_Stacks.Stack is
   begin
      return Current_Interpreter.Result_Stack;
   end Get_Result_Stack;

   -------------------------------------------------------------------------
   -- returns the compiler of the current interpreter
   function Get_Compiler return Gsl.Compilers.Compiler is
   begin
      return Current_Interpreter.Gsl_Compiler;
   end Get_Compiler;

   ---------------------------------------------------------------------------
   -- returns the context of the current interpreter
   function Get_Current_Context return String is
   begin
      return To_String (Current_Interpreter.Context);
   end Get_Current_Context;

   ---------------------------------------------------------------------------
   -- sets the context of the current interpreter
   procedure Set_Current_Context
     (Context : in String) is
   begin
      Current_Interpreter.Context := To_Unbounded_String (Context);
   end Set_Current_Context;

   --------------------------------------------------------------------------
   -- destroys a gsl interpreter
   procedure Destroy
     (Gsl_Interpreter : in out Interpreter) is

      procedure Free is new Unchecked_Deallocation
        (Interpreter_Record, Interpreter_Access);

   begin
      Gsl.Compilers.Destroy_Compiler (Gsl_Interpreter.Gsl_Compiler);
      Free (Interpreter_Access (Gsl_Interpreter));
   end Destroy;

   ------------------------------
   -- evolution implementation --
   ------------------------------

   ---------------------------------------------------------------------------
   -- initilizes the gsl interpreter for evolution
   procedure Initialize_Interpreter
     (Individual : in Interpreter;
      Name       : in String;
      Context    : in String) is

      use Gsl.Runtime;
   begin
      -- set the current interpreter and initilize context and script
      Current_Interpreter := Individual;
      Individual.Script   := To_Unbounded_String (Name);
      Individual.Context  := To_Unbounded_String (Context);

      -- initialize the result stack
      Individual.Result_Stack := Result_Stacks.Create;

      -- initialize the stack for all activation records
      Individual.Activation_Records := Activation_Record_Stacks.Create;

      -- create the main activation record and set it acitve
      Individual.Main_Activation_Record := Create_Activation_Record (null);
      Individual.Current_Activation_Record :=
        Individual.Main_Activation_Record;

      -- register runtime functions
      -- assignment (ref. GIANT Scripting Language Specification 1.5.1.1)
      Register_Runtime (Runtime_Set'Access,   "set");
      Register_Runtime (Runtime_Deref'Access, "deref");
      -- control (ref. GIANT Scripting Language Specification 1.5.1.2)
      Register_Runtime (Runtime_If'Access,    "if");
      Register_Runtime (Runtime_Loop'Access,  "loop");
      Register_Runtime (Runtime_Error'Access, "error");
      Register_Runtime (Runtime_Run'Access,   "run");
      -- arithmetic (ref. GIANT Scripting Language Specification 1.5.1.3)
      Register_Runtime (Runtime_Add'Access, "add");
      Register_Runtime (Runtime_Sub'Access, "sub");
      Register_Runtime (Runtime_Cat'Access, "cat");
      -- compare (ref. GIANT Scripting Language Specification 1.5.1.4)
      Register_Runtime (Runtime_Less'Access,      "less");
      Register_Runtime (Runtime_Equal'Access,     "equal");
      Register_Runtime (Runtime_In_Regexp'Access, "in_regexp");
      Register_Runtime (Runtime_Type_In'Access,   "type_in");
      -- sets and lists (ref. GIANT Scripting Language Specification 1.5.1.5)
      Register_Runtime (Runtime_Empty_Node_Set'Access, "empty_node_set");
      Register_Runtime (Runtime_Empty_Edge_Set'Access, "empty_edge_set");
      Register_Runtime (Runtime_Get_First'Access,      "get_first");
      Register_Runtime (Runtime_Is_In'Access,          "is_in");
      Register_Runtime (Runtime_Size_Of'Access,        "size_of");
      Register_Runtime (Runtime_Get_Entry'Access,      "get_entry");
      -- types (ref. GIANT Scripting Language Specification 1.5.1.6)
      Register_Runtime (Runtime_Is_Nodeid'Access,     "is_nodeid");
      Register_Runtime (Runtime_Is_Edgeid'Access,     "is_edgeid");
      Register_Runtime (Runtime_Is_Node_Set'Access,   "is_node_set");
      Register_Runtime (Runtime_Is_Edge_Set'Access,   "is_edge_set");
      Register_Runtime (Runtime_Is_String'Access,     "is_string");
      Register_Runtime (Runtime_Is_Boolean'Access,    "is_boolean");
      Register_Runtime (Runtime_Is_Natural'Access,    "is_natural");
      Register_Runtime (Runtime_To_Natural'Access,    "to_natural");
      Register_Runtime (Runtime_Is_List'Access,       "is_list");
      Register_Runtime (Runtime_Is_Reference'Access,  "is_reference");
      Register_Runtime (Runtime_Is_Script'Access,     "is_script");
      Register_Runtime (Runtime_Is_Object_Set'Access, "is_object_set");
      Register_Runtime (Runtime_Is_Null'Access,       "is_null");
      -- gsl interpreter (ref. GIANT Scripting Language Specification 1.5.2.1)
      Register_Runtime (Runtime_Get_Current_Window'Access,
                        "get_current_window");
      Register_Runtime (Runtime_Set_Current_Window'Access,
                        "set_current_window");
      -- iml graph (ref. GIANT Scripting Language Specification 1.5.2.2)
      Register_Runtime (Runtime_Root_Node'Access,     "root_node");
      Register_Runtime (Runtime_All_Nodes'Access,     "all_nodes");
      Register_Runtime (Runtime_All_Edges'Access,     "all_edges");
      Register_Runtime (Runtime_Has_Attribute'Access, "has_attribute");
      Register_Runtime (Runtime_Get_Attribute'Access, "get_attribute");
      Register_Runtime (Runtime_Get_Type'Access,      "get_type");
      Register_Runtime (Runtime_Instance_Of'Access,   "instance_of");
      Register_Runtime (Runtime_Get_Incoming'Access,  "get_incoming");
      Register_Runtime (Runtime_Get_Outgoing'Access,  "get_outgoing");
      Register_Runtime (Runtime_Get_Source'Access,    "get_source");
      Register_Runtime (Runtime_Get_Target'Access,    "get_target");
      -- gui (ref. GIANT Scripting Language Specification 1.5.2.3, 1.5.2.4)
      Register_Runtime (Runtime_Input'Access,         "input");
      Register_Runtime (Runtime_Exists_Window'Access, "exists_window");
      Register_Runtime (Runtime_Get_Window_Content'Access,
                        "get_window_content");
      Register_Runtime (Runtime_Create_Window'Access, "create_window");
      Register_Runtime (Runtime_Insert_Into_Window'Access,
                        "insert_into_window");
      Register_Runtime (Runtime_Remove_From_Window'Access,
                        "remove_from_window");
   end Initialize_Interpreter;

   ---------------------------------------------------------------------------
   --
   function Create_Parameter_List
      return Gsl_Params is

      Param : Gsl_Params;
   begin
      Param := Create_Gsl_List (0);
      return Param;
   end Create_Parameter_List;

   ---------------------------------------------------------------------------
   --
   procedure Add_Parameter
      (List  : in out Gsl_Params;
       Param : Gsl_Type) is

      Tmp_List : Gsl_Params;
   begin
      Tmp_List := Create_Gsl_List (Get_List_Size (List) + 1);
      for i in 1 .. Get_List_Size (List) loop
         Set_Value_At (Tmp_List, i, Copy (Get_Value_At (List, i)));
      end loop;
      Set_Value_At (Tmp_List, Get_List_Size (Tmp_List), Param);
      Destroy_Gsl_Type (Gsl_Type (List));
      List := Tmp_List;
   end Add_Parameter;

   ---------------------------------------------------------------------------
   --
   procedure Add_Parameter
     (List  : in out Gsl_Params;
      Param : in     Graph_Lib.Node_Id) is
   begin
      Add_Parameter (List, Gsl_Type (Create_Gsl_Node_Id (Param)));
   end Add_Parameter;

   ---------------------------------------------------------------------------
   --
   procedure Add_Parameter
     (List  : in out Gsl_Params;
      Param : in     Graph_Lib.Edge_Id) is
   begin
      Add_Parameter (List, Gsl_Type (Create_Gsl_Edge_Id (Param)));
   end Add_Parameter;

   ---------------------------------------------------------------------------
   --
   procedure Add_Parameter
     (List  : in out Gsl_Params;
      Param : in     String) is

      Identifier : Gsl_Identifiers.Identifier_Type :=
        Gsl_Identifiers.Get_Identifier (Param);
   begin
      Add_Parameter (List, Gsl_Type
        (Create_Gsl_Var_Reference (Subgraph, Identifier)));
   end Add_Parameter;

   ---------------------------------------------------------------------------
   --
   procedure Add_Parameter
     (List    : in out Gsl_Params;
      Param   : in     String;
      Context : in     String) is

      Identifier : Gsl_Identifiers.Identifier_Type :=
        Gsl_Identifiers.Get_Identifier (Param);
   begin
      Add_Parameter (List, Gsl_Type
        (Create_Gsl_Var_Reference (Selection, Identifier, Context)));
   end Add_Parameter;

   ---------------------------------------------------------------------------
   -- initilizes the gsl interpreter for evolution
   procedure Execute_Gsl_File
     (Individual : in Interpreter;
      Name       : in String;
      Context    : in String) is

   begin
      -- initialize the basics
      Initialize_Interpreter (Individual, Name, Context);
      Individual.Params := Gsl_Params (Gsl_Null);

      -- initialize the execution stack
      Individual.Execution_Stack := Gsl.Compilers.Get_Execution_Stack
        (Individual.Gsl_Compiler, Name);

      -- set gsl time (used for performance measurement)
      Individual.Gsl_Time := Ada.Real_Time.Clock;

      -- initilze the evolution object, comlexity is 0
      Initialize (Individual, 0);

      -- handling of exceptions
      exception
         when Syntax_Error : Gsl_Syntax_Error =>
            Controller.Show_Error
              (Ada.Exceptions.Exception_Message (Syntax_Error));
   end Execute_Gsl_File;

   ---------------------------------------------------------------------------
   -- initilizes the gsl interpreter for evolution
   procedure Execute_Script
     (Individual : in Interpreter;
      Name       : in String;
      Context    : in String;
      Params     : in Gsl_Params) is

      use Gsl.Compilers;
      use Gsl.Syntax_Tree;
      Script    : Gsl_Type;
      Param_Run : Gsl_List;
   begin
      -- initialize the basics
      Initialize_Interpreter
        (Individual,
         Gsl_Support.Get_Gsl_Include (Name & ".gsl"),
         Context);

      Individual.Params := Params;

      -- initialize the execution and the result stack with
      -- the following layout
      --
      -- |                                                       |
      -- | Script_Activation   ->   Gsl_List (Name)              |
      -- |                          Gsl_Script_Reference ("run") |
      -- | Param_Fetch         ->   Gsl_List (Param)             |
      -- | Script_Activation   ->   Gsl_Script_Reference         |
      -- | Sequence                                              |
      -- |                                                       |
      --
      -- execution stack
      Individual.Execution_Stack := Execution_Stacks.Create;
      Execution_Stacks.Push (Individual.Execution_Stack, Get_Execution_Stack
        (Individual.Gsl_Compiler, Create_Node
          (Script_Activation, Null_Node, Null_Node)));
      Execution_Stacks.Push (Individual.Execution_Stack, Get_Execution_Stack
        (Individual.Gsl_Compiler, Create_Node
          (Param_Fetch, Null_Node, Null_Node)));
      Execution_Stacks.Push (Individual.Execution_Stack, Get_Execution_Stack
        (Individual.Gsl_Compiler, Create_Node
          (Script_Activation, Null_Node, Null_Node)));
      -- Log_Execution_Stack;

      -- result stack for the activation of "run"
      Script := Copy (Get_Var (Gsl_Identifiers.Get_Identifier ("run")));
      Param_Run := Create_Gsl_List (1);
      Set_Value_At (Param_Run, 1, Gsl_Type (Create_Gsl_String (Name)));
      Result_Stacks.Push (Individual.Result_Stack, Script);
      Result_Stacks.Push (Individual.Result_Stack, Gsl_Type (Param_Run));
      -- Log_Result_Stack;

      -- set gsl time (used for performance measurement)
      Individual.Gsl_Time := Ada.Real_Time.Clock;

      -- initilze the evolution object, comlexity is 0
      Initialize (Individual, 0);
   end Execute_Script;

   ---------------------------------------------------------------------------
   -- this procedure is called during the evolution
   procedure Step
     (Individual  : access Interpreter_Record;
      Next_Action : out    Evolutions.Evolution_Action) is

      Cmd      : Syntax_Node;
      End_Time : Ada.Real_Time.Time := Ada.Real_Time."+"
        (Ada.Real_Time.Clock, Ada.Real_Time.Milliseconds (500));
   begin
      if Execution_Stacks.Is_Empty
        (Current_Interpreter.Execution_Stack) then

         Next_Action := Evolutions.Finish;
      elsif Interpreter (Individual) /= Current_Interpreter then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity,
            "Runtime error: Invalid interpreter.");
      else
         loop
            -- get a gsl command from the execution stack
            Execution_Stacks.Pop (Current_Interpreter.Execution_Stack, Cmd);

            -- execute the gsl command calling the gsl processor
            Gsl.Processors.Execute (Cmd);

            -- advance the evolution
            Evolutions.Advance_Progress (Individual, 1);

            -- destroy the gsl command, free memory
            Gsl.Syntax_Tree.Destroy_Node (Cmd);

            -- set the next action for the evolution
            Next_Action := Evolutions.Run;

            -- only for debug
            -- Log_Execution_Stack;
            -- Log_Result_Stack;
            -- Default_Logger.Debug
            --   ("________________________________________");
            -- exit the processing if script execution is finished or
            -- if a given time limit is over
            if Execution_Stacks.Is_Empty
              (Current_Interpreter.Execution_Stack) then

               Next_Action := Evolutions.Finish;
            end if;
            exit when Evolutions."/=" (Next_Action, Evolutions.Run) or else
              Ada.Real_Time.">=" (Ada.Real_Time.Clock, End_Time);
         end loop;
      end if;

      -- handling of exceptions
      exception
         when Syntax_Error : Gsl_Syntax_Error =>
            Controller.Show_Error
              (Ada.Exceptions.Exception_Message (Syntax_Error));
            Next_Action := Evolutions.Finish;

         when Runtime_Error : Gsl_Runtime_Error =>
            Controller.Show_Error
              (Ada.Exceptions.Exception_Message (Runtime_Error));
            Next_Action := Evolutions.Finish;
   end Step;

   ---------------------------------------------------------------------------
   --
   procedure Finish
     (Individual : access Interpreter_Record;
      Canceled   : in     Boolean) is

      Benchmark : Duration;
   begin
      Benchmark := Ada.Real_Time.To_Duration
        (Ada.Real_Time."-" (Ada.Real_Time.Clock, Individual.Gsl_Time));
       Default_Logger.Debug
         ("Gsl Script finished: Time " & Benchmark'Img,
          "Giant.Gsl.Interpreters");
      Destroy_Activation_Record (Individual.Main_Activation_Record);
   end Finish;

   ---------------------------------------------------------------------------
   --
   procedure Register_Runtime
     (Runtime : in Runtime_Function;
      Name    : in String) is

      Script : Gsl_Script_Reference;
   begin
      Script := Create_Gsl_Script_Reference (Runtime);
      Create_Var (Gsl_Identifiers.Get_Identifier (Name));
      Set_Var (Gsl_Identifiers.Get_Identifier (Name), Gsl_Type (Script));
   end Register_Runtime;

   ---------------------------------------------------------------------------
   --
   procedure Log_Execution_Stack is

      S : Execution_Stacks.Stack;
      E : Syntax_Node;
   begin
      S := Execution_Stacks.Copy (Current_Interpreter.Execution_Stack);
      Default_Logger.Debug ("", "Giant.Gsl");
      Default_Logger.Debug ("-- Execution Stack --", "Giant.Gsl");
      while Execution_Stacks.Is_Empty (S) = false loop
         Execution_Stacks.Pop (S, E);
         Default_Logger.Debug
           ("Execution Stack: " & Syntax_Node_Image (E), "Giant.Gsl");
      end loop;
      Default_Logger.Debug ("", "Giant.Gsl");
   end Log_Execution_Stack;

   ---------------------------------------------------------------------------
   --
   procedure Log_Result_Stack is

      S : Result_Stacks.Stack;
      E : Gsl_Type;
   begin
      S := Result_Stacks.Copy (Current_Interpreter.Result_Stack);
      Default_Logger.Debug ("", "Giant.Gsl");
      Default_Logger.Debug ("-- Result Stack --", "Giant.Gsl");
      while Result_Stacks.Is_Empty (S) = false loop
         Result_Stacks.Pop (S, E);
         Default_Logger.Debug
           ("Result Stack: " & Gsl_Type_Image (E), "Giant.Gsl");
      end loop;
      Default_Logger.Debug ("", "Giant.Gsl");
   end Log_Result_Stack;

   ------------------------
   -- activation records --
   ------------------------

   ---------------------------------------------------------------------------
   -- sets the current activation record
   procedure Set_Activation_Record
     (AR : Activation_Record) is
   begin
      -- save the current activation record on the stack
      Activation_Record_Stacks.Push
        (Current_Interpreter.Activation_Records,
         Current_Interpreter.Current_Activation_Record);
      -- set the current activation record
      Current_Interpreter.Current_Activation_Record := AR;
   end Set_Activation_Record;

   ---------------------------------------------------------------------------
   --
   procedure Restore_Activation_Record is
   begin
      Destroy_Activation_Record
        (Current_Interpreter.Current_Activation_Record);
      Activation_Record_Stacks.Pop
        (Current_Interpreter.Activation_Records,
         Current_Interpreter.Current_Activation_Record);
   end Restore_Activation_Record;

   ---------------------------------------------------------------------------
   -- creates a new Activation_Record with Parent as
   -- the parent Activation_Record
   function Create_Activation_Record
     (Parent : in Activation_Record)
      return Activation_Record is

      AR : Activation_Record;
   begin
      AR := new Activation_Record_Record;
      AR.Parent := Parent;
      AR.Vars := Gsl_Var_Hashed_Mappings.Create;
      return AR;
   end Create_Activation_Record;

   ---------------------------------------------------------------------------
   --
   function Get_Activation_Record_Parent
     (AR : in Activation_Record)
      return Activation_Record is
   begin
      return AR.Parent;
   end Get_Activation_Record_Parent;

   ---------------------------------------------------------------------------
   --
   procedure Destroy_Activation_Record
     (AR : Activation_Record) is

      Iter : Gsl_Var_Hashed_Mappings.Values_Iter;
      Var  : Gsl_Type;
   begin
      Iter := Gsl_Var_Hashed_Mappings.Make_Values_Iter (AR.Vars);
      while Gsl_Var_Hashed_Mappings.More (Iter) loop
         Gsl_Var_Hashed_Mappings.Next (Iter, Var);
         Destroy_Gsl_Type (Var);
      end loop;
      Gsl_Var_Hashed_Mappings.Destroy (AR.Vars);
   end Destroy_Activation_Record;

   --------------------------------------------------------------------------
   -- creates a new variable in the current activation record
   procedure Create_Var
     (Identifier : in Gsl_Identifiers.Identifier_Type) is

      use Ada.Strings.Unbounded;
      AR : Activation_Record;
   begin
      AR := Current_Interpreter.Current_Activation_Record;
      if Gsl_Var_Hashed_Mappings.Is_Bound (AR.Vars, Identifier) then
         declare
            Name : String := Gsl_Identifiers.Get_Name (Identifier);
         begin
            -- variable already exists, raise Exception
            Ada.Exceptions.Raise_Exception
              (Gsl_Runtime_Error'Identity,
               "Runtime error: Variable '" & Name & "' already exists.");
         end;
      else
         Gsl_Var_Hashed_Mappings.Bind
           (AR.Vars, Identifier, Gsl_Null);
      end if;
   end Create_Var;

   --------------------------------------------------------------------------
   --
   function Get_Activation_Record_Level
     (Identifier : in     Gsl_Identifiers.Identifier_Type)
      return Natural is

      Count : Natural := 0;
      AR    : Activation_Record :=
              Current_Interpreter.Current_Activation_Record;
   begin
      while AR /= null loop
         if Gsl_Var_Hashed_Mappings.Is_Bound
           (AR.Vars, Identifier) then
            return Count;
         end if;
         -- next iteration look in the parent Activation_Record
         AR := AR.Parent;
         Count := Count + 1;
      end loop;
      declare
         Name : String := Gsl_Identifiers.Get_Name (Identifier);
      begin
         -- variable was not found, raise Exception
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity,
            "Runtime error: Variable '" & Name & "' was not found.");
      end;
   end Get_Activation_Record_Level;

   --------------------------------------------------------------------------
   --
   function Get_Activation_Record_Level
     (AR : in Activation_Record)
      return Natural is

      Count : Natural := 0;
      AR1   : Activation_Record :=
              Current_Interpreter.Current_Activation_Record;
   begin
      while AR1 /= null loop
         if AR1 = AR then
            return Count;
         end if;
         -- next iteration look in the parent Activation_Record
         AR1 := AR1.Parent;
         Count := Count + 1;
      end loop;
      -- variable was not found, raise Exception
      Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
        "Runtime error: Invalid Activation_Record");
   end Get_Activation_Record_Level;

   ---------------------------------------------------------------------------
   --
   procedure Exists_Var
     (Identifier : in     Gsl_Identifiers.Identifier_Type) is

      use Ada.Strings.Unbounded;
      AR    : Activation_Record :=
              Current_Interpreter.Current_Activation_Record;
   begin
      while AR /= null loop
         if Gsl_Var_Hashed_Mappings.Is_Bound
           (AR.Vars, Identifier) then
            return;
         end if;
         -- next iteration look in the parent Activation_Record
         AR := AR.Parent;
      end loop;
      declare
         Name : String := Gsl_Identifiers.Get_Name (Identifier);
      begin
         -- variable was not found, raise Exception
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity,
            "Runtime error: Variable '" & Name & "' was not found.");
      end;
   end Exists_Var;

   ---------------------------------------------------------------------------
   --
   function Get_Var
     (Identifier : in     Gsl_Identifiers.Identifier_Type)
      return Gsl_Type is

      use Ada.Strings.Unbounded;
      AR    : Activation_Record :=
              Current_Interpreter.Current_Activation_Record;
   begin
      while AR /= null loop
         if Gsl_Var_Hashed_Mappings.Is_Bound
           (AR.Vars, Identifier) then
            return Gsl_Var_Hashed_Mappings.Fetch
              (AR.Vars, Identifier);
         end if;
         -- next iteration look in the parent Activation_Record
         AR := AR.Parent;
      end loop;
      declare
         Name : String := Gsl_Identifiers.Get_Name (Identifier);
      begin
         -- variable was not found, raise Exception
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity,
            "Runtime error: Variable '" & Name & "' was not found.");
      end;
   end Get_Var;

   ---------------------------------------------------------------------------
   --
   procedure Set_Var
     (Identifier : in     Gsl_Identifiers.Identifier_Type;
      Value      : in     Gsl_Type) is

      use Ada.Strings.Unbounded;
      AR    : Activation_Record :=
              Current_Interpreter.Current_Activation_Record;
   begin
      while AR /= null loop
         if Gsl_Var_Hashed_Mappings.Is_Bound
           (AR.Vars, Identifier) then
            Gsl_Var_Hashed_Mappings.Unbind
              (AR.Vars, Identifier);
            Gsl_Var_Hashed_Mappings.Bind
              (AR.Vars, Identifier, Value);
            -- success, leave procedure here
            return;
         end if;
         -- next iteration look in the parent Activation_Record
         AR := AR.Parent;
      end loop;
      declare
         Name : String := Gsl_Identifiers.Get_Name (Identifier);
      begin
         -- variable was not found, raise Exception
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity,
            "Runtime error: Variable '" & Name & "' was not found.");
      end;
   end Set_Var;

end Giant.Gsl.Interpreters;
