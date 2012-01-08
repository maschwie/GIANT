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
-- $RCSfile: giant-gsl-interpreters.ads,v $
-- $Author: keulsn $
-- $Date: 2003-10-05 20:49:10 $
--
-- This package implements the Gsl interpreter.
--

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;
with Ada.Real_Time;

with Giant.Evolutions;
with Giant.Graph_Lib;
with Giant.Gsl.Compilers;
with Giant.Gsl.Types;
use  Giant.Gsl.Types;

package Giant.Gsl.Interpreters is

   ---------------------------------------------------------------------------
   -- the GSL interpreter, inherits Iterative_Evolution
   type Interpreter_Record is new Evolutions.Iterative_Evolution
     with private;

   type Interpreter is access all Interpreter_Record'Class;
   type Interpreter_Access is access all Interpreter_Record;

   type Gsl_Params is new Gsl.Types.Gsl_List;

   --------------------------------------------------------------------------
   --
   function Create_Interpreter
      return Interpreter;

   ---------------------------------------------------------------------------
   --
   function Get_Current_Interpreter
      return Interpreter;

   ---------------------------------------------------------------------------
   --
   function Get_Current_Activation_Record
      return Activation_Record;

   ---------------------------------------------------------------------------
   --
   function Get_Params
      return Gsl_Params;

   ---------------------------------------------------------------------------
   --
   function Get_Execution_Stack
      return Execution_Stacks.Stack;

   ---------------------------------------------------------------------------
   --
   function Get_Result_Stack
      return Result_Stacks.Stack;

   ---------------------------------------------------------------------------
   --
   function Get_Compiler
      return Gsl.Compilers.Compiler;

   ---------------------------------------------------------------------------
   --
   function Get_Current_Context return String;

   ---------------------------------------------------------------------------
   --
   procedure Set_Current_Context
     (Context : in String);

   --------------------------------------------------------------------------
   -- destroys a Gsl Interpreter
   procedure Destroy
     (Gsl_Interpreter : in out Interpreter);

   ---------------------------------------------------------------------------
   --
   procedure Initialize_Interpreter
     (Individual : in Interpreter;
      Name       : in String;
      Context    : in String);

   ---------------------------------------------------------------------------
   --
   function Create_Parameter_List
      return Gsl_Params;

   ---------------------------------------------------------------------------
   --
   procedure Add_Parameter
     (List  : in out Gsl_Params;
      Param : in     Graph_Lib.Node_Id);

   ---------------------------------------------------------------------------
   --
   procedure Add_Parameter
     (List  : in out Gsl_Params;
      Param : in     Graph_Lib.Edge_Id);

   ---------------------------------------------------------------------------
   --
   procedure Add_Parameter
     (List  : in out Gsl_Params;
      Param : in     String);

   ---------------------------------------------------------------------------
   --
   procedure Add_Parameter
     (List    : in out Gsl_Params;
      Param   : in     String;
      Context : in     String);

   ---------------------------------------------------------------------------
   --
   procedure Execute_Gsl_File
     (Individual : in Interpreter;
      Name       : in String;
      Context    : in String);

   ---------------------------------------------------------------------------
   --
   procedure Execute_Script
     (Individual : in Interpreter;
      Name       : in String;
      Context    : in String;
      Params     : in Gsl_Params);

   ---------------------------------------------------------------------------
   --
   procedure Step
     (Individual  : access Interpreter_Record;
      Next_Action : out    Evolutions.Evolution_Action);

   ---------------------------------------------------------------------------
   --
   procedure Finish
     (Individual : access Interpreter_Record;
      Canceled   : in     Boolean);

   ---------------------------------------------------------------------------
   --
   procedure Register_Runtime
     (Runtime    : in Runtime_Function;
      Name       : in String);

   ---------------------------------------------------------------------------
   --
   procedure Log_Execution_Stack;

   ---------------------------------------------------------------------------
   --
   procedure Log_Result_Stack;

   ---------------------------------------------------------------------------
   --
   procedure Create_Var
     (Identifier : in     Gsl_Identifiers.Identifier_Type);

   ---------------------------------------------------------------------------
   --
   function Get_Activation_Record_Level
     (Identifier : in     Gsl_Identifiers.Identifier_Type)
      return Natural;

   ---------------------------------------------------------------------------
   --
   function Get_Activation_Record_Level
     (Ar : in Activation_Record)
      return Natural;

   ---------------------------------------------------------------------------
   --
   procedure Exists_Var
     (Identifier : in     Gsl_Identifiers.Identifier_Type);

   ---------------------------------------------------------------------------
   --
   function Get_Var
     (Identifier : in     Gsl_Identifiers.Identifier_Type)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   procedure Set_Var
     (Identifier : in     Gsl_Identifiers.Identifier_Type;
      Value      : in     Gsl_Type);

   ---------------------------------------------------------------------------
   --
   procedure Set_Activation_Record
     (AR : in Activation_Record);

   ---------------------------------------------------------------------------
   --
   procedure Restore_Activation_Record;

   ---------------------------------------------------------------------------
   --
   function Create_Activation_Record
     (Parent : in Activation_Record)
      return Activation_Record;

   ---------------------------------------------------------------------------
   --
   function Get_Activation_Record_Parent
     (AR : in Activation_Record)
      return Activation_Record;

   ---------------------------------------------------------------------------
   --
   procedure Destroy_Activation_Record
     (AR : in Activation_Record);

private

   ---------------------------------------------------------------------------
   --
   Current_Interpreter : Interpreter;

   ---------------------------------------------------------------------------
   -- the GSL interpreter, inherits Iterative_Evolution
   type Interpreter_Record is new Evolutions.Iterative_Evolution with
      record
         Context                   : Unbounded_String;
         Script                    : Unbounded_String;
         Params                    : Gsl_Params;
         Execution_Stack           : Execution_Stacks.Stack;
         Result_Stack              : Result_Stacks.Stack;
         Main_Activation_Record    : Activation_Record;
         Current_Activation_Record : Activation_Record;
         Activation_Records        : Activation_Record_Stacks.Stack;
         Gsl_Compiler              : Gsl.Compilers.Compiler;
         Gsl_Time                  : Ada.Real_Time.Time;
      end record;

end Giant.Gsl.Interpreters;
