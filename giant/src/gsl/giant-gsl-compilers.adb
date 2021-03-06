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
-- $RCSfile: giant-gsl-compilers.adb,v $
-- $Author: schulzgt $
-- $Date: 2003-09-23 17:20:36 $
--
-- Back-End of the Gsl compiler. Uses the syntax tree generated by the 
-- Gsl parser to generate stack code for the Gsl interpreter.
-- The Ayacc grammar for the parser and the generated code is located in the 
-- subdirectory /generated.
--

with Ada.Exceptions;
with Unchecked_Deallocation;

------------------------------------------------------------------------------
-- packages from Bauhaus Reuse
with String_Hash;

------------------------------------------------------------------------------
-- packages from Giant
with Giant.Default_Logger;
with Giant.Parser;
with Giant.Parser.Tokens;
with Giant.Scanner.IO;
with Giant.Gsl.Syntax_Tree;
use  Giant.Gsl.Syntax_Tree;

package body Giant.Gsl.Compilers is

   ---------------------------------------------------------------------------
   --
   procedure Free is new Unchecked_Deallocation
     (Gsl_Script_Record,
      Gsl_Script);
   
   ---------------------------------------------------------------------------
   --
   procedure Free is new Unchecked_Deallocation
     (Compiler_Record,
      Compiler);

   ---------------------------------------------------------------------------
   --
   function Script_Hash
     (K : Unbounded_String)
      return Integer is
   begin
      return String_Hash (To_String (K));
   end Script_Hash;

   ---------------------------------------------------------------------------
   -- creates a new gsl compiler
   function Create_Compiler return Compiler is

      Comp : Compiler;
   begin
      Comp := new Compiler_Record;
      Comp.Scripts := Script_Hashed_Mappings.Create;
      return Comp;
   end Create_Compiler;

   ---------------------------------------------------------------------------
   -- destroys a gsl compiler
   procedure Destroy_Compiler
     (Comp : in out Compiler) is

      Iter   : Script_Hashed_Mappings.Values_Iter;
      Script : Gsl_Script;
   begin
      Iter := Script_Hashed_Mappings.Make_Values_Iter (Comp.Scripts);
      while Script_Hashed_Mappings.More (Iter) loop
         Script_Hashed_Mappings.Next (Iter, Script);
         Destroy_Syntax_Tree (Script.Syntax_Tree);
         Free (Script);
      end loop;
      -- destroy hash map
      Script_Hashed_Mappings.Destroy (Comp.Scripts);
      -- free compiler
      Free (Comp);
   end Destroy_Compiler;

   ---------------------------------------------------------------------------
   -- checks if two OS_Time values are equal
   function Is_Equal
     (T1 : GNAT.OS_Lib.OS_Time;
      T2 : GNAT.OS_Lib.OS_Time)
      return Boolean is
   begin
     if (GNAT.OS_Lib.GM_Year (T1) = GNAT.OS_Lib.GM_Year (T2)) and
        (GNAT.OS_Lib.GM_Month (T1) = GNAT.OS_Lib.GM_Month (T2)) and
        (GNAT.OS_Lib.GM_Day (T1) = GNAT.OS_Lib.GM_Day (T2)) and
        (GNAT.OS_Lib.GM_Hour (T1) = GNAT.OS_Lib.GM_Hour (T2)) and
        (GNAT.OS_Lib.GM_Minute (T1) = GNAT.OS_Lib.GM_Minute (T2)) and
        (GNAT.OS_Lib.GM_Second (T1) = GNAT.OS_Lib.GM_Second (T2)) then
        -- time stamps are equal
        return True;
     else
        -- time stamps are different
        return False;
     end if;
   end;

   ---------------------------------------------------------------------------
   -- builds an execution stack for the Gsl interpreter
   -- first looks in the cache (Hash_Map) for the syntax tree
   -- if necessary the Gsl parser is used
   function Get_Execution_Stack
     (Comp : Compiler;
      Name : String)
      return Execution_Stacks.Stack is

      Script : Gsl_Script; 
      Stack  : Execution_Stacks.Stack;
   begin
      if Script_Hashed_Mappings.Is_Bound
        (Comp.Scripts, To_Unbounded_String (Name))
      then
         Script := Script_Hashed_Mappings.Fetch
           (Comp.Scripts, To_Unbounded_String(Name)); 
         if Is_Equal
           (Script.Time_Stamp, GNAT.OS_Lib.File_Time_Stamp (Name)) and
           not Script.Error
         then
            -- script was already parsed and was not changed
            -- syntax tree is in the cache
            return Get_Execution_Stack (Comp, Script.Syntax_Tree);
         else
            -- script was changed, remove from hash map and call
            -- Get_Execution_Stack again
            Script_Hashed_Mappings.Unbind
              (Comp.Scripts, To_Unbounded_String (Name));
            -- free the memory
            Destroy_Syntax_Tree (Script.Syntax_Tree);
            Free (Script);
            return Get_Execution_Stack (Comp, Name);
         end if;
      else
         -- initialize new script and move to cache
         Script := new Gsl_Script_Record (Name'Length);
         Script.Name := Name;
         Script.Time_Stamp := GNAT.OS_Lib.File_Time_Stamp (Name);
         Script.Error := False; 

        -- insert script in hash map
         Script_Hashed_Mappings.Bind (Comp.Scripts,
           To_Unbounded_String(Name), Script);

         -- use the gsl parser to generate a syntax tree
         Giant.Scanner.IO.Open_Input (Name);
         Giant.Parser.yyparse;
         Script.Syntax_Tree := Giant.Parser.Get_Syntax_Tree;
         Giant.Scanner.IO.Close_Input;
         return Get_Execution_Stack (Comp, Script.Syntax_Tree);
      end if;

      exception
         when Giant.Parser.Tokens.Syntax_Error =>
            Giant.Scanner.IO.Close_Input;
            Script.Error := True;
            Ada.Exceptions.Raise_Exception (Gsl_Syntax_Error'Identity,
              "Syntax error: " & Name & ":" &
              To_String (Giant.Parser.Get_Error_Message));
   end Get_Execution_Stack;

   ---------------------------------------------------------------------------
   -- creates and builds a new execution stack for the Gsl interpreter
   -- by a recursive traversing of the syntax tree
   -- this function is called directly by the Gsl interpreter or by the
   -- compiler function Get_Execution_Stack (Comp : Compiler; Name : String);
   function Get_Execution_Stack
     (Comp : Compiler;
      Node : Syntax_Node)
      return Execution_Stacks.Stack is

      Stack : Execution_Stacks.Stack;
   begin
      Stack := Execution_Stacks.Create;
      Push_Syntax_Node (Node, Stack);
      return Stack;
   end Get_Execution_Stack;

   ---------------------------------------------------------------------------
   -- push a syntax node on top of the execution stack
   procedure Push_Syntax_Node
     (Node  :        Syntax_Node;
      Stack : in out Execution_Stacks.Stack) is
 
      Size       : Natural := 1;
      Local_Node : Syntax_Node;
   begin
      if Node /= Null_Node then
         -- work with a copy
         Local_Node := Copy_Node (Node);
         if Get_Node_Type (Local_Node) = Sequence or 
            Get_Node_Type (Local_Node) = List then
            -- Sequence or List needs a recursive traversation
            -- push all elements using Push_Sequence
            Execution_Stacks.Push (Stack, Local_Node);
            if Get_Child1 (Local_Node) = Null_Node then
               -- empty sequence or list, set size to 0
               Set_Size (Local_Node, 0);
            else
               -- push all elements of the sequence
               Push_Sequence (Local_Node, Stack, Size);
               -- set the size of the sequence
               Set_Size (Local_Node, Size);
            end if;

         elsif Get_Node_Type (Local_Node) = Script_Activation then
            -- Script_Activation, needs to push Child1 (Script_Reference)
            -- and Child2 (parameter)
            Execution_Stacks.Push (Stack, Local_Node);
            -- push the list (parameter)
            Push_Syntax_Node (Get_Child2 (Local_Node), Stack);
            -- push the expression (Script_Reference)
            Push_Syntax_Node (Get_Child1 (Local_Node), Stack);

         else
            -- default for all other nodes (only push the node)
            Execution_Stacks.Push (Stack, Local_Node);
         end if;
      end if;
   end Push_Syntax_Node;

   ---------------------------------------------------------------------------
   -- push a sequence or list on top of the execution stack
   procedure Push_Sequence
     (Node  :        Syntax_Node;
      Stack : in out Execution_Stacks.Stack;
      Size  : in out Natural) is 

   begin
      if Get_Child2 (Node) = Null_Node then
         -- end of a sequence
         Push_Syntax_Node (Get_Child1 (Node), Stack);
      else
         -- recursive
         Size := Size + 1;
         Push_Sequence (Get_Child2 (Node) , Stack, Size);
         Push_Syntax_Node (Get_Child1 (Node), Stack);
      end if;
   end Push_Sequence;

end Giant.Gsl.Compilers;
