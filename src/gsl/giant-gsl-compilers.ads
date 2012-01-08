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
-- $RCSfile: giant-gsl-compilers.ads,v $
-- $Author: schulzgt $
-- $Date: 2003-09-23 17:20:36 $
--
------------------------------------------------------------------------------
-- This package implements a "compiler", more precise a code generator, 
-- that generates GSL code (Execution_Stack) from a GSL file or an 
-- entry point (Syntax_Node) to a syntax tree.
-- The compiler uses Giant.Parser, an AFLEX/AYACC generated
-- parser for the GSL to produce that syntax tree.
-- During to performance reasons the syntax trees are chached by the
-- compiler.
-- 
--

-- from Ada
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

-- from Gnat
with GNAT.OS_Lib;

--from Bauhaus.Reuse
with Hashed_Mappings;
pragma Elaborate_All (Hashed_Mappings);

package Giant.Gsl.Compilers is

   ---------------------------------------------------------------------------
   -- main 'compiler' datatype
   type Compiler_Record is private;
   type Compiler is access all Compiler_Record;

   ---------------------------------------------------------------------------
   -- datatype for the hash map of the compiler (to cach the GSL scripts)
   type Gsl_Script_Record (Name_Length : Natural) is private;
   type Gsl_Script is access all Gsl_Script_Record;

   ---------------------------------------------------------------------------
   -- instantiation of Hashed_Mappings for GSL scripts
   -- the hash function Script_Hash uses String_Hash
   function Script_Hash
     (K : Unbounded_String) 
      return Integer;

   package Script_Hashed_Mappings is new Hashed_Mappings
     (Key_Type => Unbounded_String,
      Value_Type => Gsl_Script,
      Hash => Script_Hash);

   ---------------------------------------------------------------------------
   -- creates a new compiler
   --
   -- Returns:
   --   the new compiler
   function Create_Compiler return Compiler;

   ---------------------------------------------------------------------------
   -- destroys a compiler with all scripts and frees the memory
   --
   -- Parameters:
   --   Comp - the compiler to destroy
   procedure Destroy_Compiler
     (Comp : in out Compiler);

   ---------------------------------------------------------------------------
   -- checks wether two timestamps are equal
   --
   -- Parameters:
   --   T1 - timestamp 1
   --   T2 - timestamp 2
   -- Returns:
   --   true if T1 and T2 are equal else false
   function Is_Equal
     (T1 : GNAT.OS_Lib.OS_Time;
      T2 : GNAT.OS_Lib.OS_Time)
      return Boolean;

   ---------------------------------------------------------------------------
   --
   function Get_Execution_Stack
     (Comp : Compiler;
      Name : String)
      return Execution_Stacks.Stack;

   ---------------------------------------------------------------------------
   --
   function Get_Execution_Stack
     (Comp : Compiler;
      Node : Syntax_Node)
      return Execution_Stacks.Stack;

private

   ---------------------------------------------------------------------------
   --
   type Compiler_Record is
      record
         Scripts : Script_Hashed_Mappings.Mapping;
      end record ;

   ---------------------------------------------------------------------------
   --
   type Gsl_Script_Record (Name_Length : Natural) is
      record
         Name        : String (1 .. Name_Length);
         Time_Stamp  : GNAT.OS_Lib.OS_Time;
         Error       : Boolean;
         Syntax_Tree : Syntax_Node;
      end record;

   ---------------------------------------------------------------------------
   --
   procedure Push_Syntax_Node
     (Node  : in     Syntax_Node;
      Stack : in out Execution_Stacks.Stack);

   ---------------------------------------------------------------------------
   --
   procedure Push_Sequence
     (Node  : in     Syntax_Node;
      Stack : in out Execution_Stacks.Stack;
      Size  : in out Natural);

end Giant.Gsl.Compilers;
