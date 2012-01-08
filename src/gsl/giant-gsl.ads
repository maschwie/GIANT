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
-- $RCSfile: giant-gsl.ads,v $
-- $Author: keulsn $
-- $Date: 2003-10-05 20:49:11 $
--
------------------------------------------------------------------------------
-- This package includes some global parts of the GSL implementation
-- provided by GIANT.
--
-- There are three parts:
--   - exceptions
--   - two datastructures (Gsl_Type and Syntax_Node)
--   - instantiations of some packages for Bauhaus.Reuse
--


-- from Bauhaus.Reuse
with Stacks_Unbounded;
pragma Elaborate_All (Stacks_Unbounded);
with Hashed_Mappings;
pragma Elaborate_All (Hashed_Mappings);

-- from Giant
with Giant.Default_Logger;
with Giant.Gsl_Identifiers;

package Giant.Gsl is

   ---------------------------------------------------------------
   -- Gsl exceptions, should be catched in the gsl interpreter) --
   ---------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- syntax errors and runtime errors should be catched in Gsl.Interpreters
   Gsl_Syntax_Error  : exception;
   Gsl_Runtime_Error : exception;


   ---------------------------
   -- general Gsl datatypes --
   ---------------------------

   ---------------------------------------------------------------------------
   -- Gsl_Type - abstract parent class for all types defined in Gsl.Types
   type Gsl_Type_Record is abstract tagged private;
   type Gsl_Type is access all Gsl_Type_Record'Class;

   ---------------------------------------------------------------------------
   -- represents the type Gsl_Null and works as null-pointer for Gsl_Type
   Gsl_Null : constant Gsl_Type;

   ---------------------------------------------------------------------------
   -- copies a Gsl_Type, has to be implemented for all subclasses of Gsl_Type
   --
   -- Parameters:
   --   Object - the object to copy
   -- Returns:
   --   a new instance of Gsl_Type
   function Copy
     (Object : access Gsl_Type_Record)
      return Gsl_Type is abstract;

   ---------------------------------------------------------------------------
   -- destroys a Gsl_Type and frees the memory,
   -- has to be implemented for all subclasses of Gsl_Type
   --
   -- Parameters:
   --   Object - the object to destroy
   procedure Destroy
     (Object : out Gsl_Type) is abstract;

   ---------------------------------------------------------------------------
   -- creates a String-representation for a Gsl_Type
   --
   -- Parameters:
   --   Object - the object
   -- Returns:
   --   String-representation of the object
   function Gsl_Type_Image
     (Object : Gsl_Type)
      return String;

   ---------------------------------------------------------------------------
   -- possible types of Syntax_Node
   -- these types are equal to the Gsl commands used by Gsl.Processors
   type Node_Type is (Literal, Visible_Var, Global_Var, Visible_Ref,
                      Var_Creation, Global_Ref, Script_Decl, List, Sequence,
                      Script_Activation, Script_Exec, Script_Finish,
                      Script_Loop, Result_Pop, Param_Fetch);

   ---------------------------------------------------------------------------
   -- Syntax_Node is the type used in Gsl.Syntax_Tree and Gsl.Execution_Stacks
   type Syntax_Node_Record is private;
   type Syntax_Node is access all Syntax_Node_Record;

   ---------------------------------------------------------------------------
   -- works as null-pointer for Syntax_Node
   Null_Node : constant Syntax_Node;

   ---------------------------------------------------------------------------
   -- creates a String-representation for a Syntax_Node
   --
   -- Parameters:
   --   Node - the Syntax_Node
   -- Returns:
   --   String-representation of the Node
   function Syntax_Node_Image
     (Node : Syntax_Node)
      return String;

   ---------------------------------------------------------------------------
   -- Activation_Record is used in Gsl.Interpreter
   type Activation_Record_Record is private;
   type Activation_Record is access all Activation_Record_Record;


   -------------------------------------------------
   -- intantiation of packages from Bauhaus.Reuse --
   -------------------------------------------------

   ---------------------------------------------------------------------------
   -- instantiation of different stack types (Stacks_Unbounded)
   package Execution_Stacks is new Stacks_Unbounded
     (Elem_Type => Syntax_Node);

   package Result_Stacks is new Stacks_Unbounded
     (Elem_Type => Gsl_Type);

   package Activation_Record_Stacks is new Stacks_Unbounded
     (Elem_Type => Activation_Record);

   package Gsl_Var_Hashed_Mappings is new Hashed_Mappings
     (Key_Type   => Gsl_Identifiers.Identifier_Type,
      Value_Type => Gsl_Type,
      Hash       => Gsl_Identifiers.Hash);

private

   ---------------------------------------------------------------------------
   -- Gsl_Type - parent class for all types in GSL defined in Gsl.Types
   type Gsl_Type_Record is abstract tagged
      record
         null;
      end record;

   type Gsl_Type_Array is array (Positive range <>) of Gsl_Type;
   Gsl_Null : constant Gsl_Type := null;

   ---------------------------------------------------------------------------
   -- Syntax_Node is the type used in Gsl.Syntax_Tree and Gsl.Execution_Stacks
   type Syntax_Node_Record is
      record
         N_Type  : Node_Type;
         Child1  : Syntax_Node;
         Child2  : Syntax_Node;
         Literal : Gsl_Type;
         Size    : Natural;
     end record;

   Null_Node : constant Syntax_Node := null;

   ---------------------------------------------------------------------------
   -- Activation_Record is used in Gsl.Interpreters
   type Activation_Record_Record is
      record
         Parent : Activation_Record;
         Vars   : Gsl_Var_Hashed_Mappings.Mapping;
      end record;

end Giant.Gsl;
