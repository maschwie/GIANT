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
-- GNU General Public License for more detail;--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
-- First Author: Gerrit Schulz
--
-- $RCSfile: giant-gsl-types.ads,v $
-- $Author: keulsn $
-- $Date: 2003-10-05 20:49:11 $
--
-- This package implements the datatypes used in GSL.
-- A detailed description can be found in the GSL specification.
-- All GSL types are inherited from the abstract class Gsl_Type.
-- The baseclass Gsl_Type can be found in giant-gsl.ads.
--

with Giant.Graph_Lib;

package Giant.Gsl.Types is

   --------------------------------------------------------------------
   -- gsl types (ref. GIANT Scripting Language Specification: 1.3.1) --
   --------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- destroys any Gsl_Type and frees all memory used by this type
   -- determines the correct type of and calls its Destroy function
   --
   -- Parameters:
   --   Var - the type to destroy
   procedure Destroy_Gsl_Type
     (Var : in out Gsl_Type);

   function Copy_Gsl_Type
     (Object : Gsl_Type)
      return Gsl_Type;


   -----------------
   -- Gsl_Node_Id --
   -----------------

   type Gsl_Node_Id_Record is new Gsl_Type_Record with private;
   type Gsl_Node_Id is access all Gsl_Node_Id_Record;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Node_Id return Gsl_Node_Id;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Node_Id
     (Value : Giant.Graph_Lib.Node_Id)
      return Gsl_Node_Id;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Node_Id
     (Var : Gsl_Type)
      return Boolean;

   ---------------------------------------------------------------------------
   --
   function Get_Value
     (Var : Gsl_Node_Id)
      return Giant.Graph_Lib.Node_Id;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value
     (Var   : Gsl_Node_Id;
      Value : Giant.Graph_Lib.Node_Id);

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Node_Id_Record)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Node_Id);

   ---------------------------------------------------------------------------
   -- Gsl_Edge_Id
   ---------------------------------------------------------------------------

   type Gsl_Edge_Id_Record is new Gsl_Type_Record with private;
   type Gsl_Edge_Id is access all Gsl_Edge_Id_Record;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Edge_Id return Gsl_Edge_Id;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Edge_Id
     (Value : Giant.Graph_Lib.Edge_Id)
      return Gsl_Edge_Id;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Edge_Id
     (Var : Gsl_Type)
      return Boolean;

   ---------------------------------------------------------------------------
   --
   function Get_Value
     (Var : Gsl_Edge_Id)
      return Giant.Graph_Lib.Edge_Id;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value
     (Var   : Gsl_Edge_Id;
      Value : Giant.Graph_Lib.Edge_Id);

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Edge_Id_Record)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Edge_Id);

   ---------------------------------------------------------------------------
   -- Gsl_Node_Set
   ---------------------------------------------------------------------------

   type Gsl_Node_Set_Record is new Gsl_Type_Record with private;
   type Gsl_Node_Set is access all Gsl_Node_Set_Record;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Node_Set
    (Value : Giant.Graph_Lib.Node_Id_Set)
     return Gsl_Node_Set;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Node_Set
     (Var : Gsl_Type)
      return Boolean;

   ---------------------------------------------------------------------------
   --
   function Get_Value
     (Var : Gsl_Node_Set)
      return Giant.Graph_Lib.Node_Id_Set;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value
     (Var   : Gsl_Node_Set;
      Value : Giant.Graph_Lib.Node_Id_Set);

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Node_Set_Record)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Node_Set);

   ---------------------------------------------------------------------------
   -- Gsl_Edge_Set
   ---------------------------------------------------------------------------

   type Gsl_Edge_Set_Record is new Gsl_Type_Record with private;
   type Gsl_Edge_Set is access all Gsl_Edge_Set_Record;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Edge_Set
     (Value : Giant.Graph_Lib.Edge_Id_Set)
      return Gsl_Edge_Set;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Edge_Set
     (Var : Gsl_Type)
      return Boolean;

   ---------------------------------------------------------------------------
   --
   function Get_Value
     (Var : Gsl_Edge_Set)
      return Giant.Graph_Lib.Edge_Id_Set;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value
     (Var   : Gsl_Edge_Set;
      Value : Giant.Graph_Lib.Edge_Id_Set);

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Edge_Set_Record)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Edge_Set);

   ---------------------------------------------------------------------------
   -- Gsl_String
   ---------------------------------------------------------------------------

   type Gsl_String_Record (Size : Natural) is new
     Gsl_Type_Record with private;
   type Gsl_String is access all Gsl_String_Record;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_String
     (Value : String)
      return Gsl_String;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_String
     (Var : Gsl_Type)
      return Boolean;

   ---------------------------------------------------------------------------
   --
   function Get_Value
     (Var : Gsl_String)
      return String;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value
     (Var   : Gsl_String;
      Value : String);

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_String_Record)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_String);

   ---------------------------------------------------------------------------
   -- Gsl_Boolean
   ---------------------------------------------------------------------------

   type Gsl_Boolean_Record is new Gsl_Type_Record with private;
   type Gsl_Boolean is access all Gsl_Boolean_Record;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Boolean
     (Value : Boolean)
      return Gsl_Boolean;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Boolean
     (Var : Gsl_Type)
      return Boolean;

   ---------------------------------------------------------------------------
   --
   function Get_Value
     (Var : Gsl_Boolean)
      return Boolean;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value
     (Var   : Gsl_Boolean;
      Value : Boolean);

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Boolean_Record)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Boolean);

   ---------------------------------------------------------------------------
   -- Gsl_Natural
   ---------------------------------------------------------------------------

   type Gsl_Natural_Record is new Gsl_Type_Record with private;
   type Gsl_Natural is access all Gsl_Natural_Record;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Natural return Gsl_Natural;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Natural
     (Value : Natural)
      return Gsl_Natural;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Natural
     (Var : Gsl_Type)
      return Boolean;

   ---------------------------------------------------------------------------
   --
   function Get_Value
     (Var : Gsl_Natural)
      return Natural;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value
     (Var   : Gsl_Natural;
      Value : Natural);

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Natural_Record)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Natural);

   ---------------------------------------------------------------------------
   -- Gsl_List
   ---------------------------------------------------------------------------

   type Gsl_List_Record (Size : Natural) is new Gsl_Type_Record with private;
   type Gsl_List is access all Gsl_List_Record;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_List
     (Size : Natural)
      return Gsl_List;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_List
     (Var : Gsl_Type)
      return Boolean;

   ---------------------------------------------------------------------------
   --
   function Get_List_Size
     (Var      : Gsl_List)
      return Natural;

   ---------------------------------------------------------------------------
   --
   function Get_Value_At
     (Var      : Gsl_List;
      Position : Natural)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value_At
     (Var      : Gsl_List;
      Position : Natural;
      Value    : Gsl_Type);

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_List_Record)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_List);

   ---------------------------------------------------------------------------
   -- Gsl_Var_Reference ("local" Var or "global" Subgraph, Selection)
   ---------------------------------------------------------------------------

   type Reference_Type is (Var, Subgraph, Selection);
   type Gsl_Var_Reference_Record
     (Context_Size : Natural) is new Gsl_Type_Record with private;
   type Gsl_Var_Reference is access all Gsl_Var_Reference_Record;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Var_Reference
     (Ref_Type : in Reference_Type;
      Ref_Name : in Gsl_Identifiers.Identifier_Type;
      Context  : in String := "")
      return Gsl_Var_Reference;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Var_Reference
     (Var : in Gsl_Type)
      return Boolean;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Global_Reference
     (Var : in Gsl_Type)
      return Boolean;

   ---------------------------------------------------------------------------
   --
   function Get_Ref_Type
     (Var : in Gsl_Var_Reference)
      return Reference_Type;

   ---------------------------------------------------------------------------
   --
   function Get_Ref_Name
     (Var : in Gsl_Var_Reference)
      return Gsl_Identifiers.Identifier_Type;

   ---------------------------------------------------------------------------
   --
   function Get_Ref_Context
     (Var : in Gsl_Var_Reference)
      return String;

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Var_Reference_Record)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Var_Reference);

   ---------------------------------------------------------------------------
   -- Gsl_Script_Reference
   ---------------------------------------------------------------------------

   type Gsl_Script_Reference_Record is new Gsl_Type_Record with private;
   type Gsl_Script_Reference is access all Gsl_Script_Reference_Record;

   type Gsl_Script_Type is (Gsl_Script, Gsl_Runtime);

   ---------------------------------------------------------------------------
   --
   type Runtime_Function is access
     function
       (Parameter : Gsl_List)
        return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Script_Reference
     (Parameter_List : Syntax_Node;
      Script_Node    : Syntax_Node)
      return Gsl_Script_Reference;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Script_Reference
     (Runtime : Runtime_Function)
      return Gsl_Script_Reference;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Script_Reference
     (Var : Gsl_Type)
      return Boolean;

   ---------------------------------------------------------------------------
   --
   function Get_Script_Type
     (Object : Gsl_Script_Reference)
      return Gsl_Script_Type;

   ---------------------------------------------------------------------------
   --
   function Get_Parameter_List
     (Object : Gsl_Script_Reference)
      return Syntax_Node;

   ---------------------------------------------------------------------------
   --
   function Get_Script_Node
     (Object : Gsl_Script_Reference)
      return Syntax_Node;

   ---------------------------------------------------------------------------
   --
   function Get_Activation_Record
     (Object : Gsl_Script_Reference)
      return Activation_Record;

   ---------------------------------------------------------------------------
   --
   procedure Set_Activation_Record
     (Object : Gsl_Script_Reference;
      AR     : Activation_Record);

   ---------------------------------------------------------------------------
   --
   function Get_Gsl_Runtime
     (Object : Gsl_Script_Reference)
      return Runtime_Function;


   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Script_Reference_Record)
      return Gsl_Type;


   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Script_Reference);

   ---------------------------------------------------------------------------
   -- others
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Object_Set
     (Var : Gsl_Type)
      return Boolean;

   ---------------------------------------------------------------------------
   -- private part
   ---------------------------------------------------------------------------

private

   ---------------------------------------------------------------------------
   -- Gsl_Node_Id
   type Gsl_Node_Id_Record is new Gsl_Type_Record with
      record
         Value : Giant.Graph_Lib.Node_Id;
      end record;

   ---------------------------------------------------------------------------
   -- Gsl_Edge_Id
   type Gsl_Edge_Id_Record is new Gsl_Type_Record with
      record
         Value : Giant.Graph_Lib.Edge_Id;
      end record;

   ---------------------------------------------------------------------------
   -- Gsl_Node_Set
   type Gsl_Node_Set_Record is new Gsl_Type_Record with
      record
         Value : Giant.Graph_Lib.Node_Id_Set;
      end record;

   ---------------------------------------------------------------------------
   -- Gsl_Edge_Set
   type Gsl_Edge_Set_Record is new Gsl_Type_Record with
      record
         Value : Giant.Graph_Lib.Edge_Id_Set;
      end record;

   ---------------------------------------------------------------------------
   -- Gsl_String
   type Gsl_String_Record (Size : Natural) is new Gsl_Type_Record with
      record
         Value : String (1 .. Size);
      end record;

   ---------------------------------------------------------------------------
   -- Gsl_Boolean
   type Gsl_Boolean_Record is new Gsl_Type_Record with
      record
         Value : Boolean;
      end record;

   ---------------------------------------------------------------------------
   -- Gsl_Natural
   type Gsl_Natural_Record is new Gsl_Type_Record with
      record
         Value : Natural;
      end record;

   ---------------------------------------------------------------------------
   -- Gsl_List
   type Gsl_List_Record (Size : Natural) is new Gsl_Type_Record with
      record
         List_Size : Natural;
         Value     : Gsl_Type_Array (1 .. Size);
      end record;

   ---------------------------------------------------------------------------
   -- Gsl_Var_Reference
   type Gsl_Var_Reference_Record
     (Context_Size : Natural) is new Gsl_Type_Record with
      record
         Ref_Type : Reference_Type;
         Ref_Name : Gsl_Identifiers.Identifier_Type;
         Context  : String (1 .. Context_Size);
      end record;

   ---------------------------------------------------------------------------
   -- Gsl_Script_Reference
   type Gsl_Script_Reference_Record is new Gsl_Type_Record with
      record
         Script_Type              : Gsl_Script_Type;
         Parameter_List           : Syntax_Node;
         Script_Node              : Syntax_Node;
         Script_Activation_Record : Activation_Record;
         Runtime                  : Runtime_Function;
      end record;

end Giant.Gsl.Types;
