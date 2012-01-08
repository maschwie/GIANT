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
-- $RCSfile: giant-gsl-types.adb,v $, $Revision: 1.20 $
-- $Author: schulzgt $
-- $Date: 2003-11-12 21:20:55 $
--
with Ada.Unchecked_Deallocation;
with Ada.Tags;
use  Ada.Tags;

package body Giant.Gsl.Types is

   ---------------------------------------------------------------------------
   --
   procedure Destroy_Gsl_Type
     (Var : in out Gsl_Type) is
   begin
      if Var = Gsl_Null then
        null;

      elsif Var'Tag = Gsl_Node_Id_Record'Tag then
         Destroy (Gsl_Node_Id (Var));

      elsif Var'Tag = Gsl_Edge_Id_Record'Tag then
         Destroy (Gsl_Edge_Id (Var));

      elsif Var'Tag = Gsl_Node_Set_Record'Tag then
         Destroy (Gsl_Node_Set (Var));

      elsif Var'Tag = Gsl_Edge_Set_Record'Tag then
         Destroy (Gsl_Edge_Set (Var));

      elsif Var'Tag = Gsl_String_Record'Tag then
         Destroy (Gsl_String (Var));

      elsif Var'Tag = Gsl_Boolean_Record'Tag then
         Destroy (Gsl_Boolean (Var));

      elsif Var'Tag = Gsl_Natural_Record'Tag then
         Destroy (Gsl_Natural (Var));

      elsif Var'Tag = Gsl_List_Record'Tag then
         Destroy (Gsl_List (Var));

      elsif Var'Tag = Gsl_Var_Reference_Record'Tag then
         Destroy (Gsl_Var_Reference (Var));

      elsif Var'Tag = Gsl_Script_Reference_Record'Tag then
         Destroy (Gsl_Script_Reference (Var));

      end if;
   end Destroy_Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Copy_Gsl_Type
     (Object : Gsl_Type)
      return Gsl_Type is
   begin
      if Object /= Gsl_Null then
         return Copy (Object);
      else
         return Gsl_Null;
      end if;
   end Copy_Gsl_Type;

------------------------------------------------------------------------------
-- Gsl_Node_Id

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Node_Id return Gsl_Node_Id is

      Var : Gsl_Node_Id;
   begin
      Var := new Gsl_Node_Id_Record;
      return Var;
   end Create_Gsl_Node_Id;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Node_Id
     (Value : Giant.Graph_Lib.Node_Id)
      return Gsl_Node_Id is

      Var : Gsl_Node_Id;
   begin
      Var := new Gsl_Node_Id_Record;
      Var.Value := Value;
      return Var;
   end Create_Gsl_Node_Id;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Node_Id
     (Var : Gsl_Type)
      return Boolean is
   begin
      if Var = Gsl_Null then
         return False;
      elsif Var'Tag = Gsl_Node_Id_Record'Tag then
         return True;
      else
         return False;
      end if;
   end Is_Gsl_Node_Id;

   ---------------------------------------------------------------------------
   --
   function Get_Value
     (Var : Gsl_Node_Id)
      return Giant.Graph_Lib.Node_Id is
   begin
      return Var.Value;
   end Get_Value;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value
     (Var   : Gsl_Node_Id;
      Value : Giant.Graph_Lib.Node_Id) is
   begin
      Var.Value := Value;
   end Set_Value;

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Node_Id_Record)
      return Gsl_Type is

      Var : Gsl_Node_Id;
   begin
      Var := new Gsl_Node_Id_Record;
      Var.all := Object.all;
      return Gsl_Type (Var);
   end Copy;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Node_Id) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Node_Id_Record, Gsl_Node_Id);

   begin
      Free (Object);
   end Destroy;

------------------------------------------------------------------------------
-- Gsl_Edge_Id

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Edge_Id return Gsl_Edge_Id is

      Var : Gsl_Edge_Id;
   begin
      Var := new Gsl_Edge_Id_Record;
      return Var;
   end Create_Gsl_Edge_Id;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Edge_Id
     (Value : Giant.Graph_Lib.Edge_Id)
      return Gsl_Edge_Id is

      Var : Gsl_Edge_Id;
   begin
      Var := new Gsl_Edge_Id_Record;
      Var.Value := Value;
      return Var;
   end Create_Gsl_Edge_Id;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Edge_Id
     (Var : Gsl_Type)
      return Boolean is
   begin
      if Var = Gsl_Null then
         return False;
      elsif Var'Tag = Gsl_Edge_Id_Record'Tag then
         return True;
      else
         return False;
      end if;
   end Is_Gsl_Edge_Id;

   ---------------------------------------------------------------------------
   --
   function Get_Value
     (Var : Gsl_Edge_Id)
      return Giant.Graph_Lib.Edge_Id is
   begin
      return Var.Value;
   end Get_Value;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value
     (Var   : Gsl_Edge_Id;
      Value : Giant.Graph_Lib.Edge_Id) is
   begin
      Var.Value := Value;
   end Set_Value;

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Edge_Id_Record)
      return Gsl_Type is

      Var : Gsl_Edge_Id;
   begin
      Var := new Gsl_Edge_Id_Record;
      Var.all := Object.all;
      return Gsl_Type (Var);
   end Copy;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Edge_Id) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Edge_Id_Record, Gsl_Edge_Id);

   begin
      Free (Object);
   end Destroy;

------------------------------------------------------------------------------
-- Gsl_Node_Set

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Node_Set
     (Value : Giant.Graph_Lib.Node_Id_Set)
      return Gsl_Node_Set is

      Var : Gsl_Node_Set;
   begin
      Var := new Gsl_Node_Set_Record;
      Var.Value := Value;
      return Var;
   end Create_Gsl_Node_Set;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Node_Set
     (Var : Gsl_Type)
      return Boolean is
   begin
      if Var = Gsl_Null then
         return False;
      elsif Var'Tag = Gsl_Node_Set_Record'Tag then
         return True;
      else
         return False;
      end if;
   end Is_Gsl_Node_Set;

   ---------------------------------------------------------------------------
   --
   function Get_Value
     (Var : Gsl_Node_Set)
      return Giant.Graph_Lib.Node_Id_Set is
   begin
      return Var.Value;
   end Get_Value;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value
     (Var   : Gsl_Node_Set;
      Value : Giant.Graph_Lib.Node_Id_Set) is
   begin
      Var.Value := Value;
   end Set_Value;

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Node_Set_Record)
      return Gsl_Type is

      Var : Gsl_Node_Set;
   begin
      Var := new Gsl_Node_Set_Record;
      Var.all := Object.all;
      Var.Value :=  Giant.Graph_Lib.Node_Id_Sets.Copy (Object.Value);
      return Gsl_Type (Var);
   end Copy;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Node_Set) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Node_Set_Record, Gsl_Node_Set);

   begin
      Giant.Graph_Lib.Node_Id_Sets.Destroy (Object.Value);
      Free (Object);
   end Destroy;

------------------------------------------------------------------------------
-- Gsl_Edge_Set

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Edge_Set
     (Value : Giant.Graph_Lib.Edge_Id_Set)
      return Gsl_Edge_Set is

      Var : Gsl_Edge_Set;
   begin
      Var := new Gsl_Edge_Set_Record;
      Var.Value := Value;
      return Var;
   end Create_Gsl_Edge_Set;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Edge_Set
     (Var : Gsl_Type)
      return Boolean is
   begin
      if Var = Gsl_Null then
         return False;
      elsif Var'Tag = Gsl_Edge_Set_Record'Tag then
         return True;
      else
         return False;
      end if;
   end Is_Gsl_Edge_Set;

   ---------------------------------------------------------------------------
   --
   function Get_Value
     (Var : Gsl_Edge_Set)
      return Giant.Graph_Lib.Edge_Id_Set is
   begin
      return Var.Value;
   end Get_Value;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value
     (Var   : Gsl_Edge_Set;
      Value : Giant.Graph_Lib.Edge_Id_Set) is
   begin
      Var.Value := Value;
   end Set_Value;

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Edge_Set_Record)
      return Gsl_Type is

      Var : Gsl_Edge_Set;
   begin
      Var := new Gsl_Edge_Set_Record;
      Var.all := Object.all;
      Var.Value :=  Giant.Graph_Lib.Edge_Id_Sets.Copy (Object.Value);
      return Gsl_Type (Var);
   end Copy;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Edge_Set) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Edge_Set_Record, Gsl_Edge_Set);

   begin
      Giant.Graph_Lib.Edge_Id_Sets.Destroy (Object.Value);
      Free (Object);
   end Destroy;

---------------------------------------------------------------------------
-- Gsl_String

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_String
     (Value : String)
      return Gsl_String is

      Var : Gsl_String;
   begin
      Var := new Gsl_String_Record (Value'Length);
      Var.Value := Value;
      return Var;
   end;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_String
     (Var : Gsl_Type)
      return Boolean is
   begin
      if Var = Gsl_Null then
         return False;
      elsif Var'Tag = Gsl_String_Record'Tag then
         return True;
      else
         return False;
      end if;
   end Is_Gsl_String;

   ---------------------------------------------------------------------------
   --
   function Get_Value
     (Var : Gsl_String)
      return String is
   begin
      return Var.Value;
   end Get_Value;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value
     (Var   : Gsl_String;
      Value : String) is
   begin
      Var.Value := Value;
   end Set_Value;

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_String_Record)
      return Gsl_Type is

      Var : Gsl_String;
   begin
      Var := new Gsl_String_Record (Object.Value'Length);
      Var.all := Object.all;
      return Gsl_Type (Var);
   end Copy;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_String) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_String_Record, Gsl_String);

   begin
      Free (Object);
   end Destroy;

------------------------------------------------------------------------------
-- Gsl_Boolean

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Boolean (Value : Boolean) return Gsl_Boolean is

      Var : Gsl_Boolean;
   begin
      Var := new Gsl_Boolean_Record;
      Var.Value := Value;
      return Var;
   end Create_Gsl_Boolean;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Boolean
     (Var : Gsl_Type)
      return Boolean is
   begin
      if Var = Gsl_Null then
         return False;
      elsif Var'Tag = Gsl_Boolean_Record'Tag then
         return True;
      else
         return False;
      end if;
   end Is_Gsl_Boolean;

   ---------------------------------------------------------------------------
   --
   function Get_Value
     (Var : Gsl_Boolean)
      return Boolean is
   begin
      return Var.Value;
   end Get_Value;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value
     (Var   : Gsl_Boolean;
      Value : Boolean) is
   begin
      Var.Value := Value;
   end Set_Value;

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Boolean_Record)
      return Gsl_Type is

      Var : Gsl_Boolean;
   begin
      Var := new Gsl_Boolean_Record;
      Var.all := Object.all;
      return Gsl_Type (Var);
   end;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Boolean) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Boolean_Record, Gsl_Boolean);

   begin
      Free (Object);
   end Destroy;

------------------------------------------------------------------------------
-- Gsl_Natural

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Natural return Gsl_Natural is
   begin
      return Create_Gsl_Natural (0);
   end Create_Gsl_Natural;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Natural
     (Value : Natural)
      return Gsl_Natural is

      Var : Gsl_Natural;
   begin
      Var := new Gsl_Natural_Record;
      Var.Value := Value;
      return Var;
   end Create_Gsl_Natural;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Natural
     (Var : Gsl_Type)
      return Boolean is
   begin
      if Var = Gsl_Null then
         return False;
      elsif Var'Tag = Gsl_Natural_Record'Tag then
         return True;
      else
         return False;
      end if;
   end Is_Gsl_Natural;

   ---------------------------------------------------------------------------
   --
   function Get_Value
     (Var : Gsl_Natural)
      return Natural is
   begin
      return Var.Value;
   end Get_Value;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value
     (Var   : Gsl_Natural;
      Value : Natural) is
   begin
      Var.Value := Value;
   end Set_Value;

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Natural_Record)
      return Gsl_Type is

      Var : Gsl_Natural;
   begin
      Var := new Gsl_Natural_Record;
      Var.all := Object.all;
      return Gsl_Type (Var);
   end Copy;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Natural) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Natural_Record, Gsl_Natural);

    begin
      Free (Object);
   end Destroy;

------------------------------------------------------------------------------
-- Gsl_List

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_List
     (Size : Natural)
      return Gsl_List is

      Var : Gsl_List;
   begin
      Var := new Gsl_List_Record (Size);
      Var.List_Size := Size;
      return Var;
   end Create_Gsl_List;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_List
     (Var : Gsl_Type)
      return Boolean is
   begin
      if Var = Gsl_Null then
         return False;
      elsif Var'Tag = Gsl_List_Record'Tag then
         return True;
      else
         return False;
      end if;
   end Is_Gsl_List;

   ---------------------------------------------------------------------------
   --
   function Get_List_Size
     (Var      : Gsl_List)
      return Natural is
   begin
      return Var.List_Size;
   end Get_List_Size;

   ---------------------------------------------------------------------------
   --
   function Get_Value_At
     (Var      : Gsl_List;
      Position : Natural)
      return Gsl_Type is
   begin
      return Var.Value (Position);
   end Get_Value_At;

   ---------------------------------------------------------------------------
   --
   procedure Set_Value_At
     (Var      : Gsl_List;
      Position : Natural;
      Value    : Gsl_Type) is
   begin
      Var.Value (Position) := Value;
   end Set_Value_At;

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_List_Record)
      return Gsl_Type is

      Var : Gsl_List;
   begin
      Var := new Gsl_List_Record (Object.Size);
      Var.all := Object.all;
      -- copy all objects
      for i in 1 .. Object.List_Size loop
         Var.Value (i) := Copy_Gsl_Type (Object.Value (i));
      end loop;
      return Gsl_Type (Var);
   end Copy;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_List) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_List_Record, Gsl_List);

   begin
      for i in 1 .. Object.List_Size loop
         Destroy_Gsl_Type (Object.Value (i));
      end loop;
      Free (Object);
   end Destroy;

------------------------------------------------------------------------------
-- Gsl_Var_Reference

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Var_Reference
     (Ref_Type : in Reference_Type;
      Ref_Name : in Gsl_Identifiers.Identifier_Type;
      Context  : in String := "")
      return Gsl_Var_Reference is

      Var : Gsl_Var_Reference;
   begin
      Var := new Gsl_Var_Reference_Record (Context'Length);
      Var.Ref_Type := Ref_Type;
      Var.Ref_Name := Ref_Name;
      Var.Context  := Context;
      return Var;
   end;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Var_Reference
     (Var : Gsl_Type)
      return Boolean is
   begin
      if Var = Gsl_Null then
         return False;
      elsif Var'Tag = Gsl_Var_Reference_Record'Tag then
         return Get_Ref_Type (Gsl_Var_Reference (Var)) = Gsl.Types.Var;
      else
         return False;
      end if;
   end Is_Gsl_Var_Reference;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Global_Reference
     (Var : Gsl_Type)
      return Boolean is
   begin
      if Var = Gsl_Null then
         return False;
      elsif Var'Tag = Gsl_Var_Reference_Record'Tag then
         return Get_Ref_Type (Gsl_Var_Reference (Var)) /= Gsl.Types.Var;
      else
         return False;
      end if;
   end Is_Gsl_Global_Reference;

   ---------------------------------------------------------------------------
   --
   function Get_Ref_Type
     (Var : Gsl_Var_Reference)
      return Reference_Type is
   begin
      return Var.Ref_Type;
   end;

   ---------------------------------------------------------------------------
   --
   function Get_Ref_Name
     (Var : Gsl_Var_Reference)
      return Gsl_Identifiers.Identifier_Type is
   begin
      return Var.Ref_Name;
   end;

   ---------------------------------------------------------------------------
   --
   function Get_Ref_Context
     (Var : Gsl_Var_Reference)
      return String is
   begin
      return Var.Context;
   end;

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Var_Reference_Record)
      return Gsl_Type is

      Var : Gsl_Var_Reference;
   begin
      Var := new Gsl_Var_Reference_Record (Object.Context'Length);
      Var.all := Object.all;
      return Gsl_Type (Var);
   end Copy;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Var_Reference) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Var_Reference_Record, Gsl_Var_Reference);

   begin
      Free (Object);
   end Destroy;

------------------------------------------------------------------------------
-- Gsl_Script_Reference

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Script_Reference
     (Parameter_List : Syntax_Node;
      Script_Node    : Syntax_Node)
      return Gsl_Script_Reference is

      Var : Gsl_Script_Reference;
   begin
      Var := new Gsl_Script_Reference_Record;
      Var.Script_Type := Gsl_Script;
      Var.Parameter_List := Parameter_List;
      Var.Script_Node := Script_Node;
      Var.Script_Activation_Record := null;
      return Var;
   end;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Script_Reference
     (Runtime : Runtime_Function)
      return Gsl_Script_Reference is

      Var : Gsl_Script_Reference;
   begin
      Var := new Gsl_Script_Reference_Record;
      Var.Script_Type := Gsl_Runtime;
      Var.Runtime := Runtime;
      return Var;
   end;

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Script_Reference
     (Var : Gsl_Type)
      return Boolean is
   begin
      if Var = Gsl_Null then
         return False;
      elsif Var'Tag = Gsl_Script_Reference_Record'Tag then
         return True;
      else
         return False;
      end if;
   end Is_Gsl_Script_Reference;

   ---------------------------------------------------------------------------
   --
   function Get_Script_Type
     (Object : Gsl_Script_Reference)
      return Gsl_Script_Type is
   begin
      return Object.Script_Type;
   end Get_Script_Type;

   ---------------------------------------------------------------------------
   --
   function Get_Parameter_List
     (Object : Gsl_Script_Reference)
      return Syntax_Node is
   begin
      return Object.Parameter_List;
   end Get_Parameter_List;

   ---------------------------------------------------------------------------
   --
   function Get_Script_Node
     (Object : Gsl_Script_Reference)
      return Syntax_Node is
   begin
      return Object.Script_Node;
   end Get_Script_Node;

   ---------------------------------------------------------------------------
   --
   function Get_Activation_Record
     (Object : Gsl_Script_Reference)
      return Activation_Record is
   begin
      return Object.Script_Activation_Record;
   end Get_Activation_Record;

   ---------------------------------------------------------------------------
   --
   procedure Set_Activation_Record
     (Object : Gsl_Script_Reference;
      AR     : Activation_Record) is
   begin
      Object.Script_Activation_Record := AR;
   end Set_Activation_Record;

   ---------------------------------------------------------------------------
   --
   function Get_Gsl_Runtime
     (Object : Gsl_Script_Reference)
      return Runtime_Function is
   begin
      return Object.Runtime;
   end Get_Gsl_Runtime;

   ---------------------------------------------------------------------------
   --
   function Copy
     (Object : access Gsl_Script_Reference_Record)
      return Gsl_Type is

      Var : Gsl_Script_Reference;
   begin
      Var := new Gsl_Script_Reference_Record;
      Var.all := Object.all;
      return Gsl_Type (Var);
   end Copy;

   ---------------------------------------------------------------------------
   --
   procedure Destroy
     (Object : in out Gsl_Script_Reference) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Script_Reference_Record, Gsl_Script_Reference);

   begin
      Free (Object);
   end Destroy;

------------------------------------------------------------------------------
--

   ---------------------------------------------------------------------------
   --
   function Is_Gsl_Object_Set
     (Var : Gsl_Type)
      return Boolean is
   begin
      if Var = Gsl_Null then
         return False;
      elsif Is_Gsl_List (Var) then
         if Get_List_Size (Gsl_List (Var)) = 2 then
            if Is_Gsl_Node_Set (Get_Value_At (Gsl_List (Var), 1)) and
               Is_Gsl_Edge_Set (Get_Value_At (Gsl_List (Var), 2)) then
               return True;
            else
               return False;
            end if;
         else
            return False;
         end if;
      else
         return False;
      end if;
   end Is_Gsl_Object_Set;

end Giant.Gsl.Types;
