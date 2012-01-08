-----------------------------------------------------------------------------
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License version 2 as
--  published by the Free Software Foundation.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
--  As a special exception, if you link this unit with the Bauhaus toolkit
--  to produce an executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
--  First Author: Steffen Keul
--
--  $RCSfile: giant-gsl_identifiers.adb,v $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
------------------------------------------------------------------------------


with Ada.Strings.Unbounded;

with Hashed_Mappings;
pragma Elaborate_All (Hashed_Mappings);
with Unbounded_String_Hash;
pragma Elaborate (Unbounded_String_Hash);

package body Giant.Gsl_Identifiers is

   package Strings renames Ada.Strings.Unbounded;

   package Name_Mappings is new Hashed_Mappings
     (Key_Type   => Strings.Unbounded_String,
      Equal      => Strings."=",
      Hash       => Unbounded_String_Hash,
      Value_Type => Identifier_Type);

   package Identifier_Mappings is new Hashed_Mappings
     (Key_Type   => Identifier_Type,
      Equal      => "=",
      Hash       => Hash,
      Value_Type => Strings.Unbounded_String);


   ----------------------
   -- Global variables --
   ----------------------

   Next_Identifier : Identifier_Type;

   Name_Map        : Name_Mappings.Mapping;
   Identifier_Map  : Identifier_Mappings.Mapping;


   -----------------
   -- Subprograms --
   -----------------

   function Get_Identifier
     (Name : in     String)
     return Identifier_Type
   is
      Value : Identifier_Type;
      Key   : Strings.Unbounded_String := Strings.To_Unbounded_String (Name);
   begin
      begin
         Value := Name_Mappings.Fetch (Name_Map, Key);
      exception
         when Name_Mappings.Not_Bound =>
            Value := Next_Identifier;
            Name_Mappings.Bind (Name_Map, Key, Value);
            Identifier_Mappings.Bind (Identifier_Map, Value, Key);
            Next_Identifier := Next_Identifier + 1;
      end;
      return Value;
   end Get_Identifier;

   function Get_Name
     (Identifier : in     Identifier_Type)
     return String is
   begin
      return Strings.To_String
        (Identifier_Mappings.Fetch (Identifier_Map, Identifier));
   end Get_Name;

   function Hash
     (Identifier : in     Identifier_Type)
     return Integer is
   begin
      return Integer (Identifier);
   end Hash;


   --------------------
   -- Initialization --
   --------------------

begin
   Next_Identifier := Identifier_Type'First;
   Name_Map := Name_Mappings.Create;
   Identifier_Map := Identifier_Mappings.Create;
end Giant.Gsl_Identifiers;
