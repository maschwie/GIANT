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
--  First Author: Oliver Kopp
--
--  $RCSfile: giant-graph_lib-node_attribute_filters.adb,v $, $Revision: 1.14 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

package body Giant.Graph_Lib.Node_Attribute_Filters is

   ---------------------------------------------------------------------------
   function Create
     (Node_Class                : in     Node_Class_Id;
      Node_Attribute_Names_List : in     String_Lists.List)
      return Filter
   is
      Temp_Filter  : Filter;
      Res          : Filter;
      I            : Natural;
      Iter         : String_Lists.ListIter;
      Current_Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Temp_Filter  := new Filter_Type
        (1..String_Lists.Length (Node_Attribute_Names_List));

      Iter := String_Lists.MakeListIter (Node_Attribute_Names_List);
      I    := 0;

      --  Fill Filter with Node_Attributes
      while String_Lists.More (Iter) loop
         String_Lists.Next (Iter, Current_Name);

         declare
            Current_Name_As_String : String :=
              Ada.Strings.Unbounded.To_String (Current_Name);
         begin
            if Graph_Lib.Does_Node_Attribute_Exist
              (Node_Class          => Node_Class,
               Node_Attribute_Name => Current_Name_As_String) then
               --  Attribute exists in given Class, add it to the filter
               I:=I+1;
               Temp_Filter (I) :=
                 Convert_Node_Attribute_Name_To_Id
                 (Node_Class, Current_Name_As_String);
            end if;
         end;
      end loop;

      --  Convert Temp_Filter to Res
      --     by adjusting length
      Res := new Filter_Type (1..I);

      if Res'Length /= 0 then
         --  "I" could also have been 0, if no given attribute existed in the
         --     given Node_Class
         Res (1..I) := Temp_Filter (1..I);
      end if;

      Free_Filter (Temp_Filter);

      return Res;
   end Create;

   ---------------------------------------------------------------------------
   procedure Destroy
     (Node_Attribute_Filter  : in out Filter)
   is

   begin
      Free_Filter (Node_Attribute_Filter);
   end Destroy;

   ---------------------------------------------------------------------------
   function Make_Filtered_Iter
     (Node_Attribute_Filter : in Filter)
     return Filtered_Iterator
   is
      Res : Filtered_Iterator;
   begin
      Res.Used_Filter      := Node_Attribute_Filter;
      Reset (Res);
      return Res;
   end Make_Filtered_Iter;

   ---------------------------------------------------------------------------
   function More
     (Iter   : in     Filtered_Iterator)
     return Boolean
   is
   begin
      return Iter.Current_Position <= Iter.Used_Filter'Last;
   end More;

   ---------------------------------------------------------------------------
   procedure Next
     (Iter   : in out Filtered_Iterator;
      Attrib :    out Node_Attribute_Id)
   is
   begin
      pragma Assert (More (Iter));
      Attrib := Iter.Used_Filter (Iter.Current_Position);
      Iter.Current_Position := Iter.Current_Position + 1;
   end Next;

   ---------------------------------------------------------------------------
   procedure Reset
     (Iter   : in out Filtered_Iterator)
   is
   begin
      Iter.Current_Position := Iter.Used_Filter'First;
   end Reset;

   ---------------------------------------------------------------------------
   function Size
     (Node_Attribute_Filter  : in Filter)
     return Natural
   is
   begin
      return Node_Attribute_Filter'Length;
   end Size;

end Giant.Graph_Lib.Node_Attribute_Filters;
