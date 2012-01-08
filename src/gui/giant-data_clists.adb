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
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-data_clists.adb,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--

with Ada.Strings.Unbounded;
with Interfaces.C.Strings;

with Glib; use type Glib.Gint;
with Gtk.Enums; 
with Gtkada.Types;

package body Giant.Data_Clists is

   procedure Create
	 (List			   :    out Giant_Data_Clist;
	  Columns		   : in     Glib.Gint;
	  Update_Procedure : in     Update_Procedure_Type)
   is
   begin
	  List := new Giant_Data_Clist_Record;
	  Initialize (List, Columns, Update_Procedure);
   end Create;

   procedure Initialize
	 (List			   : access Giant_Data_Clist_Record'Class;
	  Columns		   : in     Glib.Gint;
	  Update_Procedure : in     Update_Procedure_Type)
   is
   begin
	  Clists.Initialize (List, Columns);

	  List.Update_Procedure := Update_Procedure;
   end;

   procedure Add
	 (List : access Giant_Data_Clist_Record;
	  Item : in     Data_Type)
   is
      use Gtkada.Types;
 
      Row_Data : Gtkada.Types.Chars_Ptr_Array
		(0 .. Interfaces.C.Size_t (Get_Columns (List)));
      Row : Glib.Gint;
   begin
	  --  append row with dummy data
      for I in Row_Data'Range loop
         Row_Data (I) := Interfaces.C.Strings.New_String ("");
	  end loop;
      Row := Append (List, Row_Data);
      Free (Row_Data);

	  --  set custom data
      Data.Set (List, Row, Item);

	  --  update row
      List.Update_Procedure (List, Row, Item);
   end Add;

   function Get_Row
	 (List : access Giant_Data_Clist_Record;
	  Item : in Data_Type)
	  return Glib.Gint
   is
   begin
	  for I in 0 .. Get_Rows (List) - 1 loop
		 if (Data.Get (List, I) = Item) then
			return I;
		 end if;
	  end loop;
	  
	  return -1;
   end Get_Row;

   function Get_Selected_Item
	 (List : access Giant_Data_Clist_Record)
	  return Data_Type
   is
   begin
	  return Data.Get (List, Get_Selected_Row (List));
   end Get_Selected_Item;

   procedure Update
	 (List : access Giant_Data_Clist_Record;
	  Item : in Data_Type)
   is
      Row : Glib.Gint := Get_Row (List, Item);
   begin
      if (Row /= -1) then
         List.Update_Procedure (List, Row, Item);
      end if;
   end Update;

   procedure Remove
	 (List : access Giant_Data_Clist_Record;
	  Item : in Data_Type)
   is
      Row : Glib.Gint := Get_Row (List, Item);
   begin
      if (Row /= -1) then
		 Remove (List, Row);
      end if;
   end Remove;

end Giant.Data_Clists;
