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
--  $RCSfile: giant-data_clists.ads,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
--  Provides an convenince Gtk.Clist that has a single row data type
--  associated.
--
--  Pattern:
--    ADT
--

with Glib;
with Gtk.Clist;

with Giant.Clists;

generic

   type Data_Type (<>) is private;

package Giant.Data_Clists is

   type Giant_Data_Clist_Record is new Clists.Giant_Clist_Record with private;

   type Giant_Data_Clist is access all Giant_Data_Clist_Record'Class;

   type Update_Procedure_Type is access procedure
     (List : access Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Item : in     Data_Type);

   package Data is new Gtk.Clist.Row_Data (Data_Type);

   procedure Create
     (List             :    out Giant_Data_Clist;
      Columns          : in     Glib.Gint;
      Update_Procedure : in     Update_Procedure_Type);

   procedure Initialize
     (List             : access Giant_Data_Clist_Record'Class;
      Columns          : in     Glib.Gint;
      Update_Procedure : in     Update_Procedure_Type);

   procedure Add
     (List : access Giant_Data_Clist_Record;
      Item : in     Data_Type);

   function Get_Row
     (List : access Giant_Data_Clist_Record;
      Item : in Data_Type)
      return Glib.Gint;

   function Get_Selected_Item
     (List : access Giant_Data_Clist_Record)
      return Data_Type;

   procedure Update
     (List : access Giant_Data_Clist_Record;
      Item : in Data_Type);

   procedure Remove
     (List : access Giant_Data_Clist_Record;
      Item : in Data_Type);

private

   type Giant_Data_Clist_Record is new Clists.Giant_Clist_Record with record
      Update_Procedure : Update_Procedure_Type;
   end record;

end Giant.Data_Clists;
