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
--  $RCSfile: giant-set_operation_dialog.ads,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
--
--

with Gtk.Combo;
with Gtk.Enums;
with Gtk.Gentry;

with Giant.Default_Dialog;

package Giant.Set_Operation_Dialog is

   type Set_Operation_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with private;

   type Set_Operation_Dialog_Access is
     access all Set_Operation_Dialog_Record'Class;

   type Operation_Type is (Difference, Intersection, Union);

   Invalid_Operation_Entered : exception;

   procedure Create
     (Dialog :    out Set_Operation_Dialog_Access;
      Items  : in     Gtk.Enums.String_List.Glist);

   procedure Initialize
     (Dialog : access Set_Operation_Dialog_Record'Class;
      Items  : in     Gtk.Enums.String_List.Glist);

   function Get_Left_Source
     (Dialog : access Set_Operation_Dialog_Record)
     return String;

   function Get_Right_Source
     (Dialog : access Set_Operation_Dialog_Record)
     return String;

   function Get_Operation
     (Dialog : access Set_Operation_Dialog_Record)
     return Operation_Type;

   function Get_Target
     (Dialog : access Set_Operation_Dialog_Record)
     return String;

   function Validate
     (Dialog : access Set_Operation_Dialog_Record)
     return Boolean;

private

   type Set_Operation_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        Left_Source : Gtk.Combo.Gtk_Combo;
        Right_Source : Gtk.Combo.Gtk_Combo;
        Operation : Gtk.Combo.Gtk_Combo;
        Target : Gtk.Gentry.Gtk_Entry;
     end record;

end Giant.Set_Operation_Dialog;
