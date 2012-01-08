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
--  $RCSfile: giant-layout_dialog.ads,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
------------------------------------------------------------------------------
--
-- Provides a dialog that can open save and executes GSL scripts.
--

with Ada.Strings.Unbounded;

with Giant.Default_Dialog;
with Giant.Vis;

with Gtk.Notebook;
with Gtk.Widget;

package Giant.Layout_Dialog is

   ---------------------------------------------------------------------------
   --  Layout Container
   ---------------------------------------------------------------------------

   type Layout_Container_Record is abstract tagged private;

   type Layout_Container is access all Layout_Container_Record'Class;

   function Get_Widget
     (Container : access Layout_Container_Record)
     return Gtk.Widget.Gtk_Widget is abstract;

   function Get_Display_Name
     (Container : access Layout_Container_Record)
     return String is abstract;

   function Get_Layout_Name
     (Container : access Layout_Container_Record)
     return String is abstract;

   function Get_Layout_Parameters
     (Container : access Layout_Container_Record)
      return String is abstract;

   ---------------------------------------------------------------------------
   --  Layout Dialog
   ---------------------------------------------------------------------------

   type Layout_Dialog_Record (Container_Count : Integer) is
     new Default_Dialog.Default_Dialog_Record with private;

   type Layout_Dialog_Access is
      access all Layout_Dialog_Record'Class;

   procedure Create
     (Dialog         :    out Layout_Dialog_Access;
      Window_Name    : in     String;
      Selection_Name : in     String);

   procedure Initialize
     (Dialog         : access Layout_Dialog_Record'Class;
      Window_Name    : in     String;
      Selection_Name : in     String);

   procedure Apply_Layout
     (Dialog   : access Layout_Dialog_Record;
      Position : in     Vis.Logic.Vector_2d);

   function Can_Hide
     (Dialog : access Layout_Dialog_Record)
     return Boolean;

   procedure Show
     (Window_Name    : in String;
      Selection_Name : in String;
      Position       : in Vis.Logic.Vector_2d := Vis.Logic.Zero_2d);

private
   type Layout_Container_Record is abstract tagged null record;

   type Layout_Container_Array is array (Integer range <>)
     of Layout_Container;

   type Layout_Dialog_Record (Container_Count : Integer) is
     new Default_Dialog.Default_Dialog_Record
     with record
        Layouts_Notebook : Gtk.Notebook.Gtk_Notebook;
        Position : Vis.Logic.Vector_2d := Vis.Logic.Zero_2d;
        Selection_Name : Ada.Strings.Unbounded.Unbounded_String;
        Window_Name : Ada.Strings.Unbounded.Unbounded_String;
        Layouts : Layout_Container_Array (0 .. Container_Count);
        Layouts_Count : Natural := 0;
     end record;

end Giant.Layout_Dialog;
