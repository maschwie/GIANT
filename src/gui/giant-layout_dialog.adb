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
--  $RCSfile: giant-layout_dialog.adb,v $, $Revision: 1.8 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--

with Ada.Exceptions;
with Ada.Strings.Unbounded;

with Glib;
with Gtk.Box;
with Gtk.Main;
with Gtk.Table;
with Gtk.Window;

with Giant.Controller;
with Giant.Gui_Manager;
with Giant.Gui_Manager.Actions;
with Giant.Gui_Utils;
with Giant.Layout_Dialog.Actions;
with Giant.Layout_Dialog.Widgets;
with Giant.Layout_Factory;

package body Giant.Layout_Dialog is

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   procedure Create
     (Dialog         :    out Layout_Dialog_Access;
      Window_Name    : in     String;
      Selection_Name : in     String)
   is
   begin
      Dialog := new Layout_Dialog_Record (2);
      Initialize (Dialog, Window_Name, Selection_Name);
   end Create;

   procedure Add
     (Dialog    : access Layout_Dialog_Record'Class;
      Container : in     Layout_Container)
   is
      use Giant.Gui_Utils;
   begin
      Dialog.Layouts (Dialog.Layouts_Count) := Container;
      Dialog.Layouts_Count := Dialog.Layouts_Count + 1;

      Gtk.Notebook.Append_Page_Menu (Dialog.Layouts_Notebook,
                                     Get_Widget (Container),
                                     New_Label (Get_Display_Name
                                                (Container)),
                                     New_Label (Get_Display_Name (Container)));
   end Add;

   procedure Initialize
     (Dialog         : access Layout_Dialog_Record'Class;
      Window_Name    : in     String;
      Selection_Name : in     String)
   is
      use Giant.Gui_Utils;

      Center_Box : Gtk.Box.Gtk_Vbox;
      Table : Gtk.Table.Gtk_Table;
      Row : Glib.Guint := 0;
   begin
      Default_Dialog.Initialize (Dialog, -"Apply Layout",
                                 Default_Dialog.Button_Okay_Cancel);

      Dialog.Window_Name
        := Ada.Strings.Unbounded.To_Unbounded_String (Window_Name);
      Dialog.Selection_Name
        := Ada.Strings.Unbounded.To_Unbounded_String (Selection_Name);

      --  vbox
      Center_Box := Get_Center_Box (Dialog);

      --  node
      Gtk.Table.Gtk_New (Table, Rows => 2, Columns => 2, Homogeneous => False);
      Gtk.Table.Set_Row_Spacings (Table, DEFAULT_SPACING);
      Gtk.Table.Set_Col_Spacings (Table, DEFAULT_SPACING);
      Gtk.Table.Set_Border_Width (Table, DEFAULT_SPACING);
      Gtk.Box.Pack_Start (Center_Box, Add_Frame (Table, ""),
                          expand => False, Fill => False,
                          Padding => DEFAULT_SPACING);

      Add_Row_Labels (Table, Row, New_Label (-"Window"),
                      New_Label (Window_Name));
      Add_Row_Labels (Table, Row, New_Label (-"Selection"),
                      New_Label (Selection_Name));

      --  notebook
      Gtk.Notebook.Gtk_New (Dialog.Layouts_Notebook);
      Gtk.Notebook.Popup_Enable (Dialog.Layouts_Notebook);
      Gtk.Box.Pack_Start (Center_Box, Dialog.Layouts_Notebook,
                          Expand => True, Fill => True,
                          Padding => DEFAULT_SPACING);

      --  layout container
      Add (Dialog, Layout_Container (Widgets.Create_Matrix));
      Add (Dialog, Layout_Container (Widgets.Create_Tree));
      Add (Dialog, Layout_Container (Widgets.Create_Other));
   end;

   ---------------------------------------------------------------------------
   --  Other Methods
   ---------------------------------------------------------------------------

   function Can_Hide
     (Dialog : access Layout_Dialog_Record)
     return Boolean
   is
      use type Default_Dialog.Response_Type;
--        use type Glib.Gint;

      Window_Name : constant String
        := Ada.Strings.Unbounded.To_String (Dialog.Window_Name);
   begin
      if (Is_Modal (Dialog)) then
         Gtk.Main.Main_Quit;
      end if;

      if (Get_Response (Dialog) = Default_Dialog.Response_Okay) then

         -- XXX: should verify parameters now, otherwise error messages
         -- are shown after user has selected position

         Gui_Manager.Actions.Set_Local_Action
           (Window_Name,
            Layout_Dialog.Actions.Create (Layout_Dialog_Access (Dialog)));

         --  do not destory dialog, yet, the action will take care of
         --  the disposal
         Gtk.Window.Hide_All (Gtk.Window.Gtk_Window (Dialog));
      else
         Destroy (Dialog);
      end if;

      return False;
   end Can_Hide;

   procedure Apply_Layout
     (Dialog   : access Layout_Dialog_Record;
      Position : in     Vis.Logic.Vector_2d)
   is
      Container : Layout_Container
        := Dialog.Layouts (Integer (Gtk.Notebook.Get_Current_Page
                                    (Dialog.Layouts_Notebook)));
      Window_Name : constant String
        := Ada.Strings.Unbounded.To_String (Dialog.Window_Name);
      Selection_Name : constant String
        := Ada.Strings.Unbounded.To_String (Dialog.Selection_Name);
   begin
      Controller.Apply_Layout (Get_Layout_Name (Container),
                               Window_Name,
                               Selection_Name,
                               Position,
                               Get_Layout_Parameters (Container));
      Destroy (Dialog);
   exception
     when E: Layout_Factory.Invalid_Format =>
        Controller.Show_Error (-"Invalid layout parameter"
                               & " (" & Ada.Exceptions.Exception_Message (E)
                               & ").");
        Show_All (Dialog);
     when Layout_Factory.Unknown_Algorithm =>
        Controller.Show_Error (-"Unknown layout algorithm.");
        Show_All (Dialog);
   end Apply_Layout;

   procedure Show
     (Window_Name    : in String;
      Selection_Name : in String;
      Position       : in Vis.Logic.Vector_2d := Vis.Logic.Zero_2d)
   is
      Dialog : Layout_Dialog_Access;
   begin
      Create (Dialog, Window_Name, Selection_Name);
      Dialog.Position := Position;
      Show_All (Dialog);
   end Show;

end Giant.Layout_Dialog;
