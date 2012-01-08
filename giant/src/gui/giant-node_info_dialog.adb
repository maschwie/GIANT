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
--  $RCSfile: giant-node_info_dialog.adb,v $, $Revision: 1.14 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--

with Interfaces.C.Strings;

with Glib;
with Gtk.Box;
with Gtk.Button;
with Gtk.Table;
with Gtk.Widget;
with Gtk.Enums;
with Gtkada.Types;

with Giant.Gui_Manager;
with Giant.Gui_Manager.Actions;
with Giant.Gui_Utils;
with Giant.Node_Annotation_Dialog;

package body Giant.Node_Info_Dialog is

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   procedure On_Annotate_Button_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : Node_Info_Dialog_Access := Node_Info_Dialog_Access (Source);
   begin
      Node_Annotation_Dialog.Show (Dialog.Node);
   end On_Annotate_Button_Clicked;

   procedure On_Pick_Button_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : Node_Info_Dialog_Access := Node_Info_Dialog_Access (Source);
      Action : Actions.Pick_Node_Action_Access := Actions.Create (Dialog);
   begin
      Gui_Manager.Actions.Set_Global_Action (Action);
      Dialog.Pick_Action := Action;
   end On_Pick_Button_Clicked;

   procedure On_Update_Button_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : Node_Info_Dialog_Access := Node_Info_Dialog_Access (Source);
   begin
      Set_Node (Dialog, Dialog.Node);
   end On_Update_Button_Clicked;

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   procedure Create
     (Dialog :    out Node_Info_Dialog_Access)
   is
   begin
      Dialog := new Node_Info_Dialog_Record;
      Initialize (Dialog);
   end Create;

   procedure Initialize
     (Dialog : access Node_Info_Dialog_Record'class)
   is
      use Giant.Gui_Utils;

      Center_Box : Gtk.Box.Gtk_Vbox;
      Table : Gtk.Table.Gtk_Table;
      Row : Glib.Guint := 0;
   begin
      Default_Dialog.Initialize (Dialog, -"Node Info",
                                 Default_Dialog.Button_Close);
      Set_USize (Dialog, Glib.Gint (-1), 400);

      --  vbox
      Center_Box := Get_Center_Box (Dialog);

      --  node
      Gtk.Table.Gtk_New (Table, Rows => 2, Columns => 2, Homogeneous => False);
      Gtk.Table.Set_Row_Spacings (Table, DEFAULT_SPACING);
      Gtk.Table.Set_Col_Spacings (Table, DEFAULT_SPACING);
      Gtk.Table.Set_Border_Width (Table, DEFAULT_SPACING);
      Gtk.Box.Pack_Start (Center_Box, Add_Frame (Table, -"Node"),
                          expand => False, Fill => False,
                          Padding => DEFAULT_SPACING);

      Dialog.ID_Label := New_Label ("");
      Add_Row_Labels (Table, Row, New_Label (-"ID"), Dialog.ID_Label);

      Dialog.Type_Label := New_Label ("");
      Add_Row_Labels (Table, Row, New_Label (-"Type"), Dialog.Type_Label);

      --  attributes
      Clists.Create (Dialog.Attribute_List, 2);
      Clists.Set_Column_Title (Dialog.Attribute_List, 0, -"Attribute");
      Clists.Set_Column_Title (Dialog.Attribute_List, 1, -"Value");
      Gtk.Box.Pack_Start (Center_Box,
                          Add_Scrollbars (Dialog.Attribute_List),
                          expand => True, Fill => True,
                          Padding => DEFAULT_SPACING);

      --  buttons
      Add_Button (Dialog,
                  New_Button (-"Annotate",
                              On_Annotate_Button_Clicked'Access, Dialog));
      Add_Button (Dialog,
                  New_Button (-"Update",
                              On_Update_Button_Clicked'Access, Dialog));
      Add_Button (Dialog,
                  New_Button (-"Pick",
                              On_Pick_Button_Clicked'Access, Dialog));
   end;

   ---------------------------------------------------------------------------
   --  Other Methods
   ---------------------------------------------------------------------------

   function Can_Hide
     (Dialog : access Node_Info_Dialog_Record)
     return Boolean
   is
      use type Actions.Pick_Node_Action_Access;
   begin
      --  abort a pending pick action to avoid a dangling pointer to
      --  this dialog
      if (Dialog.Pick_Action /= null) then
         Gui_Manager.Actions.Cancel;
      end if;

      return True;
   end Can_Hide;

   procedure Set_Node
     (Dialog : access Node_Info_Dialog_Record;
      Node   : in     Graph_Lib.Node_Id)
   is
      Row : Glib.Gint;
      Row_Data : Gtkada.Types.Chars_Ptr_Array (0 .. 1);
      Iterator : Graph_Lib.Node_Attribute_Iterator;
      Attribute : Graph_Lib.Node_Attribute_Id;
      Width : Glib.Gint;
   begin
      Dialog.Node := Node;

      Gtk.Label.Set_Text (Dialog.ID_Label, Graph_Lib.Node_Id_Image (Node));
      Gtk.Label.Set_Text (Dialog.Type_Label,
                          Graph_Lib.Get_Node_Class_Tag
                          (Graph_Lib.Get_Node_Class_Id (Node)));

      --  set attributes
      Clists.Clear (Dialog.Attribute_List);
      Iterator := Graph_Lib.Make_Attribute_Iterator (Node);
      while (Graph_Lib.More (Iterator)) loop
         Graph_Lib.Next (Iterator, Attribute);
         Row_Data (0) := Interfaces.C.Strings.New_String
           (Graph_Lib.Convert_Node_Attribute_Id_To_Name (Attribute));
         declare
         begin
            Row_Data (1) := Interfaces.C.Strings.New_String
              (Graph_Lib.Get_Node_Attribute_Value_As_String (Node,
                                                             Attribute));
         exception
           when Giant.Graph_Lib.Node_Does_Not_Exist =>
              Row_Data (1) := Interfaces.C.Strings.New_String (-"*Not Found*");
         end;
         Row := Clists.Append (Dialog.Attribute_List, Row_Data);
         Gtkada.Types.Free (Row_Data);
      end loop;

      --  resize columns
      Width := Clists.Columns_Autosize (Dialog.Attribute_List);

      --  bring dialog to front
      Present (Dialog);
   end Set_Node;

   procedure Show
     (Node : in Graph_Lib.Node_Id)
   is
      Dialog: Node_Info_Dialog_Access;
   begin
      Create (Dialog);
      Set_Node (Dialog, Node);
      Show_All (Dialog);
   end Show;

   package body Actions is

      -------------------------------------------------------------------------
      --  Actions
      -------------------------------------------------------------------------

      function Create
        (Dialog : in Node_Info_Dialog_Access)
        return Pick_Node_Action_Access
      is
         Action : Pick_Node_Action_Access;
      begin
         Action := new Pick_Node_Action_Type;
         Action.Dialog := Dialog;
         return Action;
      end Create;

      procedure Cancel
        (Action : access Pick_Node_Action_Type)
      is
      begin
         Action.Dialog.Pick_Action := null;
      end Cancel;

      function Execute
        (Action   : access Pick_Node_Action_Type;
         Window   : access Graph_Window.Graph_Window_Record'Class;
         Event    : in     Graph_Widgets.Handlers.Button_Press_Action)
        return Boolean
      is
         use type Graph_Widgets.Handlers.Pressed_On_Type;
      begin
         if (Event.Pressed_On = Graph_Widgets.Handlers.On_Node) then
            Node_Info_Dialog.Set_Node (Action.Dialog, Event.Node);
            return True;
         end if;

         --  do not cancel action mode until user has selected a node
         return False;
      end Execute;

   end Actions;

end Giant.Node_Info_Dialog;
