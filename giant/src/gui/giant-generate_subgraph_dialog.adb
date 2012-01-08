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
--  $RCSfile: giant-generate_subgraph_dialog.adb,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--

with Ada.Strings.Unbounded;

with Glib;
with Gtk;
with Gtk.Enums;
with Gtk.Box;
with Gtk.Button;
with Gtk.Stock;
with Gtk.Widget;

with String_Lists;

with Giant.Config;
with Giant.Config.Class_Sets;
with Giant.Graph_Lib.Subgraphs;
with Giant.Controller;

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Generate_Subgraph_dialog is

   package Logger is new Giant.Logger ("generate_subgraph_dialog");

   procedure Initialize
     (Dialog : access Generate_Subgraph_Dialog_Record'class);

   ---------------------------------------------------------------------------
   --  Initializer
   ---------------------------------------------------------------------------

   procedure Create
     (Dialog  :    out Generate_Subgraph_Dialog_Access;
      The_Subgraph : in Graph_Lib.Subgraphs.Subgraph)
   is
   begin
      Dialog := new Generate_Subgraph_Dialog_Record;
      Dialog.The_Subgraph := The_Subgraph;
      Initialize (Dialog);
   end Create;

   procedure Update_Class
     (List : access Gui_Utils.String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String)
   is
   begin
      Gui_Utils.String_Clists.Set_Text (List, Row, 0, Name);
   end Update_Class;

   procedure On_Okay_Button_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      use type Gtk.Enums.Gint_List.Glist;

      Dialog : constant Generate_Subgraph_Dialog_Access :=
        Generate_Subgraph_Dialog_Access (Source);

      Selection : Gtk.Enums.Gint_List.Glist
        := Gui_Utils.String_Clists.Get_Selection (Dialog.Class_Set_List);
      Row       : Glib.Gint;

      New_Subgraph : Graph_Lib.Subgraphs.Subgraph;

      All_Edges    : Graph_Lib.Edge_Id_Set;
      All_Nodes    : Graph_Lib.Node_Id_Set;

      Edge_Iter    : Graph_Lib.Edge_Id_Sets.Iterator;
      Node_Iter    : Graph_Lib.Node_Id_Sets.Iterator;

      Cur_Edge     : Graph_Lib.Edge_Id;
      Cur_Node     : Graph_Lib.Node_Id;

      Cur_Edge_Class : Graph_Lib.Edge_Class_Id;
      Cur_Node_Class : Graph_Lib.Node_Class_Id;

      Cur_Class_Set   : Config.Class_Sets.Class_Set_Access;
      Class_Set_List  : Config.Class_Sets.Class_Sets_Lists.List;

      Meta_Class_Set  : Config.Class_Sets.Meta_Class_Set_Access;

  begin
     --  get class_sets
     Class_Set_List := Config.Class_Sets.Class_Sets_Lists.Create;
     while (Selection /= Gtk.Enums.Gint_List.Null_List) loop
        Row := Gtk.Enums.Gint_List.Get_Data (Selection);
        Cur_Class_Set := Config.Class_Sets.Get_Class_Set_Access
          (Gui_Utils.String_Clists.Data.Get (Dialog.Class_Set_List, Row));

        Config.Class_Sets.Class_Sets_Lists.Attach
          (Cur_Class_Set, Class_Set_List);

        Selection := Gtk.Enums.Gint_List.Next (Selection);
     end loop;
     Meta_Class_Set := Config.Class_Sets.Build (Class_Set_List);

     All_Edges := Graph_Lib.Subgraphs.Get_All_Edges (Dialog.The_Subgraph);
     All_Nodes := Graph_Lib.Subgraphs.Get_All_Nodes (Dialog.The_Subgraph);

     New_Subgraph := Graph_Lib.Subgraphs.Create
       (Gtk.Gentry.Get_Text (Dialog.New_Subgraph_Name));

     --  check'n'convert nodes
     Node_Iter := Graph_Lib.Node_Id_Sets.Make_Iterator (All_Nodes);
     while Graph_Lib.Node_Id_Sets.More (Node_Iter) loop
        Graph_Lib.Node_Id_Sets.Next (Node_Iter, Cur_Node);

        Cur_Node_Class := Graph_Lib.Get_Node_Class_Id (Cur_Node);
        if Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
          (Meta_Class_Set, Cur_Node_Class) then
           Graph_Lib.Subgraphs.Add_Node (New_Subgraph, Cur_Node);
        end if;
     end loop;
     Graph_Lib.Node_Id_Sets.Destroy (Node_Iter);

     --  check'n'convert edges
     Edge_Iter := Graph_Lib.Edge_Id_Sets.Make_Iterator (All_Edges);
     while Graph_Lib.Edge_Id_Sets.More (Edge_Iter) loop
        Graph_Lib.Edge_Id_Sets.Next (Edge_Iter, Cur_Edge);

        Cur_Edge_Class := Graph_Lib.Get_Edge_Class_Id (Cur_Edge);
        if Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
          (Meta_Class_Set, Cur_Edge_Class) then
           Graph_Lib.Subgraphs.Add_Edge (New_Subgraph, Cur_Edge);
        end if;
     end loop;
     Graph_Lib.Edge_Id_Sets.Destroy (Edge_Iter);

     Controller.Add_Subgraph (New_Subgraph);
   end On_Okay_Button_Clicked;

   procedure Initialize
     (Dialog : access Generate_Subgraph_Dialog_Record'class)
   is
      Center_Box : Gtk.Box.Gtk_Vbox;

      List       : String_Lists.List;
      Iterator   : String_Lists.ListIter;
      Name       : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Default_Dialog.Initialize (Dialog,
                                 -"Generate Subgraph",
                                 Default_Dialog.Button_Cancel);

      Gtk.Box.Gtk_New_VBox (Center_Box);
      Set_Center_Widget (Dialog, Center_Box);

      Gtk.Box.Pack_Start (Center_Box,
                          Gui_Utils.New_Label
                          (-"Please select the class sets."),
                          Expand => False, Fill => True,
                          Padding => Gui_Utils.DEFAULT_SPACING);

      Gui_Utils.String_Clists.Create
        (Dialog.Class_Set_List, 1, Update_Class'Access);
      Gui_Utils.String_Clists.Set_Column_Title
        (Dialog.Class_Set_List, 0, -"Class");
      Gui_Utils.String_Clists.Set_Selection_Mode
        (Dialog.Class_Set_List,
         Gtk.Enums.Selection_Multiple);

      List := Config.Class_Sets.Get_All_Existing_Class_Sets;
      Iterator := String_Lists.MakeListIter (List);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Name);
         Gui_Utils.String_Clists.Add (Dialog.Class_Set_List,
                                      Ada.Strings.Unbounded.To_String (Name));
      end loop;
      String_Lists.Destroy (List);
      Gtk.Box.Pack_Start (Center_Box, Dialog.Class_Set_List,
                          Expand => True, Fill => True,
                          Padding => Gui_Utils.DEFAULT_SPACING);

      Gtk.Box.Pack_Start (Center_Box,
                          Gui_Utils.New_Label (-"Name:"),
                          Expand => False, Fill => True,
                          Padding => Gui_Utils.DEFAULT_SPACING);
      Gtk.Gentry.Gtk_New (Dialog.New_Subgraph_Name);
      Gtk.Box.Pack_Start (Center_Box, Dialog.New_Subgraph_Name,
                          Expand => False, Fill => True,
                          Padding => Gui_Utils.DEFAULT_SPACING);

      Add_Button(Dialog,
                 Gui_Utils.New_Stock_Button
                 (Gtk.Stock.Stock_Ok,
                  On_Okay_Button_Clicked'Access,
                  Dialog));
   end;

   procedure Show
     (The_Subgraph : in Graph_Lib.Subgraphs.Subgraph)
   is
      Dialog : Generate_Subgraph_Dialog_Access := null;
   begin
      Create (Dialog, The_Subgraph);
      Show_All (Dialog);
   end;

end Giant.Generate_Subgraph_Dialog;
