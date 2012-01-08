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
--  $RCSfile: giant_test-graph_widget.adb,v $, $Revision: 1.11 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Gdk.Event;
with Gdk.Threads;
with Gtk.Box;
with Gtk.Button;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Main;
with Gtk.Scrolled_Window;
with Gtk.Vbutton_Box;
with Gtk.Window;

with Giant.Config.Global_Data;
with Giant.Config_Settings;
with Giant.Config.Vis_Styles;
with Giant.Default_Logger;
with Giant.File_Management;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Graph_Widgets;
with Giant.Logger;
with Giant.Mini_Maps;
with Giant.Vis;

procedure Giant_Test.Graph_Widget is

   package Test_Log is new Giant.Logger
     (Name => "Test_Prog");

   function On_Main_Window_Delete_Event
     (Window : access Gtk.Window.Gtk_Window_Record'Class)
      return Boolean is
   begin
      Gtk.Main.Main_Quit;
      return False;
   end On_Main_Window_Delete_Event;

   procedure On_Main_Window_Move_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class) is

   begin
      null;
   end On_Main_Window_Move_Clicked;

   procedure On_Main_Window_Resize_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class) is

   begin
      null;
   end On_Main_Window_Resize_Clicked;

   procedure On_Main_Window_Quit_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class) is
   begin
      Gtk.Main.Main_Quit;
   end On_Main_Window_Quit_Clicked;


   package Window_Cb is new Gtk.Handlers.Callback
     (Gtk.Window.Gtk_Window_Record);
   package Window_R_Boolean_Cb is new Gtk.Handlers.Return_Callback
     (Gtk.Window.Gtk_Window_Record,
      Boolean);
   package Button_Cb is new Gtk.Handlers.Callback
     (Gtk.Button.Gtk_Button_Record);

   Config_Filename : constant String
     := Giant.File_Management.Get_User_Config_Path & "settings.xml";
   Button_Area   : Gtk.Vbutton_Box.Gtk_Vbutton_Box;
   User_Box      : Gtk.Box.Gtk_Box;
   Horiz_Box     : Gtk.Box.Gtk_Box;
   Mini_Map      : Giant.Mini_Maps.Mini_Map;
   Graph_Widget  : Giant.Graph_Widgets.Graph_Widget;
   Move_Button   : Gtk.Button.Gtk_Button;
   Resize_Button : Gtk.Button.Gtk_Button;
   Quit_Button   : Gtk.Button.Gtk_Button;
   Main_Window   : Gtk.Window.Gtk_Window;
   Scroller      : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
begin
   Giant.Default_Logger.Init;
   Gdk.Threads.Init;
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gdk.Event.Set_Show_Events (False);

   --  load config settings
   Giant.Config_Settings.Initialize_Config_Settings
     ("resources/global_config.xml", Config_Filename);

   Giant.Config.Global_Data.Initialize_Config_Data;

   Giant.Graph_Lib.Initialize;

   Giant.Config.Vis_Styles.Initialize_Config_Vis_Styles
     (Resources_Root_Dir     => "",
      GIANT_VIS_Directory    => "",
      User_Vis_Directory     => "",
      Default_Vis_Style_File =>
        "resources/vis_styles/vis_styles_test_set_1/"
        & "test_vis_style_1_default.xml");
--        "resources/vis_styles/only_defaults_giant_vis_style.xml");
--        "../dist/vis_styles_set_1/vis_style_default.xml");

   Giant.Graph_Widgets.Create
     (Widget  => Graph_Widget);
   Giant.Mini_Maps.Create
     (Widget  => Mini_Map,
      Watched => Graph_Widget);

--   Gtk.Scrolled_Window.Gtk_New (Scroller);
--   Gtk.Scrolled_Window.Add (Scroller, Graph_Widget);

   Gtk.Button.Gtk_New (Move_Button, "Move Visual");
   Gtk.Button.Gtk_New (Resize_Button, "Resize");
   Gtk.Button.Gtk_New (Quit_Button, "Quit");

   Gtk.Vbutton_Box.Gtk_New (Button_Area);
   Gtk.Vbutton_Box.Add (Button_Area, Move_Button);
   Gtk.Vbutton_Box.Add (Button_Area, Resize_Button);
   Gtk.Vbutton_Box.Add (Button_Area, Quit_Button);

   Gtk.Box.Gtk_New_Vbox
     (Box     => User_Box,
      Spacing => 5);
   Gtk.Box.Add (User_Box, Button_Area);
   Gtk.Box.Add (User_Box, Mini_Map);

   Gtk.Box.Gtk_New_Hbox
     (Box     => Horiz_Box,
      Spacing => 5);
   Gtk.Box.Add (Horiz_Box, User_Box);
   Gtk.Box.Add (Horiz_Box, Graph_Widget);--Scroller);

   Gtk.Window.Gtk_New (Main_Window);
   Gtk.Window.Add (Main_Window, Horiz_Box);

   Window_R_Boolean_Cb.Connect
     (Widget => Main_Window,
      Name   => "delete_event",
      Marsh  => Window_R_Boolean_Cb.To_Marshaller
        (On_Main_Window_Delete_Event'Access));
   Button_Cb.Connect
     (Widget => Move_Button,
      Name   => "clicked",
      Marsh  => Button_Cb.To_Marshaller
                  (On_Main_Window_Move_Clicked'Access));
   Button_Cb.Connect
     (Widget => Resize_Button,
      Name   => "clicked",
      Marsh  => Button_Cb.To_Marshaller
                  (On_Main_Window_Resize_Clicked'Access));
   Button_Cb.Connect
     (Widget => Quit_Button,
      Name   => "clicked",
      Marsh  => Button_Cb.To_Marshaller (On_Main_Window_Quit_Clicked'Access));

   Gtk.Window.Show_All (Main_Window);


   Giant.Graph_Lib.Load ("resources/rfg_examp.iml");
   --  selection erzeugen,
   declare
      Lock          : Giant.Graph_Widgets.Lock_Type;
      Selection     : Giant.Graph_Lib.Selections.Selection;
      Node          : Giant.Graph_Lib.Node_Id;
      Second_Node   : Giant.Graph_Lib.Node_Id;
      Edge          : Giant.Graph_Lib.Edge_Id;
      Edge_Set      : Giant.Graph_Lib.Edge_Id_Sets.Set;
      Iterator      : Giant.Graph_Lib.Edge_Id_Sets.Iterator;

      function To_Integer is new Ada.Unchecked_Conversion
        (Source => Giant.Graph_Lib.Edge_Id,
         Target => Integer);
   begin
      Test_Log.Debug ("Create Funny_Selection");
      Selection := Giant.Graph_Lib.Selections.Create ("Funny_Selection");
      Node := Giant.Graph_Lib.Get_Root_Node;
      Giant.Graph_Lib.Selections.Add_Node (Selection, Node);
      Test_Log.Debug
        ("Root node added, nodes count = " & Natural'Image
         (Giant.Graph_Lib.Selections.Get_Node_Count (Selection)));

      Edge_Set := Giant.Graph_Lib.Get_Outgoing_Edges (Node);
      Iterator := Giant.Graph_Lib.Edge_Id_Sets.Make_Iterator (Edge_Set);
      pragma Assert (Giant.Graph_Lib.Edge_Id_Sets.More (Iterator));
      Giant.Graph_Lib.Edge_Id_Sets.Next (Iterator, Edge);
      Test_Log.Debug ("Edge_Id =" & Integer'Image (To_Integer (Edge)));
      Giant.Graph_Lib.Edge_Id_Sets.Destroy (Iterator);
      Giant.Graph_Lib.Edge_Id_Sets.Destroy (Edge_Set);
      Giant.Graph_Lib.Selections.Add_Edge (Selection, Edge);
      Second_Node := Giant.Graph_Lib.Get_Target_Node (Edge);
      Giant.Graph_Lib.Selections.Add_Node (Selection, Second_Node);
      --  einfügen
      Test_Log.Debug ("Insert Selection.");
      Giant.Graph_Widgets.Insert_Selection (Graph_Widget, Selection, Lock);
      Test_Log.Debug ("Move nodes");
      Giant.Graph_Widgets.Set_Top_Middle
        (Widget   => Graph_Widget,
         Node     => Second_Node,
         Location => Giant.Vis.Logic.Combine_Vector (140.0, -30.0),
         Lock     => Lock);
      Test_Log.Debug ("Release_Lock.");
      Giant.Graph_Widgets.Release_Lock (Graph_Widget, Lock);
      --  fertig
      Test_Log.Debug ("Destroy Funny_Selection.");
      Giant.Graph_Lib.Selections.Destroy (Selection);
   end;


   Gdk.Threads.Enter;
   Gtk.Main.Main;
   Gtk.Window.Destroy (Main_Window);

   Gdk.Threads.Leave;
   Giant.Default_Logger.Close;
end Giant_Test.Graph_Widget;
