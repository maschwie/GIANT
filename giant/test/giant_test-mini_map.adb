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
--  $RCSfile: giant_test-mini_map.adb,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Ada.Text_IO; use Ada.Text_IO;

with Gdk.Event;
with Gdk.Threads;
with Gtk.Box;
with Gtk.Button;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Main;
with Gtk.Vbutton_Box;
with Gtk.Window;

with Giant.Default_Logger;
with Giant.Mini_Maps;
with Giant.Vis;

procedure Giant_Test.Mini_Map is

   Graph_Widget : Giant.Mini_Maps.Graph_Widgets.Graph_Widget :=
     Giant.Mini_Maps.Graph_Widgets.Create;

   function On_Main_Window_Delete_Event
     (Window : access Gtk.Window.Gtk_Window_Record'Class)
      return Boolean is
   begin
      Gtk.Main.Main_Quit;
      return False;
   end On_Main_Window_Delete_Event;

   procedure On_Main_Window_Move_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class) is

      Visible_Area : Giant.Vis.Logic.Rectangle_2d :=
        Giant.Mini_Maps.Graph_Widgets.Get_Visible_Area (Graph_Widget);
   begin
      Giant.Vis.Logic.Move
        (Visible_Area, Giant.Vis.Logic.Combine_Vector (30.0, 0.0));
      Giant.Mini_Maps.Graph_Widgets.Set_Visible_Area
        (Graph_Widget, Visible_Area);
   end On_Main_Window_Move_Clicked;

   procedure On_Main_Window_Resize_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class) is

      Logical_Area : Giant.Vis.Logic.Rectangle_2d :=
        Giant.Mini_Maps.Graph_Widgets.Get_Logical_Area (Graph_Widget);
   begin
      Giant.Vis.Logic.Set_Bottom
        (Logical_Area, Giant.Vis.Logic.Get_Bottom (Logical_Area) + 150.0);
      Giant.Mini_Maps.Graph_Widgets.Set_Logical_Area
        (Graph_Widget, Logical_Area);
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

   Button_Area   : Gtk.Vbutton_Box.Gtk_Vbutton_Box;
   User_Box      : Gtk.Box.Gtk_Box;
   Mini_Map      : Giant.Mini_Maps.Mini_Map;
   Move_Button   : Gtk.Button.Gtk_Button;
   Resize_Button : Gtk.Button.Gtk_Button;
   Quit_Button   : Gtk.Button.Gtk_Button;
   Main_Window   : Gtk.Window.Gtk_Window;
begin
   Giant.Default_Logger.Init;
   Gdk.Threads.Init;
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gdk.Event.Set_Show_Events (False);

   Giant.Mini_Maps.Create
     (Widget  => Mini_Map,
      Watched => Graph_Widget);

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

   Gtk.Window.Gtk_New (Main_Window);
   Gtk.Window.Add (Main_Window, User_Box);

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

   Gdk.Threads.Enter;
   Gtk.Main.Main;
   Gtk.Window.Destroy (Main_Window);

   Gdk.Threads.Leave;
   Giant.Default_Logger.Close;
end Giant_Test.Mini_Map;
