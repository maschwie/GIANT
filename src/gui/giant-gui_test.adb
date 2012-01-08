------------------------------------------------------------------------------
-- GIANT - Graphical IML Analysis and Navigation Tool
--
-- Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
-- Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
-- $RCSfile: giant-gui_test.adb,v $, $Revision: 1.14 $
-- $Author: squig $
-- $Date: 2003-06-23 11:30:45 $
--
with Gtk.Main;

with Ada.Text_Io; use Ada.Text_Io;

with Giant.Default_Dialog;
with Giant.Default_Logger;
with Giant.Main_Window;
with Giant.Graph_Window;
with Giant.Gsl_Dialog;
with Giant.Make_Room_Dialog;
with Giant.Node_Annotation_Dialog;
with Giant.Progress_Dialog;
with Giant.Main_Window;
with Giant.Graph_Lib;
with Giant.Logger;
--with Config;

procedure Giant.Gui_Test
is
--     My_Gsl_Dialog : Gsl_Dialog.Gsl_Dialog_Access;
   My_Progress_Dialog : Progress_Dialog.Progress_Dialog_Access;
--     My_Graph_Window : Graph_Window.Graph_Window_Access;
--     My_Node_Annotation_Dialog :
--       Node_Annotation_Dialog.Node_Annotation_Dialog_Access;
--     Node_Access : Giant.Graph_Lib.Node_Id;
--     My_Make_Room_Dialog : Make_Room_Dialog.Make_Room_Dialog_Access;
begin
   Default_Logger.Init;
   --Config.Initialize_Config_Data ("/etc/giant/giantrc", ".giantrc");

   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

--     Default_Dialog.Show_Error_Dialog ("An Error has occured!");

--     Put_Line("Input:" & Default_Dialog.Show_Input_Dialog ("Insert Text:"));

--       Gsl_Dialog.Create (My_Gsl_Dialog);
--       Gsl_Dialog.Show_All (My_Gsl_Dialog);

   Progress_Dialog.Create (My_Progress_Dialog, "Progress Test", "Message");
   Progress_Dialog.Show_All (My_Progress_Dialog);
   Progress_Dialog.Set_Value (My_Progress_Dialog, 50.0);
   Progress_Dialog.Set_Progress_Text (My_Progress_Dialog,
                                      "Value %v von %u = %p");

   Progress_Dialog.Create (My_Progress_Dialog, "Activity Test", "Message");
   Progress_Dialog.Set_Activity_Mode (My_Progress_Dialog, True);
   Progress_Dialog.Set_Value (My_Progress_Dialog, 250.0);
   Progress_Dialog.Show_All (My_Progress_Dialog);

--     Graph_Window.Create (My_Graph_Window);
--     Graph_Window.Show_All (My_Graph_Window);

--     Graph_Lib.Create ("../../../src/vis_test/rfg_examp.iml");
--     Node_Access := Graph_Lib.Get_Root_Node;

--     Node_Annotation_Dialog.Create (My_Node_Annotation_Dialog, Node_Access);
--     Node_Annotation_Dialog.Show_All (My_Node_Annotation_Dialog);

--     Make_Room_Dialog.Create (My_Make_Room_Dialog);
--     Make_Room_Dialog.Show_All (My_Make_Room_Dialog);

--     Main_Window.Show;

   Gtk.Main.Main;

   Default_Logger.Close;
end Giant.Gui_Test;

