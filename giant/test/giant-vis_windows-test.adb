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
--  $RCSfile: giant-vis_windows-test.adb,v $, $Revision: 1.14 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
with Ada.Streams.Stream_IO;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Gtk.Main;

with String_Lists;
with Bauhaus_IO;

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

with Giant.Config;
with Giant.Config.Vis_Styles;
with Giant.Node_Annotations;

with Giant.Vis_Windows;
with Giant.Graph_Lib.Selections;

with Giant.Config.Vis_Styles;

package body Giant.Vis_Windows.Test is

   package Logger is new Giant.Logger("Giant.Vis_Windows.Test");

   ---------------------------------------------------------------------------
   -- Utilities
   ---------------------------------------------------------------------------

   function Load_Vis_Window_Into_Main_Memory
     (File_Path   : in String;
      Annotations : in Node_Annotations.Node_Annotation_Access)
     return Vis_Windows.Visual_Window_Access is

      Stream_File       : Ada.Streams.Stream_IO.File_Type;
      Ada_Stream        : Ada.Streams.Stream_IO.Stream_Access;
      Bauhaus_In_Stream : Bauhaus_IO.In_Stream_Type;
      New_Vis_Window    : Vis_Windows.Visual_Window_Access;
   begin

      Ada.Streams.Stream_IO.Open
        (Stream_File,
         Ada.Streams.Stream_IO.In_File,
         File_Path);

      Ada_Stream := Ada.Streams.Stream_IO.Stream (Stream_File);
      Bauhaus_In_Stream := Bauhaus_IO.Make_Internal (Ada_Stream);

      Vis_Windows.Visual_Window_Access_Read
        (Bauhaus_In_Stream, New_Vis_Window, Annotations);

      -- close resources
      Bauhaus_IO.Release (Bauhaus_In_Stream);
      Ada.Streams.Stream_IO.Close (Stream_File);

      return New_Vis_Window;
   end Load_Vis_Window_Into_Main_Memory;

   ---------------------------------------------------------------------------
   procedure Write_Vis_Window_To_File
     (File_Path  : in String;
      Vis_Window : in Vis_Windows.Visual_Window_Access) is

      Stream_File        : Ada.Streams.Stream_IO.File_Type;
      Ada_Stream         : Ada.Streams.Stream_IO.Stream_Access;
      Bauhaus_Out_Stream : Bauhaus_IO.Out_Stream_Type;
   begin

      -- test if file exists, create new one if necessary
      begin
         Ada.Streams.Stream_IO.Open
           (Stream_File,
            Ada.Streams.Stream_IO.Out_File,
            File_Path);
      exception
         when Ada.Streams.Stream_IO.Name_Error =>
         Ada.Streams.Stream_IO.Create
           (Stream_File,
            Ada.Streams.Stream_IO.Out_File,
            File_Path);
      end;

      Ada_Stream := Ada.Streams.Stream_IO.Stream (Stream_File);
      Bauhaus_Out_Stream := Bauhaus_IO.Make_Internal (Ada_Stream);

      Vis_Windows.Visual_Window_Access_Write
        (Bauhaus_Out_Stream, Vis_Window);

      -- close resources
      Bauhaus_IO.Release (Bauhaus_Out_Stream);
      Ada.Streams.Stream_IO.Close (Stream_File);
   end Write_Vis_Window_To_File;


   ---------------------------------------------------------------------------
   -- Tests
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Set_Up_Vis_Window (Name : in String)
     return Visual_Window_Access is

      New_Vis_Window : Visual_Window_Access;
      Selection_1 : Graph_Lib.Selections.Selection;
      Selection_2 : Graph_Lib.Selections.Selection;
      Selection_3 : Graph_Lib.Selections.Selection;

   begin

      New_Vis_Window := Vis_Windows.Create_New (Name);

      Selection_1 := Graph_Lib.Selections.Create ("Selection_1");
      Selection_2 := Graph_Lib.Selections.Create ("Selection_2");
      Selection_3 := Graph_Lib.Selections.Create ("Selection_3");

      Vis_Windows.Add_Selection (New_Vis_Window, Selection_1);
      Vis_Windows.Add_Selection (New_Vis_Window, Selection_2);
      Vis_Windows.Add_Selection (New_Vis_Window, Selection_3);

      Vis_Windows.Add_Pin
        (Vis_Window => New_Vis_Window,
         Name       => "Pin_1",
         Position   => Vis.Logic.Zero_2d,
         Zoom_Level => 0.1);

      Vis_Windows.Add_Pin
        (Vis_Window => New_Vis_Window,
         Name       => "Pin_2",
         Position   => Vis.Logic.Zero_2d,
         Zoom_Level => 0.2);

      Vis_Windows.Add_Pin
        (Vis_Window => New_Vis_Window,
         Name       => "Pin_3",
         Position   => Vis.Logic.Zero_2d,
         Zoom_Level => 0.3);

      Vis_Windows.Add_Pin
        (Vis_Window => New_Vis_Window,
         Name       => "Pin_4",
         Position   => Vis.Logic.Zero_2d,
         Zoom_Level => 0.4);

      return New_Vis_Window;
   end Set_Up_Vis_Window;

   ---------------------------------------------------------------------------
   procedure Check_Default_Win_Status
     (Test_Window : in Vis_Windows.Visual_Window_Access) is

      Test_List      : String_Lists.List;
      Test_Selection : Graph_Lib.Selections.Selection;
   begin

      -- check status;
      Assert (Get_Name (Test_Window) = "Test_Window_X",
              "Test_Vis_Window_Name");

      -- check selections
      --------------------

      -- all selections
      Test_List := Vis_Windows.Get_All_Selections (Test_Window);
      Assert (String_Lists.Length (Test_List) = 4,
              "Test Lenght of selection list");
      String_Lists.Destroy (Test_List);

      Assert
        (Vis_Windows.Does_Selection_Exist (Test_Window, "Selection_1"),
         "Test whether ""Selection_2"" exists");
      Assert
        (Vis_Windows.Does_Selection_Exist (Test_Window, "Selection_2"),
         "Test whether ""Selection_3"" exists");
      Assert
        (Vis_Windows.Does_Selection_Exist (Test_Window, "Selection_3"),
         "Test whether ""Selection_3"" exists");

      Test_Selection :=
        Vis_Windows.Get_Selection (Test_Window, "Selection_1");
      Assert
        (Graph_Lib.Selections.Get_Name (Test_Selection) = "Selection_1",
         "Test Get_Selection retrieving ""Selection_1");

      Test_Selection :=
        Vis_Windows.Get_Selection (Test_Window, "Selection_2");
      Assert
        (Graph_Lib.Selections.Get_Name (Test_Selection) = "Selection_2",
         "Test Get_Selection retrieving ""Selection_2");

      Test_Selection :=
        Vis_Windows.Get_Selection (Test_Window, "Selection_3");
      Assert
        (Graph_Lib.Selections.Get_Name (Test_Selection) = "Selection_3",
         "Test Get_Selection retrieving ""Selection_3");

      Test_Selection :=
        Vis_Windows.Get_Selection (Test_Window, "Default");
      Assert
        (Graph_Lib.Selections.Get_Name (Test_Selection) = "Default",
         "Test Get_Selection retrieving ""Default");

      -- standard selection
      Assert (Vis_Windows.Get_Standard_Selection (Test_Window) = "Default",
              "Test_Name_of_Standard_Selection");

      -- check current selection
      Assert (Vis_Windows.Get_Current_Selection (Test_Window) = "Default",
              "Test_Name_of_Current_Selection");

      -- test highlight status
      Assert (Get_Highlight_Status
        (Test_Window,
         Vis_Windows.Get_Current_Selection (Test_Window))
         = Giant.Vis_Windows.Current_Selection,
         "Test_Current_Selection_Highlight_Status");

      -- test pins
      ------------
      Test_List := Get_All_Pins (Test_Window);
      Assert (String_Lists.Length (Test_List) = 4,
              "Test Length of pin list");
      String_Lists.Destroy (Test_List);

      Assert
        (Vis_Windows.Does_Pin_Exist (Test_Window, "Pin_1"),
         "Test whether ""Pin_1"" exists");
      Assert
        (Vis_Windows.Does_Pin_Exist (Test_Window, "Pin_2"),
         "Test whether ""Pin_2"" exists");
      Assert
        (Vis_Windows.Does_Pin_Exist (Test_Window, "Pin_3"),
         "Test whether ""Pin_3"" exists");
      Assert
        (Vis_Windows.Does_Pin_Exist (Test_Window, "Pin_4"),
         "Test whether ""Pin_4"" exists");

      -- test vis style
      -----------------
      Assert
        (Vis_Windows.Get_Vis_Style (Test_Window) = "test_vis_style_1_default",
         "Check whether correct vis style loaded");


   end Check_Default_Win_Status;

   ---------------------------------------------------------------------------
   procedure Leack_Test (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_Window : Vis_Windows.Visual_Window_Access;
   begin

      for i in 1 .. 1 loop
         Test_Window := Set_Up_Vis_Window ("Test_Window_X");
         Vis_Windows.Deallocate_Vis_Window_Deep (Test_Window);
      end loop;
   end Leack_Test;

   ---------------------------------------------------------------------------
   procedure Test_Init (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_Window : Vis_Windows.Visual_Window_Access;
   begin

      Test_Window := Set_Up_Vis_Window ("Test_Window_X");
      Check_Default_Win_Status (Test_Window);

      Vis_Windows.Deallocate_Vis_Window_Deep (Test_Window);
   end Test_Init;

   ---------------------------------------------------------------------------
   procedure Test_Streaming (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_Window : Vis_Windows.Visual_Window_Access;
   begin

      Test_Window := Set_Up_Vis_Window ("Test_Window_X");

      Write_Vis_Window_To_File
        ("./resources/test_vis_window_stream_file.viswindow",
         Test_Window);

      Vis_Windows.Deallocate_Vis_Window_Deep (Test_Window);

      Test_Window := Load_Vis_Window_Into_Main_Memory
        ("./resources/test_vis_window_stream_file.viswindow",
         Node_Annotations.Create_Empty);

      Check_Default_Win_Status (Test_Window);

      Vis_Windows.Deallocate_Vis_Window_Deep (Test_Window);
   end Test_Streaming;

   ---------------------------------------------------------------------------
   procedure Test_Changing_Status_And_Content
      (R : in out AUnit.Test_Cases.Test_Case'Class) is

      use Vis.Logic;

      Test_Window    : Vis_Windows.Visual_Window_Access;
      Test_Selection : Graph_Lib.Selections.Selection;
      Test_Float     : Float;
   begin

      Test_Window := Set_Up_Vis_Window ("Test_Window_X");
      Check_Default_Win_Status (Test_Window);

      Test_Selection := Graph_Lib.Selections.Create ("Selection_1-New_Name");

      -- do some changes
      ------------------
      Vis_Windows.Change_Name (Test_Window, "Test_Window_X-New_Name");

      Vis_Windows.Change_Selection_Name
        (Test_Window,
         "Selection_1",
         "Selection_1-New_Name");

      Vis_Windows.Change_Selection_Name
        (Test_Window,
         "Selection_2",
         "Selection_2-New_Name");

      Vis_Windows.Set_Current_Selection (Test_Window, "Selection_3");

      Assert
        (not May_Highlight_Status_Be_Changed (Test_Window, "Selection_3"),
         "Test whether Higlight Status may be changed - Selection_3");

      Assert
        (May_Highlight_Status_Be_Changed
          (Test_Window, "Selection_1-New_Name"),
         "Test whether Higlight Status may be changed "
         &"- Selection_1-New_Name");

      Assert
        (Get_Highlight_Status (Test_Window, "Selection_3")
         = Current_Selection,
         "Check highlight status of new current selection");

      Assert
        (Get_Highlight_Status (Test_Window, "Default")
         = None,
         "Check highlight status of former current selection");

      Assert
        (Graph_Lib.Selections.Get_Name
          (Vis_Windows.Get_Selection
            (Test_Window, Vis_Windows.Get_Current_Selection (Test_Window)))
         = "Selection_3",
         "Test correct name of new current selection");

      Assert
        (Get_Highlight_Status (Test_Window, "Default")
         = None,
         "Check highlight status of former current selection");

      Assert
        (not Vis_Windows.May_Be_Faded_Out (Test_Window, "Selection_3"),
         "Test current selection may not be faded out - Selection_3");

      Assert
        (not Vis_Windows.May_Be_Faded_Out (Test_Window, "Default"),
         "Test standard selection may not be faded out - Default");

      Assert
        (Vis_Windows.May_Be_Faded_Out
          (Test_Window, "Selection_1-New_Name"),
         "Test selection may be faded out - Selection_1-New_Name");

      -- Selection_May_Not_Be_Faded_Out_Exception
      begin
         Vis_Windows.Fade_Out_Selection
           (Test_Window, "Default");
         Assert
           (False,
            "Check Fade_Out_Selection "
            & "- ""Selection_May_Not_Be_Faded_Out_Exception"" "
            & "not rised correctly.");
      exception
         when Selection_May_Not_Be_Faded_Out_Exception =>
            Assert
              (True,
               "Check Fade_Out_Selection "
               & "- ""Selection_May_Not_Be_Faded_Out_Exception"" "
               & "not rised correctly. - Default");
      end;

      -- Selection_May_Not_Be_Faded_Out_Exception
      begin
         Vis_Windows.Fade_Out_Selection
           (Test_Window, "Selection_3");
         Assert
           (False,
            "Check Fade_Out_Selection "
            & "- ""Selection_May_Not_Be_Faded_Out_Exception"" "
            & "not rised correctly.");
      exception
         when Selection_May_Not_Be_Faded_Out_Exception =>
            Assert
              (True,
               "Check Fade_Out_Selection "
               & "- ""Selection_May_Not_Be_Faded_Out_Exception"" "
               & "not rised correctly. - Selection_3");
      end;

      -- should also reset current selection to default
      Vis_Windows.Remove_Selection (Test_Window, "Selection_3");

      Vis_Windows.Set_Highlight_Status
        (Test_Window, "Selection_2-New_Name", None);
      Vis_Windows.Set_Highlight_Status
        (Test_Window, "Selection_2-New_Name", Color_2);

      Vis_Windows.Set_Highlight_Status
        (Test_Window, "Selection_1-New_Name", Color_1);

      Fade_Out_Selection (Test_Window, "Selection_1-New_Name");
      Fade_In_Selection (Test_Window, "Selection_1-New_Name");

      Fade_Out_Selection (Test_Window, "Selection_2-New_Name");


      -- Pins
      Vis_Windows.Remove_Pin (Test_Window, "Pin_1");

      Vis_Windows.Add_Pin
        (Vis_Window => Test_Window,
         Name       => "Pin_1",
         Position   => Vis.Logic.Zero_2d,
         Zoom_Level => 0.1);

      Vis_Windows.Remove_Pin (Test_Window, "Pin_4");

      Vis_Windows.Change_Pin_Name (Test_Window, "Pin_1", "Pin_1_New");
      Vis_Windows.Change_Pin_Name (Test_Window, "Pin_1_New", "Pin_1");

      Vis_Windows.Change_Pin_Name (Test_Window, "Pin_2", "Pin_2_New");

      -- vis styles
      Vis_Windows.Set_Vis_Style (Test_Window, "test_vis_style_2");


      -- check important exceptions
      -----------------------------

      -- Standard_Selection_Name_May_Not_Be_Changed_Exception
      begin
         Vis_Windows.Change_Selection_Name
           (Test_Window,
            "Default",
            "Default-New_Name");
         Assert
           (False,
            "Check Change_Selection_Name "
            & "- ""Standard_Selection_Name_May_Not_Be_Changed_Exception"" "
            & "not rised correctly.");
      exception
         when Standard_Selection_Name_May_Not_Be_Changed_Exception =>
            Assert
              (True,
               "Check Change_Selection_Name "
               & "- ""Standard_Selection_Name_May_Not_Be_Changed_Exception"" "
               & "not rised correctly.");
      end;

      -- Standard_Selection_May_Not_Be_Removed_Exception
      begin
         Vis_Windows.Remove_Selection (Test_Window, "Default");
         Assert
           (False,
            "Check Remove_Selection "
            & "- ""Standard_Selection_May_Not_Be_Removed_Exception"" "
            & "not rised correctly.");
      exception
         when Standard_Selection_May_Not_Be_Removed_Exception =>
            Assert
              (True,
               "Check Remove_Selection "
               & "- ""Standard_Selection_May_Not_Be_Removed_Exception"" "
               & "not rised correctly.");
      end;

      -- Highlight_Status_Of_Selection_May_Not_Be_Changed_Exception
      begin
         Vis_Windows.Set_Highlight_Status
           (Test_Window, "Default", Vis_Windows.None);
         Assert
           (False,
            "Check Set_Highlight_Status "
            & "- ""Highlight_Status_Of_Selection_May_"
            & "Not_Be_Changed_Exception"" "
            & "not rised correctly.");
      exception
         when Highlight_Status_Of_Selection_May_Not_Be_Changed_Exception =>
            Assert
              (True,
               "Check Set_Highlight_Status "
               & "- ""Highlight_Status_Of_Selection_May_"
               & "Not_Be_Changed_Exception"" "
               & "not rised correctly.");
      end;

      -- Selection_Is_Already_Part_Of_Window_Exception
      begin
         Vis_Windows.Add_Selection
           (Test_Window, Test_Selection);
         Assert
           (False,
            "Check Set_Highlight_Status "
            & "- ""Selection_Is_Already_Part_Of_Window_Exception"" "
            & "not rised correctly.");
      exception
         when Selection_Is_Already_Part_Of_Window_Exception =>
            Assert
              (True,
               "Check Set_Highlight_Status "
               & "- ""Selection_Is_Already_Part_Of_Window_Exception"" "
               & "not rised correctly.");
      end;

      -- Pin_Does_Already_Exist_Exception
      begin
         Vis_Windows.Add_Pin
           (Vis_Window => Test_Window,
            Name       => "Pin_1",
            Position   => Vis.Logic.Zero_2d,
            Zoom_Level => 0.1);
         Assert
           (False,
            "Check Add_Pin "
            & "- ""Pin_Does_Already_Exist_Exception"" "
            & "not rised correctly.");
      exception
         when Pin_Does_Already_Exist_Exception =>
            Assert
              (True,
               "Check Add_Pin "
               & "- ""Pin_Does_Already_Exist_Exception"" "
               & "not rised correctly.");
      end;

      -- Pin_With_Passed_Name_Not_Found_Exception
      begin
         Test_Float := Vis_Windows.Get_Zoom
           (Test_Window, "Pin_4");
         Assert
           (False,
            "Check Get_Zoom "
            & "- ""Pin_With_Passed_Name_Not_Found_Exception"" "
            & "not rised correctly.");
      exception
         when Pin_With_Passed_Name_Not_Found_Exception =>
            Assert
              (True,
               "Check Get_Zoom "
               & "- ""Pin_With_Passed_Name_Not_Found_Exception"" "
               & "not rised correctly.");
      end;

      -- Vis_Style_Does_Not_Exist_Exception
      begin
         Vis_Windows.Set_Vis_Style
           (Test_Window, "I_do_not_exist");
         Assert
           (False,
            "Check Set_Vis_Style "
            & "- ""Vis_Style_Does_Not_Exist_Exception"" "
            & "not rised correctly.");
      exception
         when Vis_Style_Does_Not_Exist_Exception =>
            Assert
              (True,
               "Check Set_Vis_Style "
               & "- ""Vis_Style_Does_Not_Exist_Exception"" "
               & "not rised correctly.");
      end;

      -- Stream and reload Vis_Window
      -------------------------------

      Write_Vis_Window_To_File
        ("./resources/test_vis_window_stream_file.viswindow",
         Test_Window);

      Vis_Windows.Deallocate_Vis_Window_Deep (Test_Window);

      Test_Window := Load_Vis_Window_Into_Main_Memory
        ("./resources/test_vis_window_stream_file.viswindow",
         Node_Annotations.Create_Empty);

      -- Status Check
      ----------------

      Assert
        (Vis_Windows.Get_Current_Selection (Test_Window) = "Default",
         "Test whether current selection resetted correctly to default");

      Assert (not Vis_Windows.Does_Selection_Exist
        (Test_Window, "Selection_3"),
        "Test whether Selection_3 was removed correctly");

      Assert (not Vis_Windows.Does_Selection_Exist
        (Test_Window, "Selection_1"),
        "Test whether Selection_1 was renamed correctly - 1");

      Assert (Vis_Windows.Does_Selection_Exist
        (Test_Window, "Selection_1-New_Name"),
        "Test whether Selection_1 was renamed correctly - 2");

      Assert (not Vis_Windows.Does_Selection_Exist
        (Test_Window, "Selection_2"),
        "Test whether Selection_2 was renamed correctly - 1");

      Assert (Vis_Windows.Does_Selection_Exist
        (Test_Window, "Selection_2-New_Name"),
        "Test whether Selection_2 was renamed correctly - 2");

      Assert (Graph_Lib.Selections.Get_Name
          (Get_Selection (Test_Window, "Selection_2-New_Name"))
         = "Selection_2-New_Name",
         "Test whether Selection_2 was renamed correctly - 3");

      Assert
        (Get_Highlight_Status (Test_Window, "Selection_1-New_Name")
         = Color_1,
         "Check highlight status of Selection_1-New_Name");

      Assert
        (Get_Highlight_Status (Test_Window, "Selection_2-New_Name")
         = Color_2,
         "Check highlight status of Selection_2-New_Name");

      Assert
        (not Vis_Windows.Is_Faded_Out (Test_Window, "Selection_1-New_Name"),
         "Check Fading Status of Selection_1-New_Name");

      Assert
        (Vis_Windows.Is_Faded_Out (Test_Window, "Selection_2-New_Name"),
         "Check Fading Status of Selection_2-New_Name");

      -- pins
      -------

      Assert
        (Vis_Windows.Does_Pin_Exist (Test_Window, "Pin_1"),
         "Check whether Pin_1 exits");
      Assert
        (Vis_Windows.Get_Position (Test_Window, "Pin_1")
         = Vis.Logic.Zero_2d,
         "Check position of Pin_1");
      Assert
        (Vis_Windows.Get_Zoom (Test_Window, "Pin_1") = 0.1,
         "Check zoom of Pin_1");

      Assert
        (not Vis_Windows.Does_Pin_Exist (Test_Window, "Pin_2"),
         "Check Pin_2 renamed correctly - 1");
      Assert
        (Vis_Windows.Does_Pin_Exist (Test_Window, "Pin_2_New"),
         "Check Pin_2 renamed correctly - 2");

      Assert
        (Vis_Windows.Does_Pin_Exist (Test_Window, "Pin_3"),
         "Check whether Pin_3 exits");

      Assert
        (not Vis_Windows.Does_Pin_Exist (Test_Window, "Pin_4"),
         "Check whether Pin_4 is removed");

      -- vis styles
      Assert
        (Vis_Windows.Get_Vis_Style (Test_Window) = "test_vis_style_2",
        "Test Vis Style changed correctly to ""test_vis_style_2""");

      -- free storrage
      Graph_Lib.Selections.Destroy (Test_Selection);
      Vis_Windows.Deallocate_Vis_Window_Deep (Test_Window);
   end  Test_Changing_Status_And_Content;

   ---------------------------------------------------------------------------
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Giant.Vis_Windows.Test - Basic Tests");
   end Name;

   ---------------------------------------------------------------------------
   procedure Register_Tests (T : in out Test_Case) is
   begin

      Register_Routine (T, Leack_Test'Access, "Leack_Test");
      Register_Routine (T, Test_Init'Access, "Test_Init");
      Register_Routine (T, Test_Streaming'Access, "Test_Streaming");
      Register_Routine
        (T, Test_Changing_Status_And_Content'Access,
         "Test_Changing_Status_And_Content");

   end Register_Tests;

   ---------------------------------------------------------------------------
   procedure Set_Up (T : in out Test_Case) is
   begin

      Gtk.Main.Init;

      Giant.Graph_Lib.Initialize;
      Giant.Graph_Lib.Load
        ("resources/rfg_examp.iml");
      Giant.Config.Vis_Styles.Initialize_Config_Vis_Styles
        ("",
         "resources/vis_styles/vis_styles_test_set_1",
         "",
         "resources/vis_styles/vis_styles_test_set_1/"
         & "test_vis_style_1_default.xml");
   end Set_Up;

   ---------------------------------------------------------------------------
   procedure Tear_Down (T : in out Test_Case) is
   begin

      Giant.Graph_Lib.Destroy;
      Giant.Config.Vis_Styles.Clear_Config_Vis_Styles;

   --   Gtk.Main.Gtk_Exit (0);
   end Tear_Down;

end Giant.Vis_Windows.Test;
