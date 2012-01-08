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
--  First Author: Martin Schwienbacher
--
--  $RCSfile: giant-gsl_support-test.adb,v $, $Revision: 1.7 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

with Giant.File_Management;
with Giant.Config_Settings;
with String_Lists;
with Giant.String_Split;

with Giant.Config_Settings;
with Giant.GSL_Support;

package body Giant.GSL_Support.Test is

   package Logger is new Giant.Logger("Giant.GSL_Support.Test");

   ---------------------------------------------------------------------------
   procedure Test_String_Split
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

     List : String_Lists.List;
     Iter : String_Lists.ListIter;
     Dat  : Ada.Strings.Unbounded.Unbounded_String;
   begin

      List := String_Split.Split_String ("A:B:", ":");
      Iter := String_Lists.MakeListIter (List);

      String_Lists.Next(Iter, Dat);
      Assert (Ada.Strings.Unbounded.To_String (Dat) = "A",
                "Test_Splitted_String_A");
      String_Lists.Next(Iter, Dat);
      Assert (Ada.Strings.Unbounded.To_String (Dat) = "B",
                "Test_Splitted_String_B");
      String_Lists.Next(Iter, Dat);
      Assert (Ada.Strings.Unbounded.To_String (Dat) = "",
                "Test_Splitted_String_Empty");
      Assert (not String_Lists.More (Iter), "Test_Splitted_String_Length");

      String_Lists.Destroy (List);
   end Test_String_Split;

   ---------------------------------------------------------------------------
   procedure Test_Locate_GSL_File
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

     List : String_Lists.List;
     Iter : String_Lists.ListIter;
     Dat  : Ada.Strings.Unbounded.Unbounded_String;

     Dummy_String : String (1..1);
   begin

      -- write debug info
      List := Giant.Config_Settings.Get_Setting_As_Expanded_Path_List
        ("GSL.Include_Paths");

      Iter := String_Lists.MakeListIter (List);

      while String_Lists.More (Iter) loop
         String_Lists.Next(Iter, Dat);
         Logger.Debug ("GSL_Include_Path: """
                       & Ada.Strings.Unbounded.To_String (Dat)
                       & """");
      end loop;
      String_Lists.Destroy (List);

      Logger.Debug
        ("GSL_Include_File: """
         & GSL_Support.Get_GSL_Include ("dummy_skript_2_1.gsl")
         & """");

      Logger.Debug
        ("GSL_Include_File: """
         & GSL_Support.Get_GSL_Include ("dummy_skript_1_1.gsl")
         & """");

      Logger.Debug
        ("GSL_Include_File: """
         & GSL_Support.Get_GSL_Include ("dummy_skript_1_1.gsl")
         & """");

      -- test files
      Assert
        (GSL_Support.Get_GSL_Include ("dummy_skript_2_1.gsl")
         = File_Management.Get_Absolute_Path_To_File_From_Relative
           (".",
            "resources/gsl_support_test_data/gsl_include_dir_2"
            & "/dummy_skript_2_1.gsl"),
         "Test GSL include file found ""dummy_skript_2_1.gsl"".");

      Assert
        (GSL_Support.Get_GSL_Include ("dummy_skript_1_1.gsl")
         = File_Management.Get_Absolute_Path_To_File_From_Relative
           (".",
            "resources/gsl_support_test_data/gsl_include_dir_1"
            & "/dummy_skript_1_1.gsl"),
         "Test GSL include file found ""dummy_skript_1_1.gsl"".");

      Assert
        (GSL_Support.Get_GSL_Include ("dummy_skript_1_2.gsl")
         = File_Management.Get_Absolute_Path_To_File_From_Relative
           (".",
            "resources/gsl_support_test_data/gsl_include_dir_1"
            & "/dummy_skript_1_2.gsl"),
         "Test GSL include file found ""dummy_skript_1_2.gsl"".");

      -- test exception
      begin
         Dummy_String :=
           GSL_Support.Get_GSL_Include ("not_existing_file.gsl");

         Assert (False,
            "Test Locate_GSL_Include_File"
            & "GSL_Skript_File_Does_Not_Exist_Exception");
      exception
         when GSL_Support.GSL_Script_Not_Found_Exception =>

         Assert (True,
            "Test Locate_GSL_Include_File"
            & "GSL_Skript_File_Does_Not_Exist_Exception");
      end;

   end Test_Locate_GSL_File;

   ---------------------------------------------------------------------------
   procedure Test_Fault_Tolerance
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

     Dummy_String : String (1..1);
   begin

      -- test fault tolerance for not existing gsl scripts
      begin

         Dummy_String :=
           GSL_Support.Get_GSL_Include ("not_existing_script");

         Assert (False,
            "Test Locate_GSL_Include_File"
            & "GSL_Skript_File_Does_Not_Exist_Exception");
      exception
         when GSL_Support.GSL_Script_Not_Found_Exception =>

         Assert (True,
            "Test Locate_GSL_Include_File"
            & "GSL_Skript_File_Does_Not_Exist_Exception");
      end;
   end Test_Fault_Tolerance;

   ---------------------------------------------------------------------------
   function Name (T : Test_Case)
     return Ada.Strings.Unbounded.String_Access is

   begin
      return new String'("GSL_Support");
   end Name;

   ---------------------------------------------------------------------------
   procedure Register_Tests (T : in out Test_Case) is
   begin

      Register_Routine (T, Test_String_Split'Access, "Test_String_Split");
      Register_Routine
        (T, Test_Locate_GSL_File'Access, "Test_Locate_GSL_File");
      Register_Routine
        (T, Test_Fault_Tolerance'Access, "Test_Fault_Tolerance");

   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin

      Config_Settings.Initialize_Config_Settings
        ("", "resources/gsl_support_test_data/test_config.xml");
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin

      Config_Settings.Clear_Config_Data;
   end Tear_Down;

end Giant.GSL_Support.Test;
