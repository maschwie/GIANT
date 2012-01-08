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
--  $RCSfile: giant-default_logger-test.adb,v $, $Revision: 1.7 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--

with Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

package body Giant.Default_Logger.Test is

   procedure Test_Init (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (Ada.Text_IO.Is_Open (Out_File), "Not Is_Open");
   end;

   procedure Test_Get_Level_String (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (Get_Level_String (Level_Info) = "INFO ",
              "Level_Info not INFO");
      Assert (Get_Level_String (Level_Debug) = "DEBUG",
              "Level_Debug not DEBUG");
   end;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return  new String'("Default_Logger");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Init'Access, "Init");
      Register_Routine (T, Test_Get_Level_String'Access, "Get_Level_String");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      --  the logger is already initialized, see framework_test.adb
      null;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      null;
   end Tear_Down;

end Giant.Default_Logger.Test;
