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
-- First Author: Martin Schwienbacher
--
-- $RCSfile: giant_test-file_management.adb,v $, $Revision: 1.3 $
-- $Author: squig $
-- $Date: 2003-06-15 12:45:43 $
--
-- ------
-- Used to test the package Giant.File_Management
--
with Ada.Text_IO;

with GNAT.Directory_Operations;

with Giant.File_Management;

procedure Giant_Test.File_Management is

begin

   -- Phase 1
   -- Initialize the Config ADO
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (">>>>>>>> PHASE 1: Teste Subprograms");

   --------------------
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line
     (">>>> Teste: procedure Set_Currunt_Working_Dir_To_Exec_Dir");

   Ada.Text_IO.Put_Line ("Result - Old working dir:");
   Ada.Text_IO.Put_Line  (GNAT.Directory_Operations.Get_Current_Dir);
   Giant.File_Management.Set_Currunt_Working_Dir_To_Exec_Dir;
   Ada.Text_IO.Put_Line ("Result - New working dir:");
   Ada.Text_IO.Put_Line  (GNAT.Directory_Operations.Get_Current_Dir);

   --------------------
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line
     (">>>> Teste: function Get_Absolute_Path_To_Directory_From_Relative");

   Ada.Text_IO.Put_Line ("Result - Abs Dir Path:");
   Ada.Text_IO.Put_Line
     (Giant.File_Management.Get_Absolute_Path_To_Directory_From_Relative
       (GNAT.Directory_Operations.Get_Current_Dir,
       "resources"));

   --------------------
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line
     (">>>> Teste: function Get_Absolute_Path_To_File_From_Relative");

   Ada.Text_IO.Put_Line ("Result - Abs File Path:");
   Ada.Text_IO.Put_Line
     (Giant.File_Management.Get_Absolute_Path_To_File_From_Relative
       (GNAT.Directory_Operations.Get_Current_Dir,
       "resources/rfg_examp.iml"));



end Giant_Test.File_Management;
