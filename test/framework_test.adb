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
--  $RCSfile: framework_test.adb,v $, $Revision: 1.18 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Test_Runner;

with Hashed_Mappings_Test;

with Giant.Config.Class_Sets.Test;
with Giant.Config.Test;
with Giant.Config.Vis_Styles.Test;
with Giant.Default_Logger;
with Giant.Default_Logger.Test;
with Giant.File_Management.Test;
with Giant.Graph_Lib.Test;
with Giant.Graph_Lib.Node_Attribute_Filters.Test;
with Giant.Graph_Lib.Subgraphs.Atomic_Tests;
with Giant.Graph_Lib.Subgraphs.Test;
with Giant.Gsl.Test;
with Giant.Gsl_Support.Test;
with Giant.Layout_Factory.Test;
with Giant.Matrix_Layouts.Test;
with Giant.Node_Annotations.Test;
with Giant.Projects.Test;
with Giant.Tree_Layouts.Test;
with Giant.Valid_Names.Test;
with Giant.Vis_Windows.Test;
with Giant.XML_File_Access.Test;

procedure Framework_Test is

   function Default_Suite
     return Access_Test_Suite
   is
      Result : Access_Test_Suite := new Test_Suite;
   begin
      --Add_Test (Result, new );
      Add_Test (Result, new Giant.Config.Class_Sets.Test.Test_Case);
      Add_Test (Result, new Giant.Config.Test.Test_Case);
      Add_Test (Result, new Giant.Config.Vis_Styles.Test.Test_Case);
      Add_Test (Result, new Giant.Default_Logger.Test.Test_Case);
      Add_Test (Result, new Giant.File_Management.Test.Test_Case);
      Add_Test (Result, new Giant.Graph_Lib.Test.Test_Case);
      Add_Test
        (Result, new Giant.Graph_Lib.Node_Attribute_Filters.Test.Test_Case);
      Add_Test (Result, new Giant.Graph_Lib.Subgraphs.Test.Test_Case);
      Add_Test (Result, new Giant.Graph_Lib.Subgraphs.Atomic_Tests.Test_Case);
      Add_Test (Result, new Giant.Gsl.Test.Test_Case);
      Add_Test (Result, new Giant.Gsl_Support.Test.Test_Case);
      Add_Test (Result, new Giant.Layout_Factory.Test.Test_Case);
      Add_Test (Result, new Giant.Matrix_Layouts.Test.Test_Case);
      Add_Test (Result, new Giant.Node_Annotations.Test.Test_Case);
      Add_Test (Result, new Giant.Projects.Test.Test_Case);
      Add_Test (Result, new Giant.Tree_Layouts.Test.Test_Case);
      Add_Test (Result, new Giant.Valid_Names.Test.Test_Case);
      Add_Test (Result, new Giant.Vis_Windows.Test.Test_Case);
      Add_Test (Result, new Giant.XML_File_Access.Test.Test_Case);
      return Result;
   end Default_Suite;

   function Reuse_Suite
     return Access_Test_Suite
   is
      Result : Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, new Hashed_Mappings_Test.Test_Case);
      return Result;
   end Reuse_Suite;

   function Memory_Leak_Suite
     return Access_Test_Suite
   is
      Result : Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, new Giant.XML_File_Access.Test.Test_Case);
      return Result;
   end Memory_Leak_Suite;

   procedure Run is new AUnit.Test_Runner (Default_Suite);

begin
   Giant.Default_Logger.Init ("debug.log");
   Giant.Default_Logger.Info ("Running Tests...");

   Run;

   Giant.Default_Logger.Close;
end Framework_Test;
