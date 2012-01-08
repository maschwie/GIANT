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
--  $RCSfile: giant-xml_file_access-test.adb,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.XML_File_Access;

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.XML_File_Access.Test is

   package Logger is new Giant.Logger("Giant.XML_File_Access.Test");

   ---------------------------------------------------------------------------
   procedure Test_Memory_Leacks
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

      My_XML_Document : Dom.Core.Document;
      My_Tree_Reader  : Tree_Readers.Tree_Reader;
   begin

      for i in 1 .. 50000000 loop
         Giant.XML_File_Access.Load_XML_File_Validated
           ("./resources/node_annotations/node_annotations.xml",
            My_Tree_Reader,
            My_XML_Document);

         Assert (Giant.XML_File_Access.Does_XML_Document_Belong_To_Type
           ("giant_node_annotations_file", My_XML_Document),
            "Check whether detects correct file");
         Assert (not Giant.XML_File_Access.Does_XML_Document_Belong_To_Type
           ("irgendwas", My_XML_Document),
            "Check whether detects wrong file");

         Tree_Readers.Free (My_Tree_Reader);
      end loop;
   end Test_Memory_Leacks;

   ---------------------------------------------------------------------------
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Giant.XML_File_Access.Test - Basic Tests");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Memory_Leacks'Access, "Memory_Leack_Test");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      null;
   end Tear_Down;

end Giant.XML_File_Access.Test;
