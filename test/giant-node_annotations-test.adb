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
--  $RCSfile: giant-node_annotations-test.adb,v $, $Revision: 1.7 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.XML_File_Access;
with Dom.Core;
with Dom.Core.Nodes;
with Tree_Readers;
with Dom.Core.Elements;

with Giant.Graph_Lib;

with Giant.Node_Annotations;

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Node_Annotations.Test is

   package Logger is new Giant.Logger("Giant.Node_Annotations.Test");

   procedure Test_Init
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      Logger.Warn ("=== Running Node Annotaions Tests ===");
   end Test_Init;

   ---------------------------------------------------------------------------
   procedure Test_Memory_Leacks_XML
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

      My_XML_Document : Dom.Core.Document;
      My_Tree_Reader  : Tree_Readers.Tree_Reader;

      My_Annotations : Giant.Node_Annotations.Node_Annotation_Access;
   begin
      for i in 1 .. 1 loop
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

         My_Annotations :=
           Node_Annotations.Load_From_File
             ("resources/node_annotations/node_annotations.xml");

      end loop;
   end Test_Memory_Leacks_XML;


   ---------------------------------------------------------------------------
   procedure Test_Memory_Leack
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

      My_Annotations : Giant.Node_Annotations.Node_Annotation_Access;
   begin

      for I in 1 .. 1 loop
         My_Annotations :=
           Node_Annotations.Load_From_File
             ("resources/node_annotations/node_annotations.xml");

         Node_Annotations.Write_To_File
           (My_Annotations,
            "resources/node_annotations/node_annotations2.xml");

         Node_Annotations.Deallocate (My_Annotations);

      end loop;
   end Test_Memory_Leack;

   ---------------------------------------------------------------------------
   -- Used to figure out the existing node id's that are needed for the
   -- cration of test data.
   procedure Write_IML_Node_Ids_Into_Logger
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

      All_Nodes : Graph_Lib.Node_Id_Set;
      Iter      : Graph_Lib.Node_Id_Sets.Iterator;
   begin

      All_Nodes := Graph_Lib.Get_All_Nodes;
      Iter      := Graph_Lib.Node_Id_Sets.Make_Iterator (All_Nodes);

      while Graph_Lib.Node_Id_Sets.More (Iter) loop

         Logger.Debug
           ("Node Id Image from IML_Graph: """
            & Graph_Lib.Node_Id_Image
              (Graph_Lib.Node_Id_Sets.Current (Iter))
            & """");

         Graph_Lib.Node_Id_Sets.Next (Iter);
      end loop;

      Graph_Lib.Node_Id_Sets.Destroy (Iter);
   end Write_IML_Node_Ids_Into_Logger;

   ---------------------------------------------------------------------------
   procedure Test_File_Loading
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

      My_Annotations : Giant.Node_Annotations.Node_Annotation_Access;
   begin

      My_Annotations :=
        Node_Annotations.Load_From_File
          ("resources/node_annotations/node_annotations.xml");

      Node_Annotations.Write_To_File
        (My_Annotations,
         "resources/node_annotations/node_annotations_generated_1.xml");

      Node_Annotations.Deallocate (My_Annotations);
   end Test_File_Loading;

   ---------------------------------------------------------------------------
   procedure Test_Annotations_Status
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

     Node_Id_18 : Giant.Graph_Lib.Node_Id := Graph_Lib.Node_Id_Value ("18");
     -- Node_Id_3 is already annotated (annotation loaded from file)
     Node_Id_3 : Giant.Graph_Lib.Node_Id := Graph_Lib.Node_Id_Value ("3");

     My_Annotations : Giant.Node_Annotations.Node_Annotation_Access;
   begin

      My_Annotations :=
        Node_Annotations.Load_From_File
          ("resources/node_annotations/node_annotations.xml");

      Assert
        (Node_Annotations.Is_Annotated (My_Annotations, Node_Id_3),
         "Node with Id ""3"" is annotated.");

      Assert
        (Node_Annotations.Get_Annotation_Text (My_Annotations, Node_Id_3)
         = "I am an existing Node.",
         "Node with Id ""3"" annotation text correct annotated.");

      Assert
        (not Node_Annotations.Is_Annotated (My_Annotations, Node_Id_18),
        "Node with Id ""18"" is not annotated.");

      Node_Annotations.Deallocate (My_Annotations);
   end Test_Annotations_Status;


   ---------------------------------------------------------------------------
   procedure Test_Editing_Annotations
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

     Node_Id_18 : Giant.Graph_Lib.Node_Id := Graph_Lib.Node_Id_Value ("18");

     Node_Id_3 : Giant.Graph_Lib.Node_Id := Graph_Lib.Node_Id_Value ("3");
     Node_Id_4 : Giant.Graph_Lib.Node_Id := Graph_Lib.Node_Id_Value ("4");
     Node_Id_7 : Giant.Graph_Lib.Node_Id := Graph_Lib.Node_Id_Value ("7");

     My_Annotations : Giant.Node_Annotations.Node_Annotation_Access;

     Annotated_Nodes_List : Graph_Lib.Node_Id_Lists.List;

     Test_String : String (1 .. 2);
   begin

      --  Build new file
      ------------------
      My_Annotations :=
        Node_Annotations.Create_Empty;

      Add_Node_Annotation (My_Annotations, Node_Id_18 , "I am node XXX18");
      Add_Node_Annotation (My_Annotations, Node_Id_3 , "I am node 3");
      Add_Node_Annotation (My_Annotations, Node_Id_4, "I am node 4");


      begin
         Add_Node_Annotation (My_Annotations, Node_Id_18 , "I am node XXX18");
         Assert (False, "Test Exception Add_Node_Annotation "
           & "-  Node_Is_Already_Annoated_Exceptionnot not thrown");
      exception
         when Node_Is_Already_Annoated_Exception =>
           Assert (True, "Test Exception Add_Node_Annotation "
           & "-  Node_Is_Already_Annoated_Exceptionnot");
      end;


      begin
         Change_Node_Annotation (My_Annotations, Node_Id_7 , "I am node 7");
         Assert (False, "Test Exception Change_Node_Annotation "
           & "-  Node_Is_Not_Annotated_Exception not thrown");
      exception
         when Node_Is_Not_Annotated_Exception =>
           Assert (True, "Test Exception Change_Node_Annotation "
           & "-  Node_Is_Not_Annotated_Exception");
      end;

      begin
         Remove_Node_Annotation (My_Annotations, Node_Id_7);
         Assert (False, "Test Exception Remove_Node_Annotation "
           & "-  Node_Is_Not_Annotated_Exception not thrown");
      exception
         when Node_Is_Not_Annotated_Exception =>
           Assert (True, "Test Exception Remove_Node_Annotation "
           & "-  Node_Is_Not_Annotated_Exception");
      end;

      Add_Node_Annotation    (My_Annotations, Node_Id_7 , "I am node 7");
      Change_Node_Annotation (My_Annotations, Node_Id_18 , "I am node 18");

      Remove_Node_Annotation (My_Annotations, Node_Id_3);

      -- Test Status
      --------------
      Assert (not Is_Annotated (My_Annotations, Node_Id_3),
              "Test Removal of node id ""3"".");
      begin
         Test_String :=
           Get_Annotation_Text (My_Annotations, Node_Id_3) (1..2);
         Assert (False, "Test Exception Get_Annotation_Text "
           & "-  Node_Is_Not_Annotated_Exception not thrown");
      exception
         when Node_Is_Not_Annotated_Exception =>
           Assert (True, "Test Exception Get_Annotation_Text "
           & "-  Node_Is_Not_Annotated_Exception");
      end;

      Assert(Get_Annotation_Text (My_Annotations, Node_Id_18) = "I am node 18",
             "Test Annotation Text Node 18");

      Assert(Get_Annotation_Text (My_Annotations, Node_Id_4) = "I am node 4",
             "Test Annotation Text Node 4");

      Assert(Get_Annotation_Text (My_Annotations, Node_Id_7) = "I am node 7",
             "Test Annotation Text Node 7");

   end Test_Editing_Annotations;

   ---------------------------------------------------------------------------
   procedure Test_Lists_Generation
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

     Node_Id_18 : Giant.Graph_Lib.Node_Id := Graph_Lib.Node_Id_Value ("18");

     Node_Id_3 : Giant.Graph_Lib.Node_Id := Graph_Lib.Node_Id_Value ("3");
     Node_Id_4 : Giant.Graph_Lib.Node_Id := Graph_Lib.Node_Id_Value ("4");
     Node_Id_7 : Giant.Graph_Lib.Node_Id := Graph_Lib.Node_Id_Value ("7");

     My_Annotations : Giant.Node_Annotations.Node_Annotation_Access;

     Annotated_Nodes_List : Graph_Lib.Node_Id_Lists.List;

   begin

      --  Build new type
      ------------------
      My_Annotations :=
        Node_Annotations.Create_Empty;

      Add_Node_Annotation (My_Annotations, Node_Id_18 , "I am node 18");
      Add_Node_Annotation (My_Annotations, Node_Id_3 , "I am node 3");
      Add_Node_Annotation (My_Annotations, Node_Id_4, "I am node 4");
      Add_Node_Annotation (My_Annotations, Node_Id_7, "I am node 7");

      -- Test List Output
      -------------------
      Annotated_Nodes_List := Get_All_Annotated_Nodes (My_Annotations);

      Assert (Graph_Lib.Node_Id_Lists.Length (Annotated_Nodes_List) = 4,
              "Test Length of List over all annotated nodes");

      Assert (Graph_Lib.Node_Id_Lists.IsInList
        (Annotated_Nodes_List, Node_Id_18), "Test Node Id 18 in List");
      Assert (Graph_Lib.Node_Id_Lists.IsInList
        (Annotated_Nodes_List, Node_Id_3), "Test Node Id 3 in List");
      Assert (Graph_Lib.Node_Id_Lists.IsInList
        (Annotated_Nodes_List, Node_Id_4), "Test Node Id 4 in List");
      Assert (Graph_Lib.Node_Id_Lists.IsInList
        (Annotated_Nodes_List, Node_Id_7), "Test Node Id 7 in List");

      Graph_Lib.Node_Id_Lists.Destroy (Annotated_Nodes_List);

      Node_Annotations.Deallocate (My_Annotations);
   end Test_Lists_Generation;

   ---------------------------------------------------------------------------
   procedure Test_Iterator
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

     Node_Id_18 : Giant.Graph_Lib.Node_Id := Graph_Lib.Node_Id_Value ("18");

     Node_Id_3 : Giant.Graph_Lib.Node_Id := Graph_Lib.Node_Id_Value ("3");


     My_Annotations : Giant.Node_Annotations.Node_Annotation_Access;

     The_Node_ID_Iter : Node_ID_Iter;
     Test_Node : Graph_Lib.Node_Id;
     Count : Integer;

   begin

      --  Build new type
      ------------------
      My_Annotations :=
        Node_Annotations.Create_Empty;

      Add_Node_Annotation (My_Annotations, Node_Id_18 , "I am node 18");
      Add_Node_Annotation (My_Annotations, Node_Id_3 , "I am node 3");

      -- Test Iterator
      -------------------
      The_Node_ID_Iter := Make_Node_ID_Iter (My_Annotations);

      Count := 0;
      while More (The_Node_ID_Iter) loop
        Next (The_Node_ID_Iter, Test_Node);
        Count := Count + 1;
      end loop;

      Assert (Count = 2, "Test correct ammount of iterated elements.");

      Node_Annotations.Deallocate (My_Annotations);
   end Test_Iterator;

   ---------------------------------------------------------------------------
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Giant.Node_Annotations.Test - Basic Tests");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Init'Access, "Test_Init");

      --  Register_Routine
      --    (T, Test_Memory_Leacks_XML'Access, "Test_Memory_Leacks_XML");


      Register_Routine
        (T, Test_Memory_Leack'Access, "Test_Memory_Leack");

      --  Register_Routine
      --    (T, Write_IML_Node_Ids_Into_Logger'Access,
      --     "Reading_Node_Id_Images_From_IML_Graph");
      Register_Routine
        (T, Test_File_Loading'Access, "Test_File_Loading");

      Register_Routine
        (T, Test_Annotations_Status'Access, "Test_Annotations_Status");

      Register_Routine
        (T, Test_Editing_Annotations'Access, "Test_Editing_Annotations");

      Register_Routine
        (T, Test_Lists_Generation'Access, "Test_Lists_Generation");

      Register_Routine
        (T, Test_Iterator'Access, "Test_Iterator");

   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      Giant.Graph_Lib.Initialize;
      Giant.Graph_Lib.Load ("resources/rfg_examp.iml");
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      Giant.Graph_Lib.Unload;
      Giant.Graph_Lib.Destroy;
   end Tear_Down;

end Giant.Node_Annotations.Test;
