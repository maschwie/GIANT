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
--  $RCSfile: giant-config-class_sets-test.adb,v $, $Revision: 1.11 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
with Ada.Strings.Unbounded;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Graph_Lib;

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Config.Class_Sets.Test is

   package Logger is new Giant.Logger("giant.config.class_sets.test");

   ---------------------------------------------------------------------------
   --  test of parent_edge_class_set.xml
   --    copied from shared/ to test on 2003-10-01
   procedure Parent_Edge
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Parent_Edge_Set     : Config.Class_Sets.Class_Set_Access;
      An_Edge_Class       : Giant.Graph_Lib.Edge_Class_Id;

   begin
      Logger.Info ("Case: Parent_Edge");

      Config.Class_Sets.Initialize_Class_Sets
        ("resources/class_sets/");

      Parent_Edge_Set :=
        Config.Class_Sets.Get_Class_Set_Access ("parent_edge_class_set");

      An_Edge_Class :=
        GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
        (Graph_Lib.Convert_Node_Class_Name_To_Id ("IML_Root"),
         Graph_Lib.Convert_Node_Attribute_Name_To_Id
         ("IML_Root", "Parent"));
      Assert
        (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
         (Parent_Edge_Set, An_Edge_Class),
         "holds edge class ""IML_Root.Parent""");

      Config.Class_Sets.Clear_Class_Sets;

      Logger.Info ("End of case: Parent_Edge");
   end Parent_Edge;

   procedure All_Nodes
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

      All_Nodes_Set : Config.Class_Sets.Class_Set_Access;
      A_Node_Class  : Giant.Graph_Lib.Node_Class_Id;

   begin
      Config.Class_Sets.Initialize_Class_Sets
        ("resources/class_sets/");

      All_Nodes_Set :=
        Config.Class_Sets.Get_Class_Set_Access ("all_nodes");

      A_Node_Class := Graph_Lib.Convert_Node_Class_Name_To_Id ("IML_Root");
      Assert
        (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
         (All_Nodes_Set, A_Node_Class),
         "All_Nodes contain node class ""IML_Root""");
      Config.Class_Sets.Clear_Class_Sets;
   end All_Nodes;

   ---------------------------------------------------------------------------
   procedure Test_Init_Class_Sets
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

      All_Class_Sets_List : String_Lists.List;

      Set_1 :  Config.Class_Sets.Class_Set_Access;
      Set_2 :  Config.Class_Sets.Class_Set_Access;
      Set_3 :  Config.Class_Sets.Class_Set_Access;
      Set_4 :  Config.Class_Sets.Class_Set_Access;

      A_Node_Class : Giant.Graph_Lib.Node_Class_id;
      An_Edge_Class : Giant.Graph_Lib.Edge_Class_Id;
   begin

      for i in 1 .. 1 loop

         Config.Class_Sets.Initialize_Class_Sets
           ("resources/class_sets/test_class_sets_1/");

         All_Class_Sets_List :=
           Config.Class_Sets.Get_All_Existing_Class_Sets;
         Assert
           (String_Lists.Length (All_Class_Sets_List) = 5,
            "Test correct number of class sets loaded");
         String_Lists.Destroy (All_Class_Sets_List);


         -- Test single class set status
         Set_1 := Config.Class_Sets.Get_Class_Set_Access ("class_set_1");
         Set_2 := Config.Class_Sets.Get_Class_Set_Access ("class_set_2");
         Set_3 := Config.Class_Sets.Get_Class_Set_Access ("class_set_3");
         Set_4 := Config.Class_Sets.Get_Class_Set_Access ("class_set_4");

         -- class_set_1
         Assert
           (Config.Class_Sets.Is_Empty (Set_1),
            "Tests whether class_set_1 is empty");

         -- class_set_2
         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("IML_Root");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Set_2, A_Node_Class),
           "Test class_set_2 holds node class ""IML_Root""");

         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("HPGNode");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Set_2, A_Node_Class),
           "Test class_set_2 holds node class ""HPGNode""");

         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("SymNode");
         Assert
           (not Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Set_2, A_Node_Class),
           "Test class_set_2 not holds node class ""SymNode""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("IML_Root"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("IML_Root", "Parent"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_2, An_Edge_Class),
           "Test class_set_2 holds edge class ""IML_Root.Parent""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("HPGNode"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("HPGNode", "Parent"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_2, An_Edge_Class),
           "Test class_set_2 holds edge class ""HPGNode.Parent""");

         -- class_set_3
         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("SymNode");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Set_3, A_Node_Class),
           "Test class_set_3 holds node class ""SymNode""");

         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Entity");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Set_3, A_Node_Class),
           "Test class_set_3 holds node class ""OC_Entity""");

        An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("HPGNode"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("HPGNode", "Parent"));
         Assert
           (not Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_3, An_Edge_Class),
           "Test class_set_3 not holds edge class ""HPGNode.Parent""");

         -- class_set_4
          A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("T_Node");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Set_4, A_Node_Class),
           "Test class_set_4 holds node class ""T_Node""");

         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("O_Node");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Set_4, A_Node_Class),
           "Test class_set_4 holds node class ""O_Node""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("T_Node"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("T_Node", "Parent"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_4, An_Edge_Class),
           "Test class_set_4 holds edge class ""T_Node.Parent""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("O_Node"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("O_Node", "Parent"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_4, An_Edge_Class),
           "Test class_set_4 holds edge class ""O_Node.Parent""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Component"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("OC_Component", "Its_Type"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_4, An_Edge_Class),
           "Test class_set_4 holds edge class ""OC_Component.Its_Type""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Enum"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("OC_Enum", "Its_Type"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_4, An_Edge_Class),
           "Test class_set_4 holds edge class ""OC_Enum.Its_Type""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Enum"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("OC_Enum", "Parent"));
         Assert
           (not Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_4, An_Edge_Class),
            "Test class_set_4 not holds edge class ""OC_Enum.Parent""");

         -- deallocate
         Config.Class_Sets.Clear_Class_Sets;
      end loop;

   end Test_Init_Class_Sets;

   ---------------------------------------------------------------------------
   procedure Test_Meta_Class_Set_Build
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

      All_Class_Sets_List : String_Lists.List;

      S_Set_1 : Config.Class_Sets.Class_Set_Access;
      S_Set_2 : Config.Class_Sets.Class_Set_Access;
      S_Set_3 : Config.Class_Sets.Class_Set_Access;
      S_Set_4 : Config.Class_Sets.Class_Set_Access;

      Meta_1  : Config.Class_Sets.Meta_Class_Set_Access;
      Meta_E1 : Config.Class_Sets.Meta_Class_Set_Access;
      Meta_E2 : Config.Class_Sets.Meta_Class_Set_Access;

      Empty_Class_Set_List : Class_Sets_Lists.List;

      A_Node_Class : Giant.Graph_Lib.Node_Class_id;
      An_Edge_Class : Giant.Graph_Lib.Edge_Class_Id;
   begin

      Config.Class_Sets.Initialize_Class_Sets
        ("resources/class_sets/test_class_sets_1/");

      -- Test single class set status
      S_Set_1 := Config.Class_Sets.Get_Class_Set_Access ("class_set_1");
      S_Set_2 := Config.Class_Sets.Get_Class_Set_Access ("class_set_2");
      S_Set_3 := Config.Class_Sets.Get_Class_Set_Access ("class_set_3");
      S_Set_4 := Config.Class_Sets.Get_Class_Set_Access ("class_set_4");

      Empty_Class_Set_List := Class_Sets_Lists.Create;

      for i in 1 .. 1 loop

         Meta_1 := Config.Class_Sets.Build
           (Elements => (S_Set_1, S_Set_2, S_Set_3, S_Set_4));

         Meta_E1 := Config.Class_Sets.Build
           (Elements => Class_Set_Array'(1 .. 1 => S_Set_1));

         Meta_E2 :=  Config.Class_Sets.Build (Empty_Class_Set_List);


         -- Test empty meta class sets
         -----------------------------

         Assert
           (Config.Class_Sets.Is_Empty (Meta_E1),
            "Test whether Meta_E1 empty");

         Assert
           (Config.Class_Sets.Is_Empty (Meta_E2),
            "Test whether Meta_E2 empty");

         -- Test Content of Meta_1
         --------------------------
         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("IML_Root");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Meta_1, A_Node_Class),
           "Test Meta_1 holds node class ""IML_Root""");

         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("HPGNode");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Meta_1, A_Node_Class),
           "Test Meta_1 holds node class ""HPGNode""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("IML_Root"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("IML_Root", "Parent"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Meta_1, An_Edge_Class),
           "Test Meta_1 holds edge class ""IML_Root.Parent""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("HPGNode"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("HPGNode", "Parent"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Meta_1, An_Edge_Class),
           "Test Meta_1 holds edge class ""HPGNode.Parent""");

         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("SymNode");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Meta_1, A_Node_Class),
           "Test Meta_1 holds node class ""SymNode""");

         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Entity");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Meta_1, A_Node_Class),
           "Test Meta_1 holds node class ""OC_Entity""");

         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("T_Node");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Meta_1, A_Node_Class),
           "Test Meta_1 holds node class ""T_Node""");

         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("O_Node");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Meta_1, A_Node_Class),
           "Test Meta_1 holds node class ""O_Node""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("T_Node"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("T_Node", "Parent"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Meta_1, An_Edge_Class),
           "Test Meta_1 holds edge class ""T_Node.Parent""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("O_Node"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("O_Node", "Parent"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Meta_1, An_Edge_Class),
           "Test Meta_1 holds edge class ""O_Node.Parent""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Component"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("OC_Component", "Its_Type"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Meta_1, An_Edge_Class),
           "Test Meta_1 holds edge class ""OC_Component.Its_Type""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Enum"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("OC_Enum", "Its_Type"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Meta_1, An_Edge_Class),
           "Test Meta_1 holds edge class ""OC_Enum.Its_Type""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Enum"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("OC_Enum", "Parent"));
         Assert
           (not Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Meta_1, An_Edge_Class),
           "Test Meta_1 not holds edge class ""OC_Enum.Parent""");

         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Record");
         Assert
           (not Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Meta_1, A_Node_Class),
           "Test Meta_1 not holds node class ""OC_Record""");

         Config.Class_Sets.Destroy (Meta_1);
         Config.Class_Sets.Destroy (Meta_E1);
         Config.Class_Sets.Destroy (Meta_E2);
      end loop;

      Config.Class_Sets.Clear_Class_Sets;
      Class_Sets_Lists.Destroy (Empty_Class_Set_List);
   end Test_Meta_Class_Set_Build;


   ---------------------------------------------------------------------------
   procedure Test_Inheritance
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Set_Inherit  :  Config.Class_Sets.Class_Set_Access;

      A_Node_Class : Giant.Graph_Lib.Node_Class_id;
      An_Edge_Class : Giant.Graph_Lib.Edge_Class_Id;
   begin

      for i in 1 .. 1 loop

         Config.Class_Sets.Initialize_Class_Sets
           ("resources/class_sets/test_class_sets_1/");

         Set_Inherit :=
           Config.Class_Sets.Get_Class_Set_Access
             ("class_set_5_inheritance");

         -- test node classes - inheritance
         ----------------------------------

         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("T_Node");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Set_Inherit, A_Node_Class),
           "Test Set_Inherit holds node class ""T_Node""");

         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("Prog_Unit");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Set_Inherit, A_Node_Class),
           "Test Set_Inherit holds node class ""Prog_Unit""");

         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("O_Node");
         Assert
           (not Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Set_Inherit, A_Node_Class),
           "Test Set_Inherit not holds node class ""O_Node""");

         -- inherited from "Prog_Unit"
         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("Unit");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Set_Inherit, A_Node_Class),
           "Test Set_Inherit holds node class ""Unit""");

         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("Routine");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Set_Inherit, A_Node_Class),
           "Test Set_Inherit holds node class ""Routine""");

         -- inherited from "T_Node"
         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("TC_Completed_Array");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Set_Inherit, A_Node_Class),
           "Test Set_Inherit holds node class ""TC_Completed_Array""");

         A_Node_Class :=
           Graph_Lib.Convert_Node_Class_Name_To_Id ("TC_Record");
         Assert
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
             (Set_Inherit, A_Node_Class),
           "Test Set_Inherit holds node class ""TC_Record""");

         -- test edge classes - inheritance
         ----------------------------------

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("IML_Root"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("IML_Root", "Parent"));
         Assert
           (not Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_Inherit, An_Edge_Class),
           "Test holds edge class ""IML_Root.Parent""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("Prog_Unit"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("Prog_Unit", "Subunits"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_Inherit, An_Edge_Class),
           "Test holds edge class ""Prog_Unit.Subunits""");

         -- inherited from "Prog_Unit.Subunits"
         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("Routine"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("Routine", "Subunits"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_Inherit, An_Edge_Class),
           "Test holds edge class ""Routine.Subunits""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("Routine"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("Routine", "Symbol_Table"));
         Assert
           (not Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_Inherit, An_Edge_Class),
           "Test not holds edge class ""Routine.Symbol_Table""");

         -- inherited from "Op."*""
         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("Op"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("Op", "Parent"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_Inherit, An_Edge_Class),
           "Test holds edge class ""Op.Parent""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("Op"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("Op", "CF_Basic_Block"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_Inherit, An_Edge_Class),
           "Test holds edge class ""Op.CF_Basic_Block""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("Explicit_Conversion"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("Explicit_Conversion", "Source_Type"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_Inherit, An_Edge_Class),
           "Test holds edge class "
           & """Explicit_Conversion.Source_Type""");

         -- inherited from "Return_Statement."*""
         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("Return_Statement"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("Return_Statement", "EType"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_Inherit, An_Edge_Class),
           "Test holds edge class "
           & """Return_Statement.EType""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("Return_With_Value"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("Return_With_Value", "EType"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_Inherit, An_Edge_Class),
           "Test holds edge class "
           & """Return_With_Value.EType""");

         An_Edge_Class :=
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("Return_With_Value"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id
               ("Return_With_Value", "Return_Value"));
         Assert
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
             (Set_Inherit, An_Edge_Class),
           "Test holds edge class "
           & """Return_With_Value.Return_Value""");

         -- deallocate
         Config.Class_Sets.Clear_Class_Sets;
      end loop;

   end Test_Inheritance;

   ---------------------------------------------------------------------------
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Config.Class_Sets.Test - Basic Tests");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine
        (T, All_Nodes'Access, "All_Nodes");
      Register_Routine
        (T, Parent_Edge'Access, "Parent_Edge");
      Register_Routine
        (T, Test_Init_Class_Sets'Access, "Test_Init_Class_Sets");
      Register_Routine
        (T, Test_Meta_Class_Set_Build'Access, "Test_Meta_Class_Set_Build");
      Register_Routine
        (T, Test_Inheritance'Access, "Test_Inheritance");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      Giant.Graph_Lib.Initialize;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      Giant.Graph_Lib.Destroy;
   end Tear_Down;

end Giant.Config.Class_Sets.Test;
