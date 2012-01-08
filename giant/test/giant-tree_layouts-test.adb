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
--  First Author: Oliver Kopp
--
--  $RCSfile: giant-tree_layouts-test.adb,v $, $Revision: 1.7 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--

with Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Config;
with Giant.Config.Class_Sets;
with Giant.Config.Vis_Styles;
with Giant.Graph_Widgets;
with Giant.Graph_Lib; use Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

with Gtk.Main;

package body Giant.Tree_Layouts.Test is

   type Graph_Data_Record (Len : Natural) is record
      Filename   : String (1..Len);
      Edge_Count : Natural;
      Node_Count : Natural;
   end record;
   type Graph_Data is access constant Graph_Data_Record;

   Rfg_Example : aliased constant Graph_Data_Record :=
     (Len => 23,
      Filename => "resources/rfg_examp.iml",
      Edge_Count => 631,  --  with all: 646
      Node_Count => 202); --  with all: 216

   Graphs : constant array (1..1) of Graph_Data :=
     ( 1 => Rfg_Example'Access );

   --------------------------------------------------------------------------
   --  Index in Graphs of graph to test with
   --  TBD: extention, that all graphs are tested with the same test cases
   Test_Graph_Number : constant := 1;

   --------------------------------------------------------------------------
   package Logger is new Giant.Logger("T:Tree-Layouts");

   --------------------------------------------------------------------------
   Widget : Graph_Widgets.Graph_Widget;

   -----------------
   --  Testcases  --
   -----------------

   ---------------------------------------------------------------------------
   procedure Init (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Gtk.Main.Init;

      Giant.Graph_Lib.Initialize;
      Giant.Graph_Lib.Load (Graphs (Test_Graph_Number).FileName);

      Giant.Config.Vis_Styles.Initialize_Config_Vis_Styles
        ("",
         "",
         "",
         "resources/vis_styles/vis_styles_test_set_1/"
         & "test_vis_style_2.xml");

      Graph_Widgets.Create (Widget);
   end Init;

   procedure Small_Tree (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Sel    : Graph_Lib.Selections.Selection;
      Lock   : Graph_Widgets.Lock_Type;
      Layout : Tree_Layouts.Tree_Layout;
   begin
      --  Generate Selection
      Sel := Graph_Lib.Selections.Create ("");
      Graph_Lib.Selections.Add_Node (Sel, Graph_Lib.Node_Id_Value ("3"));
      Graph_Lib.Selections.Add_Node (Sel, Graph_Lib.Node_Id_Value ("10"));
      Graph_Lib.Selections.Add_Node (Sel, Graph_Lib.Node_Id_Value ("68"));
      Graph_Lib.Selections.Add_Node (Sel, Graph_Lib.Node_Id_Value ("70"));
      Graph_Lib.Selections.Add_Node (Sel, Graph_Lib.Node_Id_Value ("123"));

      Graph_Widgets.Insert_Selection (Widget, Sel, Lock);

      Layout := Tree_Layouts.Initialize
        (Widget              => Widget,
         Widget_Lock         => Lock,
         Selection_To_Layout => Sel,
         Target_Position     => Vis.Logic.Combine_Vector (0.0, 0.0),
         Root_Node           => Graph_Lib.Node_Id_Value ("3"),
         --  q&d -- a destroy of this empty class_set should be called
         Meta_Class_Set_To_Layout =>
           Config.Class_Sets.Build
         (Config.Class_Sets.Class_Sets_Lists.Create),
         Process_Edges_Reverse => False);

      Evolutions.Start_Calculation_Blocked (Layout);

      -- Finish test
      Graph_Lib.Selections.Destroy (Sel);
   end Small_Tree;

   procedure Done (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      --  TBD: destroy widget

      Giant.Graph_Lib.Unload;
      Giant.Graph_Lib.Destroy;
   end Done;

   --------------------------------
   --  Routines from AUnit-Test  --
   --------------------------------

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Layouts-Tree_Layouts");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Init'Access, "Init");
      Register_Routine (T, Small_Tree'Access, "Small Tree");
      Register_Routine (T, Done'Access, "Done");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      null;
   end Tear_Down;

end Giant.Tree_Layouts.Test;
