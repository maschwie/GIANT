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
--  $RCSfile: giant-graph_lib-test.adb,v $, $Revision: 1.20 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--

with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Graph_Lib; use Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Graph_Lib.Test is

   --------------------------------------------------------------------------
   --  Parameters for the graph to use for testing

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

   Httpd : aliased constant Graph_Data_Record :=
     (Len => 19,
      Filename => "resources/httpd.iml",
      Edge_Count => 2108289,  --  with all: 2316410
      Node_Count => 790020);  --  with all: 797777

   Wget : aliased constant Graph_Data_Record :=
     (Len => 18,
      Filename => "resources/wget.iml",
      Edge_Count => 1130446, --  with all: 1274794
      Node_Count => 465457); --  with all: 472583

   Concept_Analysis : aliased constant Graph_Data_Record :=
     (Len => 30,
      Filename => "resources/concept_analysis.iml",
      Edge_Count => 141758, -- with all: 156072
      Node_Count => 53398); -- with all:

   --  all graphs known
   --  ordered (by hand) by size
   Graphs : constant array (1..4) of Graph_Data :=
     ( Rfg_Example'Access,
       Concept_Analysis'Access,
       Wget'Access,
       Httpd'Access );

   --------------------------------------------------------------------------
   --  Index in Graphs of graph to test with
   --  TBD: extention, that all graphs are tested with the same test cases
   Test_Graph_Number : constant := 1;

   -------------------------------------
   --  Global variables and routines  --
   -------------------------------------

   package Logger is new Giant.Logger("T:graph_lib");

   Root : Node_Id;

   ---------------------------------------------------------------------------
   procedure Output_Attributes (Node : in Node_Id) is
      Iter : Node_Attribute_Iterator;
      Attr : Node_Attribute_Id;
   begin
      Iter := Make_Attribute_Iterator (Node);
      while More (Iter) loop
         Next (Iter, Attr);

         Logger.Debug
           ("Attribute " & Convert_Node_Attribute_Id_To_Name (Attr));

         case Get_Node_Attribute_Class_Id (Attr) is
            when Class_Node_Id =>
               null;
            when Class_SLoc =>
               null;
            when others =>
               null;
         end case;

      end loop;
   end Output_Attributes;

   -----------------
   --  Testcases  --
   -----------------

   ---------------------------------------------------------------------------
   --  Init of the graph
   procedure Init (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Giant.Graph_Lib.Initialize;
      Giant.Graph_Lib.Load (Graphs (Test_Graph_Number).FileName);

      Root := Get_Root_Node;
   end Init;

   ---------------------------------------------------------------------------
   procedure Output_RootNode (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Logger.Debug ("Root Node with ID: " & Node_Id_Image (Root));
      Output_Attributes (Root);
   end Output_RootNode;

   ---------------------------------------------------------------------------
   --  Looks for a node with enums
   procedure Enums
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      All_Nodes : Node_Id_Sets.Set;
      Node_Iter : Node_Id_Sets.Iterator;
      Cur_Node  : Node_Id;
      Attr_Iter : Node_Attribute_Iterator;
      Cur_Attr  : Node_Attribute_Id;

   begin
      All_Nodes := Get_All_Nodes;
      Node_Iter := Node_Id_Sets.Make_Iterator (All_Nodes);

      while Node_Id_Sets.More (Node_Iter) loop
         Node_Id_Sets.Next (Node_Iter, Cur_Node);

         --  Logger.Debug (Node_Id_Image (Cur_Node));

         --  TBD: find out how to inspect ada's interal class
         --    Logger.Debug (Cur_Node.IML_Node'External_Tag);

         --  crashed here - constrainterror at graphlib
         --    because edge_targets with non-IML_Roots were imported
         --  Logger.Debug ("  Class: " &
         --              Get_Node_Class_Tag (Get_Node_Class_Id (Cur_Node)));

         Attr_Iter := Make_Attribute_Iterator (Cur_Node);

         while More (Attr_Iter) loop
            Next (Attr_Iter, Cur_Attr);

            if Cur_Attr.all in IML_Reflection.Enumerator_Field'Class then
               declare
                  Enum       : IML_Reflection.Enumerator_Field :=
                    IML_Reflection.Enumerator_Field (Cur_Attr.all);
                  Enum_Value : Natural;
               begin
                  Logger.Debug ("Enum " & Enum.Name & " @ " &
                                Node_Id_Image (Cur_Node));
                  Enum_Value := Enum.Get_Value (Cur_Node.IML_Node);
                  Logger.Debug (Integer'Image (Enum_Value) & " " &
                                Enum.Type_Id.Enumerators (Enum_Value).all);
               end;
            end if;

         end loop;

      end loop;

      Node_Id_Sets.Destroy (All_Nodes);
   end Enums;

   ---------------------------------------------------------------------------
   procedure Check_Counts (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      --  check count directly
      --  n/a, since edges are not referenced directly

      Logger.Debug ("Edges: " & Integer'Image (Get_Edge_Count));
      Logger.Debug ("Nodes: " & Integer'Image (Get_Node_Count));

      Assert (Get_Edge_Count =
              Graphs (Test_Graph_Number).Edge_Count, "Edge_Count");
      Assert (Get_Node_Count =
              Graphs (Test_Graph_Number).Node_Count, "Node_Count");

      --  check count indirectly
      declare
         Set : Edge_Id_Set;
      begin
         Set := Giant.Graph_Lib.Get_All_Edges;
         Assert (Edge_Id_Sets.Size (Set) =
                 Graphs (Test_Graph_Number).Edge_Count, "All_Edges.Size");
         Edge_Id_Sets.Destroy (Set);
      end;

      --  check count directly
      Assert (IML_Node_ID_Hashed_Mappings.Size (IML_Node_ID_Mapping) =
              Graphs (Test_Graph_Number).Node_Count,
              "All_Nodes.Size (via Mapping)");

      --  check count indirectly
      declare
         Set : Node_Id_Set;
      begin
         Set := Giant.Graph_Lib.Get_All_Nodes;
         Logger.Debug ("Size: " & Integer'Image (Node_Id_Sets.Size (Set)));
         Assert (Node_Id_Sets.Size (Set) =
                 Graphs (Test_Graph_Number).Node_Count, "All_Nodes.Size");
         Node_Id_Sets.Destroy (Set);
      end;

   end Check_Counts;

   ---------------------------------------------------------------------------
   --  TBD: find out for what this test was meant for
   procedure Edge_Set_Test (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      null;
   end Edge_Set_Test;

   ---------------------------------------------------------------------------
   --  Tests if source and target of all edges are valid
   procedure Edge_Properties (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      All_Graph_Edges    : Edge_Id_Set;
      All_Incoming_Edges : Edge_Id_Set;
      All_Outgoing_Edges : Edge_Id_Set;

      procedure Execute_Incoming (Node : in Node_Id)
      is
         Incoming_Edges : Edge_Id_Array := Get_Incoming_Edges (Node);
      begin
         for I in Incoming_Edges'Range loop
            Assert (Get_Target_Node (Incoming_Edges (I)) = Node,
                    "Target node does not match");
            Edge_Id_Sets.Remove (All_Graph_Edges, Incoming_Edges (I));
            Edge_Id_Sets.Insert (All_Incoming_Edges, Incoming_Edges (I));
         end loop;
      end Execute_Incoming;

      procedure Apply_Incoming is new Node_Id_Sets.Apply
        (Execute => Execute_Incoming);

      procedure Execute_Outgoing (Node : in Node_Id)
      is
         Outgoing_Edges : Edge_Id_Array := Get_Outgoing_Edges (Node);
      begin
         for I in Outgoing_Edges'Range loop
            Assert (Get_Source_Node (Outgoing_Edges (I)) = Node,
                    "Source node does not match");
            Edge_Id_Sets.Remove (All_Incoming_Edges, Outgoing_Edges (I));
            Edge_Id_Sets.Insert (All_Outgoing_Edges, Outgoing_Edges (I));
         end loop;
      end Execute_Outgoing;

      procedure Apply_Outgoing is new Node_Id_Sets.Apply
        (Execute => Execute_Outgoing);

      All_Nodes             : Node_Id_Set;
      All_Graph_Edges_Count : Natural;
   begin
      All_Nodes := Get_All_Nodes;
      All_Graph_Edges := Get_All_Edges;
      All_Incoming_Edges := Edge_Id_Sets.Empty_Set;
      All_Outgoing_Edges := Edge_Id_Sets.Empty_Set;

      All_Graph_Edges_Count := Edge_Id_Sets.Size (All_Graph_Edges);

      Apply_Incoming (All_Nodes);

      --  All_Graph_Edges has to be empty, since all edges were moved
      --    to All_Incoming_Edges
      Assert (Edge_Id_Sets.Is_Empty (All_Graph_Edges),
              "Not all edges are incoming edges");

      Apply_Outgoing (All_Nodes);

      --  All_Incoming_Edges has to be empty, since all edges were moved
      --    to All_Outgoing_Edges
      Assert (Edge_Id_Sets.Is_Empty (All_Incoming_Edges),
              "Not all incoming edges are outgoing");
      Assert (Edge_Id_Sets.Size (All_Outgoing_Edges) = All_Graph_Edges_Count,
              "Not all graph-edges are outgoing edges");

      Node_Id_Sets.Destroy (All_Nodes);
      Edge_Id_Sets.Destroy (All_Graph_Edges);
      Edge_Id_Sets.Destroy (All_Incoming_Edges);
      Edge_Id_Sets.Destroy (All_Outgoing_Edges);
   end Edge_Properties;

   ---------------------------------------------------------------------------
   --  Could be implemented more efficiently
   --    a) traverse All_Nodes "by hand"
   --         don't use function Get_All_Nodes, but travel through the hashmap
   --         directly
   procedure All_Edges_Test (R : in out AUnit.Test_Cases.Test_Case'Class)
   is

      function Get_All_Edges_Indirectly return Edge_Id_Set
      is
         All_Nodes : Node_Id_Sets.Set;
         Iter      : Node_Id_Sets.Iterator;
         Cur_Node  : Node_Id;
         Result    : Edge_Id_Sets.Set;
      begin
         Result := Edge_Id_Sets.Empty_Set;

         All_Nodes := Get_All_Nodes;

         Iter := Node_Id_Sets.Make_Iterator (All_Nodes);

         while Node_Id_Sets.More (Iter) loop
            Node_Id_Sets.Next (Iter, Cur_Node);

            Edge_Id_Sets.Union (Result, Get_Outgoing_Edges (Cur_Node));
         end loop;

         Node_Id_Sets.Destroy (All_Nodes);

         return Result;
      end Get_All_Edges_Indirectly;

      All_Edges_Indirectly : Edge_Id_Sets.Set;
      All_Edges_Directly   : Edge_Id_Sets.Set;

   begin
      All_Edges_Indirectly := Get_All_Edges_Indirectly;
      All_Edges_Directly   := Get_All_Edges;

      Assert (Edge_Id_Sets."=" (All_Edges_Directly, All_Edges_Indirectly),
              "Edges aren't matching");

      Edge_Id_Sets.Destroy (All_Edges_Directly);
      Edge_Id_Sets.Destroy (All_Edges_Indirectly);
   end All_Edges_Test;

   ---------------------------------------------------------------------------
   procedure Check_All_Node_Classes_Consistency
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is

      ------------------------------------------------------------------------
      function Get_Via_Hashmap return Node_Class_Id_Sets.Set
      is
         Set      : Node_Class_Id_Set;

         Iter     : Node_Class_Id_Hashed_Mappings.Keys_Iter;
         CurClass : Node_Class_Id;

      begin
         Set  := Node_Class_Id_Sets.Empty_Set;
         Iter := Node_Class_Id_Hashed_Mappings.Make_Keys_Iter
           (Node_Class_Id_Mapping);

         while Node_Class_Id_Hashed_Mappings.More (Iter) loop
            Node_Class_Id_Hashed_Mappings.Next (Iter, CurClass);
            Node_Class_Id_Sets.Insert (Set, CurClass);
         end loop;

         return Set;
      end Get_Via_Hashmap;

      Via_Hashmap : Node_Class_Id_Sets.Set;
      Via_Lib     : Node_Class_Id_Sets.Set;

   begin
      Via_Hashmap := Get_Via_Hashmap;
      Via_Lib     := Get_All_Node_Class_Ids;

      Assert (Node_Class_Id_Sets."=" (Via_Hashmap, Via_Lib),
              "All known node classes are not equal");

      Node_Class_Id_Sets.Destroy (Via_Hashmap);
      Node_Class_Id_Sets.Destroy (Via_Lib);
   end Check_All_Node_Classes_Consistency;

   ---------------------------------------------------------------------------
   procedure Done (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Giant.Graph_Lib.Unload;
      Giant.Graph_Lib.Destroy;
   end Done;

   ---------------------------------------------------------------------------
   procedure Edge_Id_Null_Test (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Node          : Giant.Graph_Lib.Node_Id;
      Edge          : Giant.Graph_Lib.Edge_Id;
      Edge_Set      : Giant.Graph_Lib.Edge_Id_Sets.Set;
      Iterator      : Giant.Graph_Lib.Edge_Id_Sets.Iterator;

      function To_Integer is new Ada.Unchecked_Conversion
        (Source => Giant.Graph_Lib.Edge_Id,
         Target => Integer);
   begin
      Node := Giant.Graph_Lib.Get_Root_Node;

      Edge_Set := Giant.Graph_Lib.Get_Outgoing_Edges (Node);
      Iterator := Giant.Graph_Lib.Edge_Id_Sets.Make_Iterator (Edge_Set);
      if Giant.Graph_Lib.Edge_Id_Sets.More (Iterator) then
         Giant.Graph_Lib.Edge_Id_Sets.Next (Iterator, Edge);

         Assert (Edge /= null, "Edge is null");
         Assert (Giant.Graph_Lib.Get_Target_Node (Edge) /= null,
                 "Target node is null");
      end if;
      Giant.Graph_Lib.Edge_Id_Sets.Destroy (Iterator);
      Giant.Graph_Lib.Edge_Id_Sets.Destroy (Edge_Set);
   end Edge_Id_Null_Test;

   --------------------------------
   --  Routines from AUnit-Test  --
   --------------------------------

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Graph_Lib");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Init'Access, "Init");
      Register_Routine (T, Output_RootNode'Access, "Output_RootNode");
      Register_Routine (T, Enums'Access, "Enums");
      Register_Routine (T, Check_Counts'Access, "Check_Counts");
      Register_Routine (T, Edge_Set_Test'Access, "Edge_Set_Test");
      Register_Routine (T, Edge_Properties'Access, "Edge_Properties");
      Register_Routine (T, All_Edges_Test'Access, "All Edges Test");
      Register_Routine (T, Edge_Id_Null_Test'Access, "Edge_Id = null Test");
      Register_Routine (T, Check_All_Node_Classes_Consistency'Access,
                        "Check_All_Node_Classes_Consistency");
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

end Giant.Graph_Lib.Test;
