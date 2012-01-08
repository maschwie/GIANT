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
--  $RCSfile: dump_iml_data.adb,v $, $Revision: 1.14 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
-- -----
-- Used to dump content (node classes, attributes, nodes and edges of an
--   IML Graph into the logger.
--
with Giant; use Giant;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Subgraphs;
with Giant.Logger;
with Giant.Default_Logger;

-- with Ada.Strings.Unbounded;

procedure dump_iml_data is

   package Logger is new Giant.Logger("dump_iml_data");

   --------------------------------------------------------------------------
   procedure Output_All_Nodes
   is

      procedure Output_All_Attributes (Node : in Graph_Lib.Node_Id)
      is
         Iter   : Graph_Lib.Node_Attribute_Iterator;
         Attrib : Graph_Lib.Node_Attribute_Id;
         Class  : Graph_Lib.Node_Attribute_Class_Id;
      begin
         Logger.Info ("Attributes");
         Iter := Graph_Lib.Make_Attribute_Iterator (Node);
         while Graph_Lib.More (Iter) loop
            Graph_Lib.Next (Iter, Attrib);
            Class := Graph_Lib.Get_Node_Attribute_Class_Id (Attrib);
            Logger.Info
              (Graph_Lib.Convert_Node_Attribute_Id_To_Name (Attrib) & " (" &
               Graph_Lib.Convert_Node_Attribute_Class_Id_To_Name (Class) &
               "): " &
               Graph_Lib.Get_Node_Attribute_Value_As_String (Node, Attrib));
         end loop;
      end Output_All_Attributes;

      procedure Execute (Node : in Graph_Lib.Node_Id)
      is
         Edges_In  : constant Graph_Lib.Edge_Id_Array :=
           Graph_Lib.Get_Incoming_Edges (Node);
         Edges_Out : constant Graph_Lib.Edge_Id_Array :=
           Graph_Lib.Get_Outgoing_Edges (Node);

      begin
         Logger.Info ("Node " & Graph_Lib.Node_Id_Image (Node));

         Logger.Info ("Edges in");
         for I in Edges_In'Range loop
            Logger.Info
              (Graph_Lib.Edge_Id_Image (Edges_In (I)) & " (" &
               Graph_Lib.Node_Id_Image
               (Graph_Lib.Get_Source_Node (Edges_In(I))) & ")");
         end loop;

         Logger.Info ("Edges out");
         for I in Edges_Out'Range loop
            Logger.Info
              (Graph_Lib.Edge_Id_Image (Edges_Out (I)) & " (" &
               Graph_Lib.Node_Id_Image
               (Graph_Lib.Get_Source_Node (Edges_Out(I))) & ")");
         end loop;

         Output_All_Attributes (Node);
      end Execute;

      procedure Apply is new Graph_Lib.Node_Id_Sets.Apply
        (Execute => Execute);

      All_Nodes : Graph_Lib.Node_Id_Set;
   begin
      Logger.Info ("All nodes:");

      All_Nodes := Graph_Lib.Get_All_Nodes;

      Apply (All_Nodes);

      Graph_Lib.Node_Id_Sets.Destroy (All_Nodes);
   end Output_All_Nodes;

   procedure Output_All_Edges is

      procedure Execute (Edge : in Graph_Lib.Edge_Id) is
      begin
         Logger.Info
           (Graph_Lib.Edge_Id_Image (Edge) & ": " &
            Graph_Lib.Node_Id_Image
            (Graph_Lib.Get_Source_Node (Edge)) & " --> " &
            Graph_Lib.Node_Id_Image
            (Graph_Lib.Get_Target_Node (Edge)) & " [" &
            Graph_Lib.Get_Edge_Class_Tag
            (Graph_Lib.Get_Edge_Class_Id (Edge)) & "]"
            );
      end Execute;

      procedure Apply is new Graph_Lib.Edge_Id_Sets.Apply
        (Execute => Execute);

      All_Edges : Graph_Lib.Edge_Id_Set;
   begin
      Logger.Info ("All edges:");

      All_Edges := Graph_Lib.Get_All_Edges;

      Apply (All_Edges);

      Graph_Lib.Edge_Id_Sets.Destroy (All_Edges);
   end Output_All_Edges;

   procedure Load_And_Dump_Graph
   is
   begin
      Graph_Lib.Load ("resources/rfg_examp.iml");
      --  Graph_Lib.Load ("resources/graphs/concept_analysis.iml");
      --  Graph_Lib.Load ("resources/graphs/wget.iml");
      --  Graph_Lib.Load ("resources/graphs/httpd.iml");
      Output_All_Nodes;
      Output_All_Edges;
      Graph_Lib.Unload;
   end Load_And_Dump_Graph;

   Node_Classes      : Giant.Graph_Lib.Node_Class_Id_Set;
   Node_Classes_Iter : Giant.Graph_Lib.Node_Class_Id_Sets.Iterator;
   A_Node_Class      : Giant.Graph_Lib.Node_Class_Id;

   Attributes_Iter : Giant.Graph_Lib.Node_Attribute_Iterator;
   A_Attribute     : Giant.Graph_Lib.Node_Attribute_Id;

   Edge_Attr_Set  : Giant.Graph_Lib.Edge_Class_Id_Set;
   Edge_Attr_Iter : Giant.Graph_Lib.Edge_Class_Id_Sets.Iterator;
   A_Edge_Attr    : Giant.Graph_Lib.Edge_Class_Id;

   Node_Class_Counter : Integer;
   Edge_Class_Counter : Integer;
   Attr_Counter       : Integer;

begin
   Giant.Graph_Lib.Initialize;

   Giant.Default_Logger.Init ("an_iml_dump.log");
   Logger.Info ("Starting Data Dump ...");

   Node_Classes := Giant.Graph_Lib.Get_All_Node_Class_Ids;


   -- All node classes
   -------------------
   Logger.Info ("");
   Logger.Info ("");
   Logger.Info ("All Node Classes of the iml graph:");
   Logger.Info ("----------------------------------");

   Node_Classes_Iter :=
     Giant.Graph_Lib.Node_Class_Id_Sets.Make_Iterator (Node_Classes);

   Node_Class_Counter := 1;
   while Giant.Graph_Lib.Node_Class_Id_Sets.More (Node_Classes_Iter) loop

      Giant.Graph_Lib.Node_Class_Id_Sets.Next
        (Node_Classes_Iter, A_Node_Class);

      Logger.Info
        (Integer'Image (Node_Class_Counter) & ". " & "Node Class: """
         & Giant.Graph_Lib.Get_Node_Class_Tag (A_Node_Class)
         & """");

      Node_Class_Counter := Node_Class_Counter + 1;
   end loop;

   Giant.Graph_Lib.Node_Class_Id_Sets.Destroy (Node_Classes_Iter);


   -- All edge classes
   -------------------
   Logger.Info ("");
   Logger.Info ("");
   Logger.Info ("All Edge Classes of the iml graph:");
   Logger.Info ("----------------------------------");

   Edge_Attr_Set :=
     Giant.Graph_Lib.Get_All_Edge_Class_Ids;

   Edge_Attr_Iter :=
     Giant.Graph_Lib.Edge_Class_Id_Sets.Make_Iterator (Edge_Attr_Set);

   Edge_Class_Counter := 1;
   while Giant.Graph_Lib.Edge_Class_Id_Sets.More (Edge_Attr_Iter) loop

      Giant.Graph_Lib.Edge_Class_Id_Sets.Next
        (Edge_Attr_Iter, A_Edge_Attr);

      Logger.Info
        (Integer'Image (Edge_Class_Counter) & ". " & "Edge Class: """
         & Giant.Graph_Lib.Get_Edge_Class_Tag
             (A_Edge_Attr)
         & """");

      Edge_Class_Counter := Edge_Class_Counter + 1;
   end loop;

   Giant.Graph_Lib.Edge_Class_Id_Sets.Destroy (Edge_Attr_Set);
   Giant.Graph_Lib.Edge_Class_Id_Sets.Destroy (Edge_Attr_Iter);


   -- Details for each node class
   ------------------------------
   Logger.Info ("");
   Logger.Info ("");
   Logger.Info ("Details about node classes of the iml graph:");
   Logger.Info ("--------------------------------------------");

   Node_Classes_Iter :=
     Giant.Graph_Lib.Node_Class_Id_Sets.Make_Iterator (Node_Classes);

   Node_Class_Counter := 1;
   while Giant.Graph_Lib.Node_Class_Id_Sets.More (Node_Classes_Iter) loop

      Giant.Graph_Lib.Node_Class_Id_Sets.Next
        (Node_Classes_Iter, A_Node_Class);

      Logger.Info ("");
      Logger.Info ("");
      Logger.Info
        (Integer'Image (Node_Class_Counter) & ". " & "Node Class: """
         & Giant.Graph_Lib.Get_Node_Class_Tag (A_Node_Class)
         & """");
      Logger.Info ("");


      -- 1. Dump all attributes
      -------------------------
      Attributes_Iter := Giant.Graph_Lib.Make_Attribute_Iterator
        (A_Node_Class);

      Attr_Counter := 1;
      while Giant.Graph_Lib.More (Attributes_Iter) loop

         Giant.Graph_Lib.Next (Attributes_Iter, A_Attribute);

         Logger.Info
           ("  " & Integer'Image (Attr_Counter) & ". " & "Attribute: """
            & Giant.Graph_Lib.Convert_Node_Attribute_Id_To_Name
                (A_Attribute)
            & """");
         Attr_Counter := Attr_Counter + 1;

      end loop;

      Logger.Info ("");


      -- 2. Dump all edge attributes (subset of  1.)
      ----------------------------------------------
      Edge_Attr_Set :=
        Giant.Graph_Lib.Get_All_Edge_Class_Ids_For_Node_Class (A_Node_Class);

      Edge_Attr_Iter :=
        Giant.Graph_Lib.Edge_Class_Id_Sets.Make_Iterator (Edge_Attr_Set);

      Edge_Class_Counter := 1;
      while Giant.Graph_Lib.Edge_Class_Id_Sets.More (Edge_Attr_Iter) loop

         Giant.Graph_Lib.Edge_Class_Id_Sets.Next
           (Edge_Attr_Iter, A_Edge_Attr);

         Logger.Info
           ("  " & Integer'Image (Edge_Class_Counter) & ". " & "Edge_Attr: """
            & Giant.Graph_Lib.Get_Edge_Class_Tag
                (A_Edge_Attr)
            & """");

         Edge_Class_Counter := Edge_Class_Counter + 1;
      end loop;

      Giant.Graph_Lib.Edge_Class_Id_Sets.Destroy (Edge_Attr_Set);
      Giant.Graph_Lib.Edge_Class_Id_Sets.Destroy (Edge_Attr_Iter);

      Node_Class_Counter := Node_Class_Counter + 1;
   end loop;

   Giant.Graph_Lib.Node_Class_Id_Sets.Destroy (Node_Classes_Iter);
   Giant.Graph_Lib.Node_Class_Id_Sets.Destroy (Node_Classes);

   Load_And_Dump_Graph;

   Giant.Graph_Lib.Destroy;
end dump_iml_data;

