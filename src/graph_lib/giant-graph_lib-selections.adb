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
--  $RCSfile: giant-graph_lib-selections.adb,v $, $Revision: 1.23 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $

with Untagged_Ptr_Ops;

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Graph_Lib.Selections is

   ---------------------------------------------------------------------------
   package Logger is new Giant.Logger("giant.graph_lib.subgraphs");

   ---------------------------------------------------------------------------
   function "<"
     (Left  : in Selection;
      Right : in Selection)
      return Boolean
   is

      package Compare is new Untagged_Ptr_Ops
        (T     => Selection_Record,
         T_Ptr => Selection);

   begin
      return Compare.Less (Left, Right);
   end "<";

   ---------------------------------------------------------------------------
   procedure Add_Edge
     (The_Selection : in Selection;
      Edge          : in Edge_Id)
   is
   begin
      Edge_Id_Sets.Insert (The_Selection.Edges, Edge);
   end Add_Edge;

   ---------------------------------------------------------------------------
   procedure Add_Edge_Set
     (The_Selection : in Selection;
      Edge_Set      : in Edge_Id_Set)
   is

      procedure Execute (Edge : in Edge_Id) is
      begin
         Add_Edge (The_Selection, Edge);
      end Execute;

      procedure Apply is new Edge_Id_Sets.Apply (Execute => Execute);

   begin
      Apply (Edge_Set);
   end Add_Edge_Set;

   ---------------------------------------------------------------------------
   procedure Add_Node
     (The_Selection : in Selection;
      Node          : in Node_Id)
   is
   begin
      Node_Id_Sets.Insert
        (The_Selection.Nodes,
         Node);
   end Add_Node;

   ---------------------------------------------------------------------------
   procedure Add_Node_Set
     (The_Selection : in Selection;
      Node_Set      : in Node_Id_Set)
   is

      procedure Execute (Node : in Node_Id) is
      begin
         Add_Node (The_Selection, Node);
      end Execute;

      procedure Apply is new Node_Id_Sets.Apply (Execute => Execute);

   begin
      Apply (Node_Set);
   end Add_Node_Set;

   ---------------------------------------------------------------------------
   procedure Clear
      (The_Selection : in Selection)
   is
   begin
      Node_Id_Sets.Remove_All (The_Selection.Nodes);
      Edge_Id_Sets.Remove_All (The_Selection.Edges);
   end Clear;

   ---------------------------------------------------------------------------
   function Clone
     (Selection_To_Clone : in Selection;
      Name_Of_Result     : in String)
      return Selection
   is
      Res : Selection;
   begin
      Res := new Selection_Record
        (Name_Length => Name_Of_Result'Length);
      Res.Name  := Name_Of_Result;
      Res.Edges := Edge_Id_Sets.Copy (Selection_To_Clone.Edges);
      Res.Nodes := Node_Id_Sets.Copy (Selection_To_Clone.Nodes);
      return Res;
   end Clone;

   ---------------------------------------------------------------------------
   function Create
     (Name : in    String)
      return Selection
   is
      Res      : Selection;
   begin
      Res := new Selection_Record (Name_Length => Name'Length);
      Res.Name  := Name;
      Res.Edges := Edge_Id_Sets.Empty_Set;
      Res.Nodes := Node_Id_Sets.Empty_Set;
      return Res;
   end Create;

   ----------------------------------------------------------------------------
   procedure Destroy
     (Selection_To_Destroy : in out Selection)
   is

   begin
      Edge_Id_Sets.Destroy (Selection_To_Destroy.Edges);
      Node_Id_Sets.Destroy (Selection_To_Destroy.Nodes);

      Free_Selection (Selection_To_Destroy);
   end Destroy;

   ---------------------------------------------------------------------------
   function Get_All_Edges
     (Sel : in Selection)
     return Edge_Id_Set
   is
   begin
      return Sel.Edges;
   end Get_All_Edges;

   ---------------------------------------------------------------------------
   function Get_All_Nodes
     (Sel : in Selection)
     return Node_Id_Set
   is
   begin
      return Sel.Nodes;
   end Get_All_Nodes;

   ---------------------------------------------------------------------------
   function Get_Edge_Count
     (Sel : in Selection)
     return Natural
   is
   begin
      return Edge_Id_Sets.Size (Sel.Edges);
   end Get_Edge_Count;

   ----------------------------------------------------------------------------
   function Get_Name
     (Selection_To_Read : in Selection)
      return String
   is
   begin
      return Selection_To_Read.Name;
   end Get_Name;

   ---------------------------------------------------------------------------
   function Get_Node_Count
     (Sel : in Selection)
     return Natural
   is
   begin
      return Node_Id_Sets.Size (Sel.Nodes);
   end Get_Node_Count;

   ----------------------------------------------------------------------------
   function Intersection
     (Left           : in Selection;
      Right          : in Selection;
      Name_Of_Result : in String)
      return Selection
   is
      Res      : Selection;
   begin
      Res       := new Selection_Record (Name_Length => Name_Of_Result'Length);
      Res.Name  := Name_Of_Result;
      Res.Edges := Edge_Id_Sets."*" (Left.Edges, Right.Edges);
      Res.Nodes := Node_Id_Sets."*" (Left.Nodes, Right.Nodes);
      return Res;
   end Intersection;

   ---------------------------------------------------------------------------
   function Is_Member
     (Sel  : in Selection;
      Edge : in Edge_Id)
     return Boolean
   is
   begin
      return Edge_Id_Sets.Is_Member (Sel.Edges, Edge);
   end Is_Member;

   ---------------------------------------------------------------------------
   function Is_Member
     (Sel  : in Selection;
      Node : in Node_Id)
     return Boolean
   is
   begin
      return Node_Id_Sets.Is_Member (Sel.Nodes, Node);
   end Is_Member;

   ----------------------------------------------------------------------------
   procedure Remove
     (Left  : in Selection;
      Right : in Selection)
   is
      Old_Edges : Edge_Id_Set := Left.Edges;
      Old_Nodes : Node_Id_Set := Left.Nodes;
   begin
      Left.Edges := Edge_Id_Sets."-" (Left.Edges, Right.Edges);
      Left.Nodes := Node_Id_Sets."-" (Left.Nodes, Right.Nodes);

      Edge_Id_Sets.Destroy (Old_Edges);
      Node_Id_Sets.Destroy (Old_Nodes);
   end Remove;

   ----------------------------------------------------------------------------
   procedure Remove_Edge
     (The_Selection : in Selection;
      Edge          : in Edge_Id)
   is
   begin
      begin
         Edge_Id_Sets.Remove (The_Selection.Edges, Edge);
      exception
         when Edge_Id_Sets.No_Member =>
            raise Edge_Does_Not_Exist;
      end;
   end Remove_Edge;

   ----------------------------------------------------------------------------
   procedure Remove_Edge_Set
     (The_Selection : in Selection;
      Edge_Set      : in Edge_Id_Set)
   is

      procedure Execute (Edge : in Edge_Id) is
      begin
         Remove_Edge (The_Selection, Edge);
      end Execute;

      procedure Apply is new Edge_Id_Sets.Apply (Execute => Execute);

   begin
      Apply (Edge_Set);
   end Remove_Edge_Set;

   ----------------------------------------------------------------------------
   procedure Remove_Node
     (The_Selection : in Selection;
      Node          : in Node_Id)
   is
   begin
      begin
         Node_Id_Sets.Remove (The_Selection.Nodes, Node);
      exception
         when Node_Id_Sets.No_Member =>
            raise Node_Does_Not_Exist;
      end;
   end Remove_Node;

   ----------------------------------------------------------------------------
   procedure Remove_Node_Set
     (The_Selection : in Selection;
      Node_Set      : in Node_Id_Set)
   is

      procedure Execute (Node : in Node_Id) is
      begin
         Remove_Node (The_Selection, Node);
      end Execute;

      procedure Apply is new Node_Id_Sets.Apply (Execute => Execute);

   begin
      Apply (Node_Set);
   end Remove_Node_Set;

   ----------------------------------------------------------------------------
   procedure Rename
     (The_Selection : in out Selection;
      New_Name      : in     String)
   is
      Res  : Selection;
   begin
      Res       := new Selection_Record (Name_Length => New_Name'Length);
      Res.Name  := New_Name;
      Res.Edges := The_Selection.Edges;
      Res.Nodes := The_Selection.Nodes;

      Free_Selection (The_Selection);
      The_Selection := Res;
   end Rename;

   ----------------------------------------------------------------------------
   procedure Selection_Read
     (Stream        : in      Bauhaus_Io.In_Stream_Type;
      The_Selection :     out Selection)
   is
      Len    : Natural;

      ----------------------------------------------------------------------
      --  Is implemented here, since it is used only once
      --    if it will be used more, please put it in giant-graph_lib.ads
      procedure Read_Node_Id_Set is new Node_Id_Sets.Read_Set
        (Read_Element => Read_Node_Id);

      ----------------------------------------------------------------------
      --  Is implemented here, since it is used only once
      --    if it will be used more, please put it in giant-graph_lib.ads
      procedure Read_Edge_Id_Set is new Edge_Id_Sets.Read_Set
        (Read_Element => Read_Edge_Id);

   begin
      --  read name
      Bauhaus_Io.Read_Natural (Stream, Len);
      The_Selection := new Selection_Record (Name_Length => Len);
      Bauhaus_Io.Read_String (Stream, The_Selection.Name);

      --  read contained edges and nodes
      Read_Edge_Id_Set (Stream, The_Selection.Edges);
      Read_Node_Id_Set (Stream, The_Selection.Nodes);
   end Selection_Read;

   ----------------------------------------------------------------------------
   procedure Selection_Write
     (Stream        : in Bauhaus_Io.Out_Stream_Type;
      The_Selection : in Selection)
   is

      ----------------------------------------------------------------------
      --  Is implemented here, since it is used only once
      --    if it will be used more, please put it in giant-graph_lib.ads
      procedure Write_Node_Id_Set is new Node_Id_Sets.Write_Set
        (Write_Element => Write_Node_Id);

      ----------------------------------------------------------------------
      --  Is implemented here, since it is used only once
      --    if it will be used more, please put it in giant-graph_lib.ads
      procedure Write_Edge_Id_Set is new Edge_Id_Sets.Write_Set
        (Write_Element => Write_Edge_Id);

   begin
      --  write name
      Bauhaus_Io.Write_Natural (Stream, The_Selection.Name'Length);
      Bauhaus_Io.Write_String  (Stream, The_Selection.Name);

      --  write contained edges and nodes
      Write_Edge_Id_Set (Stream, The_Selection.Edges);
      Write_Node_Id_Set (Stream, The_Selection.Nodes);
   end Selection_Write;

   ----------------------------------------------------------------------------
   function Difference
     (Left           : in Selection;
      Right          : in Selection;
      Name_Of_Result : in String)
      return Selection
   is
      Res      : Selection;
   begin
      Res       := new Selection_Record (Name_Length => Name_Of_Result'Length);
      Res.Name  := Name_Of_Result;
      Res.Edges := Edge_Id_Sets."-" (Left.Edges, Right.Edges);
      Res.Nodes := Node_Id_Sets."-" (Left.Nodes, Right.Nodes);
      return Res;
   end Difference;

   ----------------------------------------------------------------------------
   function Union
     (Left          : in Selection;
      Right         : in Selection;
      Name_Of_Result : in String)
      return Selection
   is
      Res      : Selection;
   begin
      Res       := new Selection_Record (Name_Length => Name_Of_Result'Length);
      Res.Name  := Name_Of_Result;
      Res.Edges := Edge_Id_Sets."+" (Left.Edges, Right.Edges);
      Res.Nodes := Node_Id_Sets."+" (Left.Nodes, Right.Nodes);
      return Res;
   end Union;

end Giant.Graph_Lib.Selections;
