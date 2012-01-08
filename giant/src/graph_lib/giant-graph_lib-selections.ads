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
--  $RCSfile: giant-graph_lib-selections.ads,v $, $Revision: 1.26 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
------------------------------------------------------------------------------
--
--  Contains the Selections

with Ada.Unchecked_Deallocation;

--  from Bauhaus
with Lists;
with Bauhaus_Io;

package Giant.Graph_Lib.Selections is

   ---------------------------------------------------------------------------
   --  It is assured, that a copy makes an alias
   --  Internally, it is a pointer
   type Selection is private;

   ---------------------------------------------------------------------------
   --  Creates a new selection with no nodes and edges
   --   has to be destroyed with "destroy" after usage
   --  Name is converted to a String and not used during the run
   function Create
      (Name : in    String)
      return Selection;

   ---------------------------------------------------------------------------
   --  Removes given selection from memory
   procedure Destroy
      (Selection_To_Destroy : in out Selection);

   ---------------------------------------------------------------------------
   --  Creates a deep-copy of the selection
   function Clone
     (Selection_To_Clone : in Selection;
      Name_Of_Result     : in String)
     return Selection;

   ---------------------------------------------------------------------------
   --  The_Selection will be modified
   procedure Rename
      (The_Selection  : in out Selection;
       New_Name       : in     String);

   ---------------------------------------------------------------------------
   --  Don't know about the semantic
   --  Returns randomly true or false
   function "<"
      (Left  : in Selection;
       Right : in Selection)
      return Boolean;

   -----------------
   --  Inspecors  --
   -----------------

   ---------------------------------------------------------------------------
   function Get_Name
      (Selection_To_Read : in Selection)
      return String;

   ---------------------------------------------------------------------------
   --  Returns:
   --    Number of edges contained in given selection
   function Get_Edge_Count
     (Sel : in Selection)
     return Natural;

   ---------------------------------------------------------------------------
   --  Returns:
   --    Number of nodes contained in given selection
   function Get_Node_Count
     (Sel : in Selection)
     return Natural;

   ---------------------------------------------------------------------------
   --  RESULT MAY NOT BE MODIFIED
   --
   --  This routine was mainly created for subgraphs to be able to access
   --    the data of a selection. Therefore a reference to the internal
   --    structure is returned.
   --
   --  Returns:
   --    All Nodes included in selection
   --
   --  TBD: refactoring! - see below @ Get_All_Edges
   function Get_All_Nodes
     (Sel : in Selection)
     return Node_Id_Set;

   ---------------------------------------------------------------------------
   --  RESULT MAY NOT BE MODIFIED
   --
   --  This routine was mainly created for subgraphs to be able to access
   --    the data of a selection. Therefore a reference to the internal
   --    structure is returned.
   --
   --  Returns:
   --    All Nodes included in selection
   --
   --  TBD: refactoring! - subgraph should be a child of selections
   --       then this breaking of information hiding is not needed any more,
   --       since the routines of subgraphs can directly access the internal
   --       structure
   function Get_All_Edges
     (Sel : in Selection)
     return Edge_Id_Set;

   ---------------------------------------------------------------------------
   --  Returns:
   --    true   if given node is member in given selection
   --    false  otherwise
   function Is_Member
     (Sel  : in Selection;
      Edge : in Edge_Id)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Returns:
   --    true   if given node is member in given selection
   --    false  otherwise
   function Is_Member
     (Sel  : in Selection;
      Node : in Node_Id)
     return Boolean;

   ---------------
   --  Streams  --
   ---------------

   ---------------------------------------------------------------------------
   --  cp. Barnes p543
   --  Maybe u have to use "for Selection'Write use Selection_Write"
   procedure Selection_Write
      (Stream        : in Bauhaus_Io.Out_Stream_Type;
       The_Selection : in Selection);

   ---------------------------------------------------------------------------
   procedure Selection_Read
      (Stream        : in     Bauhaus_Io.In_Stream_Type;
       The_Selection :    out Selection);

   -----------------------
   --  Routines to add  --
   -----------------------

   ---------------------------------------------------------------------------
   --  Adds given Node to the given Selection
   procedure Add_Node
      (The_Selection : in Selection;
       Node          : in Node_Id);

   ---------------------------------------------------------------------------
   --  Adds all nodes in given set to the Selection
   procedure Add_Node_Set
      (The_Selection : in Selection;
       Node_Set      : in Node_Id_Set);

   ---------------------------------------------------------------------------
   --  Adds given Edge to the given Selection
   procedure Add_Edge
      (The_Selection : in Selection;
       Edge          : in Edge_Id);

   ---------------------------------------------------------------------------
   --  Adds all nodes in given set to the Selection
   procedure Add_Edge_Set
      (The_Selection : in Selection;
       Edge_Set      : in Edge_Id_Set);

   --------------------------
   --  Routines to remove  --
   --------------------------

   ---------------------------------------------------------------------------
   --  Removes Right from Left.
   --
   --  See:
   --    Difference
   procedure Remove
     (Left  : in Selection;
      Right : in Selection);

   ---------------------------------------------------------------------------
   --  Removes given Node from given selection
   --
   --  Raises:
   --    Node_Does_Not_Exist - if given node is not contained in
   --      given selection
   procedure Remove_Node
      (The_Selection : in Selection;
       Node          : in Node_Id);

   ---------------------------------------------------------------------------
   --  Removes all nodes in given set from given selection
   --
   --  Raises:
   --    Node_Does_Not_Exist - if a node of the given set is not contained in
   --      given selection
   procedure Remove_Node_Set
      (The_Selection : in Selection;
       Node_Set      : in Node_Id_Set);

   ---------------------------------------------------------------------------
   --  Removes given edge from given selection
   --
   --  Raises:
   --    Edge_Does_Not_Exist - if given edge is not contained in
   --      given selection
   procedure Remove_Edge
      (The_Selection : in Selection;
       Edge          : in Edge_Id);

   ---------------------------------------------------------------------------
   --  Removes all edges in given set from given selection
   --
   --
   --  Raises:
   --    Edge_Does_Not_Exist - if an edge of the given set is not contained in
   --      given selection
   procedure Remove_Edge_Set
      (The_Selection : in Selection;
       Edge_Set      : in Edge_Id_Set);

   ---------------------------------------------------------------------------
   --  Removes all edges and nodes
   procedure Clear
      (The_Selection : in Selection);

   ----------------------
   --  Set-operations  --
   ----------------------

   ---------------------------------------------------------------------------
   --  Creates a new selection where the two given Selections are unified
   function Union
      (Left           : in Selection;
       Right          : in Selection;
       Name_Of_Result : in String)
      return Selection;

   ---------------------------------------------------------------------------
   --  Creates a new selection
   function Difference
      (Left           : in Selection;
       Right          : in Selection;
       Name_Of_Result : in String)
      return Selection;

   ---------------------------------------------------------------------------
   --  Creates a new selection
   function Intersection
      (Left           : in Selection;
       Right          : in Selection;
       Name_Of_Result : in String)
      return Selection;

private

   type Selection_Record (Name_Length : Natural) is record
      Name  : String (1..Name_Length);
      Nodes : Node_Id_Set;
      Edges : Edge_Id_Set;
   end record;

   type Selection is access Selection_Record;

   ---------------------------------------------------------------------------
   procedure Free_Selection is new Ada.Unchecked_Deallocation
     (Selection_Record, Selection);

end Giant.Graph_Lib.Selections;
