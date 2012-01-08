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
--  $RCSfile: giant-tree_layouts.ads,v $, $Revision: 1.20 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
--  Contains the treelayout-algorithm
--

with Lists;
pragma Elaborate_All (Lists);

with Hashed_Mappings;
pragma Elaborate_All (Hashed_Mappings);

--  Speed optimization:
--    Include Last_Element-"hack" into Stacks_Unbounded
--    Otherwise Push has O(n)
with Stacks_Unbounded;
pragma Elaborate_All (Stacks_Unbounded);

with Giant.Config;
with Giant.Config.Class_Sets;
with Giant.Evolutions;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Graph_Widgets;
with Giant.Vis;

package Giant.Tree_Layouts is

   ---------------------------------------------------------------------------
   --  Concurrent can't be used, because of the stacks
   --    But the bounded-stackpackage could be used, since
   --    after Init_Run, the maximum height of the tree is known
   --    this is /not/ the max. size of the stack
   --    max size is tree-height * (degree-1) or size (Nodes_To_Layout)
   type Tree_Layout_Record is
     new Giant.Evolutions.Iterative_Evolution with private;
   type Tree_Layout is access Tree_Layout_Record;

   ---------------------------------------------------------------------------
   --  Maximum number of nost to be processed in one run
   Max_Nodes_In_One_Run : constant := 1000;

   ---------------------------------------------------------------------------
   --  X-Distance of two neighbours as proportion of
   --    Get_Current_Maximum_Node_Width
   X_Distance           : constant := 0.07;

   ---------------------------------------------------------------------------
   --  Y-Distance of two neighbours as proportion of
   --    The height of predecessing row
   Y_Distance           : constant := 0.07;

   ---------------------------------------------------------------------------
   --  Initializes the tree-layout-algorithm
   --
   --  Parameters (cp. specification 11.1.1.1: Treelayout / Parameter)
   --
   --    Selection_To_Layout:
   --      Selection containing the nodes and edges to layout
   --      Edges are ignored
   --
   --    Widget:
   --      Graph_Widet where the nodes to layout reside
   --
   --    Widget_Lock:
   --      Lock of the widget, released at the end of the layout
   --
   --    Target_Position:
   --      Position on window, where the middle of root-node has to
   --      be placed
   --
   --    Root:
   --      Root of the selection to be layouted
   --
   --    Meta_Class_Set_To_Layout:
   --      Meta_Class_Set containing node- and edge_classes to include in
   --      the tree-layout
   --      This Meta_Class_Set is NOT destroyed after usage, the caller has
   --        to take care for the unloading
   --
   --    Process_Edges_Reverse:
   --      Normally, an outgoing edge indicates the target to be the child.
   --      If this parameter is set to true, the source of an incoming edge
   --      is regarded as child
   --
   --  Returns:
   --    derived Evolutions-Object to do the layout
   --
   --  Precondition:
   --    Root_Node is in Selection_To_Layout.Get_All_Nodes
   --
   function Initialize
     (Widget                   : in Graph_Widgets.Graph_Widget;
      Widget_Lock              : in Graph_Widgets.Lock_Type;
      Selection_To_Layout      : in Graph_Lib.Selections.Selection;
      Target_Position          : in Vis.Logic.Vector_2d;
      Root_Node                : in Graph_Lib.Node_Id;
      Meta_Class_Set_To_Layout : in Config.Class_Sets.Meta_Class_Set_Access;
      Process_Edges_Reverse    : in Boolean)
     return Tree_Layout;

   -------------------
   --  Calculation  --
   -------------------

   ---------------------------------------------------------------------------
   procedure Finish
     (Layout   : access Tree_Layout_Record;
      Canceled : in     Boolean);

   ---------------------------------------------------------------------------
   procedure Step
     (Layout      : access Tree_Layout_Record;
      Next_Action :    out Evolutions.Evolution_Action);

   ---------------------------------------------------------------------------
   procedure Synchronized_Step
     (Layout      : access Tree_Layout_Record;
      Next_Action :    out Evolutions.Evolution_Action);

private
   ---------------------------------------------------------------------------
   type Layout_State is (Init_Start,
                         Init_Run_Part_One,
                         Init_Run_Part_Two,
                         FirstWalk_Start,
                         FirstWalk_Run,
                         SecondWalk_Start,
                         SecondWalk_Run,
                         Matrix);

   ---------------------------------------------------------------------------
   type Node_Layout_Data_Record;
   type Node_Layout_Data is access Node_Layout_Data_Record;
   type Node_Layout_Data_Record is record
      Thread            : Node_Layout_Data;

      --  by algorithm
      Ancestor          : Node_Layout_Data;

      --  by init-routine
      Parent            : Node_Layout_Data;

      --  = self, if self is leftmost silbling
      Leftmost_Silbling : Node_Layout_Data;

      -- used in the algorithm, null if n/a
      Left_Silbling     : Node_Layout_Data;

      --  for checking, if v is a silbling of w
      --  null if n/a
      Right_Silbling    : Node_Layout_Data;

      Leftmost_Child    : Node_Layout_Data;

      Rightmost_Child   : Node_Layout_Data;

      Silbling_Number   : Positive;

      Modf              : Vis.Logic_Float;
      Prelim            : Vis.Logic_Float;
      Change            : Vis.Logic_Float;
      Shift             : Vis.Logic_Float;

      --  Level in the tree
      Level             : Positive;

      --  Node in graph_lib
      Node              : Graph_Lib.Node_Id;
   end record;

   ---------------------------------------------------------------------------
   package Node_Layout_Data_Lists is new
     Lists (ItemType => Node_Layout_Data);

   ---------------------------------------------------------------------------
   package Node_Layout_Data_Stacks is new
     Stacks_Unbounded (Elem_Type => Node_Layout_Data);

   ---------------------------------------------------------------------------
   type FirstWalk_Data_Record is record
      V               : Node_Layout_Data;
      W               : Node_Layout_Data;
      DefaultAncestor : Node_Layout_Data;
   end record;

   ---------------------------------------------------------------------------
   package FirstWalk_Stacks is new
     Stacks_Unbounded (Elem_Type => FirstWalk_Data_Record);

   ---------------------------------------------------------------------------
   type SecondWalk_Data_Record is record
      V               : Node_Layout_Data;
      M               : Vis.Logic_Float;
   end record;

   ---------------------------------------------------------------------------
   package SecondWalk_Stacks is new
     Stacks_Unbounded (Elem_Type => SecondWalk_Data_Record);

   ---------------------------------------------------------------------------
   function Id (P : in Positive) return Positive;
   package Level_Mappings is new
     Hashed_Mappings
     (Key_Type   => Positive,
      Value_Type => Vis.Logic_Float,
      Hash       => Id);

   ---------------------------------------------------------------------------
   type Tree_Layout_Record is
     new Evolutions.Iterative_Evolution with record
        --  Init by Initialize
        Widget           : Graph_Widgets.Graph_Widget;
        Widget_Lock      : Graph_Widgets.Lock_Type;
        Nodes_To_Layout  : Graph_Lib.Node_Id_Set;
        Target_Position  : Vis.Logic.Vector_2d;
        Root_Node        : Graph_Lib.Node_Id;
        Meta_Class_Set   : Config.Class_Sets.Meta_Class_Set_Access;
        Process_Edges_Reverse : Boolean;
        State            : Layout_State;

        --  used at finish to distinguish different cases
        --   (0, 1 or more nodes to layout)
        Count_Nodes_To_Layout : Natural;

        ----------------------------------------
        --  Init by Step / Synchronized_Step  --
        ----------------------------------------

        --  Rootnode of the tree to layout
        Tree_Root        : Node_Layout_Data;

        --  Distance between two nodes; used at FirstWalk
        X_Distance       : Vis.Logic_Float;

        --  Maximum X-Coordinate used at tree
        --  Set at SecondWalk
        Max_X            : Vis.Logic_Float;

        --  Used at conversion of Nodes_To_Layout to Layout_Tree
        --  Queue_Last is for speed optimization
        --    since Node_Id_Lists.Last has O(n) and gets O(1) with this "hack"
        --
        --  Queue may never be empty during the usage
        Queue_Last       : Node_Layout_Data_Lists.List;
        Queue            : Node_Layout_Data_Lists.List;

        Level_Heights              : Level_Mappings.Mapping;

        FirstWalk_Stack            : FirstWalk_Stacks.Stack;
        SecondWalk_Stack           : SecondWalk_Stacks.Stack;
     end record;

   ---------------------------------------------------------------------------
   function Are_Silblings
     (First  : in Node_Layout_Data;
      Second : in Node_Layout_Data)
     return Boolean;

   --------------------------------------------------------------------------
   --  If a Node is seen at the first time, it has to be pushed with this
   --  procedure. It sets the initial values correctly
   --
   --  Parameters:
   --    Stack : Layout.FirstWalk_Stack - stack where to push to
   --    V     : Node to be pushed
   procedure FirstWalk_Stack_Initial_Push
     (Stack : in out FirstWalk_Stacks.Stack;
      V     : in     Node_Layout_Data);

   ---------------------------------------------------------------
   --  1:1 - Implementations of routines of Walker's algorithm  --
   ---------------------------------------------------------------

   ---------------------------------------------------------------------------
   procedure ExecuteShifts (V : in Node_Layout_Data);

   ------------------------------------------------------------------
   function NextLeft
     (V : in Node_Layout_Data)
     return Node_Layout_Data;

   ------------------------------------------------------------------
   function NextRight
     (V : in Node_Layout_Data)
     return Node_Layout_Data;

   ------------------------------------------------------------------
   procedure MoveSubtree
     (WM    : in Node_Layout_Data;
      WP    : in Node_Layout_Data;
      Shift : in Vis.Logic_Float);

   ------------------------------------------------------------------
   function Ancestor
     (VIM             : in Node_Layout_Data;
      V               : in Node_Layout_Data;
      DefaultAncestor : in Node_Layout_Data)
     return Node_Layout_Data;

   ---------------------------------------------------------------------
   --  Additional Parameters:
   --    X_Distance   - Distance between two nodes
   procedure Apportion
     (V               : in     Node_Layout_Data;
      DefaultAncestor : in out Node_Layout_Data;
      X_Distance      : in     Vis.Logic_Float);

   ---------------------------------------------------------------------
   --  Represents the part after the "for all children"-loop
   --    in FirstWalk(v) in the paper
   --
   --  Additional Parameters:
   --    X_Distance   - Distance between two nodes
   procedure FirstWalk_Visit_Self
     (V          : in Node_Layout_Data;
      X_Distance : in Vis.Logic_Float);

end Giant.Tree_Layouts;
