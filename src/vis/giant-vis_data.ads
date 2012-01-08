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
--  First Author: Steffen Keul
--
--  $RCSfile: giant-vis_data.ads,v $, $Revision: 1.32 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
--  Provides data structures to handle the visual representation of edges
--  and nodes.
--


with Ada.Finalization;
with Ada.Unchecked_Deallocation;

with Bauhaus_IO;
with Hashed_Mappings;
pragma Elaborate_All (Hashed_Mappings);
with Ordered_Sets;
with Lists;
pragma Elaborate_All (Lists);

with Giant.Graph_Lib;
with Giant.Merging_Iterators;
pragma Elaborate_All (Giant.Merging_Iterators);
with Giant.Simple_Priority_Queues;
pragma Elaborate_All (Giant.Simple_Priority_Queues);
with Giant.Vis;

package Giant.Vis_Data is


   ------------
   -- Layers --
   ------------

   ----------------------------------------------------------------------------
   --  Represents the layer in that an edge or a node lies
   --  In order to create new layers a Layer_Pool should be used.
   type Layer_Type is private;

   ----------------------------------------------------------------------------
   --  Tests if 'Low' is a layer situated below the layer of 'High'
   --
   --  Parameters:
   --    Low  - The first layer
   --    High - The second layer
   --  Returns:
   --    True if 'Low' is below 'High', False otherwise.
   function Is_Below
     (Low  : in     Layer_Type;
      High : in     Layer_Type)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Tests if 'Low' is a layer situated below the layer of 'High' or
   --  if 'Low = High'
   --
   --  Parameters:
   --    Low  - The first layer
   --    High - The second layer
   --  Returns:
   --    True if 'Low' is below or at level with 'High', False otherwise.
   function Is_Below_Or_Equal
     (Low  : in     Layer_Type;
      High : in     Layer_Type)
     return Boolean;


   -----------------
   -- Layer Pools --
   -----------------

   ----------------------------------------------------------------------------
   --  Represents a pool of used layers. Must be reset using 'Reset_Pool'
   --  before a variable can be used.
   type Layer_Pool is private;

   ----------------------------------------------------------------------------
   --  The layer for which:
   --  for all L : Layer_Pool: Is_Below_Or_Equal (Bottom_Layer, L)
   Bottom_Layer : constant Layer_Pool;

   ----------------------------------------------------------------------------
   --  The layer for which:
   --  for all L : Layer_Pool: Is_Below_Or_Equal (L, Top_Layer)
   Top_Layer : constant Layer_Pool;

   ----------------------------------------------------------------------------
   --  Resets 'Pool' so it contains only 'Bottom_Layer'
   procedure Reset_Pool
     (Pool : in out Layer_Pool);

   ----------------------------------------------------------------------------
   --  Gets the highest layer in 'Pool'
   function Get_Highest_Layer
     (Pool : in     Layer_Pool)
     return Layer_Type;

   ----------------------------------------------------------------------------
   --  Adds a new layer to 'Pool' that will be the highest layer in 'Pool'
   --  Precondition:
   --    Is_Below (Get_Highest_Layer, Top_Layer)
   --  Raises:
   --    Constraint_Error if Precondition not satisfied
   procedure Enlarge_Pool
     (Pool : in out Layer_Pool);

   ----------------------------------------------------------------------------
   --  Removes the highest layer from 'Pool'
   --  Precondition:
   --    Is_Below (Bottom_Layer, Get_Highest_Layer (Pool))
   --  Raises:
   --    Constraint_Error if Precondition not satisfied
   procedure Shrink_Pool
     (Pool : in out Layer_Pool);


   --------------
   -- Clipping --
   --------------

   type Layer_Clipping_Change_Type is (Add, Delete);

   type Layer_Clipping_Type is
      record
         Height : Layer_Type;
         Area   : Vis.Absolute.Rectangle_2d;
      end record;

   type Layer_Clipping_Access is access Layer_Clipping_Type;

   procedure Free
     (Clipping : in out Layer_Clipping_Access);

   function Is_Below
     (Left  : in     Layer_Clipping_Access;
      Right : in     Layer_Clipping_Access)
     return Boolean;

   package Clipping_Queues is new Simple_Priority_Queues
     (Item_Type           => Layer_Clipping_Access,
      Has_Higher_Priority => Is_Below);

   type Clipping_Queue_Access is access Clipping_Queues.Queue_Type;


   -----------
   -- Flags --
   -----------

   type Flags_Enumeration_Type is
     (Sized, Locked,                    --  only for nodes
      Incident_Visible, Annotated,
      Hidden, Obsolete,                 --  for edges and nodes
      Current_Local, First_Local, Second_Local, Third_Local,
      First_Global, Second_Global, Third_Global
                                        --  only for edges
      );

   subtype Highlight_Type is
     Flags_Enumeration_Type range Current_Local .. Third_Global;

   subtype Local_Highlight_Type is
     Highlight_Type range Current_Local .. Third_Local;

   subtype Global_Highlight_Type is
     Highlight_Type range First_Global .. Third_Global;

   subtype Edge_Flags_Enumeration_Type is Flags_Enumeration_Type range
     Hidden .. Flags_Enumeration_Type'Last;

   subtype Node_Flags_Enumeration_Type is Flags_Enumeration_Type range
     Flags_Enumeration_Type'First .. Highlight_Type'Last;

   type All_Flags_Type is array (Flags_Enumeration_Type range <>) of Boolean;
   pragma Pack (All_Flags_Type);

   subtype Edge_Flags_Type is All_Flags_Type
     (Edge_Flags_Enumeration_Type'Range);
   subtype Node_Flags_Type is All_Flags_Type
     (Node_Flags_Enumeration_Type'Range);
   subtype Highlight_Array is All_Flags_Type
     (Highlight_Type'Range);

   procedure Write_Highlight_Array
     (Stream       : in     Bauhaus_IO.Out_Stream_Type;
      Highlighting : in     Highlight_Array);

   procedure Read_Highlight_Array
     (Stream       : in     Bauhaus_IO.In_Stream_Type;
      Highlighting :    out Highlight_Array);


   --------------------
   -- Edges & Nodes  --
   --------------------

   ----------------------------------------------------------------------------
   --  Number of inflexion/begin/end points in one edge
   subtype Edge_Point_Number is Positive range 2 .. Positive'Last;

   ----------------------------------------------------------------------------
   --  Type for handling the visual representation of an edge
   type Vis_Edge_Record (Number_Of_Points : Edge_Point_Number) is
     limited private;

   type Vis_Edge_Id is access all Vis_Edge_Record;

   ----------------------------------------------------------------------------
   --  Type for handling the visual representation of a node
   type Vis_Node_Record is limited private;

   type Vis_Node_Id is access all Vis_Node_Record;



   -----------
   -- Edges --
   -----------

   function Create_Edge
     (Graph_Edge  : in     Graph_Lib.Edge_Id;
      Source      : in     Vis_Node_Id;
      Target      : in     Vis_Node_Id;
      Layer       : in     Layer_Type;
      Inflections : in     Natural := 0)
     return Vis_Edge_Id;

   --  Note: Must be manually
   --    * Removed from region manager
   --    * Removed from incident nodes
   procedure Destroy
     (Edge        : in out Vis_Edge_Id);

   --  Removes 'Edge' from the lists of incoming and outgoing edges in
   --  target and source node. Source and target may have been cleared
   --  (see 'Remove_From_Edges').
   --  No subprogram must be called on 'Edge' except 'Destroy' after this.
   procedure Remove_From_Nodes
     (Edge        : in     Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Gets the edge for the visual representation
   function Get_Graph_Edge
     (Edge : in     Vis_Edge_Id)
     return Graph_Lib.Edge_Id;

   function Get_Source
     (Edge : in     Vis_Edge_Id)
     return Vis_Node_Id;

   function Get_Target
     (Edge : in     Vis_Edge_Id)
     return Vis_Node_Id;

   function Is_Loop
     (Edge : in     Vis_Edge_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Gets the layer 'Edge' is inside. No other edge must be in the same
   --  layer. The user must ensure this.
   function Get_Layer
     (Edge  : in     Vis_Edge_Id)
     return Layer_Type;

   function Get_Thickness
     (Edge : in     Vis_Edge_Id)
     return Vis.Absolute_Natural;

   ----------------------------------------------------------------------------
   --  Number of Points for the main line of edge (excluding the arrow)
   function Get_Number_Of_Points
     (Edge : in     Vis_Edge_Id)
     return Edge_Point_Number;

   ----------------------------------------------------------------------------
   --  Get a point of the main line (excluding the arrow). The main line
   --  is drawn along the points returned by this function in ascending
   --  order of 'Num'.
   --
   --  Precondition:
   --    1 <= Num <= Get_Number_Of_Points
   function Get_Point
     (Edge : in     Vis_Edge_Id;
      Num  : in     Positive)
     return Vis.Absolute.Vector_2d;

   function Get_Left_Arrow_Point
     (Edge : in     Vis_Edge_Id)
     return Vis.Absolute.Vector_2d;

   function Get_Right_Arrow_Point
     (Edge : in     Vis_Edge_Id)
     return Vis.Absolute.Vector_2d;

   function Has_Text_Area
     (Edge : in     Vis_Edge_Id)
     return Boolean;

   function Get_Text_Area
     (Edge : in     Vis_Edge_Id)
     return Vis.Absolute.Rectangle_2d;

   function Hits
     (Edge  : in     Vis_Edge_Id;
      Point : in     Vis.Absolute.Vector_2d)
     return Boolean;

   --  Does NOT consider arrowhead.
   --  Does NOT consider thickness.
   function Intersects
     (Edge  : in     Vis_Edge_Id;
      Area  : in     Vis.Absolute.Rectangle_2d)
     return Boolean;

   function Is_Obsolete
     (Edge : in     Vis_Edge_Id)
     return Boolean;

   function Is_Hidden
     (Edge : in     Vis_Edge_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Returns True if one of the nodes incident to 'Edge' has the
   --  'Are_Incident_Visible'-Flag set.
   function Must_Be_Visible
     (Edge : in     Vis_Edge_Id)
     return Boolean;

   function Get_Highlighting
     (Edge : in     Vis_Edge_Id)
     return Highlight_Array;

   procedure Add_Highlight_Color
     (Edge   : in     Vis_Edge_Id;
      Color  : in     Highlight_Type);

   procedure Remove_Highlight_Color
     (Edge   : in     Vis_Edge_Id;
      Color  : in     Highlight_Type);

   procedure Remove_All_Highlight_Colors
     (Edge   : in     Vis_Edge_Id);

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Edge)
   --  Postcontition:
   --    Get_Thickness (Edge) = Thickness
   procedure Set_Thickness
     (Edge      : in     Vis_Edge_Id;
      Thickness : in     Vis.Absolute_Natural);

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Edge)
   --  Postcondition:
   --    Has_Text_Area (Edge) and Get_Size (Get_Text_Area (Edge))) = Size
   procedure Set_Text_Area_Size
     (Edge      : in     Vis_Edge_Id;
      Size      : in     Vis.Absolute.Vector_2d);

   --  Sets the position of the text area
   --
   --  Parameters:
   --    Edge       - This edge's text area will be moved
   --    Position   - Position, the text area will be moved to
   --    Align_Left - If True, then the text area will be left aligned relative
   --                 to 'Position' otherwise it will be right aligned relative
   --                 to 'Position'
   --    Align_Top  - If True, then the text area will be top aligned relative
   --                 to 'Position', otherwise it will be bottom aligned
   --                 relative to 'Position'
   --  Precondition:
   --    * The text area must not be contained in any region manager
   --    * The size of the text area must have been set using
   --      'Set_Text_Area_Size'
   procedure Move_Text_Area_To
     (Edge       : in     Vis_Edge_Id;
      Position   : in     Vis.Absolute.Vector_2d;
      Align_Left : in     Boolean;
      Align_Top  : in     Boolean);

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Edge)
   --  Postcondition:
   --    not Has_Text_Area (Edge)
   procedure Remove_Text_Area
     (Edge      : in     Vis_Edge_Id);

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Edge)
   --  Postcondition:
   --    Get_Point (Edge, Num) = Point
   procedure Set_Point
     (Edge      : in     Vis_Edge_Id;
      Num       : in     Positive;
      Point     : in     Vis.Absolute.Vector_2d);

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Edge)
   --  Postcondition:
   --    Get_Left_Arrow_Point (Edge) = Point
   procedure Set_Left_Arrow_Point
     (Edge      : in     Vis_Edge_Id;
      Point     : in     Vis.Absolute.Vector_2d);

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Edge)
   --  Postcondition:
   --    Get_Right_Arrow_Point (Edge) = Point
   procedure Set_Right_Arrow_Point
     (Edge      : in     Vis_Edge_Id;
      Point     : in     Vis.Absolute.Vector_2d);

   procedure Set_Layer
     (Edge      : in     Vis_Edge_Id;
      Layer     : in     Layer_Type);

   procedure Set_Obsolete
     (Edge      : in     Vis_Edge_Id;
      State     : in     Boolean);

   procedure Set_Hidden
     (Edge      : in     Vis_Edge_Id;
      State     : in     Boolean);

   ----------------------------------------------------------------------------
   --  Total ordering on Vis_Edge_Id
   --  Returns:
   --    True if 'Left' is situated in a layer above 'Right', False else
   function Is_Edge_Below
     (Left  : in     Vis_Edge_Id;
      Right : in     Vis_Edge_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Linear list of 'Vis_Edge_Id's
   package Vis_Edge_Lists is new Lists
     (ItemType   => Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Set of 'Vis_Edge_Id's
   package Vis_Edge_Sets is new Ordered_Sets
     (Item_Type  => Vis_Edge_Id,
      "<"        => Is_Edge_Below);

   ----------------------------------------------------------------------------
   --  Iterators over 'Vis_Edge_Id's in Layer-ascending order
   package Edge_Update_Iterators is new Merging_Iterators
     (Item_Type => Vis_Edge_Id,
      "<"       => Is_Edge_Below,
      Sets      => Vis_Edge_Sets);

   type Vis_Edge_Id_Array is array (Positive range <>) of Vis_Edge_Id;
   type Vis_Edge_Id_Array_Access is access Vis_Edge_Id_Array;


   -----------
   -- Nodes --
   -----------

   function Create_Node
     (Graph_Node : in     Graph_Lib.Node_Id;
      Layer      : in     Layer_Type)
     return Vis_Node_Id;

   --  Note: Must be manually
   --    * Removed from region manager
   --    * Removed from incident edges
   procedure Destroy
     (Node       : in out Vis_Node_Id);

   --  Clears the lists of incoming and outgoing edges. Clears also the
   --  target or source field in those edges. No subprogram except
   --  'Clear_From_Nodes' and 'Destroy' may be called on such an edge.
   procedure Remove_From_Edges
     (Node       : in     Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Gets the node for the visual representation
   function Get_Graph_Node
     (Node : in     Vis_Node_Id)
     return Graph_Lib.Node_Id;

   function Get_Position
     (Node : in     Vis_Node_Id)
     return Vis.Logic.Vector_2d;

   function Get_Top_Center
     (Node : in     Vis_Node_Id)
     return Vis.Absolute.Vector_2d;

   function Get_Center
     (Node : in     Vis_Node_Id)
     return Vis.Absolute.Vector_2d;

   function Get_Extent
     (Node : in     Vis_Node_Id)
     return Vis.Absolute.Rectangle_2d;

   function Hits
     (Node  : in     Vis_Node_Id;
      Point : in     Vis.Absolute.Vector_2d)
     return Boolean;

   function Intersects
     (Node  : in     Vis_Node_Id;
      Area  : in     Vis.Absolute.Rectangle_2d)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Gets the layer 'Node' is inside. No other node must be in the same
   --  layer. The user must ensure this.
   function Get_Layer
     (Node : in     Vis_Node_Id)
     return Layer_Type;

   procedure Make_Incoming_Iterator
     (Node           : in     Vis_Node_Id;
      Incoming_Edges :    out Vis_Edge_Lists.ListIter);

   procedure Make_Outgoing_Iterator
     (Node           : in     Vis_Node_Id;
      Outgoing_Edges :    out Vis_Edge_Lists.ListIter);

   function Is_Obsolete
     (Node : in     Vis_Node_Id)
     return Boolean;

   function Is_Hidden
     (Node : in     Vis_Node_Id)
     return Boolean;

   function Is_Annotated
     (Node : in     Vis_Node_Id)
     return Boolean;

   function Is_Sized
     (Node : in     Vis_Node_Id)
     return Boolean;

   function Is_Locked
     (Node : in     Vis_Node_Id)
     return Boolean;

   function Are_Incident_Visible
     (Node : in     Vis_Node_Id)
     return Boolean;

   function Get_Highlighting
     (Node : in     Vis_Node_Id)
     return Highlight_Array;

   procedure Set_Position
     (Node     : in Vis_Node_Id;
      Position : in Vis.Logic.Vector_2d);

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Node)
   procedure Set_Node_Size
     (Node : in     Vis_Node_Id;
      Size : in     Vis.Absolute.Vector_2d);

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Node)
   procedure Move_Node
     (Node   : in     Vis_Node_Id;
      Offset : in     Vis.Absolute.Vector_2d);

   procedure Set_Layer
     (Node   : in     Vis_Node_Id;
      Layer  : in     Layer_Type);

   procedure Set_Obsolete
     (Node   : in     Vis_Node_Id;
      State  : in     Boolean);

   procedure Set_Hidden
     (Node   : in     Vis_Node_Id;
      State  : in     Boolean);

   procedure Set_Annotated
     (Node   : in     Vis_Node_Id;
      State  : in     Boolean);

   procedure Set_Sized
     (Node   : in     Vis_Node_Id;
      State  : in     Boolean);

   procedure Set_Locked
     (Node   : in     Vis_Node_Id;
      State  : in     Boolean);

   procedure Set_Incident_Visible
     (Node   : in     Vis_Node_Id;
      State  : in     Boolean);

   procedure Add_Highlight_Color
     (Node   : in     Vis_Node_Id;
      Color  : in     Highlight_Type);

   procedure Remove_Highlight_Color
     (Node   : in     Vis_Node_Id;
      Color  : in     Highlight_Type);

   procedure Remove_All_Highlight_Colors
     (Node   : in     Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Total ordering on Vis_Node_Id
   --  Returns:
   --    True if 'Left' is situated in a layer above 'Right', False else
   function Is_Node_Below
     (Left  : in     Vis_Node_Id;
      Right : in     Vis_Node_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Linear list of 'Vis_Edge_Id's
   package Vis_Node_Lists is new Lists
     (ItemType   => Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Set of 'Vis_Node_Id's
   package Vis_Node_Sets is new Ordered_Sets
     (Item_Type  => Vis_Node_Id,
      "<"        => Is_Node_Below);

   ----------------------------------------------------------------------------
   --  Iterators over 'Vis_Node_Id's in Layer-ascending order
   package Node_Update_Iterators is new Merging_Iterators
     (Item_Type  => Vis_Node_Id,
      "<"        => Is_Node_Below,
      Sets       => Vis_Node_Sets);

   type Vis_Node_Id_Array is array (Positive range <>) of Vis_Node_Id;
   type Vis_Node_Id_Array_Access is access Vis_Node_Id_Array;


   --------------------
   -- Region Manager --
   --------------------

   ----------------------------------------------------------------------------
   --  Type used to manage a graph within a two dimensional embedding
   --  provides functionality to add edges and nodes, to remove them and
   --  to find intersection among them.
   --  Should be limited type, but is not in order to allow objects of this
   --  type to be fields in gtk-widget-types (which are not limited).
   --  The Assignment is nevertheless forbidden and can lead to undefined
   --  behavior.
   type Region_Manager is private;

   package Rectangle_2d_Lists is new Lists
     (ItemType => Vis.Absolute.Rectangle_2d);

   ----------------------------------------------------------------------------
   --  Must be called before the region manager can be used.
   --
   --  Parameters:
   --    Manager - The object to initialize
   procedure Set_Up
     (Manager : in out Region_Manager);

   ----------------------------------------------------------------------------
   --  Frees all storage used by a region manager. Leaves the region manager
   --  in an undefined state.
   --
   --  Parameters:
   --    Manager - The object to destroy. Must not be used anymore
   procedure Destroy
     (Manager : in out Region_Manager);

   ----------------------------------------------------------------------------
   --  Initializes a managed graph display region. If at all then must be
   --  called before any other subprogram operating on 'Region_Manager'.
   --
   --  Gives the region manager an initial size and an initial number of
   --  managed nodes. This is only an estimate and the actual size can grow
   --  infinitely (except for storage limitations).
   --
   --  If this subprogram is not called then default values will be used.
   --
   --  Parameters:
   --    Manager         - The object to initialize
   --    Size            - Estimate of the size of the display region
   --    Number_Of_Nodes - Estimate of the number of nodes managed
   procedure Init_Region_Manager
     (Manager         : in out Region_Manager;
      Size            : in     Vis.Absolute.Vector_2d;
      Number_Of_Nodes : in     Natural);

   ----------------------------------------------------------------------------
   --  Optimizes the size of a drawing area so that update operations in
   --  that area will be necessary as less often as possible.
   --
   --  Parameters:
   --    Manager - The region manager to optimize for
   --    Area    - The drawing area
   --  Postcondition:
   --    All points previously contained in 'Area' will still be contained in
   --    'Area'
   procedure Optimize_Drawing_Area
     (Manager : in     Region_Manager;
      Area    : in out Vis.Absolute.Rectangle_2d);

   ----------------------------------------------------------------------------
   --  Inserts one edge into the set of managed edges. All content above the
   --  visual representation of that edge and the visual representation itself
   --  are polluted.
   procedure Insert_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Drops one edge. This edge is not managed anymore. All content
   --  intersecting the visual representation of that edge are polluted.
   procedure Drop_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Returns the top-most edge at 'Point' or null if such an edge does
   --  not exist in 'Manager'
   function Get_Edge_At
     (Manager : in     Region_Manager;
      Point   : in     Vis.Absolute.Vector_2d)
     return Vis_Edge_Id;

   ----------------------------------------------------------------------------
   --  Returns True if 'Edge' is contained in a region manager,
   --  False otherwise.
   --  Note:
   --    An edge can be contained in one region manager at most.
   function Has_Manager
     (Edge    : in     Vis_Edge_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Changes the layer of 'Edge' and adds the necessary pollution
   procedure Change_Layer
     (Manager   : in out Region_Manager;
      Edge      : in     Vis_Edge_Id;
      New_Layer : in     Layer_Type);

   ----------------------------------------------------------------------------
   --  Inserts one node into the set of managed nodes. All content above the
   --  visual representation of that node and the visual representation
   --  itself are polluted.
   procedure Insert_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Drops one node. This node is not managed anymore. All content
   --  intersecting the visual representation of that node are polluted.
   procedure Drop_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Returns the top-most node at 'Point' or null if such a node does not
   --  exist in 'Manager'
   function Get_Node_At
     (Manager : in     Region_Manager;
      Point   : in     Vis.Absolute.Vector_2d)
     return Vis_Node_Id;

   ----------------------------------------------------------------------------
   --  Returns True if 'Node' is contained in a region manager,
   --  False otherwise.
   --  Note:
   --    An edge can be contained in one region manager at most.
   function Has_Manager
     (Node    : in     Vis_Node_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Changes the layer of 'Node' and adds the necessary pollution
   procedure Change_Layer
     (Manager   : in out Region_Manager;
      Node      : in     Vis_Node_Id;
      New_Layer : in     Layer_Type);

   ----------------------------------------------------------------------------
   --  Pollutes all content above the visual representation of one edge
   --  and the visual representation itself.
   --
   --  Parameters:
   --    Manager - The region manager
   --    Edge    - The edge to be polluted
   procedure Pollute_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Pollutes all content above the visual representation of one node and
   --  the visual representation itself.
   --
   --  Parameters:
   --    Manager - The region manager
   --    Node    - The node to be polluted
   procedure Pollute_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Pollutes all content and background within an area.
   --
   --  Parameters:
   --    Manager - The region manager
   --    Area    - Area to be polluted
   procedure Pollute_Area
     (Manager : in out Region_Manager;
      Area    : in     Vis.Absolute.Rectangle_2d);

   ----------------------------------------------------------------------------
   --  Takes as input a rectangle and a set of nodes contained in or
   --  intersecting that rectangle, returns lists of nodes to be added to
   --  or removed from that set if the rectangle is changed to a different
   --  rectangle
   --
   --  Parameters:
   --    Manager      - The region manager
   --    Old_Area     - A rectangle
   --    New_Area     - A new rectangle
   --    Add_Edges    - List of edges to be added to 'Content', so 'Content'
   --                   will contain all edges in or intersecting 'New_Area'.
   --                   This list must be destroyed.
   --    Remove_Edges - List of edges to be removed from 'Content', so
   --                   'Content' will not contain any edge not in or
   --                   intersecting 'New_Area'. This list must be destroyed.
   --    Add_Nodes    - List of nodes to be added to 'Content', so 'Content'
   --                   will contain all nodes in or intersecting 'New_Area'.
   --                   This list must be destroyed.
   --    Remove_Nodes - List of nodes to be removed from 'Content', so
   --                   'Content' will not contain any node not in or
   --                   intersecting 'New_Area'. This list must be destroyed.
   procedure Update_Area_Content
     (Manager      : in     Region_Manager;
      Old_Area     : in     Vis.Absolute.Rectangle_2d;
      New_Area     : in     Vis.Absolute.Rectangle_2d;
      Add_Edges    :    out Vis_Edge_Lists.List;
      Remove_Edges :    out Vis_Edge_Lists.List;
      Add_Nodes    :    out Vis_Node_Lists.List;
      Remove_Nodes :    out Vis_Node_Lists.List);

   ----------------------------------------------------------------------------
   --  See 'Update_Area_Content'. But works only on nodes. Ignores any edges
   --  intersected by 'Old_Area' or 'New_Area' to increase performance only.
   procedure Update_Area_Content_Nodes
     (Manager      : in     Region_Manager;
      Old_Area     : in     Vis.Absolute.Rectangle_2d;
      New_Area     : in     Vis.Absolute.Rectangle_2d;
      Add_Nodes    :    out Vis_Node_Lists.List;
      Remove_Nodes :    out Vis_Node_Lists.List);

   ----------------------------------------------------------------------------
   --  Command used to organize a buffer refresh for the content af a region
   --  manager. Refresh must be done in the order:
   --  1. Background
   --  2. Edges
   --  3. Nodes
   --  (4. Unchanged space)
   --
   --  Fields:
   --    Reset         - The rectangles are areas to be reset to background
   --                    color. If a rectangle is drawn, then clipping on
   --                    that rectangle is opened for edge and node drawing.
   --    Edge_Clipping - Clipping of a specified rectangle opens before any
   --                    edge in the same layer is drawn.
   --    Edges         - All edges to be drawn in correct (bottom-to-top)
   --                    order. Only drawing inside open clipping area is
   --                    needed. Other drawing must be avoided or removed
   --                    later.
   --    Node_Clipping - Same as Edge_Clipping, but for nodes. Note that
   --                    all rectangles opened by Reset or by Edge_Clipping
   --                    are open as well
   --    Nodes         - Same as Edges for nodes
   --    Unchanged     - All rectangles where clipping remained closed. The
   --                    previous content should remain here unchanged.
   --  Note:
   --    * NO rectangle may be contained in more than one "list" of
   --      Reset, Edge_Clipping, Node_Clipping, Unchanged
   --    * The desired area must be covered entirely by the rectangles in
   --      Reset, Edge_Clipping, Node_Clipping, Unchanged
   type Refresh_Command_Type is
      record
         Reset          : Rectangle_2d_Lists.List;
         Edge_Clipping  : Clipping_Queue_Access;
         Edges          : Edge_Update_Iterators.Merger_Access;
         Node_Clipping  : Clipping_Queue_Access;
         Nodes          : Node_Update_Iterators.Merger_Access;
         Unchanged      : Rectangle_2d_Lists.List;
      end record;

   procedure Start_Refresh_Operation
     (Manager         : in out Region_Manager;
      Refresh_Area    : in     Vis.Absolute.Rectangle_2d;
      Command         :    out Refresh_Command_Type;
      Refresh_Pending : in     Boolean);

   procedure End_Refresh_Operation
     (Command         : in out Refresh_Command_Type);


private

   ----------------------------------------------------------------------------
   --  Height level of a Vis_Edge or a Vis_Node. The higher the value, the
   --  higher the Vis_Edge/Node_Edge
   type Layer_Type is new Natural;

   ----------------------------------------------------------------------------
   --  Top layer in the pool.
   type Layer_Pool is new Natural; --  Layer_Type;

   Bottom_Layer : constant Layer_Pool := Layer_Pool'First;
   Top_Layer    : constant Layer_Pool := Layer_Pool'Last;


   ----------------------------------------------------------------------------
   --  Frees all items and frees clipping queue
   procedure Destroy_Clipping_Queue
     (Queue : in out Clipping_Queue_Access);


   ----------------------------------------------------------------------------
   --  Container that covers a certain region. Contains an estimate of all
   --  'Vis_Edge_Id's and all 'Vis_Node_Id's that have intersection with
   --  that region.
   type Region_Record;

   type Region_Id is access all Region_Record;

   ----------------------------------------------------------------------------
   --  Region coordinate system
   type Region_Position is new Vis.Absolute.Vector_2d;

   ----------------------------------------------------------------------------
   --  Hash function on 'Region_Position's.
   --
   --  Parameters:
   --    Key - position of a region
   --  Returns:
   --    Hash value
   --  Postcondition:
   --    For all A, B: Region_Position:
   --      (A = B)  ==>  (Hash_Region_Position (A) = Hash_Region_Position (B))
   function Hash_Region_Position
     (Key : in Region_Position)
     return Integer;

   ----------------------------------------------------------------------------
   --  Strict order on 'Region_Position's
   --
   --  Parameters:
   --    Left  - position of a region
   --    Right - position of a region
   function Order_Position
     (Left  : in    Region_Position;
      Right : in    Region_Position)
     return Boolean;

   package Position_Sets is new Ordered_Sets
     (Item_Type => Region_Position,
      "<"       => Order_Position);

   ----------------------------------------------------------------------------
   --  A 'Position_Pool' represents a rectangle of 'Region_Position's
   type Position_Pool is new Vis.Absolute.Rectangle_2d;

   ----------------------------------------------------------------------------
   --  Get the 'Position_Pool' so that the regions that correspond to
   --  positions within the pool contain 'Area'.
   function Create_Position_Pool_From_Area
     (Manager      : in     Region_Manager;
      Area         : in     Vis.Absolute.Rectangle_2d)
     return Position_Pool;

   function Create_Position_Pool
     (Top_Left     : in     Region_Position;
      Bottom_Right : in     Region_Position)
     return Position_Pool;

   --  Get the area actually covered by 'Pool'. This area might contain more
   --  points than the 'Area' given into 'Create_Position_Pool_From_Area'
   function Get_Position_Pool_Area
     (Manager      : in     Region_Manager;
      Pool         : in     Position_Pool)
     return Vis.Absolute.Rectangle_2d;

   function Get_Position_Pool_Size
     (Pool         : in     Position_Pool)
     return Natural;

   ----------------------------------------------------------------------------
   --  Allows iteration over all 'Region_Position's in a 'Position_Pool'
   type Position_Iterator is
      record
         Current : Region_Position;
         Pool    : Position_Pool;
      end record;

   procedure Make_Position_Iterator
     (Pool     : in     Position_Pool;
      Iterator :    out Position_Iterator);

   function Has_More
     (Iterator : in     Position_Iterator)
     return Boolean;

   function Get_Current
     (Iterator : in     Position_Iterator)
     return Region_Position;

   procedure Next
     (Iterator : in out Position_Iterator);

   ----------------------------------------------------------------------------
   --  Linear lists of 'Region_Id's
   package Region_Lists is new Lists
     (ItemType => Region_Id);

   package Region_Mappings is new Hashed_Mappings
     (Key_Type   => Region_Position,
      Hash       => Hash_Region_Position,
      Value_Type => Region_Id);

   ----------------------------------------------------------------------------
   --  Array of Points
   type Absolute_Point_Array is array
     (Positive range <>)
     of Vis.Absolute.Vector_2d;

   type Vis_Edge_Record (Number_Of_Points : Edge_Point_Number) is
      record
         --  Represented edge
         Edge              : Graph_Lib.Edge_Id;
         --  Source node of this edge
         Source            : Vis_Node_Id;
         --  Target node of this edge
         Target            : Vis_Node_Id;
         --  Width of this edge
         Thickness         : Vis.Absolute_Natural;
         --  Area where text can be drawn or else Get_Height = 'Last
         Text_Area         : Vis.Absolute.Rectangle_2d;
         --  Layer of this edge
         Layer             : Layer_Type;
         --  Start point, inflection points, end point
         Points            : Absolute_Point_Array (1 .. Number_Of_Points);
         --  Left point of arrow
         Left_Arrow_Point  : Vis.Absolute.Vector_2d;
         --  Right point of arrow
         Right_Arrow_Point : Vis.Absolute.Vector_2d;
         --  (Over-)Estimate of regions this edge is contained in
         Regions           : Region_Lists.List;
         --  Visual attributes of this edge
         Flags             : Edge_Flags_Type;
      end record;

   type Vis_Node_Record is
      record
         --  Position of this node in logical space
         Position       : Vis.Logic.Vector_2d;
         --  Represented node
         Node           : Graph_Lib.Node_Id;
         --  Rectangle of this node
         Extent         : Vis.Absolute.Rectangle_2d;
         --  Hight level of this node
         Layer          : Layer_Type;
         --  Edges with this node as target
         Incoming_Edges : Vis_Edge_Lists.List;
         --  Edges with this node as source
         Outgoing_Edges : Vis_Edge_Lists.List;
         --  (Over-)Estimate of regions this node is contained in
         Regions        : Region_Lists.List;
         --  Visual attributes of this node
         Flags          : Node_Flags_Type;
      end record;

   type Region_Record is limited
      record
         --  Size of this region
         Extent              : Vis.Absolute.Rectangle_2d;
         --  All contents in this region above this 'Vis_Edge_Id' and this
         --  'Vis_Edge_Id' are polluted, or null
         Polluted_Edge       : Vis_Edge_Id;
         --  All contents in this region above this 'Vis_Node_Id' and this
         --  'Node_Id' are polluted, or null
         Polluted_Node       : Vis_Node_Id;
         --  The Background of this region and all contents are polluted
         --  iff True.
         Background_Polluted : Boolean;
         --  Edges possibly contained in or intersecting this region
         Edges               : Vis_Edge_Sets.Set;
         --  Nodes possibly contained in or intersecting this region
         Nodes               : Vis_Node_Sets.Set;
      end record;


   Default_Region_Width  : constant := 350;
   Default_Region_Height : constant := 600;

   type Region_Manager is new Ada.Finalization.Controlled with
      record
         Region_Width              : Vis.Absolute_Natural;
         Region_Height             : Vis.Absolute_Natural;
         Regions                   : Region_Mappings.Mapping;
      end record;

end Giant.Vis_Data;
