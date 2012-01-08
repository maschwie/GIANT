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
--  $RCSfile: giant-graph_widgets.ads,v $, $Revision: 1.51 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
--  This package contains the graph widget. The graph widget is used to
--  display the graph within a GtkAda Window.
--
--  The user can perform the following actions within a graph widget, these
--  actions are handled by the graph widget:
--  * Resizing the graph widget
--  * Move a Node via Drag'n'Drop
--
--  The user can perform the following actions within a graph widget, these
--  actions are recognized by the graph widget and the graph widget's clients
--  will be notified. See package Graph_Widgets.Notifications.
--  * Open a PopUp Menu on the background of the graph widget
--  * Open a PopUp Menu on one specific edge
--  * Open a PopUp Menu on one specific node
--  * select the command to clear the current selection
--  * Select one specific edge or one specific node and only this item
--  * Invert the selection state of one specific edge/node
--  * Open a box and select the command to select all edges and nodes that
--    are situated within this frame (and only those).
--  * Open a box and select the command to invert the selection status of
--    all edges and nodes that are situated within this frame.
--  * Open a box and select the command to add all edges and nodes that are
--    situated within this frame to the current selection.
--
--  The graph widget can be controlled by the program to:
--  * Insert edges or nodes into the graph widget
--  * Remove edges or nodes from the graph widget
--  * Set the color scheme to a certain visualization style
--  * Display some edges or nodes in a certain "global" highlight color
--  * Display some edges or nodes in a certain "local" highlight color
--


with Ada.Streams;

with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Cursor;
with Gtk.Drawing_Area;
with Gdk.Font;
with Gdk.GC;
with Gdk.Pixmap;
with Gdk.Types;
with Glib;
with Gtk.Adjustment;
with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Main;
pragma Elaborate_All (Gtk.Main);

with Bauhaus_IO;
with Hashed_Mappings;
pragma Elaborate_All (Hashed_Mappings);
with Ordered_Sets;
pragma Elaborate_All (Ordered_Sets);

with Giant.Config;
with Giant.Config.Global_Data;
with Giant.Config.Vis_Styles;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Graph_Lib.Subgraphs;
with Giant.Node_Annotations;
with Giant.Vis;
with Giant.Vis_Data;

package Giant.Graph_Widgets is

   -------------------
   -- Graph Widgets --
   -------------------

   ----------------------------------------------------------------------------
   --  A widget to display a graph
   type Graph_Widget_Record is
     new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;

   type Graph_Widget is access all Graph_Widget_Record'Class;


   ------------------------
   -- General Exceptions --
   ------------------------

   ----------------------------------------------------------------------------
   --  Raised whenever a feature is activated that is not supported by the
   --  current version of Graph_Widgets.
   Unimplemented : exception;

   ----------------------------------------------------------------------------
   --  Raised whenever an Edge_Id is provided as an argument, but this
   --  Edge_Id was not previously inserted into the graph widget
   Unknown_Edge_Id : exception;

   ----------------------------------------------------------------------------
   --  Raised whenever a Node_Id is provided as an argument, but this
   --  Node_Id was not previously inserted into the graph widget
   Unknown_Node_Id : exception;


   -------------------------------
   -- Construction, Destruction --
   -------------------------------

   ----------------------------------------------------------------------------
   --  Creates an empty graph widget.
   --
   --  Parameters:
   --    Widget       - Access to a new graph widget
   --    Style        - The visualization style to be used for 'Widget'
   procedure Create
     (Widget      :    out Graph_Widget;
      Style       : in     Config.Vis_Styles.Visualisation_Style_Access :=
        Config.Vis_Styles.Get_Default_Vis_Style;
      Annotations : in     Node_Annotations.Node_Annotation_Access      :=
        Node_Annotations.Create_Empty);

   ----------------------------------------------------------------------------
   --  Creates a new Graph_Widget and sets its state to the state stored in
   --  'Stream'.
   --
   --  Parameters:
   --    Stream - The stream to read the state from
   --    Widget - Access to a new graph widget
   --    Pool   - Pool of node annotations
   --  Raises
   --    Any of Ada.Io_Exceptions
   procedure Read_Graph_Widget
     (Stream      : in     Bauhaus_IO.In_Stream_Type;
      Widget      :    out Graph_Widget;
      Style       : in     Config.Vis_Styles.Visualisation_Style_Access;
      Annotations : in     Node_Annotations.Node_Annotation_Access);

   ----------------------------------------------------------------------------
   --  Outputs a graph widget to 'Stream'. It can then be read into memory
   --  again using 'Read_Graph_Widget'. The local highlight colors are stored,
   --  global highlighting is not.
   --
   --  Parameters:
   --    Stream - The stream to output the state to
   --    Widget - The graph widget to output
   --  Raises
   --    Any of Ada.Io_Exceptions
   procedure Write_Graph_Widget
     (Stream : in     Bauhaus_IO.Out_Stream_Type;
      Widget : access Graph_Widget_Record);


   -------------------
   -- Configuration --
   -------------------

   --  Here are the configuration subprograms available for graph widgets.
   --  Note that the greatest part of the configuration is obtained
   --  directly from the packages 'Giant.Config.*'.
   --  See 'Giant.Graph_Widgets.Settings' for more configuration.

   ----------------------------------------------------------------------------
   --  Sets the node annotation pool used for 'Widget'. Every node
   --  in 'Widget' will retrieve its annotation info from this pool.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Pool   - The node annotation pool
   procedure Set_Node_Annotations
     (Widget : access Graph_Widget_Record'Class;
      Pool   : in     Node_Annotations.Node_Annotation_Access);

   ----------------------------------------------------------------------------
   --  Sets the cursor used inside the graph widget during the time when
   --  the graph widget is not in action mode and is not waiting for a lock
   --  to be released.
   --
   --  If this subprogram is never called, then the cursor for
   --  'Gdk.Types.Left_Ptr' will be used.
   --
   --  Note:
   --    The cursor is never destroyed. It is the user's responsibility to
   --    do so, after either a different cursor has been set in the graph
   --    widget, or the graph widget is destroyed.
   --  Parameters:
   --    Widget - The graph widget
   --    Cursor - The cursor
   --  Precondition:
   --    Cursor /= Null_Cursor, else nothing will be done.
   procedure Set_Default_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor);

   ----------------------------------------------------------------------------
   --  Sets the cursor used inside the graph widget during the time when
   --  the graph widget is waiting for a lock to be released or when
   --  the graph widget is performing heavy calculations.
   --
   --  If this subprogram is never called, then the cursor for
   --  'Gdk.Types.Watch' will be used.
   --
   --  Note:
   --    The cursor is never destroyed. It is the user's responsibility to
   --    do so, after either a different cursor has been set in the graph
   --    widget, or the graph widget is destroyed.
   --  Parameters:
   --    Widget - The graph widget
   --    Cursor - The cursor
   --  Precondition:
   --    Cursor /= Null_Cursor, else nothing will be done.
   procedure Set_Waiting_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor);

   ----------------------------------------------------------------------------
   --  Sets the cursor used inside the graph widget during the time when
   --  the graph widget is in action mode.
   --
   --  If this subprogram is never called, then the cursor for
   --  'Gdk.Types.Crosshair' will be used.
   --
   --  Note:
   --    The cursor is never destroyed. It is the user's responsibility to
   --    do so, after either a different cursor has been set in the graph
   --    widget, or the graph widget is destroyed.
   --  Parameters:
   --    Widget - The graph widget
   --    Cursor - The cursor
   --  Precondition:
   --    Cursor /= Null_Cursor, else nothing will be done.
   procedure Set_Action_Mode_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor);


   --------------------------------------------
   -- Insertion, Deletion of Edges and Nodes --
   --------------------------------------------

   ----------------------------------------------------------------------------
   --  Describes a Lock that a Layouter must acquire before making changes
   --  to the layout.
   type Lock_Type is private;

   ----------------------------------------------------------------------------
   --  Checks if an edge is contained in a graph widget
   --
   --  Note:
   --    Even if an edge is contained in the widget, it might still be
   --    hidden and thus the user might be unaware of its presence.
   --  Parameters:
   --    Widget - The graph widget
   --    Edge   - The edge to be searched for
   --  Returns:
   --    True if 'Edge' is contained in 'Widget', False else
   function Contains
     (Widget   : access Graph_Widget_Record'Class;
      Edge     : in     Graph_Lib.Edge_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Checks if a node is contained in a graph widget
   --
   --  Note:
   --    Even if a node is contained in the widget, it might still be
   --    hidden and thus the user might be unaware of its presence.
   --  Parameters:
   --    Widget - The graph widget
   --    Node   - Node to be searched for
   --  Returns:
   --    True if 'Node' is contained in 'Widget', False else
   function Contains
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Graph_Lib.Node_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Copies the all edges and all nodes contained in 'Widget' into a new
   --  subgraph.
   --
   --  Note: The subgraph returned by this function must be destroyed as
   --  described in package 'Graph_Lib.Subgraphs'
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Name   - Name for the new subgraph
   --  Returns:
   --    A new subgraph covering all content of 'Widget'. May be modified
   --    and must be destroyed.
   function Get_Content
     (Widget    : access Graph_Widget_Record'Class;
      Name      : in     String := "")
     return Graph_Lib.Subgraphs.Subgraph;

   ----------------------------------------------------------------------------
   --  Inserts edges and nodes from 'Selection' into a 'Widget'. An edge
   --  is inserted only if its two incident nodes are contained in 'Widget'.
   --
   --  This subprogram produces a lock for the inserted selection. The
   --  inserted selection is locked until 'Lock' is released by a call
   --  to 'Release_Lock' (see below). Until then the newly inserted selection
   --  is not shown inside the graph widget.
   --
   --  Usually a layouter should be started on the selection and after that
   --  layouter has finished the lock should be released.
   --
   --  Note:
   --    The user probably cannot perform any action on the graph widget until
   --    the lock is released.
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - A collection of edges and nodes
   --    Lock      - A lock for the inserted selection
   --  Postcondition:
   --    For all N: Node_Id in 'Selection': 'Contains (Widget, N)'
   --    For all E: Edge_Id in 'Selection': 'Contains (Widget, E)' if and only
   --      if {source (E), target (E)} is a subset of
   --      'Selection' union {N: Node_Id | 'Contains (Widget, N)'}
   procedure Insert_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in     Graph_Lib.Selections.Selection;
      Lock      :    out Lock_Type);

   ----------------------------------------------------------------------------
   --  Removes all edges and all nodes from 'Selection' that 'Widget' contains
   --  already. Then, it inserts the remaining edges and nodes into 'Widget'.
   --
   --  This subprogram produces a lock for the inserted selection. The
   --  inserted selection is locked until 'Lock' is release by a call
   --  to 'Release_Lock' (see below). Until then the newly inserted selection
   --  is not shown inside the graph widget.
   --
   --  Usually a layouter should be started on the selection and after that
   --  layouter has finished the lock should be released.
   --
   --  Note:
   --    * The contents of 'Selection' are modified by this subprogram!
   --    * The user probably cannot perform actions on the graph widget until
   --      the lock is released.
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - Set of edges and nodes that should be inserted into
   --                'Widget', also used as out-Parameter
   procedure Insert_Selection_Difference
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in out Graph_Lib.Selections.Selection;
      Lock      :    out Lock_Type);

   ----------------------------------------------------------------------------
   --  Removes all edges and nodes in 'Selection' from 'Widget'. If there
   --  is a lock held on 'Widget' then the removal is deferred until all
   --  locks are released to allow a concurrently executed layouter to finish.
   --
   --  Note that all nodes in the graph widget that are incident to any
   --  edge in 'Selection' are removed as well.
   --
   --  If edges or nodes that are contained in the current selection
   --  are removed then a change signal will be emitted.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - Set of edges and nodes to be removed
   procedure Remove_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in     Graph_Lib.Selections.Selection);

   ----------------------------------------------------------------------------
   --  Removes all edges and nodes from 'Widget'. If there is a lock held
   --  on 'Widget' then the removal is deferred until all locks are released
   --  to allow a concurrently executed layouter to finish.
   --
   --  Parameters:
   --    Widget - The graph widget
   procedure Clear
     (Widget : access Graph_Widget_Record'Class);


   -----------------
   -- Action Mode --
   -----------------

   ----------------------------------------------------------------------------
   --  Enables action mode. During action mode the user can
   --  only move the visual area inside the graph widget and click onto the
   --  graph widget. After each such click the graph widget emits a signal.
   --  See Giant.Graph_Widgets.Handlers for details.
   --
   --  Parameters:
   --    Widget - The graph widget
   procedure Start_Action_Mode
     (Widget : access Graph_Widget_Record'Class);

   ----------------------------------------------------------------------------
   --  If the graph widget is in action mode, then cancels action mode and
   --  sets the cursor back to the graph widget's default cursor or to the
   --  waiting cursor. Does nothing otherwise.
   --
   --  Parameters:
   --    Widget - The graph widget
   procedure Cancel_Action_Mode
     (Widget : access Graph_Widget_Record'Class);

   ----------------------------------------------------------------------------
   --  Checks if the graph widget is in action mode.
   --
   --  Parameters:
   --    Widget - The graph widget
   --  Returns:
   --    True if the graph widget is in action mode, False otherwise.
   function Is_Action_Mode_Active
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;


   ------------
   -- Layout --
   ------------

   ----------------------------------------------------------------------------
   --  Raised whenever a layout-modifying subprogram is called without the
   --  correct lock.
   Illegal_Lock_State : exception;

   ----------------------------------------------------------------------------
   --  Acquires a global lock for all nodes contained in 'Widget'. The owner
   --  of that lock may modify the layout on all nodes. The graph widget
   --  will stop displaying its content until the lock is released.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Lock   - The lock handle
   procedure Lock_All_Content
     (Widget    : access Graph_Widget_Record'Class;
      Lock      :    out Lock_Type);

   ----------------------------------------------------------------------------
   --  Acquires a lock for a given selection. The owner of that lock may
   --  modify the layout on all nodes in that selection. The graph widget
   --  will stop displaying its content until the lock is released.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - The selection to be locked
   --    Lock      - The lock handle
   --  Precondition:
   --    'Selection' is a subset of {E: Edge_Id | Contains (Widget, E)} union
   --                                {N: Node_Id | Contains (Widget, N)}
   --  Raises:
   --    * Unknown_Edge_Id if Precondition not satisfied
   --    * Unknown_Node_Id if Precondition not satisfied
   procedure Lock_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in     Graph_Lib.Selections.Selection;
      Lock      :    out Lock_Type);

   ----------------------------------------------------------------------------
   --  Modifies the position of one node.
   --
   --  Parameters:
   --    Widget   - The graph widget containing 'Node'
   --    Node     - The node to be moved
   --    Location - The new Position of the top middle point of 'Node'
   --    Lock     - The Lock handle previously acquired for a 'Node'
   --  Precondition:
   --    'Contains (Widget, Node)' and 'Lock' locks 'Node'
   --  Postcondition:
   --    'Get_Top_Middle (Widget, Node)' = 'Location'
   --  Note:
   --    Request will be ignored if Precondition not satisfied.
   procedure Set_Top_Middle
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Graph_Lib.Node_Id;
      Location  : in     Vis.Logic.Vector_2d;
      Lock      : in     Lock_Type);

   ----------------------------------------------------------------------------
   --  Gets the position of one node.
   --
   --  Parameters:
   --    Widget - The graph widget containing 'Node'
   --    Node   - A node
   --  Returns:
   --    The top middle point of node in logical vector space
   --  Precondition:
   --    'Contains (Widget, Node)' or 'Node' was removed and a lock was held
   --    and not removed before the call to this function.
   --  Raises:
   --    Unknown_Node_Id if Precondition not satisfied
   function Get_Top_Middle
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Graph_Lib.Node_Id)
     return Vis.Logic.Vector_2d;

   ----------------------------------------------------------------------------
   --  Gets the maximum node width using the current settings and the current
   --  detail level in 'Widget'.
   --
   --  Note:
   --    * The value returned by this function may change at any point of time.
   --      It may only be used as a general clue.
   --    * There is no 'Get_Current_Maximum_Node_Height' because the height
   --      is dependent on the number of attributes shown within each node.
   --      Use 'Get_Current_Node_Height' instead.
   --  Parameters:
   --    Widget - The graph widget
   --  Returns:
   --    An estimate of the maximum width that will be assigned to a node in
   --    'Widget'
   function Get_Current_Maximum_Node_Width
     (Widget    : access Graph_Widget_Record'Class)
     return Vis.Logic_Float;

   ----------------------------------------------------------------------------
   --  Gets the current width of one node given the current settings and the
   --  current detail level in 'Widget'.
   --
   --  Note:
   --    The value returned by this function may change at any point of time.
   --    It may only be used as a general clue.
   --  Parameters:
   --    Widget - The graph widget
   --    Node   - The node to be measured
   --  Returns:
   --    An estimate of the width that will be assigned to 'Node' in 'Widget'
   function Get_Current_Node_Width
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Graph_Lib.Node_Id)
     return Vis.Logic_Float;

   ----------------------------------------------------------------------------
   --  Gets the current height of one node given the current settings and the
   --  current detail level in 'Widget'.
   --
   --  Note:
   --    The value returned by this function may change at any point of time.
   --    It may only be used as a general clue.
   --  Parameters:
   --    Widget - The graph widget
   --    Node   - The node to be measured
   --  Returns:
   --    An estimate of the height that will be assigned to 'Node' in 'Widget'
   --  Precondition:
   --    'Contains (Widget, Node)' or 'Node' was removed while a lock was
   --    held on 'Widget' and not released before the call to this function
   --  Raises:
   --    Unknown_Node_Id if precondition not satisfied
   function Get_Current_Node_Height
     (Widget    : access Graph_Widget_Record'Class;
      Node       : in     Graph_Lib.Node_Id)
     return Vis.Logic_Float;

   ----------------------------------------------------------------------------
   --  Discards a lock previously acquired by 'Lock_All_Content' or
   --  'Lock_Selection'. That lock must not be used anymore.
   --  The graph widget will display all changes made to the layout.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Lock   - The lock to be destroyed
   procedure Release_Lock
     (Widget    : access Graph_Widget_Record'Class;
      Lock      : in     Lock_Type);


   ---------------------------------------
   -- Layout manipulations without lock --
   ---------------------------------------

   ----------------------------------------------------------------------------
   --  Shifts nodes aside in order to open an empty space.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Center - The center point of the empty space
   --    Width  - The width of the empty space
   --    Height - The height of the empty space
   procedure Make_Room
     (Widget    : access Graph_Widget_Record'Class;
      Center    : in     Vis.Logic.Vector_2d;
      Width     : in     Vis.Logic_Float;
      Height    : in     Vis.Logic_Float);

   ----------------------------------------------------------------------------
   --  Moves all nodes in a selection by a given vector.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - Nodes to be moved
   --    Move      - Direction and length to be moved
   procedure Move_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in     Graph_Lib.Selections.Selection;
      Move      : in     Vis.Logic.Vector_2d);


   --------------------------
   -- Visualization Styles --
   --------------------------

   ----------------------------------------------------------------------------
   --  Get the Visualization style the graph widget is currently displayed
   --  with.
   --
   --  Parameters:
   --    Widget - The graph widget
   --  Returns:
   --    The Visualization style
   function Get_Vis_Style
     (Widget     : access Graph_Widget_Record'Class)
      return Config.Vis_Styles.Visualisation_Style_Access;

   ----------------------------------------------------------------------------
   --  Set the Visualization style the graph widget is to be displayed with
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Style  - The visualization style
   procedure Set_Vis_Style
     (Widget     : access Graph_Widget_Record'Class;
      Style      : in     Config.Vis_Styles.Visualisation_Style_Access);


   ------------------
   -- Highlighting --
   ------------------

   ----------------------------------------------------------------------------
   --  Add a highlight color to all edges and nodes in a selection.
   --  If the content of that selection changes, the highlighting must be
   --  removed manually from all deleted edges and nodes. The highlighting
   --  must be added manually to all new edges and nodes.
   --  The graph widget does not remember the selection after the call
   --  to this subprogram is complete. Highlighting is an attribute specific
   --  to single edges and nodes, not to selections.
   --
   --  Ignores all edges and nodes in 'Selection' that are not contained in
   --  'Widget'.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - The edges and nodes to be highlighted
   --    Color     - The highlight color to be added to 'Selection's contents
   procedure Add_Local_Highlighting
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in     Graph_Lib.Selections.Selection;
      Color     : in     Config.Global_Data.Selection_High_Light_ID);

   ----------------------------------------------------------------------------
   --  Remove a highlight color from all edges and nodes in a selection.
   --
   --  Ignores all edges and nodes in 'Selection' that are not contained in
   --  'Widget'.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - The edges and nodes to be highlighted
   --    Color     - The highlight color to be added to 'Selection's contents
   procedure Remove_Local_Highlighting
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in     Graph_Lib.Selections.Selection;
      Color     : in     Config.Global_Data.Selection_High_Light_ID);

   ----------------------------------------------------------------------------
   --  Add a highlight color to all edges and nodes in a subgraph.
   --  If the content of that subgraph changes, the highlighting must be
   --  removed manually from all deleted edges and nodes. The highlighting
   --  must be added manually to all new edges and nodes.
   --  The graph widget does not remember the subgraph after the call
   --  to this subprogram is complete. Highlighting is an attribute specific
   --  to single edges and nodes, not to selections.
   --
   --  Ignores all edges and nodes in 'Subgraph' that are not contained in
   --  'Widget'.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - The edges and nodes to be highlighted
   --    Color     - The highlight color to be added to 'Selection's contents
   procedure Add_Global_Highlighting
     (Widget   : access Graph_Widget_Record'Class;
      Subgraph : in     Graph_Lib.Subgraphs.Subgraph;
      Color    : in     Config.Global_Data.Subgraph_High_Light_ID);

   ----------------------------------------------------------------------------
   --  Removes a highlight color from all the edges and nodes in a subgraph.
   --
   --  Ignores all edges and nodes in 'Subgraph' that are not contained in
   --  'Widget'.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - The edges and nodes to be highlighted
   --    Color     - The highlight color to be added to 'Selection's contents
   procedure Remove_Global_Highlighting
     (Widget   : access Graph_Widget_Record'Class;
      Subgraph : in     Graph_Lib.Subgraphs.Subgraph;
      Color    : in     Config.Global_Data.Subgraph_High_Light_ID);

   ----------------------------------------------------------------------------
   --  Deletes all highlight colors from all the edges and all nodes in
   --  'Widget'
   --
   --  Parameters:
   --    Widget - The graph widget
   procedure Clear_Highlighting
     (Widget     : access Graph_Widget_Record'Class);


   -----------------------
   -- Visual Attributes --
   -----------------------

   ----------------------------------------------------------------------------
   --  Checks if an edge is hidden.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Edge   - The edge
   --  Returns:
   --    True if 'Edge' is hidden, False else
   --  Precondition:
   --    'Contains (Widget, Edge)'
   --  Raises:
   --    Unknown_Edge_Id if Precondition is not satisfied
   function Is_Hidden
     (Widget     : access Graph_Widget_Record'Class;
      Edge       : in     Graph_Lib.Edge_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Checks if a node is hidden.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Node   - The node to be checked
   --  Returns:
   --    True if 'Node' is hidden, False else
   --  Precondition:
   --    'Contains (Widget, Node)'
   --  Raises:
   --    Unknown_Node_Id if Precondition is not satisfied
   function Is_Hidden
     (Widget     : access Graph_Widget_Record'Class;
      Node       : in     Graph_Lib.Node_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Sets the "hidden" status of all edges and all nodes in 'Selection'.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - Container of edges and nodes whose status is to be set
   --    Hidden    - Desired result of status change
   --  Precondition:
   --    * For all E: Edge_Id:
   --        E contained in 'Selection' ==> 'Contains (Widget, E)'
   --    * For all N: Node_Id:
   --        N contained in 'Selection' ==> 'Contains (Widget, N)'
   --  Postcondition:
   --    * For all E: Edge_Id:
   --        E contained in 'Selection' ==> 'Is_Hidden (Widget, E)' = 'Hidden'
   --    * For all N: Node_Id:
   --        N contained in 'Selection' ==> 'Is_Hidden (Widget, N)' = 'Hidden'
   --  Raises:
   --    * Unknown_Edge_Id if Precondition is not satisfied
   --    * Unknown_Node_Id if Precondition is not satisfied
   --
   --  Note: UNIMPLEMENTED!
   --
--     procedure Set_Hidden
--       (Widget     : access Graph_Widget_Record'Class;
--        Selection  : in     Graph_Lib.Selections.Selection;
--        Hidden     : in     Boolean);

   ----------------------------------------------------------------------------
   --  Unhides all nodes and all edges in 'Widget'.
   --
   --  Parameters:
   --    Widget - The graph widget
   --  Postcondition:
   --    * For all E: Edge_Id:
   --        'Contains (Widget, E)' ==> not 'Is_Hidden (Widget, E)'
   --    * For all N: Node_Id:
   --        'Contains (Widget, N)' ==> not 'Is_Hidden (Widget, N)'
   --
   --  Note: UNIMPLEMENTED!
   --
--     procedure Unhide_All
--       (Widget     : access Graph_Widget_Record'Class);

   ----------------------------------------------------------------------------
   --  Must be called when an annotation is created or destroyed. Updates
   --  the display, so the user can determine whether a 'Node' is annotated
   --  or not.
   --  This subprogram only needs to be called if the status of an already
   --  inserted node is changed. If a node is newly inserted, then the graph
   --  widget will check that node's status automatically.
   --  This subprogram may be called even if 'Node' is not contained in
   --  'Widget'.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Node   - A node with a new annotation or a node whose annotation was
   --             deleted
   --  Raises:
   --    Unknown_Node_Id if Precondition is not satisfied
   procedure Change_Annotation_State
     (Widget     : access Graph_Widget_Record'Class;
      Node       : in     Graph_Lib.Node_Id);


   ------------------------
   -- Zooming and Moving --
   ------------------------

   ----------------------------------------------------------------------------
   --  Returns the greatest zoom level applicable for 'Widget'. This is
   --  limited by the maximum range of internal variables.
   --
   --  Parameters:
   --    Widget - The graph widget
   --  Returns:
   --    Maximum zoom level
   function Get_Maximum_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Zoom_Level;

   ----------------------------------------------------------------------------
   --  Returns the current zoom level within 'Widget'.
   --
   --  Parameters:
   --    Widget - The graph widget
   --  Returns:
   --    Current zoom level
   function Get_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Zoom_Level;

   ----------------------------------------------------------------------------
   --  Sets the current zoom level in 'Widget'. If 'Zoom' is greater than
   --  Max = 'Get_Maximum_Zoom_Level (Widget)' then Max is used instead. If
   --  'Zoom' < 0.0 then 0.0 is used.
   --  If the zoom level is set to 0.0, then the center point of the visible
   --  area is set to 0.0. If after the application of the new zoom level, the
   --  center point of the visible area will be lying outside of the possible
   --  range, then the center point is adjusted to a value close to its
   --  old value.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Zoom   - The desired zoom level
   procedure Set_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class;
      Zoom       : in     Vis.Zoom_Level);

   ----------------------------------------------------------------------------
   --  Changes zoom level and location so that the nodes incident to 'Edge'
   --  fill 'Widget' and are both located within the visible area. If the
   --  zoom level needed for that purpose is too great, then the maximum
   --  zoom level will be used and 'Edge' will be centered.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Edge   - The Edge to be centered
   --  Precondition:
   --    'Contains (Widget, Edge)'
   --  Raises:
   --    Unknown_Edge_Id if Precondition is not satisfied
   procedure Zoom_To_Edge
     (Widget     : access Graph_Widget_Record'Class;
      Edge       : in     Graph_Lib.Edge_Id);

   ----------------------------------------------------------------------------
   --  Changes zoom level and location so that the rectangle 'Rectangle'
   --  fills 'Widget'. If the zoom level needed for that purpose is too great,
   --  then the maximum zoom level will be used and 'Rectangle's center
   --  will be centered in 'Widget'.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Rectangle - The rectangle to be shown inside widget
   procedure Zoom_To_Rectangle
     (Widget     : access Graph_Widget_Record'Class;
      Rectangle  : in     Vis.Logic.Rectangle_2d);

   ----------------------------------------------------------------------------
   --  Changes zoom level and location so 'Selection' fills 'Widget'
   --  and all nodes in 'Selection' are located within the visible area. If the
   --  necessary zoom level is too great, then the maximum zoom level will
   --  be used and 'Selection' will be centered. If 'Selection' is empty then
   --  nothing is done.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - The Selection to be zoomed onto
   --  Precondition:
   --    * For all E: Edge_Id:
   --        E contained in 'Selection' ==> 'Contains (Widget, E)'
   --    * For all N: Node_Id:
   --        N contained in 'Selection' ==> 'Contains (Widget, N)'
   --  Raises:
   --    * Unknown_Edge_Id if Precondition is not satisfied
   --    * Unknown_Node_Id if Precondition is not satisfied
   procedure Zoom_To_Selection
     (Widget     : access Graph_Widget_Record'Class;
      Selection  : in     Graph_Lib.Selections.Selection);

   ----------------------------------------------------------------------------
   --  Changes zoom level and location so that all content fills 'Widget'
   --  and all nodes are located within the visible area. If the
   --  zoom level needed for that purpose is too great, then the maximum
   --  zoom level will be used and the content will be centered. If there
   --  is no content in 'Widget' then nothing is done.
   --
   --  Parameters:
   --    Widget - The graph widget
   procedure Zoom_To_All
     (Widget     : access Graph_Widget_Record'Class);

   ----------------------------------------------------------------------------
   --  Returns an approximation of the smallest rectangle containing the
   --  complete graph
   --
   --  Parameters:
   --    Widget - The graph widget
   --  Result:
   --    Approximation of the rectangle the entire graph is contained in
   function Get_Logical_Area
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Logic.Rectangle_2d;

   ----------------------------------------------------------------------------
   --  Returns the rectangle currently visible in the graph widget
   --
   --  Parameters:
   --    Widget - The graph widget
   --  Result:
   --    The rectangle currently visible inside graph widget
   function Get_Visible_Area
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Logic.Rectangle_2d;

   ----------------------------------------------------------------------------
   --  Returns the center of the visible area currently visible within
   --  'Widget'.
   --
   --  Parameters:
   --    Widget - The graph widget
   --  Returns:
   --    Center point of the visible area
   function Get_Location
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Logic.Vector_2d;

   ----------------------------------------------------------------------------
   --  Sets the center point of the visible area in 'Widget'. If zoom level
   --  is 0.0 then nothing is done. If 'Location' is outside of the
   --  displayable area then a point close to 'Location' is chosen.
   procedure Set_Location
     (Widget     : access Graph_Widget_Record'Class;
      Location   : in     Vis.Logic.Vector_2d);

   ----------------------------------------------------------------------------
   --  Sets the zoom level in 'Widget' and then the center point of the
   --  visible area in 'Widget'.
   --  If 'Zoom' is greater than Max = 'Get_Maximum_Zoom_Level (Widget)'
   --  then Max is used instead. If 'Zoom' < 0.0 then 0.0 is used.
   --  If 'Location' is outside of the displayable area then a point close
   --  to 'Location' is chosen. If the zoom level is set to 0.0 then the
   --  center point of the visible area is set to 0.0. Else the center point
   --  is set to 'Location'.
   --
   --  Parameters:
   --    Widget   - The graph widget
   --    Location - Desired center point
   --    Zoom     - Desired zoom level
   procedure Set_Location_And_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class;
      Location   : in     Vis.Logic.Vector_2d;
      Zoom       : in     Vis.Zoom_Level);


   ----------------------------------------------------------------------------
   --  Actions the user can perform on the current selection
   --
   --  Enumeration Literals:
   --    Insert - Request to insert certain edges and/or nodes from the
   --             current selection
   --    Remove - Request to remove certain edges and/or nodes from the
   --             current selection
   --    Change - Request to change the contents of the current selection
   --    Clear  - Request to clear the current selection
   type Selection_Change_Type is (Insert, Remove, Change, Clear);

   ----------------------------------------------------------------------------
   --  Style for drawing of edges
   --  can be 'Continuous_Line', 'Dashed_Line', 'Dotted_Line'
   type Edge_Style_Type is new Config.Vis_Styles.Edge_Line_Style;

   ----------------------------------------------------------------------------
   --  Details to be shown in a graph widget:
   --  * Low     - Nodes are shown as rectangles
   --  * Average - + Nodes contain icons, ids and type names
   --  * High    - + Nodes contain attributes
   type Detail_Level_Type is (Low, Average, High);

                           ------------------
private                    -- private part --
                           ------------------

   Current_Version_Number : constant := 1;

   package Vis_Edge_Sets renames Vis_Data.Vis_Edge_Sets;
   package Vis_Edge_Lists renames Vis_Data.Vis_Edge_Lists;

   package Vis_Node_Sets renames Vis_Data.Vis_Node_Sets;
   package Vis_Node_Lists renames Vis_Data.Vis_Node_Lists;


   -------------------------
   -- Private subprograms --
   -------------------------

   ----------------------------------------------------------------------------
   --  Prepares 'Widget' for deallocation
   procedure Shut_Down_Graph_Widget
     (Widget : access Graph_Widget_Record'Class);

   ----------------------------------------------------------------------------
   --  Enlarges the logic area by 'Position'
   --  After all calls to 'Add_Logic_Position' are made,
   --  'States.Has_Logic_Area_Changed' should be checked and, if necessary,
   --  'Notifications.Logical_Area_Changed' should be called.
   procedure Add_Logic_Position
     (Widget    : access Graph_Widget_Record'Class;
      Position  : in     Vis.Logic.Vector_2d);

   procedure Add_Logic_Area_Rectangle
     (Widget    : access Graph_Widget_Record'Class;
      Rectangle : in     Vis.Logic.Rectangle_2d);

   ----------------------------------------------------------------------------
   --  Changes the size of 'Widget' and makes buffers grow when necessary
   procedure Resize_Graph_Widget
     (Widget : access Graph_Widget_Record'Class;
      Size   : in     Vis.Absolute.Vector_2d);


   ---------------------
   -- Private: Moving --
   ---------------------

   procedure Move_Nodes
     (Widget : access Graph_Widget_Record'Class;
      Nodes  : in     Vis_Node_Sets.Set;
      Offset : in     Vis.Logic.Vector_2d);

   procedure Set_Visible_Center
     (Widget : access Graph_Widget_Record'Class;
      Center : in     Vis.Absolute.Vector_2d);


   ---------------------------
   -- Private: Highlighting --
   ---------------------------

   --  Precondition:
   --    'Edge' must be locked
   procedure Add_Edge_Highlighting
     (Widget    : access Graph_Widget_Record'Class;
      Edge      : in     Vis_Data.Vis_Edge_Id;
      Highlight : in     Vis_Data.Highlight_Type);

   --  Precondition:
   --    'Node' must be locked
   procedure Add_Node_Highlighting
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Vis_Data.Vis_Node_Id;
      Highlight : in     Vis_Data.Highlight_Type);

   --  Precondition:
   --    'Edge' must be locked
   procedure Remove_Edge_Highlighting
     (Widget    : access Graph_Widget_Record'Class;
      Edge      : in     Vis_Data.Vis_Edge_Id;
      Highlight : in     Vis_Data.Highlight_Type);

   --  Precondition:
   --    'Node' must be locked
   procedure Remove_Node_Highlighting
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Vis_Data.Vis_Node_Id;
      Highlight : in     Vis_Data.Highlight_Type);


   ------------------------
   -- Private: Selection --
   ------------------------

   function Is_Edge_In_Selection
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id)
     return Boolean;

   function Is_Node_In_Selection
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id)
     return Boolean;

   --  The returned set must not be modified!
   function Get_Selected_Edges
     (Widget : access Graph_Widget_Record'Class)
     return Vis_Edge_Sets.Set;

   --  The returned set must not be modified!
   function Get_Selected_Nodes
     (Widget : access Graph_Widget_Record'Class)
     return Vis_Node_Sets.Set;

   --  Note:
   --    Highlighting must be adjustet manually
   --    Notification must be done manually
   procedure Add_Edge_To_Selection
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id);

   --  Note:
   --    Highlighting must be adjustet manually
   --    Notification must be done manually
   procedure Add_Node_To_Selection
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id);

   --  Note:
   --    Highlighting must be adjustet manually
   --    Notification must be done manually
   procedure Remove_Edge_From_Selection
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id);

   --  Note:
   --    Highlighting must be adjustet manually
   --    Notification must be done manually
   procedure Remove_Node_From_Selection
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id);


   ----------------------------------------------------------------------------
   --  Selection modifications can be: add items, toggle selection state of
   --  an item or change the selection to contain only specified items. This
   --  type is used for program logic
   type Internal_Selection_Modify_Type is (Add, Toggle, Change, Remove);

   ----------------------------------------------------------------------------
   --  Selection modifications for user actions can be: add items, toggle
   --  selection state of an item or change the selection to contain only
   --  specified items
   subtype Selection_Modify_Type is
     Internal_Selection_Modify_Type range Add .. Change;

   --  Postcondition:
   --    Edges is destroyed
   --    Nodes is destroyed
   --  Note:
   --    Adjusts highlighting
   --    Notification must be done manually
   procedure Modify_Selection
     (Widget : access Graph_Widget_Record'Class;
      Edges  : in out Vis_Edge_Lists.List;
      Nodes  : in out Vis_Node_Lists.List;
      Mode   : in     Internal_Selection_Modify_Type);

   --  Note:
   --    Adjusts highlighting
   --    Notification must be done manually
   procedure Clear_Selection
     (Widget : access Graph_Widget_Record'Class);

   --  Notify all listeners of current selection state
   procedure Notify_Selection_Change
     (Widget : access Graph_Widget_Record'Class);

   --  Note:
   --    Adjusts highlighting
   --    Notifies listeners
   procedure Modify_Selection_With_Edge_And_Notify
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id;
      Mode   : in     Selection_Modify_Type);

   --  Note:
   --    Adjusts highlighting
   --    Notifies listeners
   procedure Modify_Selection_With_Node_And_Notify
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id;
      Mode   : in     Selection_Modify_Type);

   --  Note:
   --    Adjusts highlighting
   --    Notifies listeners
   procedure Clear_Selection_And_Notify
     (Widget : access Graph_Widget_Record'Class);


   -----------------------
   -- Private: Updating --
   -----------------------

   ----------------------------------------------------------------------------
   --  Sets the obsolete flag on 'Edge' and moves 'Edge' to the unsized queue.
   --  'Edge' will be destroyed on 'Flush_Locked'.
   procedure Mark_Edge_Obsolete
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Sets the obsolete flag on 'Node' and moves 'Node' to the unsized queue.
   --  'Node' and all edges incident to 'Node' will be destroyed on
   --  'Flush_Locked'. It is NOT necessary to mark edges incident to 'Node'.
   --  This will be done by 'Flush_Locked', since more edges might be added
   --  before the flush.
   procedure Mark_Node_Obsolete
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Sets the obsolete Flag on all edges incident to 'Node' and moves those
   --  edges to the unsized queue. They will be destroyed on 'Flush_Locked'
   procedure Mark_Incident_Obsolete
     (Widget : access Graph_Widget_Record'Class;
      Node   : in    Vis_Data.Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Inserts those edges into the region manager that are incident to
   --  all nodes that were not in 'Old_Visible_Area' but are now in the
   --  visible area. Removes all edges that were in 'Old_Visible_Area' but
   --  are not in the visible area.
   procedure Make_Edges_Appear
     (Widget           : access Graph_Widget_Record'Class;
      Old_Visible_Area : in     Vis.Absolute.Rectangle_2d);

   ----------------------------------------------------------------------------
   --  Returns True if and only if 'Node' is (partially) inside the visible
   --  area and thus all edges incident to 'Node' must be displayed.
   function Is_In_Visible_Area
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Switches all nodes in 'Nodes' to 'Visible_Incident' state.
   --  Creates a new set 'Incident' and adds all edges to be newly
   --  inserted to that set.
   --  The edges in 'Incident' must then be added to the region manager.
   procedure Switch_Incident_Visible
     (Widget   : access Graph_Widget_Record'Class;
      Nodes    : in     Vis_Node_Lists.List;
      Incident :    out Vis_Edge_Sets.Set);

   ----------------------------------------------------------------------------
   --  Inserts all edges in 'Edges' to the region manager.
   procedure Insert_Incident_Edges
     (Widget   : access Graph_Widget_Record'Class;
      Edges    : in     Vis_Edge_Sets.Set);

   ----------------------------------------------------------------------------
   --  Removes all edges incident to any of the nodes in 'Nodes' from the
   --  region manager, if both adjacent nodes are outside of the visible area.
   procedure Check_Remove_Incident_Edges
     (Widget : access Graph_Widget_Record'Class;
      Nodes  : in out Vis_Node_Lists.List);

   ----------------------------------------------------------------------------
   --  Raises 'Edge' on top of all other edges
   procedure Raise_Edge
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Raises 'Node' on top of all other nodes
   procedure Raise_Node
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Raises the floating selection on top of all other edges and nodes if
   --  a floating selection exists.
   procedure Raise_Floating
     (Widget : access Graph_Widget_Record'Class);

   ----------------------------------------------------------------------------
   --  Returns the a reference to the set of all floating nodes. The Set must
   --  not be modified.
   function Get_Floating_Nodes
     (Widget : access Graph_Widget_Record'Class)
     return Vis_Node_Sets.Set;

   ----------------------------------------------------------------------------
   --  Drops 'Edge' from region manager and adds 'Edge' to the set of
   --  locked edges
   procedure Add_Edge_To_Locked
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id);

   --  Drops all edges in 'Edges' from the region manager and adds them to
   --  the set of locked edges.
   procedure Add_Edges_To_Locked
     (Widget : access Graph_Widget_Record'Class;
      Edges  : in out Vis_Edge_Lists.ListIter);

   --  Drops 'Node' from the region manager and adds it to the set of locked
   --  nodes. Calls 'Add_Edges_To_Locked' on all incident edges.
   procedure Add_Node_To_Locked
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id);

   --  Drops 'Edge' from the region manager, removes 'Edge' from the locked
   --  queue and adds it to the unsized queue.
   procedure Move_Edge_To_Unsized
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id);

   --  Drops all edges in  'Edges' from the region manager, removes them
   --  from the locked queue and adds them to the unsized queue.
   procedure Move_Edges_To_Unsized
     (Widget : access Graph_Widget_Record'Class;
      Edges  : in     Vis_Edge_Lists.ListIter);

   --  Drops 'Node' from the region manager, removes 'Node' from the locked
   --  queue and adds it to the unsized queue. Calls 'Move_Edge_To_Unsized'
   --  on all incident edges.
   procedure Move_Node_To_Unsized
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id);

   procedure Move_All_Edges_To_Unsized
     (Widget : access Graph_Widget_Record'Class);

   procedure Move_All_Nodes_To_Unsized
     (Widget : access Graph_Widget_Record'Class);

   --  flushes the locking-sets into the region manager
   procedure Flush_Locked
     (Widget : access Graph_Widget_Record'Class);

   --  enqueus a redraw event if necessary.
   procedure Redraw
     (Widget : access Graph_Widget_Record'Class);


   -------------------------------------------------
   -- Private: Mapping Graph_Lib --> Graph_Widget --
   -------------------------------------------------

   --  looks up an existing edge or creates a new one if necessary.
   --  if the nodes incident cannot be looked up, then sets 'Edge' to null.
   procedure Find_Or_Create
     (Widget     : access Graph_Widget_Record'Class;
      Graph_Edge : in     Graph_Lib.Edge_Id;
      Edge       :    out Vis_Data.Vis_Edge_Id);

   --  looks up an existing node or creates a new one if necessary.
   procedure Find_Or_Create
     (Widget     : access Graph_Widget_Record'Class;
      Graph_Node : in     Graph_Lib.Node_Id;
      Node       :    out Vis_Data.Vis_Node_Id);

   --  returns null if 'not Contains (Widget, Graph_Edge)'
   function Look_Up
     (Widget     : access Graph_Widget_Record'Class;
      Graph_Edge : in     Graph_Lib.Edge_Id)
     return Vis_Data.Vis_Edge_Id;

   --  returns null if 'not Contains (Widget, Graph_Node)'
   function Look_Up
     (Widget     : access Graph_Widget_Record'Class;
      Graph_Node : in     Graph_Lib.Node_Id)
     return Vis_Data.Vis_Node_Id;

   --  Destroys the visual representation of an edge. 'Edge' must not be
   --  contained in the region manager or in any queue. It must not be
   --  contained in the selection.
   procedure Destroy_Edge
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in out Vis_Data.Vis_Edge_Id);

   --  Destroys the visual representation of a node. 'Node' must not be
   --  containted in the region manager or in any queue. It must not be
   --  contained in the selection.
   procedure Destroy_Node
     (Widget : access Graph_Widget_Record'Class;
      Node   : in out Vis_Data.Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Default size of a graph widget
   Default_Width                : constant := 300;
   Default_Height               : constant := 200;
   --  Default zoom level
   Default_Zoom_Level           : constant := 1.0;
   --  Minimum zoom level for that node size is calculated into the logical
   --  area. For smaller zoom levels only one point is considered per node.
   Default_Minimum_Precise_Zoom : constant := 10.0 / 100.0;
   --  Default width of a node displayed at 'Default_Zoom_Level'
   Default_Node_Width           : constant := 150;


   ----------------------------------------------------------------------------
   --  Hashmap that maps Graph_Lib.Edge_Id to Vis_Data.Vis_Edge_Id
   package Edge_Id_Mappings is new Hashed_Mappings
     (Key_Type   => Graph_Lib.Edge_Id,
      Hash       => Graph_Lib.Hash_Edge_Id,
      Value_Type => Vis_Data.Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Hashmap that maps Graph_Lib.Node_Id to Vis_Data.Vis_Node_Id
   package Node_Id_Mappings is new Hashed_Mappings
     (Key_Type   => Graph_Lib.Node_Id,
      Hash       => Graph_Lib.Hash_Node_Id,
      Value_Type => Vis_Data.Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  A Lock that can be obtained to lock content of the graph widget
   type Lock_Type is new Natural;


   ------------
   -- States --
   ------------

   package Lock_Sets is new Ordered_Sets
     (Item_Type => Lock_Type);

   type Cursor_State_Type is (Default, Action, Waiting);

   type Cursor_State_Array is array (Cursor_State_Type) of
     Gdk.Cursor.Gdk_Cursor;

   type Mouse_State_Type is (None, Clicking, Moving, Dragging, Rectangling);

   type States_Type is
      record
         Logic_Area_Changed  : Boolean                := False;
         Visual_Area_Changed : Boolean                := False;

         Drawing_Ready       : Boolean                := False;

         Visual_Polluted     : Boolean                := True;
         Temporary_Changed   : Boolean                := True;

         Action_Mode         : Boolean                := False;

         Highest_Lock        : Lock_Type              := 0;
         Locks               : Lock_Sets.Set          := Lock_Sets.Empty_Set;
         Lock_Flush_Pending  : Boolean                := False;

         Cursors             : Cursor_State_Array     :=
           (others => Gdk.Cursor.Null_Cursor);
         Current_Cursor      : Cursor_State_Type      := Default;

         Mouse_State         : Mouse_State_Type       := None;
         Mouse_Origin        : Vis.Absolute.Vector_2d := Vis.Absolute.Zero_2d;
         Mouse_Position      : Vis.Absolute.Vector_2d := Vis.Absolute.Zero_2d;
         Mouse_On_Node       : Vis_Data.Vis_Node_Id   := null;
         Mouse_On_Edge       : Vis_Data.Vis_Edge_Id   := null;
         Mouse_Modifiers     : Gdk.Types.Gdk_Modifier_Type := 0;

         Auto_Scrolling      : Boolean                := False;
      end record;


   -----------------
   -- Positioning --
   -----------------

   type Positioning_Type is
      record
         Transformation : Vis.Transformation_Type;
      end record;


   -------------
   -- Drawing --
   -------------

   type Edge_Style_GCs is array (Edge_Style_Type) of Gdk.GC.Gdk_GC;

   type Highlight_GCs is array (Vis_Data.Highlight_Type) of Gdk.GC.Gdk_GC;

   type Drawing_Type is
      record
         Buffer         : Gdk.Pixmap.Gdk_Pixmap;
         Buffer_Area    : Vis.Absolute.Rectangle_2d;
         Ready_Buffer   : Gdk.Pixmap.Gdk_Pixmap;
         Display        : Gdk.Pixmap.Gdk_Pixmap;

         Visible_Area   : Vis.Absolute.Rectangle_2d :=
           Vis.Absolute.Combine_Rectangle
           (Top_Left     => Vis.Absolute.Zero_2d,
            Bottom_Right => Vis.Absolute.Zero_2d);

         Debug_Gc       : Gdk.GC.Gdk_GC;

         Background     : Gdk.GC.Gdk_GC;

         Node_Border    : Gdk.GC.Gdk_GC;
         Node_Fill      : Gdk.GC.Gdk_GC;
         Node_Text      : Gdk.GC.Gdk_GC;
         Node_Light     : Highlight_GCs;

         Edge_Line      : Edge_Style_GCs;
         Edge_Label     : Gdk.GC.Gdk_GC;
         Edge_Light     : Highlight_GCs;

         Rectangle_Gc   : Gdk.GC.Gdk_GC;
      end record;


   --------------
   -- Settings --
   --------------

   type Color_Array_Access is access Gdk.Color.Gdk_Color_Array;

   type Settings_Type is
      record
         --  Pool of node annotations for some nodes
         Node_Annotation_Pool   : Node_Annotations.Node_Annotation_Access;
         --  Visualisation style of a graph widget, used to configure
         --  colors and icons
         Vis_Style              : Config.Vis_Styles.Visualisation_Style_Access;
         --  Colors used for drawing. All those colors are allocated in the
         --  default Colormap.
         All_Colors             : Color_Array_Access;
         --  Offset for highlight colors. Addition to
         --  ...'Pos (Vis_Data'Highlight_Type'First) gives the index of
         --  first highlight color in 'All_Colors'
         Highlight_Index_Offset : Integer;
         --  Font for drawing text on the graph widget.
         Font                   : Gdk.Font.Gdk_Font := Gdk.Font.Null_Font;
         --  Height of 'Font' cached for not accessing the Xserver everytime
         --  needed
         Font_Height            : Vis.Absolute_Natural;
         --  The Height 'Font' is desired to have. Is not necessarily equal
         --  to 'Font_Height' which is the actual height.
         Font_Choice            : Vis.Absolute_Natural;
         --  Width for each node in a graph widget. Depends on the Zoom_Level
         Node_Width             : Vis.Absolute_Natural := Default_Node_Width;
         --  Detail level for display of a graph widget
         Detail_Level           : Detail_Level_Type := High;
      end record;


   ---------------
   -- Callbacks --
   ---------------

   type Callbacks_Type is
      record
         Mouse_Grab_Count        : Natural := 0;

         Accept_Scrolling        : Natural := 0;

         Previous_Selected_Edges : Vis_Edge_Lists.List;
         Previous_Selected_Nodes : Vis_Node_Lists.List;

         Auto_Scroll_Handler     : Gtk.Main.Timeout_Handler_Id;
         Auto_Scroll_Connected   : Boolean := False;

         Horizontal_Adjustment   : Gtk.Adjustment.Gtk_Adjustment :=
           Gtk.Adjustment.Null_Adjustment;
         Horizontal_Handler      : Gtk.Handlers.Handler_Id;
         Vertical_Adjustment     : Gtk.Adjustment.Gtk_Adjustment :=
           Gtk.Adjustment.Null_Adjustment;
         Vertical_Handler        : Gtk.Handlers.Handler_Id;
      end record;


   -------------------------
   -- Graph_Widget_Record --
   -------------------------

   ----------------------------------------------------------------------------
   --  The one and only graph widget tagged type
   type Graph_Widget_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
      record
         --  Region manager for the graph widget. May be accessed
         --  by subpackages.
         Manager        : Vis_Data.Region_Manager;

         --  Must only be used by subpackage Drawing
         Drawing        : Drawing_Type;
         --  Must only be used by subpackage Positioning
         Positioning    : Positioning_Type;
         --  Must only be used by subpackage Settings
         Settings       : Settings_Type;
         --  Must only be used by subpackage States
         States         : States_Type;
         --  Must only be used by subpackage Callbacks
         Callbacks      : Callbacks_Type;

         --  The following fields must not be used by any subpackage
         Logic_Area     : Vis.Logic.Rectangle_2d;

         Edge_Map       : Edge_Id_Mappings.Mapping;
         Edge_Layers    : Vis_Data.Layer_Pool;
         Node_Map       : Node_Id_Mappings.Mapping;
         Node_Layers    : Vis_Data.Layer_Pool;

         Unsized_Edges  : Vis_Edge_Sets.Set;

         Locked_Nodes   : Vis_Node_Lists.List;
         Unsized_Nodes  : Vis_Node_Lists.List;

         Selected_Edges : Vis_Edge_Sets.Set;
         Selected_Nodes : Vis_Node_Sets.Set;
      end record;

end Giant.Graph_Widgets;
