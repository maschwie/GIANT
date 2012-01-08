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
--  $RCSfile: giant-graph_widgets-drawing.ads,v $, $Revision: 1.13 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
--  This package performs the actual drawing work for a graph widget.
--  It updates the buffers according to the modifications on the data
--  managed by the package 'Vis_Data' and then maps the buffers to the
--  'Drawable' provided by GtkAda.
--


package Giant.Graph_Widgets.Drawing is

   ----------------------------------------------------------------------------
   --  Settings must have been 'Set_Up' before.
   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class);

   procedure Shut_Down
     (Widget : access Graph_Widget_Record'Class);

   ----------------------------------------------------------------------------
   --  Ensures that the display buffer is up to date. Clearing the window
   --  results in correct display of the graph widget.
   procedure Update_Display
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Absolute.Rectangle_2d);

   ----------------------------------------------------------------------------
   --  Ensures that all floating objects are drawn onto the display buffer.
   --  MUST be called after Update_Display if there are floating objects.
   procedure Update_Temporary
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Absolute.Rectangle_2d);

   ----------------------------------------------------------------------------
   --  Updates the size of an edge and sets the values. Uses the 'Settings'
   --  package. Usually 'Edge' should be dropped from the region manager,
   --  then 'Settings' should be updated if necessary, then this procedure
   --  should be called, then 'Edge's source and target nodes should be
   --  resized and moved, then the new points of 'Edge' should be set and
   --  finally 'Edge' should be re-inserted into the region manager.
   --  * Thickness
   --  * Text area
   procedure Update_Edge_Size
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Returns the maximum number of points drawn around an edge
   --  if that edge is highlighted in all colors.
   function Get_Maximum_Edge_Highlight_Width
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute_Natural;

   ----------------------------------------------------------------------------
   --  Updates the size of a node and sets the values. Uses the 'Settings'
   --  package. Usually 'Node' should be dropped from the region manager,
   --  then 'Settings' should be updated if necessary, then this procedure
   --  should be called, then the new position of 'Node' should be set
   --  if necessary and finally 'Node' should be re-inserted into the region
   --  manager.
   --  * Width
   --  * Height
   procedure Update_Node_Size
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Returns the maximum number of points drawn around the node rectangle
   --  if that node is highlighted in all colors.
   function Get_Maximum_Node_Highlight_Width
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute_Natural;

   ----------------------------------------------------------------------------
   --  Returns the top center point of the actual border of 'Node'. This
   --  might not be equal to the extent of 'Node' (obtained by calling
   --  Vis_Data.Get_Extent), because 'Node's highlighting might be drawn
   --  around the node rectangle.
   function Get_Node_Border_Top_Center
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id)
     return Vis.Absolute.Vector_2d;

   ----------------------------------------------------------------------------
   --  Returns the rectangle displayed inside the graph widget.
   function Get_Visible_Area
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute.Rectangle_2d;

   ----------------------------------------------------------------------------
   --  Moves the visible area, so 'Point' is its new center.
   --  Adds pollution to the region manager, and informs 'States' about
   --  the changes in the visible area.
   procedure Move_Visible_Area_To
     (Widget : access Graph_Widget_Record'Class;
      Point  : in     Vis.Absolute.Vector_2d);

   ----------------------------------------------------------------------------
   --  Resizes the display. Must be called after the size of a graph window
   --  has changed.
   procedure Resize_Display
     (Widget : access Graph_Widget_Record'Class);

   ----------------------------------------------------------------------------
   --  Pollutes all buffers and calls 'States.Changed_Display (Widget)'
   procedure Pollute_Everything
     (Widget : access Graph_Widget_Record'Class);

private

   Default_Dash_Length           : constant := 5;
   Default_Dash_Separation       : constant := 2;
   Default_Dot_Length            : constant := 2;
   Default_Dot_Separation        : constant := 2;

   Default_Edge_Line_Thickness   : constant := 0;
   Default_Edge_Light_Thickness  : constant := 4;

   Default_Node_Light_Thickness  : constant := 6;

   Default_Text_Spacing          : constant := 3;
   Default_Text_Abbreviation     : constant String := "...";

   ----------------------------------------------------------------------------
   --  GtkAda cannot use the complete 'Gint'Range' as coordinate space for
   --  drawing lines via 'Draw_Line'. Thus we are forced to do some clipping
   --  for ourselves. To avoid off-by-one rounding errors when drawing one line
   --  in multiple segments during scolling, we use the maximum range possible.
   --  Since this is undocumented, this maximum range is determined by
   --  experiment and likely to change.
   Line_Drawing_Inside_Rectangle : constant Vis.Absolute.Rectangle_2d :=
     Vis.Absolute.Combine_Rectangle
     (X_1 => Vis.Absolute_Int (-2**13 - 1),
      Y_1 => Vis.Absolute_Int (-2**13 - 1),
      X_2 => Vis.Absolute_Int (2**13 - 1),
      Y_2 => Vis.Absolute_Int (2**13 - 1));

end Giant.Graph_Widgets.Drawing;
