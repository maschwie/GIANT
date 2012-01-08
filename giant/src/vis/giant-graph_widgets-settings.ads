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
--  $RCSfile: giant-graph_widgets-settings.ads,v $, $Revision: 1.11 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Giant.Graph_Lib.Node_Attribute_Filters;

package Giant.Graph_Widgets.Settings is

   ------------------------------
   -- Initialization & Updates --
   ------------------------------

   ---------------------------------------------------------------------------
   --  Changes the style.
   --  If possible ('Set_Up' must have been called before), then changes the
   --  style-dependent settings:
   --  * node colors
   --  * edge colors
   --  * background color
   procedure Set_Style
     (Widget : access Graph_Widget_Record'Class;
      Style  : in     Config.Vis_Styles.Visualisation_Style_Access);

   ---------------------------------------------------------------------------
   --  Changes the node annotation pool. All nodes in a graph widget should
   --  be updated to reflect the new pool.
   procedure Set_Annotation_Pool
     (Widget : access Graph_Widget_Record'Class;
      Pool   : in     Node_Annotations.Node_Annotation_Access);

   ---------------------------------------------------------------------------
   --  Returns the style set in 'Widget'
   function Get_Style
     (Widget : access Graph_Widget_Record'Class)
     return Config.Vis_Styles.Visualisation_Style_Access;

   ---------------------------------------------------------------------------
   --  Sets up all configuration-dependent settings:
   --  * highlight colors
   --  * node colors
   --  * edge colors
   --  * background colors
   --  Must be called one and only one time during the lifetime of a graph
   --  widget. 'Set_Style' must have been called before.
   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class);


   procedure Shut_Down
     (Widget : access Graph_Widget_Record'Class);


   procedure Set_Zoom
     (Widget : access Graph_Widget_Record'Class;
      Zoom   : in     Vis.Zoom_Level);


   function Get_Detail_Level
     (Widget : access Graph_Widget_Record'Class)
     return Detail_Level_Type;


   -----------------------
   -- Color inspections --
   -----------------------

   function Get_Highlight_Color
     (Widget       : access Graph_Widget_Record'Class;
      Highlighting : in     Vis_Data.Highlight_Type)
     return Gdk.Color.Gdk_Color;

   function Get_Background_Color
     (Widget       : access Graph_Widget_Record'Class)
     return Gdk.Color.Gdk_Color;


   -----------
   -- Edges --
   -----------

   function Get_Edge_Style
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Edge_Style_Type;

   function Get_Edge_Color
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Gdk.Color.Gdk_Color;

   function Show_Edge_Label
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Boolean;

   function Get_Edge_Label_Color
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Gdk.Color.Gdk_Color;

   function Get_Edge_Font
     (Widget : access Graph_Widget_Record'Class)
     return Gdk.Font.Gdk_Font;

   function Get_Edge_Font_Height
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute_Natural;


   -----------
   -- Nodes --
   -----------

   function Get_Node_Border_Color
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Gdk.Color.Gdk_Color;

   function Get_Node_Fill_Color
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Gdk.Color.Gdk_Color;

   function Get_Node_Text_Color
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Gdk.Color.Gdk_Color;

   function Has_Annotation
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Gets the icon to be shown inside annotated nodes or 'Null_Pixmap'
   procedure Get_Annotation_Icon
     (Widget       : access Graph_Widget_Record'Class;
      Icon         :    out Gdk.Pixmap.Gdk_Pixmap;
      Width        :    out Glib.Gint;
      Height       :    out Glib.Gint);

   function Get_Annotation_Icon_Size
     (Widget       : access Graph_Widget_Record'Class)
     return Vis.Absolute.Vector_2d;

   ---------------------------------------------------------------------------
   --  The icon to be shown for 'Node' or 'Null_Pixmap'
   procedure Get_Node_Icon
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id;
      Icon         :    out Gdk.Pixmap.Gdk_Pixmap;
      Width        :    out Glib.Gint;
      Height       :    out Glib.Gint);

   function Get_Node_Icon_Size
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Vis.Absolute.Vector_2d;

   function Show_Node_Class_Name
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Boolean;

   function Get_Node_Attribute_Count
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Natural;

   function Get_Node_Attributes
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Graph_Lib.Node_Attribute_Filters.Filtered_Iterator;

   function Get_Node_Font
     (Widget : access Graph_Widget_Record'Class)
     return Gdk.Font.Gdk_Font;

   function Get_Node_Font_Height
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute_Natural;

   ----------------------------------------------------------------------------
   --  Returns the width for a node to be used in 'Widget'. This width
   --  is dependant on the detail and zoom level in 'Widget'. It does not
   --  include highlighting drawn outside of the node rectangle.
   function Get_Node_Width
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute_Natural;


private

   procedure Update_Font_Choice
     (Widget : access Graph_Widget_Record'Class);


   Default_Minimum_Node_Width       : constant := 10;
   Default_Node_Text_Lines_Estimate : constant := 11;
   Default_Minimum_Font_Size        : constant := 6;
   Default_Maximum_Font_Size        : constant := 40;


end Giant.Graph_Widgets.Settings;
