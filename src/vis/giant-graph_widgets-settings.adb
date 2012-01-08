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
--  $RCSfile: giant-graph_widgets-settings.adb,v $, $Revision: 1.23 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Gdk.Bitmap;
with Gdk.Drawable;
with Gdk.Pixmap;

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Graph_Widgets.Settings is

   function Default_Font_Name
     (Size : in     Natural)
     return String is
   begin
     return "-*-courier-*-r-*-*-" &
       Ada.Strings.Fixed.Trim (Natural'Image (Size), Ada.Strings.Both) &
       "-*-*-*-*-*-iso8859-*";
   end Default_Font_Name;


   package Settings_Logger is new Logger
     (Name => "Giant.Graph_Widgets.Settings");


   ---------------------------------------------------------------------------
   --  Manages colors for a graph widget
   package Colors is

      procedure Set_Up_Color_Array
        (Widget : access Graph_Widget_Record'Class);

      function Get_Color
        (Widget : access Graph_Widget_Record'Class;
         Index  : in     Integer)
        return Gdk.Color.Gdk_Color;

      function Get_Highlight_Color
        (Widget : access Graph_Widget_Record'Class;
         Light  : in     Vis_Data.Highlight_Type)
        return Gdk.Color.Gdk_Color;

      procedure Shut_Down_Color_Array
        (Widget : access Graph_Widget_Record'Class);

   end Colors;

   ----------------------------------------------------------------------------
   --  Generic package to map the color encodings in Config.Vis_Styles
   generic
      type Graph_Object is private;
      type Vis_Object is private;
      type Graph_Object_Class is private;
      with function Get_Graph_Object
        (Source    : in     Vis_Object)
        return Graph_Object;
      with function Get_Class
        (Object    : in     Graph_Object)
        return Graph_Object_Class;
   package Color_Retrieval is

      generic
         with function Get_Index
           (Vis_Style : in     Config.Vis_Styles.Visualisation_Style_Access;
            Object    : in     Graph_Object_Class)
           return Integer;
      function Find_Color
        (Widget : access Graph_Widget_Record'Class;
         Object : in     Vis_Object)
         return Gdk.Color.Gdk_Color;

   end Color_Retrieval;

   package body Color_Retrieval is

      function Find_Color
        (Widget : access Graph_Widget_Record'Class;
         Object : in     Vis_Object)
         return Gdk.Color.Gdk_Color is

         Index : Integer;
      begin
         Index := Get_Index
           (Vis_Style => Get_Vis_Style (Widget),
            Object    => Get_Class (Get_Graph_Object (Object)));
         return Colors.Get_Color (Widget, Index);
      end Find_Color;

   end Color_Retrieval;

   --  instance for edges
   package Edge_Colors is new Color_Retrieval
     (Vis_Object         => Vis_Data.Vis_Edge_Id,
      Graph_Object       => Graph_Lib.Edge_Id,
      Graph_Object_Class => Graph_Lib.Edge_Class_Id,
      Get_Graph_Object   => Vis_Data.Get_Graph_Edge,
      Get_Class          => Graph_Lib.Get_Edge_Class_Id);

   --  instance for nodes
   package Node_Colors is new Color_Retrieval
     (Vis_Object         => Vis_Data.Vis_Node_Id,
      Graph_Object       => Graph_Lib.Node_Id,
      Graph_Object_Class => Graph_Lib.Node_Class_Id,
      Get_Graph_Object   => Vis_Data.Get_Graph_Node,
      Get_Class          => Graph_Lib.Get_Node_Class_Id);


   ---------------------------------------------------------------------------
   --  Manages the Pixmaps for icons.
   --
   --  It is assumed that all graph widgets can share pixmaps to save
   --  resources. Actually this is can lead to problems because each
   --  pixmap should be inialized specially for one Gdk_Window.
   package Icons is

      function Is_Set_Up
        return Boolean;

      procedure Set_Up_Icon_Array
        (Widget : access Graph_Widget_Record'Class);

      procedure Get_Icon
        (Widget : access Graph_Widget_Record'Class;
         Index  : in     Integer;
         Icon   :    out Gdk.Pixmap.Gdk_Pixmap;
         Width  :    out Glib.Gint;
         Height :    out Glib.Gint);

      procedure Get_Annotation_Icon
        (Widget : access Graph_Widget_Record'Class;
         Icon   :    out Gdk.Pixmap.Gdk_Pixmap;
         Width  :    out Glib.Gint;
         Height :    out Glib.Gint);

      procedure Shut_Down_Icon_Array
        (Widget : access Graph_Widget_Record'Class);


   private

      type Icon_Record is
         record
            Icon   : Gdk.Pixmap.Gdk_Pixmap;
            Width  : Glib.Gint;
            Height : Glib.Gint;
         end record;

      type Icon_Array_Type is array (Integer range <>) of Icon_Record;

      type Icon_Array_Access is access Icon_Array_Type;

      -------------------------------------------------------------------------
      --  All icons ever displayed in a graph widget. Last is the annotation
      --  icon. Should be stored inside 'Graph_Widget_Record', but is not
      --  to save storage.
      Icons            : Icon_Array_Access := null;
   end Icons;



   ----------------
   -- Life cycle --
   ----------------

   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class) is

      function "+"
        (Left  : in Glib.Gint;
         Right : in Glib.Gint)
        return Glib.Gint renames Glib."+";
   begin
      Update_Font_Choice (Widget);

      Icons.Set_Up_Icon_Array (Widget);
      Colors.Set_Up_Color_Array (Widget);
   end Set_Up;

   procedure Shut_Down
     (Widget : access Graph_Widget_Record'Class) is
   begin
      if Gdk."/=" (Widget.Settings.Font, Gdk.Font.Null_Font) then
         Gdk.Font.Unref (Widget.Settings.Font);
      end if;

      Colors.Shut_Down_Color_Array (Widget);
      --  Icons not shut down because shared resource for all widgets
      --  Icons.Shut_Down_Icon_Array (Widget);
   end Shut_Down;

   procedure Set_Style
     (Widget : access Graph_Widget_Record'Class;
      Style  : in     Config.Vis_Styles.Visualisation_Style_Access) is
   begin
      Widget.Settings.Vis_Style := Style;
   end Set_Style;

   function Get_Style
     (Widget : access Graph_Widget_Record'Class)
     return Config.Vis_Styles.Visualisation_Style_Access is
   begin
      return Widget.Settings.Vis_Style;
   end Get_Style;

   procedure Set_Annotation_Pool
     (Widget : access Graph_Widget_Record'Class;
      Pool   : in     Node_Annotations.Node_Annotation_Access) is
   begin
      Widget.Settings.Node_Annotation_Pool := Pool;
   end Set_Annotation_Pool;

   function Get_Annotation_Pool
     (Widget : access Graph_Widget_Record'Class)
     return Node_Annotations.Node_Annotation_Access is
   begin
      return Widget.Settings.Node_Annotation_Pool;
   end Get_Annotation_Pool;


   procedure Update_Font_Choice
     (Widget : access Graph_Widget_Record'Class) is

      Width               : Vis.Absolute_Natural := Widget.Settings.Node_Width;
      Proposed_Font_Height: Vis.Absolute_Natural;
   begin
      Proposed_Font_Height := Width / Default_Node_Text_Lines_Estimate;
      if Proposed_Font_Height < Default_Minimum_Font_Size then
         Proposed_Font_Height := Default_Minimum_Font_Size;
      elsif Proposed_Font_Height > Default_Maximum_Font_Size then
         Proposed_Font_Height := Default_Maximum_Font_Size;
      end if;
      if Widget.Settings.Font_Choice /= Proposed_Font_Height then
         Widget.Settings.Font_Choice := Proposed_Font_Height;

         declare
            Font_Name : String := Default_Font_Name
              (Widget.Settings.Font_Choice);
         begin
            Gdk.Font.Load
              (Font      => Widget.Settings.Font,
               Font_Name => Font_Name);
            if Gdk."=" (Widget.Settings.Font, Gdk.Font.Null_Font) then
               Settings_Logger.Error
                 ("Could not load font """ & Font_Name & """. Using "
                  & "Null_Font instead.");
               Widget.Settings.Font_Height := 0;
            else
               --  above base line + base line + below base line
               Widget.Settings.Font_Height := Vis.Absolute_Natural
                 (Gdk.Font.Get_Ascent (Widget.Settings.Font)) +
                  1 +
                  Vis.Absolute_Natural
                    (Gdk.Font.Get_Descent (Widget.Settings.Font));
               Settings_Logger.Debug
                 ("Font loaded: """ & Font_Name & """, Height ="
                  & Integer'Image (Widget.Settings.Font_Height));
            end if;
         end;
      end if;
   end Update_Font_Choice;


   procedure Set_Zoom
     (Widget : access Graph_Widget_Record'Class;
      Zoom   : in     Vis.Zoom_Level) is

      Width : Vis.Absolute_Natural;
   begin
      Width := Vis.Absolute_Natural (Zoom * Float (Default_Node_Width));
      if Width < Default_Minimum_Node_Width then
         Width := Default_Minimum_Node_Width;
      end if;
      Widget.Settings.Node_Width := Width;
      Update_Font_Choice (Widget);

      if Zoom < 0.3 then
         Widget.Settings.Detail_Level := Low;
      elsif Zoom < 0.7 then
         Widget.Settings.Detail_Level := Average;
      else
         Widget.Settings.Detail_Level := High;
      end if;
   end Set_Zoom;

   function Get_Detail_Level
     (Widget : access Graph_Widget_Record'Class)
     return Detail_Level_Type is
   begin
      return Widget.Settings.Detail_Level;
   end Get_Detail_Level;


   -----------------------
   -- Color inspections --
   -----------------------

   function Get_Highlight_Color
     (Widget       : access Graph_Widget_Record'Class;
      Highlighting : in     Vis_Data.Highlight_Type)
     return Gdk.Color.Gdk_Color is
   begin
      return Colors.Get_Highlight_Color (Widget, Highlighting);
   end Get_Highlight_Color;

   function Get_Background_Color
     (Widget       : access Graph_Widget_Record'Class)
     return Gdk.Color.Gdk_Color is
   begin
      return Colors.Get_Color
        (Widget,
         Config.Vis_Styles.Get_Vis_Window_Background_Color
           (Widget.Settings.Vis_Style));
   end Get_Background_Color;


   -----------
   -- Edges --
   -----------

   function Get_Edge_Style
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Edge_Style_Type is

      Graph_Edge : Graph_Lib.Edge_Id := Vis_Data.Get_Graph_Edge (Edge);
   begin
      return Edge_Style_Type (Config.Vis_Styles.Get_Line_Style
        (Vis_Style  => Get_Vis_Style (Widget),
         Edge_Class => Graph_Lib.Get_Edge_Class_Id (Graph_Edge)));
   end Get_Edge_Style;

   function Get_Edge_Color
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Gdk.Color.Gdk_Color is

      function Find is new Edge_Colors.Find_Color
        (Get_Index => Config.Vis_Styles.Get_Line_Color);

   begin
      return Find (Widget, Edge);
   end Get_Edge_Color;

   function Show_Edge_Label
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Boolean is

      Graph_Edge : Graph_Lib.Edge_Id := Vis_Data.Get_Graph_Edge (Edge);
   begin
      return Config.Vis_Styles.Show_Label_For_Edge_Class_Name
        (Vis_Style  => Get_Vis_Style (Widget),
         Edge_Class => Graph_Lib.Get_Edge_Class_Id (Graph_Edge));
   end Show_Edge_Label;

   function Get_Edge_Label_Color
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Gdk.Color.Gdk_Color is

      function Find is new Edge_Colors.Find_Color
        (Get_Index => Config.Vis_Styles.Get_Text_Color);

   begin
      return Find (Widget, Edge);
   end Get_Edge_Label_Color;

   function Get_Edge_Font
     (Widget : access Graph_Widget_Record'Class)
     return Gdk.Font.Gdk_Font is
   begin
      return Widget.Settings.Font;
   end Get_Edge_Font;

   function Get_Edge_Font_Height
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute_Natural is
   begin
      return Widget.Settings.Font_Height;
   end Get_Edge_Font_Height;


   -----------
   -- Nodes --
   -----------

   function Get_Node_Border_Color
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Gdk.Color.Gdk_Color is

      function Find is new Node_Colors.Find_Color
        (Get_Index => Config.Vis_Styles.Get_Border_Color);

   begin
      return Find (Widget, Node);
   end Get_Node_Border_Color;

   function Get_Node_Fill_Color
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Gdk.Color.Gdk_Color is

      function Find is new Node_Colors.Find_Color
        (Get_Index => Config.Vis_Styles.Get_Fill_Color);

   begin
      return Find (Widget, Node);
   end Get_Node_Fill_Color;

   function Get_Node_Text_Color
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Gdk.Color.Gdk_Color is

      function Find is new Node_Colors.Find_Color
        (Get_Index => Config.Vis_Styles.Get_Text_Color);

   begin
     return Find (Widget, Node);
   end Get_Node_Text_Color;

   function Has_Annotation
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Boolean is
   begin
      return Node_Annotations.Is_Annotated
        (Widget.Settings.Node_Annotation_Pool,
         Vis_Data.Get_Graph_Node (Node));
   end Has_Annotation;


   ------------------
   -- Nodes: Icons --
   ------------------

   procedure Get_Annotation_Icon
     (Widget       : access Graph_Widget_Record'Class;
      Icon         :    out Gdk.Pixmap.Gdk_Pixmap;
      Width        :    out Glib.Gint;
      Height       :    out Glib.Gint) is
   begin
      Icons.Get_Annotation_Icon (Widget, Icon, Width, Height);
   end Get_Annotation_Icon;

   function Get_Annotation_Icon_Size
     (Widget       : access Graph_Widget_Record'Class)
     return Vis.Absolute.Vector_2d is

      Icon   : Gdk.Pixmap.Gdk_Pixmap;
      Width  : Glib.Gint;
      Height : Glib.Gint;
   begin
      Get_Annotation_Icon (Widget, Icon, Width, Height);
      return Vis.Absolute.Combine_Vector
        (Vis.Absolute_Int (Width), Vis.Absolute_Int (Height));
   end Get_Annotation_Icon_Size;

   procedure Get_Node_Icon
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id;
      Icon         :    out Gdk.Pixmap.Gdk_Pixmap;
      Width        :    out Glib.Gint;
      Height       :    out Glib.Gint) is

      Node_Class : Graph_Lib.Node_Class_Id;
      Index      : Integer;
   begin
      Node_Class := Graph_Lib.Get_Node_Class_Id
        (Node       => Vis_Data.Get_Graph_Node (Node));
      Index := Config.Vis_Styles.Get_Node_Icon_Encoding
        (Vis_Style  => Get_Vis_Style (Widget),
         Node_Class => Node_Class);
      Icons.Get_Icon (Widget, Index, Icon, Width, Height);
   end Get_Node_Icon;

   function Get_Node_Icon_Size
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Vis.Absolute.Vector_2d is

      Icon       : Gdk.Pixmap.Gdk_Pixmap;
      Width      : Glib.Gint;
      Height     : Glib.Gint;
   begin
      Get_Node_Icon (Widget, Node, Icon, Width, Height);
      return Vis.Absolute.Combine_Vector
        (Vis.Absolute_Int (Width), Vis.Absolute_Int (Height));
   end Get_Node_Icon_Size;

   function Get_Node_Width
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute_Natural is
   begin
      return Widget.Settings.Node_Width;
   end Get_Node_Width;


   ------------------
   -- Nodes: Fonts --
   ------------------

   function Get_Node_Font
     (Widget : access Graph_Widget_Record'Class)
     return Gdk.Font.Gdk_Font is
   begin
      return Widget.Settings.Font;
   end Get_Node_Font;

   function Get_Node_Font_Height
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute_Natural is
   begin
      return Widget.Settings.Font_Height;
   end Get_Node_Font_Height;


   -----------------------
   -- Nodes: Attributes --
   -----------------------

   function Show_Node_Class_Name
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Boolean is
   begin
      return True;
   end Show_Node_Class_Name;

   function Get_Node_Attribute_Count
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Natural is

      Node_Class : Graph_Lib.Node_Class_Id;
      Filter     : Graph_Lib.Node_Attribute_Filters.Filter;
   begin
      Node_Class := Graph_Lib.Get_Node_Class_Id
        (Node       => Vis_Data.Get_Graph_Node (Node));
      Filter := Config.Vis_Styles.Get_Attribute_Filter
        (Vis_Style  => Widget.Settings.Vis_Style,
         Node_Class => Node_Class);
      return Graph_Lib.Node_Attribute_Filters.Size (Filter);
   end Get_Node_Attribute_Count;

   function Get_Node_Attributes
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Graph_Lib.Node_Attribute_Filters.Filtered_Iterator is

      Node_Class : Graph_Lib.Node_Class_Id;
      Filter     : Graph_Lib.Node_Attribute_Filters.Filter;
   begin
      Node_Class := Graph_Lib.Get_Node_Class_Id
        (Node       => Vis_Data.Get_Graph_Node (Node));
      Filter := Config.Vis_Styles.Get_Attribute_Filter
        (Vis_Style  => Widget.Settings.Vis_Style,
         Node_Class => Node_Class);
      return Graph_Lib.Node_Attribute_Filters.Make_Filtered_Iter (Filter);
   end Get_Node_Attributes;


   ------------
   -- Colors --
   ------------

   package body Colors is

      function Create
        (Color_Access : in     Config.Color_Access)
        return Gdk.Color.Gdk_Color is

         Color_Spec : String := Config.Get_Color_Value (Color_Access);
      begin
         return Gdk.Color.Parse (Color_Spec);
      exception
         when Gdk.Color.Wrong_Color =>
            Settings_Logger.Error
              ("Color """ & Color_Spec & """ could not be parsed. Use default"
               & " color instead.");
            return Gdk.Color.Null_Color;
      end Create;

      function Highlight_Index
        (Widget : access Graph_Widget_Record'Class;
         Light  : in     Vis_Data.Highlight_Type)
        return Natural is
      begin
         return Widget.Settings.Highlight_Index_Offset +
           Vis_Data.Highlight_Type'Pos (Light);
      end Highlight_Index;

      procedure Set_Up_Highlight_Colors
        (Widget : access Graph_Widget_Record'Class) is

         package C renames Config.Global_Data;
      begin
         Widget.Settings.All_Colors
           (Highlight_Index (Widget, Vis_Data.Current_Local)) :=
           Create (C.Get_Selection_Highlight_Color (C.Current_Selection));
         Widget.Settings.All_Colors
           (Highlight_Index (Widget, Vis_Data.First_Local)) :=
           Create (C.Get_Selection_Highlight_Color (C.Color_1));
         Widget.Settings.All_Colors
           (Highlight_Index (Widget, Vis_Data.Second_Local)) :=
           Create (C.Get_Selection_Highlight_Color (C.Color_2));
         Widget.Settings.All_Colors
           (Highlight_Index (Widget, Vis_Data.Third_Local)) :=
           Create (C.Get_Selection_Highlight_Color (C.Color_3));
         Widget.Settings.All_Colors
           (Highlight_Index (Widget, Vis_Data.First_Global)) :=
           Create (C.Get_Subgraph_Highlight_Color (C.Color_1));
         Widget.Settings.All_Colors
           (Highlight_Index (Widget, Vis_Data.Second_Global)) :=
           Create (C.Get_Subgraph_Highlight_Color (C.Color_2));
         Widget.Settings.All_Colors
           (Highlight_Index (Widget, Vis_Data.Third_Global)) :=
           Create (C.Get_Subgraph_Highlight_Color (C.Color_3));
      end Set_Up_Highlight_Colors;

      procedure Set_Up_Color_Array
        (Widget : access Graph_Widget_Record'Class) is

         Color_Accesses : Config.Vis_Styles.Color_Access_Array_Access :=
           Config.Vis_Styles.Get_All_Colors;
         Num_Highlight : constant Integer :=
           Vis_Data.Highlight_Type'Pos (Vis_Data.Highlight_Type'Last) -
           Vis_Data.Highlight_Type'Pos (Vis_Data.Highlight_Type'First) + 1;
      begin
         if Widget.Settings.All_Colors /= null then
            Shut_Down_Color_Array (Widget);
         end if;

         Widget.Settings.Highlight_Index_Offset := Color_Accesses'Last -
           Vis_Data.Highlight_Type'Pos (Vis_Data.Highlight_Type'First) + 1;

         Widget.Settings.All_Colors := new Gdk.Color.Gdk_Color_Array
           (Color_Accesses'First .. Color_Accesses'Last + Num_Highlight);
         Set_Up_Highlight_Colors (Widget);
         for I in Color_Accesses'Range loop
            Widget.Settings.All_Colors (I) := Create (Color_Accesses (I));
         end loop;

         declare
            Success      : Glib.Boolean_Array
              (Widget.Settings.All_Colors'Range);
            Result_Count : Glib.Gint;
         begin
            Gdk.Color.Alloc_Colors
              (Colormap => Get_Colormap (Widget),
               Colors   => Widget.Settings.All_Colors.all,
               Success  => Success,
               Result   => Result_Count);
            if Glib.">" (Result_Count, 0) then
               Settings_Logger.Error
                 (Glib.Gint'Image (Result_Count)
                  & " colors could not be allocated. Using default colors "
                  & "instead.");
               for I in Success'Range loop
                  if not Success (I) then
                     Widget.Settings.All_Colors (I) := Gdk.Color.Null_Color;
                  end if;
               end loop;
            end if;
         end;
      end Set_Up_Color_Array;

      function Get_Color
        (Widget : access Graph_Widget_Record'Class;
         Index  : in     Integer)
        return Gdk.Color.Gdk_Color is
      begin
         pragma Assert
           (Index < Widget.Settings.Highlight_Index_Offset +
              Vis_Data.Highlight_Type'Pos
              (Vis_Data.Highlight_Type'First));
         return Widget.Settings.All_Colors (Index);
      end Get_Color;

      function Get_Highlight_Color
        (Widget : access Graph_Widget_Record'Class;
         Light  : in     Vis_Data.Highlight_Type)
        return Gdk.Color.Gdk_Color is
      begin
         return Widget.Settings.All_Colors
           (Vis_Data.Highlight_Type'Pos (Light) +
            Widget.Settings.Highlight_Index_Offset);
      end Get_Highlight_Color;

      procedure Shut_Down_Color_Array
        (Widget : access Graph_Widget_Record'Class) is

         procedure Free is new Ada.Unchecked_Deallocation
           (Object => Gdk.Color.Gdk_Color_Array,
            Name   => Color_Array_Access);

      begin
         Gdk.Color.Free_Colors
           (Get_Colormap (Widget), Widget.Settings.All_Colors.all);
         Free (Widget.Settings.All_Colors);
      end Shut_Down_Color_Array;

   end Colors;


   -----------
   -- Icons --
   -----------

   package body Icons is

      function Load_Icon
        (Widget    : access Graph_Widget_Record'Class;
         File_Name : in     String)
        return Icon_Record is

         Pixmap : Gdk.Pixmap.Gdk_Pixmap;
         Mask   : Gdk.Bitmap.Gdk_Bitmap := Gdk.Bitmap.Null_Bitmap;
         Width  : Glib.Gint := 0;
         Height : Glib.Gint := 0;
      begin
         Gdk.Pixmap.Create_From_Xpm
           (Pixmap      => Pixmap,
            Window      => Get_Window (Widget),
            Mask        => Mask,
            Transparent => Gdk.Color.Null_Color,
            Filename    => File_Name);
         if Gdk."/=" (Mask, Gdk.Bitmap.Null_Bitmap) then
            Gdk.Bitmap.Unref (Mask);
            Settings_Logger.Debug ("Discarded Bitmap");
         end if;
         if Gdk."/=" (Pixmap, Gdk.Pixmap.Null_Pixmap) then
            Settings_Logger.Debug ("Icon loaded: " & File_Name);
            Gdk.Drawable.Get_Size (Pixmap, Width, Height);
         else
            Settings_Logger.Error
              ("Failed to load icon from file """ & File_Name & """.");
         end if;
         return (Pixmap, Width, Height);
      end Load_Icon;

      function Is_Set_Up
        return Boolean is
      begin
         return Icons /= null;
      end Is_Set_Up;

      procedure Set_Up_Icon_Array
        (Widget : access Graph_Widget_Record'Class) is

         Files : Config.Vis_Styles.Node_Icons_Array_Access :=
           Config.Vis_Styles.Get_All_Node_Icons;
      begin
         if Icons /= null then
            return;
            --  Shut_Down_Icon_Array (Widget);
            --  Do not call shutdown because the Icons array is shared for
            --  all widgets
         end if;
         Icons := new Icon_Array_Type'
           (Files'First .. Files'Last + 1 =>
              (Gdk.Pixmap.Null_Pixmap, 0, 0));
         Settings_Logger.Debug ("Load annotation Icon...");
         Icons (Icons'Last) := Load_Icon
           (Widget, Config.Global_Data.Get_Node_Annotations_Icon);
         Settings_Logger.Debug ("Load node icons");
         for I in Files'Range loop
            Icons (I) := Load_Icon
              (Widget, Ada.Strings.Unbounded.To_String (Files (I)));
         end loop;
      end Set_Up_Icon_Array;

      procedure Get_Icon
        (Widget : access Graph_Widget_Record'Class;
         Index  : in     Integer;
         Icon   :    out Gdk.Pixmap.Gdk_Pixmap;
         Width  :    out Glib.Gint;
         Height :    out Glib.Gint) is
      begin
         pragma Assert (Is_Set_Up);
         if Index in Icons'Range then
            Icon := Icons (Index).Icon;
            Width := Icons (Index).Width;
            Height := Icons (Index).Height;
         else
            Icon := Gdk.Pixmap.Null_Pixmap;
            Width := 0;
            Height := 0;
         end if;
      end Get_Icon;

      procedure Get_Annotation_Icon
        (Widget : access Graph_Widget_Record'Class;
         Icon   :    out Gdk.Pixmap.Gdk_Pixmap;
         Width  :    out Glib.Gint;
         Height :    out Glib.Gint) is
      begin
         pragma Assert (Is_Set_Up);
         Icon := Icons (Icons'Last).Icon;
         Width := Icons (Icons'Last).Width;
         Height := Icons (Icons'Last).Height;
      end Get_Annotation_Icon;

      procedure Shut_Down_Icon_Array
        (Widget : access Graph_Widget_Record'Class) is

         procedure Free is new Ada.Unchecked_Deallocation
           (Object => Icon_Array_Type,
            Name   => Icon_Array_Access);

      begin
         if Icons /= null then
            for I in Icons'Range loop
               if Gdk."/=" (Icons (I).Icon, Gdk.Pixmap.Null_Pixmap) then
                  Gdk.Pixmap.Unref (Icons (I).Icon);
               end if;
            end loop;
            Free (Icons);
         end if;
      end Shut_Down_Icon_Array;

   end Icons;

end Giant.Graph_Widgets.Settings;
