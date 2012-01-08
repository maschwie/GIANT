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
--  $RCSfile: giant-graph_widgets-drawing.adb,v $, $Revision: 1.37 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Ada.Unchecked_Conversion;

with Gdk.Drawable;
with Gdk.Window;

with Giant.Graph_Lib.Node_Attribute_Filters;
with Giant.Graph_Widgets.Settings;
with Giant.Graph_Widgets.States;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Graph_Widgets.Drawing is


   use Vis.Absolute;


   package Drawing_Logger is new Logger
     (Name => "Giant.Graph_Widgets.Drawing");


   type Layer_Clipping_Array is array (Positive range <>) of
     Vis_Data.Layer_Clipping_Access;


   function Get_Height
     (Font : in     Gdk.Font.Gdk_Font)
     return Vis.Absolute_Int is
   begin
      return Vis.Absolute_Int (Gdk.Font.Get_Ascent (Font)) +
        Vis.Absolute_Int (Gdk.Font.Get_Descent (Font));
   end Get_Height;

   function Get_Maximum_Number_Of_Lights
     return Natural is
   begin
      return Vis_Data.Highlight_Type'Pos (Vis_Data.Highlight_Type'Last) -
        Vis_Data.Highlight_Type'Pos (Vis_Data.Highlight_Type'First) + 1;
   end Get_Maximum_Number_Of_Lights;


   function Get_Maximum_Number_Of_Global_Lights
     return Natural is
   begin
      return Vis_Data.Global_Highlight_Type'Pos
        (Vis_Data.Global_Highlight_Type'Last) -
        Vis_Data.Global_Highlight_Type'Pos
        (Vis_Data.Global_Highlight_Type'First) + 1;
   end Get_Maximum_Number_Of_Global_Lights;

   function Get_Number_Of_Global_Lights
     (Node : Vis_Data.Vis_Node_Id)
     return Natural is

      Number_Of_Lights : Natural;
      Highlighting : Vis_Data.Highlight_Array :=
        Vis_Data.Get_Highlighting (Node);
   begin
      Number_Of_Lights := 0;
      for I in Vis_Data.Global_Highlight_Type loop
         if Highlighting (I) then
            Number_Of_Lights := Number_Of_Lights + 1;
         end if;
      end loop;
      return Number_Of_Lights;
   end Get_Number_Of_Global_Lights;

   procedure Reset_Buffers
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Absolute.Rectangle_2d) is

      Window : Gdk.Window.Gdk_Window := Get_Window (Widget);
      Size   : Vis.Absolute.Vector_2d := Get_Size (Area);
   begin
      if Gdk."/=" (Widget.Drawing.Buffer, Gdk.Pixmap.Null_Pixmap) then
         Gdk.Pixmap.Unref (Widget.Drawing.Buffer);
      end if;
      Gdk.Pixmap.Gdk_New
        (Pixmap => Widget.Drawing.Buffer,
         Window => Window,
         Width  => Glib.Gint (Get_X (Size)),
         Height => Glib.Gint (Get_Y (Size)));

      if Gdk."/=" (Widget.Drawing.Ready_Buffer, Gdk.Pixmap.Null_Pixmap) then
         Gdk.Pixmap.Unref (Widget.Drawing.Ready_Buffer);
      end if;
      Gdk.Pixmap.Gdk_New
        (Pixmap => Widget.Drawing.Ready_Buffer,
         Window => Window,
         Width  => Glib.Gint (Get_X (Size)),
         Height => Glib.Gint (Get_Y (Size)));

      Widget.Drawing.Buffer_Area := Area;
   end Reset_Buffers;


   ----------------------------------------------------------------------------
   --  Clears a drawable using Gc
   procedure Clear
     (Gc       : in     Gdk.GC.Gdk_GC;
      Drawable : in     Gdk.Drawable.Gdk_Drawable) is

      Width  : Glib.Gint;
      Height : Glib.Gint;
   begin
      Gdk.Drawable.Get_Size
        (Drawable => Drawable,
         Width    => Width,
         Height   => Height);
      Gdk.Drawable.Draw_Rectangle
        (Drawable => Drawable,
         Gc       => Gc,
         Filled   => True,
         X        => 0,
         Y        => 0,
         Width    => Width,
         Height   => Height);
   end Clear;

   procedure Draw_Filled
     (Drawable  : in     Gdk.Drawable.Gdk_Drawable;
      Gc        : in     Gdk.GC.Gdk_GC;
      Rectangle : in     Vis.Absolute.Rectangle_2d;
      Origin    : in     Vis.Absolute.Vector_2d) is

      Top_Left : Vis.Absolute.Vector_2d := Get_Top_Left (Rectangle) - Origin;
      X        : Glib.Gint              := Glib.Gint (Get_X (Top_Left));
      Y        : Glib.Gint              := Glib.Gint (Get_Y (Top_Left));
   begin
      Gdk.Drawable.Draw_Rectangle
        (Drawable => Drawable,
         Gc       => Gc,
         Filled   => True,
         X        => X,
         Y        => Y,
         Width    => Glib.Gint (Get_Width (Rectangle)),
         Height   => Glib.Gint (Get_Height (Rectangle)));
   end Draw_Filled;
   pragma Inline (Draw_Filled);

   procedure Draw_Border
     (Drawable  : in     Gdk.Drawable.Gdk_Drawable;
      Gc        : in     Gdk.GC.Gdk_GC;
      Rectangle : in     Vis.Absolute.Rectangle_2d;
      Origin    : in     Vis.Absolute.Vector_2d) is

      Top_Left : Vis.Absolute.Vector_2d := Get_Top_Left (Rectangle) - Origin;
      X        : Glib.Gint              := Glib.Gint (Get_X (Top_Left));
      Y        : Glib.Gint              := Glib.Gint (Get_Y (Top_Left));
   begin
      Gdk.Drawable.Draw_Rectangle
        (Drawable => Drawable,
         Gc       => Gc,
         Filled   => False,
         X        => X,
         Y        => Y,
         Width    => Glib.Gint (Get_Width (Rectangle) - 1),
         Height   => Glib.Gint (Get_Height (Rectangle) - 1));
   end Draw_Border;
   pragma Inline (Draw_Border);

   procedure Draw_Text
     (Buffer : in     Gdk.Drawable.Gdk_Drawable;
      Font   : in     Gdk.Font.Gdk_Font;
      Gc     : in     Gdk.GC.Gdk_GC;
      Area   : in     Vis.Absolute.Rectangle_2d;
      Origin : in     Vis.Absolute.Vector_2d := Vis.Absolute.Zero_2d;
      Text   : in     String) is

      use type Glib.Gint;
      Width          : Glib.Gint := Glib.Gint (Get_Width (Area));
      Text_Width     : Glib.Gint;
      Avg_Char_Width : Glib.Gint;
      Minimum        : Integer;
      Maximum        : Integer;
      Last           : Integer;
   begin
      if Get_Height (Font) > Get_Height (Area) or else Text'Length < 1 then
         return;
      end if;

      Text_Width := Gdk.Font.String_Measure (Font, Text);
      if Text_Width > Width then
         --  leave width for abbreviation string
         Width := Width -
           Gdk.Font.String_Measure (Font, Default_Text_Abbreviation);

         --  can draw at least abbreviation string?
         if Width >= 0 then
            --  Text (Text'First .. Minimum) can be drawn.
            Minimum := Text'First - 1;
            --  upper bound for maximum number of characters
            Maximum := Text'Last - 1;
            --  Maximum >= Minimum because of Text'Length >= 1 (see above)

            --  last time tried this bound
            Last := Text'Last;
            --  Last >= Text'First because of Text'Length >= 1 (see above)

            while Maximum > Minimum loop
               --  Last >= Text'First ==> Last - Text'First + 1 > 0
               Avg_Char_Width := Text_Width /
                 Glib.Gint (Last - Text'First + 1);
               if Avg_Char_Width > 0 then
                  Last := Integer (Width / Avg_Char_Width) + Text'First;
               end if;
               if Last > Maximum then
                  Last := Maximum;
               elsif Last <= Minimum then
                  Last := Minimum + 1;
                  --  Text'First <= Minimum + 1 <= Last ==> Last >= Text'First
               end if;
               --  Minimum + 1 <= Last <= Maximum
               Text_Width := Gdk.Font.String_Width
                 (Font, Text (Text'First .. Last));
               if Text_Width < Width then
                  --  Minimum becomes greater, Text (Text'First .. Minimum)
                  --  will still be ok.
                  Minimum := Last;
               elsif Text_Width > Width then
                  --  Maximum becomes smaller
                  Maximum := Last - 1;
               else
                  --  Minimum becomes greater (found optimum anyway)
                  Minimum := Last;
                  Maximum := Last;
               end if;
               --  loop will terminate because in each iteration either
               --  Minimum becomes greater or Maximum becomes smaller
               --  Minimum is assured to be a possible value
            end loop;

            Gdk.Drawable.Draw_Text
              (Drawable => Buffer,
               Font     => Font,
               Gc       => Gc,
               X        => Glib.Gint (Get_Left (Area) - Get_X (Origin)),
               Y        => Glib.Gint (Get_Top (Area) - Get_Y (Origin)) +
                             Gdk.Font.Get_Ascent (Font),
               Text     => Text (Text'First .. Minimum) &
                             Default_Text_Abbreviation);

         else
            null;
            --  no drawing done at all: no space for entire text and no
            --  space for abbreviation.
         end if;
      else
         Gdk.Drawable.Draw_Text
           (Drawable => Buffer,
            Font     => Font,
            Gc       => Gc,
            X        => Glib.Gint (Get_Left (Area) - Get_X (Origin)),
            Y        => Glib.Gint (Get_Top (Area) - Get_Y (Origin)) +
                          Gdk.Font.Get_Ascent (Font),
            Text     => Text);
      end if;
   end Draw_Text;

   procedure Copy_Ready_Into_Normal_Buffer
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Absolute.Rectangle_2d;
      Origin : in     Vis.Absolute.Vector_2d) is

      X : Glib.Gint;
      Y : Glib.Gint;
   begin
      Vis.To_Gdk (Get_Top_Left (Area) - Origin, X, Y);
      Gdk.Drawable.Draw_Drawable
        (Drawable => Widget.Drawing.Buffer,
         GC       => Widget.Drawing.Background,
         XDest    => X,
         YDest    => Y,
         Src      => Widget.Drawing.Ready_Buffer,
         XSrc     => X,
         YSrc     => Y,
         Width    => Glib.Gint (Get_Width (Area)),
         Height   => Glib.Gint (Get_Height (Area)));
   end Copy_Ready_Into_Normal_Buffer;
   pragma Inline (Copy_Ready_Into_Normal_Buffer);

   --  NOTE: Must be synced with 'Draw_Edge'
   function Get_Maximum_Edge_Highlight_Width
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute_Natural is
   begin
      return Get_Maximum_Number_Of_Lights * Default_Edge_Light_Thickness;
   end Get_Maximum_Edge_Highlight_Width;

   --  NOTE: Must be synced with 'Draw_Edge'
   procedure Update_Edge_Size
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id) is

      Number_Of_Lights : Natural;
      Highlighting     : Vis_Data.Highlight_Array :=
        Vis_Data.Get_Highlighting (Edge);
      Width            : Vis.Absolute_Natural;
      Height           : Vis.Absolute_Natural;
      Font             : Gdk.Font.Gdk_Font;
      Graph_Edge       : Graph_Lib.Edge_Id := Vis_Data.Get_Graph_Edge (Edge);
   begin
      Number_Of_Lights := 0;
      for I in Vis_Data.Highlight_Type loop
         if Highlighting (I) then
            Number_Of_Lights := Number_Of_Lights + 1;
         end if;
      end loop;
      Vis_Data.Set_Thickness
        (Edge,
         Default_Edge_Line_Thickness +
           Number_Of_Lights * Default_Edge_Light_Thickness);

      if Settings.Show_Edge_Label (Widget, Edge) then
         Font := Settings.Get_Edge_Font (Widget);
         Height := Settings.Get_Edge_Font_Height (Widget);
         Width := Vis.Absolute_Natural
           (Gdk.Font.String_Measure (Font,
                                     Graph_Lib.Get_Edge_Tag (Graph_Edge))) + 1;
         Vis_Data.Set_Text_Area_Size (Edge, Combine_Vector (Width, Height));
      else
         Vis_Data.Remove_Text_Area (Edge);
      end if;
   end Update_Edge_Size;

   ----------------------------------------------------------------------------
   --  Draws an edge onto 'Buffer'
   --  NOTE: Must be synced with 'Update_Edge_Size'
   procedure Draw_Edge
     (Widget : access Graph_Widget_Record'Class;
      Buffer : in     Gdk.Pixmap.Gdk_Pixmap;
      Edge   : in     Vis_Data.Vis_Edge_Id;
      Origin : in     Vis.Absolute.Vector_2d) is

      procedure Draw_Edge_Line
        (Gc     : in     Gdk.GC.Gdk_GC;
         From   : in     Vis.Absolute.Vector_2d;
         To     : in     Vis.Absolute.Vector_2d) is

         From_Point : Vis.Absolute.Vector_2d := From;
         To_Point   : Vis.Absolute.Vector_2d := To;
         From_X     : Glib.Gint;
         From_Y     : Glib.Gint;
         To_X       : Glib.Gint;
         To_Y       : Glib.Gint;
         Intersect  : Boolean;
         Gc_To_Use  : Gdk.GC.Gdk_Gc := Gc;
      begin
         --  now the funny part: although 'Draw_Line' takes 'Gint' as parameter
         --  type, all values must be in 'Line_Drawing_Inside_Rectangle'.
         --  Thus we need to do some clipping manually...
         Vis.Clip_Line_To_Rectangle
           (From      => From_Point,
            To        => To_Point,
            Rectangle => Line_Drawing_Inside_Rectangle,
            Intersect => Intersect);

         if Intersect then
            Vis.To_Gdk (From_Point, From_X, From_Y);
            Vis.To_Gdk (To_Point, To_X, To_Y);

            Gdk.Drawable.Draw_Line
              (Drawable   => Buffer,
               Gc         => Gc_To_Use,
               X1         => From_X,
               Y1         => From_Y,
               X2         => To_X,
               Y2         => To_Y);
         end if;
      end Draw_Edge_Line;

      procedure Draw_All_Edge_Lines
        (Gc     : in     Gdk.GC.Gdk_GC;
         Style  : in     Edge_Style_Type;
         Width  : in     Vis.Absolute_Natural) is

         Source     : Vis.Absolute.Vector_2d;
         Target     : Vis.Absolute.Vector_2d;
         Line_Style : Gdk.GC.Gdk_Line_Style;
      begin
         case Style is
            when Continuous_Line =>
               Line_Style := Gdk.GC.Line_Solid;
            when Dashed_Line =>
               Line_Style := Gdk.GC.Line_On_Off_Dash;
            when Dotted_Line =>
               Line_Style := Gdk.GC.Line_On_Off_Dash;
         end case;
         --  set line width
         Gdk.GC.Set_Line_Attributes
           (GC         => Gc,
            Line_Width => Glib.Gint (Width),
            Line_Style => Line_Style,
            Cap_Style  => Gdk.GC.Cap_Round,
            Join_Style => Gdk.GC.Join_Round);
         --  Cannot use 'Gdk.Drawable.Draw_Lines' because
         --  'Gdk.Types.Gdk_Points_Array' uses Glib.Gint16 as component type
         --  wich is too small.
         Target := Vis_Data.Get_Point (Edge, 1) - Origin;
         for I in 2 .. Vis_Data.Get_Number_Of_Points (Edge) loop
            Source := Target;
            Target := Vis_Data.Get_Point (Edge, I) - Origin;
            Draw_Edge_Line
              (Gc     => Gc,
               From   => Source,
               To     => Target);
         end loop;
         --  Target contains end point
         Draw_Edge_Line
           (Gc     => Gc,
            From   => Vis_Data.Get_Left_Arrow_Point (Edge) - Origin,
            To     => Target);
         Draw_Edge_Line
           (Gc     => Gc,
            From   => Vis_Data.Get_Right_Arrow_Point (Edge) - Origin,
            To     => Target);
      end Draw_All_Edge_Lines;

      type Light_Array is array
        (1 .. Vis_Data.Highlight_Type'Pos (Vis_Data.Highlight_Type'Last) -
              Vis_Data.Highlight_Type'Pos (Vis_Data.Highlight_Type'First) + 1)
        of Vis_Data.Highlight_Type;
      Lights                : Light_Array;
      Light_Count           : Integer;
      Current_Thickness     : Vis.Absolute_Natural;
      Light_Extra_Thickness : Vis.Absolute_Natural;
      Style                 : Edge_Style_Type :=
        Settings.Get_Edge_Style (Widget, Edge);
      Highlighting          : Vis_Data.Highlight_Array :=
        Vis_Data.Get_Highlighting (Edge);
   begin
      if Vis_Data.Is_Hidden (Edge) then
         return;
      end if;

      Light_Count := Lights'First - 1;
      for Light in Vis_Data.Highlight_Type loop
         if Highlighting (Light) then
            Light_Count := Light_Count + 1;
            Lights (Light_Count) := Light;
         end if;
      end loop;
      if Light_Count >= Lights'First then
         Current_Thickness := Vis_Data.Get_Thickness (Edge);
         Light_Extra_Thickness :=
           (Current_Thickness - Default_Edge_Line_Thickness) /
           (Light_Count - Lights'First + 1);
         loop
            Draw_All_Edge_Lines
              (Gc    => Widget.Drawing.Edge_Light (Lights (Light_Count)),
               Style => Continuous_Line,
               Width => Current_Thickness);

            Light_Count := Light_Count - 1;
            exit when Light_Count < Lights'First;
            Current_Thickness := Current_Thickness - Light_Extra_Thickness;
         end loop;
      end if;

      Gdk.GC.Set_Foreground
        (GC    => Widget.Drawing.Edge_Line (Style),
          Color => Settings.Get_Edge_Color (Widget, Edge));
      Draw_All_Edge_Lines
        (Gc    => Widget.Drawing.Edge_Line (Style),
         Style => Style,
         Width => Default_Edge_Line_Thickness);

      if Vis_Data.Has_Text_Area (Edge) then
         Gdk.GC.Set_Foreground
           (GC     => Widget.Drawing.Edge_Label,
            Color  => Settings.Get_Edge_Label_Color (Widget, Edge));
         Draw_Text
           (Buffer => Buffer,
            Font   => Settings.Get_Edge_Font (Widget),
            Gc     => Widget.Drawing.Edge_Label,
            Area   => Vis_Data.Get_Text_Area (Edge),
            Origin => Origin,
            Text   => Graph_Lib.Get_Edge_Tag (Vis_Data.Get_Graph_Edge (Edge)));
      end if;
   end Draw_Edge;


   --  NOTE: Must be synced with 'Draw_Node'
   function Get_Maximum_Node_Highlight_Width
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute_Natural is
   begin
      return Get_Maximum_Number_Of_Global_Lights *
        Default_Node_Light_Thickness;
   end Get_Maximum_Node_Highlight_Width;


   --  NOTE: Must be synced with 'Draw_Node'
   procedure Update_Node_Size
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id) is

      Height               : Vis.Absolute_Natural;
      Width                : Vis.Absolute_Natural;
      Number_Of_Lights     : Natural := Get_Number_Of_Global_Lights (Node);
      Border_Thickness     : Vis.Absolute_Natural;
      Attributes_Height    : Vis.Absolute_Natural;
      Number_Of_Attributes : Natural :=
        Settings.Get_Node_Attribute_Count (Widget, Node);
      Font_Height          : Vis.Absolute_Natural :=
        Settings.Get_Node_Font_Height (Widget);
      Icon_Size            : Vis.Absolute.Vector_2d;
      Icon_Height          : Vis.Absolute_Natural;
      Annotation_Height    : Vis.Absolute_Natural;
      Header_Height        : Vis.Absolute_Natural;
      Highlighting         : Vis_Data.Highlight_Array :=
        Vis_Data.Get_Highlighting (Node);
      Detail_Level         : Detail_Level_Type :=
        Settings.Get_Detail_Level (Widget);
   begin
      Border_Thickness := Number_Of_Lights * Default_Node_Light_Thickness + 1;

      Width := 2 * Border_Thickness + Settings.Get_Node_Width (Widget);

      if Detail_Level >= Average then
         Header_Height := Default_Text_Spacing + Font_Height;
         Icon_Size := Settings.Get_Node_Icon_Size (Widget, Node);
         if Get_X (Icon_Size) <= Settings.Get_Node_Width (Widget) then
            Icon_Height := Get_Y (Icon_Size);
            if Icon_Height > Header_Height then
               Header_Height := Icon_Height;
            end if;
         end if;
         if Vis_Data.Is_Annotated (Node) then
            Annotation_Height := Get_Y
              (Settings.Get_Annotation_Icon_Size (Widget));
            if Annotation_Height > Header_Height then
               Header_Height := Annotation_Height;
            end if;
         end if;

         Height := 2 * Border_Thickness + Header_Height;
         if Settings.Show_Node_Class_Name (Widget, Node) then
            Height := Height + 2 * Default_Text_Spacing + Font_Height;
         end if;

         if Detail_Level >= High then
            Attributes_Height := Number_Of_Attributes *
              (Default_Text_Spacing + Font_Height);

            Height := Height + Attributes_Height;
         end if;
      else
         Height := Width;
      end if;

      Vis_Data.Set_Node_Size (Node, Combine_Vector (Width, Height));
   end Update_Node_Size;

   --  NOTE: Must be synced with 'Draw_Node' and
   --  'Get_Maximum_Node_Highlight_Width
   function Get_Node_Border_Top_Center
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id)
     return Vis.Absolute.Vector_2d is
   begin
      return Vis_Data.Get_Top_Center (Node) + Combine_Vector
        (0, Get_Number_Of_Global_Lights (Node) * Default_Node_Light_Thickness);
   end Get_Node_Border_Top_Center;

   ----------------------------------------------------------------------------
   --  Draws a node onto 'Buffer'
   --  NOTE: Must be synced with 'Update_Node_Size' and
   --        'Get_Node_Border_Top_Center
   procedure Draw_Node
     (Widget : access Graph_Widget_Record'Class;
      Buffer : in     Gdk.Pixmap.Gdk_Pixmap;
      Node   : in     Vis_Data.Vis_Node_Id;
      Origin : in     Vis.Absolute.Vector_2d) is

      Detail_Level : Detail_Level_Type := Settings.Get_Detail_Level (Widget);

      procedure Draw_Node_Highlighting
        (Gc        : in     Gdk.GC.Gdk_GC;
         Thickness : in     Vis.Absolute_Int;
         Rect      : in     Vis.Absolute.Rectangle_2d) is

         Top_Part    : Vis.Absolute.Rectangle_2d;
         Left_Part   : Vis.Absolute.Rectangle_2d;
         Right_Part  : Vis.Absolute.Rectangle_2d;
         Bottom_Part : Vis.Absolute.Rectangle_2d;
      begin
         pragma Assert (Thickness > 0);
         Top_Part := Combine_Rectangle
           (Top_Left     => Get_Top_Left (Rect),
            Bottom_Right => Combine_Vector
                              (X => Get_Right (Rect),
                               Y => Get_Top (Rect) + Thickness - 1));
         Bottom_Part := Combine_Rectangle
           (Top_Left     => Combine_Vector
                              (X => Get_Left (Rect),
                               Y => Get_Bottom (Rect) - Thickness + 1),
            Bottom_Right => Get_Bottom_Right (Rect));
         Left_Part := Combine_Rectangle
           (X_1 => Get_Left (Top_Part),
            Y_1 => Get_Bottom (Top_Part) + 1,
            X_2 => Get_Left (Top_Part) + Thickness - 1,
            Y_2 => Get_Top (Bottom_Part) - 1);
         Right_Part := Combine_Rectangle
           (X_1 => Get_Right (Top_Part) - Thickness + 1,
            Y_1 => Get_Bottom (Top_Part) + 1,
            X_2 => Get_Right (Top_Part),
            Y_2 => Get_Top (Bottom_Part) - 1);
         Draw_Filled
           (Drawable  => Buffer,
            Gc        => Gc,
            Rectangle => Top_Part,
            Origin    => Zero_2d);
         Draw_Filled
           (Drawable  => Buffer,
            Gc        => Gc,
            Rectangle => Left_Part,
            Origin    => Zero_2d);
         Draw_Filled
           (Drawable  => Buffer,
            Gc        => Gc,
            Rectangle => Right_Part,
            Origin    => Zero_2d);
         Draw_Filled
           (Drawable  => Buffer,
            Gc        => Gc,
            Rectangle => Bottom_Part,
            Origin    => Zero_2d);
      end Draw_Node_Highlighting;

      procedure Draw_Node_Border
        (Rect      : in     Vis.Absolute.Rectangle_2d) is

         Gc : Gdk.GC.Gdk_GC := Widget.Drawing.Node_Border;
      begin
         Gdk.GC.Set_Foreground
           (GC        => Gc,
            Color     => Settings.Get_Node_Border_Color (Widget, Node));
         Draw_Border
           (Drawable  => Buffer,
            Gc        => Gc,
            Rectangle => Rect,
            Origin    => Zero_2d);
      end Draw_Node_Border;

      procedure Draw_Node_Filling
        (Rect      : in     Vis.Absolute.Rectangle_2d) is

         Gc : Gdk.GC.Gdk_GC := Widget.Drawing.Node_Fill;
      begin
         Gdk.GC.Set_Foreground
           (GC        => Gc,
            Color     => Settings.Get_Node_Fill_Color (Widget, Node));
         Draw_Filled
           (Drawable  => Buffer,
            Gc        => Gc,
            Rectangle => Rect,
            Origin    => Zero_2d);
      end Draw_Node_Filling;

      procedure Draw_Node_Rectangle
        (Inner_Rect       :    out Vis.Absolute.Rectangle_2d) is

         type Light_Array is array
           (1 .. Vis_Data.Local_Highlight_Type'Pos
                   (Vis_Data.Local_Highlight_Type'Last) -
                 Vis_Data.Local_Highlight_Type'Pos
                   (Vis_Data.Local_Highlight_Type'First) + 1)
           of Vis_Data.Local_Highlight_Type;
         Lights          : Light_Array;
         Light_Count     : Integer;
         Light_Rect      : Vis.Absolute.Rectangle_2d;
         Light_Thickness : Vis.Absolute_Int;
         Draw_Rect       : Vis.Absolute.Rectangle_2d;
         Highlighting    : Vis_Data.Highlight_Array;
      begin
         Highlighting := Vis_Data.Get_Highlighting (Node);
         Draw_Rect := Vis_Data.Get_Extent (Node);
         Move (Draw_Rect, -Origin);

         --  draw global highlight-borders if any
         for Light in Vis_Data.Global_Highlight_Type loop
            if Highlighting (Light) then
               Draw_Node_Highlighting
                 (Widget.Drawing.Node_Light (Light),
                  Default_Node_Light_Thickness,
                  Draw_Rect);
               Shrink (Draw_Rect, Default_Node_Light_Thickness);
            end if;
         end loop;

         --  draw node border
         Draw_Node_Border (Draw_Rect);
         Shrink (Draw_Rect, 1);

         --  count highlighting
         Light_Count := Lights'First;
         for Light in Vis_Data.Local_Highlight_Type loop
            if Highlighting (Light) then
               Lights (Light_Count) := Light;
               Light_Count := Light_Count + 1;
            end if;
         end loop;
         if Light_Count - Lights'First > 0 then
            --  draw filling using local highlighting
            Light_Rect := Draw_Rect;
            --  draw equally sized borders for each color
            Light_Thickness := Vis.Absolute_Int'Min
              (Get_Width (Light_Rect), Get_Height (Light_Rect)) /
              (2 * (Light_Count - Lights'First));

            --  draw first colors
            Light_Count := Light_Count - 1;
            while Light_Count > Lights'First loop
               Draw_Node_Highlighting
                 (Widget.Drawing.Node_Light (Lights (Light_Count)),
                  Light_Thickness,
                  Light_Rect);
               Shrink (Light_Rect, Light_Thickness);
               Light_Count := Light_Count - 1;
            end loop;
            --  draw last color
            Draw_Filled
              (Drawable  => Buffer,
               Gc        => Widget.Drawing.Node_Light (Lights (Light_Count)),
               Rectangle => Light_Rect,
               Origin    => Zero_2d);
         else
            --  draw filling without highlighting
            Draw_Node_Filling (Draw_Rect);
         end if;

         Inner_Rect := Draw_Rect;
      end Draw_Node_Rectangle;

      procedure Draw_Node_Content
        (Inner_Rect : in    Vis.Absolute.Rectangle_2d) is

         Icons_Width  : Glib.Gint := 0;
         Icons_Height : Glib.Gint := 0;

         procedure Draw_Icon
           (Icon   : in     Gdk.Pixmap.Gdk_Pixmap;
            Width  : in out Glib.Gint;
            Height : in out Glib.Gint) is

            use type Glib.Gint;
            Inner_Width  : Glib.Gint := Glib.Gint (Get_Width (Inner_Rect));
            Inner_Height : Glib.Gint := Glib.Gint (Get_Height (Inner_Rect));
            Too_Much     : Glib.Gint;
            X            : Glib.Gint;
            Y            : Glib.Gint;
         begin
            if Gdk."/=" (Icon, Gdk.Pixmap.Null_Pixmap) then
               X := Glib.Gint (Get_Left (Inner_Rect)) + Icons_Width;
               Y := Glib.Gint (Get_Top (Inner_Rect));

               Icons_Width := Icons_Width + Width;
               if Icons_Height < Height then
                  Icons_Height := Height;
               end if;

               if Icons_Width > Inner_Width then
                  Too_Much := Icons_Width - Inner_Width;
                  Width := Width - Too_Much;
                  Icons_Width := Inner_Width;
               end if;
               if Icons_Height > Inner_Height then
                  Too_Much := Icons_Height - Inner_Height;
                  Height := Height - Too_Much;
                  Icons_Height := Inner_Height;
               end if;
               if Width > 0 and then Height > 0 then
                  Gdk.Drawable.Draw_Pixmap
                    (Drawable => Buffer,
                     Gc       => Widget.Drawing.Node_Text,
                     Src      => Icon,
                     Xsrc     => 0,
                     Ysrc     => 0,
                     Xdest    => X,
                     Ydest    => Y,
                     Width    => Width,
                     Height   => Height);
               end if;
            end if;
         end Draw_Icon;

         Font              : Gdk.Font.Gdk_Font :=
           Settings.Get_Node_Font (Widget);
         Icon              : Gdk.Pixmap.Gdk_Pixmap;
         Width             : Glib.Gint;
         Height            : Glib.Gint;
         Draw_Rect         : Vis.Absolute.Rectangle_2d := Inner_Rect;
         Icons_Bottom      : Vis.Absolute_Int;
         Id_Rect           : Vis.Absolute.Rectangle_2d;
         Class_Name_Rect   : Vis.Absolute.Rectangle_2d;
         Attrib_Name_Rect  : Vis.Absolute.Rectangle_2d;
         Attrib_Value_Rect : Vis.Absolute.Rectangle_2d;
         Iterator         : Graph_Lib.Node_Attribute_Filters.Filtered_Iterator;
         Attribute         : Graph_Lib.Node_Attribute_Id;
         Graph_Node        : Graph_Lib.Node_Id :=
           Vis_Data.Get_Graph_Node (Node);
         Graph_Node_Class  : Graph_Lib.Node_Class_Id :=
           Graph_Lib.Get_Node_Class_Id (Graph_Node);
         Line_Feed         : Vis.Absolute.Vector_2d :=
           Combine_Vector (0, Get_Height (Font) + Default_Text_Spacing);
      begin
         Gdk.GC.Set_Foreground
           (GC        => Widget.Drawing.Node_Text,
            Color     => Settings.Get_Node_Text_Color (Widget, Node));
         --  Icons
         Settings.Get_Node_Icon
           (Widget, Node,
            Icon, Width, Height);
         if Vis.Absolute_Natural (Width) <= Get_Width (Inner_Rect) then
            Draw_Icon (Icon, Width, Height);
         end if;
         if Vis_Data.Is_Annotated (Node) then
            Settings.Get_Annotation_Icon
              (Widget,
               Icon, Width, Height);
            Draw_Icon (Icon, Width, Height);
         end if;

         Icons_Bottom := Get_Top (Draw_Rect) + Vis.Absolute_Int'Max
           (Vis.Absolute_Int (Icons_Height),
            Default_Text_Spacing + Get_Height (Font));

         Shrink (Draw_Rect, Default_Text_Spacing);

         --  Node Id
         if Vis.Absolute_Int (Icons_Width) < Get_Width (Draw_Rect) then
            Id_Rect := Combine_Rectangle
              (X_1 => Get_Left (Draw_Rect) + Vis.Absolute_Int (Icons_Width),
               Y_1 => Get_Top (Draw_Rect),
               X_2 => Get_Right (Draw_Rect),
               Y_2 => Icons_Bottom);
            Draw_Text
              (Buffer => Buffer,
               Font   => Font,
               Gc     => Widget.Drawing.Node_Text,
               Area   => Id_Rect,
               Text   => Graph_Lib.Node_Id_Image (Graph_Node));
         end if;

         --  Class name
         Class_Name_Rect := Combine_Rectangle
           (X_1 => Get_Left (Draw_Rect),
            Y_1 => Icons_Bottom + Default_Text_Spacing,
            X_2 => Get_Right (Draw_Rect),
            Y_2 => Icons_Bottom + Default_Text_Spacing + Get_Height (Font));

         if Settings.Show_Node_Class_Name (Widget, Node) then
            Draw_Text
              (Buffer => Buffer,
               Font   => Font,
               Gc     => Widget.Drawing.Node_Text,
               Area   => Class_Name_Rect,
               Text   => Graph_Lib.Get_Node_Class_Tag (Graph_Node_Class));
         end if;

         if Detail_Level >= High then
            --  Attributes
            Attrib_Name_Rect := Combine_Rectangle
              (Top_Left     => Get_Top_Left (Class_Name_Rect),
               Bottom_Right => Get_Bottom_Center (Class_Name_Rect) -
                                 Combine_Vector (Default_Text_Spacing / 2, 0));
            Attrib_Value_Rect := Combine_Rectangle
              (Top_Left     => Get_Top_Center (Class_Name_Rect) +
                                 Combine_Vector (Default_Text_Spacing / 2, 0),
               Bottom_Right => Get_Bottom_Right (Class_Name_Rect));

            Iterator := Settings.Get_Node_Attributes (Widget, Node);
            while Graph_Lib.Node_Attribute_Filters.More (Iterator) loop
               Move (Attrib_Name_Rect, Line_Feed);
               --  pragma Assert
               --    (Get_Bottom (Attrib_Name_Rect) <= Get_Bottom (Draw_Rect));
               Move (Attrib_Value_Rect, Line_Feed);

               Graph_Lib.Node_Attribute_Filters.Next (Iterator, Attribute);

               Draw_Text
                 (Buffer => Buffer,
                  Font   => Font,
                  Gc     => Widget.Drawing.Node_Text,
                  Area   => Attrib_Name_Rect,
                  Text   => Graph_Lib.Convert_Node_Attribute_Id_To_Name
                              (Attribute));
               Draw_Text
                 (Buffer => Buffer,
                  Font   => Font,
                  Gc     => Widget.Drawing.Node_Text,
                  Area   => Attrib_Value_Rect,
                  Text   => Graph_Lib.Get_Node_Attribute_Value_As_String
                              (Graph_Node, Attribute));
            end loop;
         end if;
      end Draw_Node_Content;

      Inner_Rect : Vis.Absolute.Rectangle_2d;
   begin
      if Vis_Data.Is_Hidden (Node) then
         return;
      end if;

      Draw_Node_Rectangle (Inner_Rect);
      if Detail_Level >= Average then
         Draw_Node_Content (Inner_Rect);
      end if;
   end Draw_Node;

   procedure Update_Buffer_Edges
     (Widget   : access Graph_Widget_Record'Class;
      Clipping : in     Vis_Data.Clipping_Queue_Access;
      Edges    : in     Vis_Data.Edge_Update_Iterators.Merger_Access) is

      package Clipping_Queues renames Vis_Data.Clipping_Queues;
      package Iterators renames Vis_Data.Edge_Update_Iterators;

      Current_Clipping : Vis_Data.Layer_Clipping_Access;
      Current_Edge     : Vis_Data.Vis_Edge_Id;
      Current_Layer    : Vis_Data.Layer_Type;
      Origin           : Vis.Absolute.Vector_2d :=
        Get_Top_Left (Widget.Drawing.Buffer_Area);
   begin
      while Iterators.Has_More (Edges) loop
         Current_Edge := Iterators.Get_Current (Edges);
         Current_Layer := Vis_Data.Get_Layer (Current_Edge);

         loop
            --  open/close clip mask according to 'Current_Layer'
            exit when Clipping_Queues.Is_Empty (Clipping.all);
            Current_Clipping := Clipping_Queues.Get_Head (Clipping.all);
            exit when Vis_Data.Is_Below
              (Current_Layer, Current_Clipping.Height);
            Copy_Ready_Into_Normal_Buffer
              (Widget => Widget,
               Area   => Current_Clipping.Area,
               Origin => Origin);
            Clipping_Queues.Remove_Head (Clipping.all);
            Vis_Data.Free (Current_Clipping);
         end loop;

         Draw_Edge
           (Widget, Widget.Drawing.Buffer, Current_Edge, Origin);

         Iterators.Forward (Edges);
      end loop;
   end Update_Buffer_Edges;

   procedure Update_Buffer_Nodes
     (Widget   : access Graph_Widget_Record'Class;
      Clipping : in     Vis_Data.Clipping_Queue_Access;
      Nodes    : in     Vis_Data.Node_Update_Iterators.Merger_Access) is

      package Clipping_Queues renames Vis_Data.Clipping_Queues;
      package Iterators renames Vis_Data.Node_Update_Iterators;

      function To_Natural is new Ada.Unchecked_Conversion
        (Source => Vis_Data.Layer_Type, Target => Natural);

      Current_Clipping : Vis_Data.Layer_Clipping_Access;
      Current_Node     : Vis_Data.Vis_Node_Id;
      Current_Layer    : Vis_Data.Layer_Type;
      Origin           : Vis.Absolute.Vector_2d :=
        Get_Top_Left (Widget.Drawing.Buffer_Area);
   begin
      while Iterators.Has_More (Nodes) loop
         Current_Node := Iterators.Get_Current (Nodes);
         Current_Layer := Vis_Data.Get_Layer (Current_Node);

         loop
            --  open/close clip mask according to 'Current_Layer'
            exit when Clipping_Queues.Is_Empty (Clipping.all);
            Current_Clipping := Clipping_Queues.Get_Head (Clipping.all);
            exit when Vis_Data.Is_Below
              (Current_Layer, Current_Clipping.Height);
            Copy_Ready_Into_Normal_Buffer
              (Widget => Widget,
               Area   => Current_Clipping.Area,
               Origin => Origin);
            Clipping_Queues.Remove_Head (Clipping.all);
            Vis_Data.Free (Current_Clipping);
         end loop;

         Draw_Node
           (Widget, Widget.Drawing.Buffer, Current_Node, Origin);

         Iterators.Forward (Nodes);
      end loop;
   end Update_Buffer_Nodes;


   procedure Update_Buffer_Background
     (Widget     : access Graph_Widget_Record'Class;
      Rectangles : in     Vis_Data.Rectangle_2d_Lists.List) is

      package Rect_Lists renames Vis_Data.Rectangle_2d_Lists;
      Area          : Vis.Absolute.Rectangle_2d;
      Iterator      : Rect_Lists.ListIter;
      Buffer_Origin : Vis.Absolute.Vector_2d :=
        Get_Top_Left (Widget.Drawing.Buffer_Area);
   begin
      --  Drawing_Logger.Debug ("Update_Buffer_Background");

      Iterator := Rect_Lists.MakeListIter (Rectangles);
      while Rect_Lists.More (Iterator) loop
         Rect_Lists.Next (Iterator, Area);
         --  Drawing_Logger.Debug
         --    ("... Background rectangle: " & Vis.Absolute.Image (Area));
         Draw_Filled
           (Drawable  => Widget.Drawing.Buffer,
            Gc        => Widget.Drawing.Background,
            Rectangle => Area,
            Origin    => Buffer_Origin);
         --  Draw_Border
         --    (Drawable  => Widget.Drawing.Buffer,
         --     Gc        => Widget.Drawing.Debug_Gc,
         --     Rectangle => Area,
         --     Origin    => Buffer_Origin);
         --  Draw_Text
         --    (Buffer => Widget.Drawing.Buffer,
         --     Font   => Settings.Get_Node_Font (Widget),
         --     Gc     => Widget.Drawing.Debug_Gc,
         --     Area   => Area,
         --     Origin => Buffer_Origin,
         --     Text   => Vis.Absolute.Image (Area));
      end loop;

      --  Drawing_Logger.Debug ("... Background done.");
   end Update_Buffer_Background;

   procedure Update_Buffer_Unchanged
     (Widget     : access Graph_Widget_Record'Class;
      Rectangles : in     Vis_Data.Rectangle_2d_Lists.List) is

      package Rect_Lists renames Vis_Data.Rectangle_2d_Lists;
      Area          : Vis.Absolute.Rectangle_2d;
      Iterator      : Rect_Lists.ListIter;
      Buffer_Origin : Vis.Absolute.Vector_2d :=
        Get_Top_Left (Widget.Drawing.Buffer_Area);
   begin
      --  Drawing_Logger.Debug ("Update_Buffer_Unchanged");

      Iterator := Rect_Lists.MakeListIter (Rectangles);
      while Rect_Lists.More (Iterator) loop
         Rect_Lists.Next (Iterator, Area);
         --  Drawing_Logger.Debug
         --    ("... Unchanged rectangle: " & Vis.Absolute.Image (Area));
         Copy_Ready_Into_Normal_Buffer
           (Widget => Widget,
            Area   => Area,
            Origin => Buffer_Origin);
      end loop;

      --  Drawing_Logger.Debug ("... Unchanged done.");
   end Update_Buffer_Unchanged;


   ----------------------------------------------------------------------------
   --  Updates the 'Buffer'
   procedure Update_Buffer
     (Widget : access Graph_Widget_Record'Class) is

      Buffer_Copy : Gdk.Pixmap.Gdk_Pixmap;
      Command     : Vis_Data.Refresh_Command_Type;
   begin
      Vis_Data.Start_Refresh_Operation
        (Manager         => Widget.Manager,
         Refresh_Area    => Widget.Drawing.Buffer_Area,
         Command         => Command,
         Refresh_Pending => True);

      Update_Buffer_Background (Widget, Command.Reset);
      Update_Buffer_Edges (Widget, Command.Edge_Clipping, Command.Edges);
      Update_Buffer_Nodes (Widget, Command.Node_Clipping, Command.Nodes);
      Update_Buffer_Unchanged (Widget, Command.Unchanged);

      Vis_Data.End_Refresh_Operation (Command);

      Buffer_Copy := Widget.Drawing.Buffer;
      Widget.Drawing.Buffer := Widget.Drawing.Ready_Buffer;
      Widget.Drawing.Ready_Buffer := Buffer_Copy;
   end Update_Buffer;


   procedure Update_Display
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Absolute.Rectangle_2d) is
   begin
      --  Drawing_Logger.Debug
      --    ("Update_Display: " & Vis.Absolute.Image (Area));
      Update_Buffer (Widget);

      Gdk.Drawable.Draw_Drawable
        (Drawable => Widget.Drawing.Display,
         GC       => Widget.Drawing.Background,
         Xdest    => 0,
         Ydest    => 0,
         Src      => Widget.Drawing.Ready_Buffer,
         XSrc     => Glib.Gint (Get_Left (Widget.Drawing.Visible_Area) -
                                Get_Left (Widget.Drawing.Buffer_Area)),
         YSrc      => Glib.Gint (Get_Top (Widget.Drawing.Visible_Area) -
                                 Get_Top (Widget.Drawing.Buffer_Area)),
         Width    => Glib.Gint
                      (Vis.Absolute.Get_Width (Widget.Drawing.Visible_Area)),
         Height   => Glib.Gint
                      (Vis.Absolute.Get_Height (Widget.Drawing.Visible_Area)));
   end Update_Display;


   procedure Update_Temporary
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Absolute.Rectangle_2d) is

      Floating  : Vis_Node_Sets.Iterator;
      Node      : Vis_Data.Vis_Node_Id;
      Origin    : Vis.Absolute.Vector_2d;
      Point     : Vis.Absolute.Vector_2d;
      Rectangle : Vis.Absolute.Rectangle_2d;
   begin
      if States.Is_Drag_Current (Widget) then
         Origin := Get_Top_Left (Widget.Drawing.Visible_Area) -
           States.Get_Mouse_Move_Distance (Widget);
         Floating := Vis_Node_Sets.Make_Iterator (Get_Floating_Nodes (Widget));
         while Vis_Node_Sets.More (Floating) loop
            Vis_Node_Sets.Next (Floating, Node);
            Draw_Node
              (Widget => Widget,
               Buffer => Widget.Drawing.Display,
               Node   => Node,
               Origin => Origin);
         end loop;
         Vis_Node_Sets.Destroy (Floating);
      end if;
      if States.Is_Rectangle_Current (Widget) then
         Origin := States.Get_Click_Point (Widget);
         Point := States.Get_Mouse_Position (Widget);
         Rectangle := Combine_Rectangle
           (X_1 => Get_X (Origin),
            Y_1 => Get_Y (Origin),
            X_2 => Get_X (Point),
            Y_2 => Get_Y (Point));
         Draw_Filled
           (Drawable  => Widget.Drawing.Display,
            Gc        => Widget.Drawing.Rectangle_Gc,
            Rectangle => Rectangle,
            Origin    => Get_Top_Left (Widget.Drawing.Visible_Area));
      end if;
   end Update_Temporary;


   function Get_Visible_Area
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute.Rectangle_2d is
   begin
      return Widget.Drawing.Visible_Area;
   end Get_Visible_Area;


   ----------------------------------------------------------------------------
   --  Calculates the size of the display (desktop size + optimization)
   function Calculate_Display_Size
     (Widget        : access Graph_Widget_Record'Class)
     return Vis.Absolute.Vector_2d is

      X             : Glib.Gint;
      Y             : Glib.Gint;
      Width         : Glib.Gint;
      Height        : Glib.Gint;
      Depth         : Glib.Gint;
      Window_Width  : Glib.Gint;
      Window_Height : Glib.Gint;
      Area          : Vis.Absolute.Rectangle_2d;
   begin
      Gdk.Window.Get_Geometry
        (Gdk.Window.Null_Window, X, Y, Width, Height, Depth);
      Gdk.Window.Get_Geometry
        (Get_Window (Widget), X, Y, Window_Width, Window_Height, Depth);
      Width  := Glib.Gint'Max (Width, Window_Width);
      Height := Glib.Gint'Max (Height, Window_Height);
      Area := Vis.Absolute.Combine_Rectangle
        (X_1 => 0,
         Y_1 => 0,
         X_2 => Vis.Absolute_Int (Width) - 1,
         Y_2 => Vis.Absolute_Int (Height) - 1);
      return Get_Size (Area);
   end Calculate_Display_Size;

   procedure Move_Visible_Area_To
     (Widget : access Graph_Widget_Record'Class;
      Point  : in     Vis.Absolute.Vector_2d) is

      New_Visible      : Vis.Absolute.Rectangle_2d;
      Old_Buffer       : Vis.Absolute.Rectangle_2d;
      New_Buffer       : Vis.Absolute.Rectangle_2d;
      Intersection     : Vis.Absolute.Rectangle_2d;
      Copy_From        : Vis.Absolute.Vector_2d;
      Copy_To          : Vis.Absolute.Vector_2d;
      Old_Ready_Buffer : Gdk.Pixmap.Gdk_Pixmap;
   begin
      if not States.Can_Move (Widget) then
         return;
      end if;
      New_Visible := Widget.Drawing.Visible_Area;
      Set_Center (New_Visible, Point);
      Old_Buffer := Widget.Drawing.Buffer_Area;
      New_Buffer := New_Visible;
      Vis_Data.Optimize_Drawing_Area
        (Manager => Widget.Manager,
         Area    => New_Buffer);

      States.Changed_Visual (Widget);
      States.Visual_Area_Changed (Widget);

      --  If new buffers are created, then still need old ready buffer
      Old_Ready_Buffer := Widget.Drawing.Ready_Buffer;
      Gdk.Pixmap.Ref (Old_Ready_Buffer);
      if Get_Size (Old_Buffer) /= Get_Size (New_Buffer) then
         Reset_Buffers (Widget, New_Buffer);
      end if;

      if New_Buffer = Old_Buffer then
         null;
      elsif Get_Right (Old_Buffer) >= Get_Left (New_Buffer) and then
        Get_Right (New_Buffer) >= Get_Left (Old_Buffer) and then
        Get_Bottom (Old_Buffer) >= Get_Top (New_Buffer) and then
        Get_Bottom (New_Buffer) >= Get_Top (Old_Buffer) then

         --  Intersecting areas, reuse intersection
         Intersection := Combine_Rectangle
           (X_1 => Vis.Absolute_Int'Max (Get_Left (Old_Buffer),
                                         Get_Left (New_Buffer)),
            Y_1 => Vis.Absolute_Int'Max (Get_Top (Old_Buffer),
                                         Get_Top (New_Buffer)),
            X_2 => Vis.Absolute_Int'Min (Get_Right (Old_Buffer),
                                         Get_Right (New_Buffer)),
            Y_2 => Vis.Absolute_Int'Min (Get_Bottom (Old_Buffer),
                                         Get_Bottom (New_Buffer)));
         Copy_From := Get_Top_Left (Intersection) - Get_Top_Left (Old_Buffer);
         Copy_To := Get_Top_Left (Intersection) - Get_Top_Left (New_Buffer);

         Gdk.Drawable.Draw_Drawable
           (Drawable => Widget.Drawing.Ready_Buffer,
            GC       => Widget.Drawing.Background,
            Xdest    => Glib.Gint (Get_X (Copy_To)),
            Ydest    => Glib.Gint (Get_Y (Copy_To)),
            Src      => Old_Ready_Buffer,
            XSrc     => Glib.Gint (Get_X (Copy_From)),
            YSrc     => Glib.Gint (Get_Y (Copy_From)),
            Width    => Glib.Gint (Get_Width (Intersection)),
            Height   => Glib.Gint (Get_Height (Intersection)));

         --  Pollution for discarded space
         if Get_Top (Old_Buffer) < Get_Top (New_Buffer) then
            Vis_Data.Pollute_Area
              (Widget.Manager,
               Combine_Rectangle
                 (Top_Left     => Get_Top_Left (Old_Buffer),
                  Bottom_Right => Combine_Vector (Get_Right (Old_Buffer),
                                                  Get_Top (New_Buffer) - 1)));
         end if;
         if Get_Bottom (Old_Buffer) > Get_Bottom (New_Buffer) then
            Vis_Data.Pollute_Area
              (Widget.Manager,
               Combine_Rectangle
                 (Top_Left     => Combine_Vector (Get_Left (Old_Buffer),
                                                  Get_Bottom (New_Buffer) + 1),
                  Bottom_Right => Get_Bottom_Right (Old_Buffer)));
         end if;
         if Get_Left (Old_Buffer) < Get_Left (New_Buffer) then
            Vis_Data.Pollute_Area
              (Widget.Manager,
               Combine_Rectangle
                 (Top_Left     => Get_Top_Left (Old_Buffer),
                  Bottom_Right => Combine_Vector (Get_Left (New_Buffer) - 1,
                                                  Get_Bottom (Old_Buffer))));
         end if;
         if Get_Right (Old_Buffer) > Get_Right (New_Buffer) then
            Vis_Data.Pollute_Area
              (Widget.Manager,
               Combine_Rectangle
                 (Top_Left     => Combine_Vector (Get_Right (New_Buffer) + 1,
                                                  Get_Top (Old_Buffer)),
                  Bottom_Right => Get_Bottom_Right (Old_Buffer)));
         end if;
      else
         --  No intersection
         Vis_Data.Pollute_Area (Widget.Manager, Old_Buffer);
      end if;
      Gdk.Pixmap.Unref (Old_Ready_Buffer);
      Widget.Drawing.Buffer_Area := New_Buffer;
      Widget.Drawing.Visible_Area := New_Visible;
   end Move_Visible_Area_To;

   procedure Resize_Display
     (Widget : access Graph_Widget_Record'Class) is

      Window           : Gdk.Window.Gdk_Window := Get_Window (Widget);
      Old_Area         : Vis.Absolute.Rectangle_2d;
      Old_Size         : Vis.Absolute.Vector_2d;
      New_Area         : Vis.Absolute.Rectangle_2d;
      New_Size         : Vis.Absolute.Vector_2d;
      Old_Ready_Buffer : Gdk.Pixmap.Gdk_Pixmap;
      X                : Glib.Gint;
      Y                : Glib.Gint;
      Width            : Glib.Gint;
      Height           : Glib.Gint;
      Depth            : Glib.Gint;
   begin
      States.Changed_Visual (Widget);
      if not States.Can_Resize (Widget) then
         return;
      end if;
      Old_Area := Widget.Drawing.Buffer_Area;
      Old_Size := Get_Size (Old_Area);

      --  update display size
      Gdk.Window.Get_Geometry
        (Get_Window (Widget), X, Y, Width, Height, Depth);
      Width := Glib.Gint'Max (Width, 1);
      Height := Glib.Gint'Max (Height, 1);
      Set_Size
        (Rectangle => Widget.Drawing.Visible_Area,
         Size      => Combine_Vector (X => Vis.Absolute_Natural (Width),
                                      Y => Vis.Absolute_Natural (Height)));

      New_Area := Widget.Drawing.Visible_Area;
      Vis_Data.Optimize_Drawing_Area
        (Manager => Widget.Manager,
         Area    => New_Area);
      New_Size := Get_Size (New_Area);

      if New_Size /= Old_Size then
         Old_Ready_Buffer := Widget.Drawing.Ready_Buffer;
         Gdk.Pixmap.Ref (Old_Ready_Buffer);
         Reset_Buffers (Widget, New_Area);
         Gdk.Drawable.Draw_Drawable
           (Drawable => Widget.Drawing.Ready_Buffer,
            GC       => Widget.Drawing.Background,
            Xdest    => 0,
            Ydest    => 0,
            Src      => Old_Ready_Buffer,
            XSrc     => 0,
            YSrc     => 0,
            Width    => Glib.Gint (Vis.Absolute_Int'Min
                                   (Get_X (Old_Size), Get_X (New_Size))),
            Height   => Glib.Gint (Vis.Absolute_Int'Min
                                   (Get_Y (Old_Size), Get_Y (New_Size))));
         Gdk.Pixmap.Unref (Old_Ready_Buffer);

         --  If size of buffer is smaller in any coordinate then must
         --  add pollution. If size has increased then pollution must be
         --  set already
         if Get_X (New_Size) < Get_X (Old_Size) then
            Vis_Data.Pollute_Area
              (Widget.Manager,
               Combine_Rectangle
                 (Top_Left     => Combine_Vector
                                    (X => Get_Right (New_Area) + 1,
                                     Y => Get_Top (Old_Area)),
                  Bottom_Right => Get_Bottom_Right (Old_Area)));
         end if;
         if Get_Y (New_Size) < Get_Y (Old_Size) then
            Vis_Data.Pollute_Area
              (Widget.Manager,
               Combine_Rectangle
                 (Top_Left     => Combine_Vector
                                    (X => Get_Left (Old_Area),
                                     Y => Get_Bottom (New_Area) + 1),
                  Bottom_Right => Get_Bottom_Right (Old_Area)));
         end if;

         --  update 'Display's size
         New_Size := Calculate_Display_Size (Widget);
         Gdk.Drawable.Get_Size
           (Drawable => Widget.Drawing.Display,
            Width    => Width,
            Height   => Height);
         Old_Size := Combine_Vector
           (X => Vis.Absolute_Int (Width),
            Y => Vis.Absolute_Int (Height));
         if New_Size /= Old_Size then
            Width := Glib.Gint'Max (Width, Glib.Gint (Get_X (New_Size)));
            Height := Glib.Gint'Max (Height, Glib.Gint (Get_Y (New_Size)));
            Gdk.Pixmap.Unref (Widget.Drawing.Display);
            Gdk.Pixmap.Gdk_New
              (Pixmap => Widget.Drawing.Display,
               Window => Window,
               Width  => Width,
               Height => Height);
            Clear (Widget.Drawing.Background, Widget.Drawing.Display);
         end if;

         --  force redraw
         Queue_Draw (Widget);
      end if;
   end Resize_Display;


   procedure Pollute_Everything
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Vis_Data.Pollute_Area (Widget.Manager, Widget.Drawing.Buffer_Area);
      States.Changed_Visual (Widget);
   end Pollute_Everything;


   ----------------------------------------------------------------------------
   --  Sets up the 'Background' gc
   procedure Set_Up_Background_Gc
     (Widget : access Graph_Widget_Record'Class) is

      Window : Gdk.Window.Gdk_Window := Get_Window (Widget);
   begin
      Gdk.GC.Gdk_New (Widget.Drawing.Debug_Gc, Window);
      Gdk.GC.Set_Foreground
        (Widget.Drawing.Debug_Gc,
         Gdk.Color.Black (Get_Colormap (Widget)));
      Gdk.GC.Gdk_New (Widget.Drawing.Background, Window);
      Gdk.GC.Set_Foreground
        (Widget.Drawing.Background, Settings.Get_Background_Color (Widget));
   end Set_Up_Background_Gc;

   ----------------------------------------------------------------------------
   --  Sets up all gcs for drawing nodes
   procedure Set_Up_Node_Gcs
     (Widget : access Graph_Widget_Record'Class) is

      Window : Gdk.Window.Gdk_Window := Get_Window (Widget);
   begin
      Gdk.GC.Gdk_New (Widget.Drawing.Node_Border, Window);
      Gdk.GC.Gdk_New (Widget.Drawing.Node_Fill, Window);
      Gdk.GC.Gdk_New (Widget.Drawing.Node_Text, Window);
      Gdk.GC.Set_Font
        (Widget.Drawing.Node_Text, Settings.Get_Node_Font (Widget));

      for I in Vis_Data.Highlight_Type loop
         Gdk.GC.Gdk_New (Widget.Drawing.Node_Light (I), Window);
         Gdk.GC.Set_Foreground
           (Widget.Drawing.Node_Light (I),
            Settings.Get_Highlight_Color (Widget, I));
      end loop;
   end Set_Up_Node_Gcs;

   ----------------------------------------------------------------------------
   --  Sets up all gcs for drawing edges
   procedure Set_Up_Edge_Gcs
     (Widget : access Graph_Widget_Record'Class) is

      Window : Gdk.Window.Gdk_Window := Get_Window (Widget);
      Dashes : Glib.Guchar_Array :=
        (0 => Glib.Guchar (Default_Dash_Length),
         1 => Glib.Guchar (Default_Dash_Separation));
      Dots   : Glib.Guchar_Array :=
        (0 => Glib.Guchar (Default_Dot_Length),
         1 => Glib.Guchar (Default_Dot_Separation));
   begin
      for I in Edge_Style_Type loop
         Gdk.GC.Gdk_New (Widget.Drawing.Edge_Line (I), Window);
      end loop;
      Gdk.GC.Set_Dashes
        (Gc          => Widget.Drawing.Edge_Line (Dashed_Line),
         Dash_Offset => Glib.Gint (Dashes'First),
         Dash_List   => Dashes);
      Gdk.GC.Set_Dashes
        (Gc          => Widget.Drawing.Edge_Line (Dotted_Line),
         Dash_Offset => Glib.Gint (Dots'First),
         Dash_List   => Dots);

      Gdk.GC.Gdk_New (Widget.Drawing.Edge_Label, Window);
      Gdk.GC.Set_Font
        (Widget.Drawing.Edge_Label, Settings.Get_Edge_Font (Widget));

      for I in Vis_Data.Highlight_Type loop
         Gdk.GC.Gdk_New (Widget.Drawing.Edge_Light (I), Window);
         Gdk.GC.Set_Foreground
           (Widget.Drawing.Edge_Light (I),
            Settings.Get_Highlight_Color (Widget, I));
      end loop;
   end Set_Up_Edge_Gcs;

   ----------------------------------------------------------------------------
   --  Sets up the gc for drawing rectangles opened by a mouse action
   procedure Set_Up_Rectangle_Gc
     (Widget : access Graph_Widget_Record'Class) is

      Window : Gdk.Window.Gdk_Window := Get_Window (Widget);
   begin
      Gdk.GC.Gdk_New (Widget.Drawing.Rectangle_Gc, Window);
      Gdk.GC.Set_Foreground
        (Widget.Drawing.Rectangle_Gc, Settings.Get_Background_Color (Widget));
      Gdk.GC.Set_Function (Widget.Drawing.Rectangle_Gc, Gdk.GC.Invert);
   end Set_Up_Rectangle_Gc;


   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class) is

      Window      : Gdk.Window.Gdk_Window := Get_Window (Widget);
      Buffer_Size : Vis.Absolute.Vector_2d;
      Height      : Glib.Gint;
      Width       : Glib.Gint;
   begin
      Buffer_Size := Calculate_Display_Size (Widget);
      Vis.To_Gdk (Buffer_Size, Width, Height);
      Gdk.Pixmap.Gdk_New
        (Pixmap => Widget.Drawing.Display,
         Window => Window,
         Width  => Width,
         Height => Height);

      --  Visible area
      Gdk.Drawable.Get_Size
        (Get_Window (Widget), Width, Height);
      --  GdkAda can set a size of 0. Since our rectangle definition does
      --  not provide empty rectangles, we force the size to be non-null
      Width := Glib.Gint'Max (Width, 1);
      Height := Glib.Gint'Max (Height, 1);
      Widget.Drawing.Visible_Area := Combine_Rectangle
        (Top_Left     => Vis.Absolute.Zero_2d,
         Bottom_Right => Combine_Vector
                           (X => Vis.Absolute_Int (Width) - 1,
                            Y => Vis.Absolute_Int (Height) - 1));
      --  Buffer area, optimized for region manager
      Widget.Drawing.Buffer_Area := Widget.Drawing.Visible_Area;
      Vis_Data.Optimize_Drawing_Area
        (Manager => Widget.Manager,
         Area    => Widget.Drawing.Buffer_Area);
      --  Buffer
      Reset_Buffers (Widget, Widget.Drawing.Buffer_Area);

      Set_Up_Background_Gc (Widget);
      Set_Up_Edge_Gcs (Widget);
      Set_Up_Node_Gcs (Widget);
      Set_Up_Rectangle_Gc (Widget);

      Clear (Widget.Drawing.Background, Widget.Drawing.Display);
      Clear (Widget.Drawing.Background, Widget.Drawing.Ready_Buffer);

      --  Set Display as background pixmap
      Gdk.Window.Set_Back_Pixmap
        (Window          => Window,
         Pixmap          => Widget.Drawing.Display,
         Parent_Relative => False);
      --  Enqueue refresh
      States.Enable_Drawing (Widget);
      Queue_Draw (Widget);
   end Set_Up;

   procedure Shut_Down
     (Widget : access Graph_Widget_Record'Class) is
   begin
      States.Disable_Drawing (Widget);

      Gdk.GC.Destroy (Widget.Drawing.Debug_Gc);
      Gdk.GC.Destroy (Widget.Drawing.Background);
      Gdk.GC.Destroy (Widget.Drawing.Rectangle_Gc);

      Gdk.GC.Destroy (Widget.Drawing.Node_Border);
      Gdk.GC.Destroy (Widget.Drawing.Node_Fill);
      Gdk.GC.Destroy (Widget.Drawing.Node_Text);
      for I in Vis_Data.Highlight_Type loop
         Gdk.GC.Destroy (Widget.Drawing.Node_Light (I));
      end loop;

      for I in Edge_Style_Type loop
         Gdk.GC.Destroy (Widget.Drawing.Edge_Line (I));
      end loop;
      Gdk.GC.Destroy (Widget.Drawing.Edge_Label);
      for I in Vis_Data.Highlight_Type loop
         Gdk.GC.Destroy (Widget.Drawing.Edge_Light (I));
      end loop;

      Gdk.Bitmap.Unref (Widget.Drawing.Ready_Buffer);
      Gdk.Pixmap.Unref (Widget.Drawing.Buffer);
      Gdk.Pixmap.Unref (Widget.Drawing.Display);
   end Shut_Down;

end Giant.Graph_Widgets.Drawing;
