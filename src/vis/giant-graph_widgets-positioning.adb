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
--  $RCSfile: giant-graph_widgets-positioning.adb,v $, $Revision: 1.15 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with Giant.Graph_Widgets.Drawing;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Graph_Widgets.Positioning is


   use Vis.Absolute;
   use Vis.Logic;


   package Positioning_Logger is new Logger
     (Name => "Giant.Graph_Widgets.Positioning");


   package Trigonometry is new Ada.Numerics.Generic_Elementary_Functions
     (Float_Type => Float);


   ------------------
   -- Calculations --
   ------------------

   function Calculate_Docking_Point_Not_Vertical
     (Direction    : in     Vis.Logic.Vector_2d;
      Reversing    : in     Boolean;
      Extent       : in     Vis.Absolute.Rectangle_2d;
      Spacing      : in     Vis.Absolute_Natural;
      Displacement : in     Vis.Absolute_Int)
     return Vis.Absolute.Vector_2d is

      Sign          : Vis.Absolute_Int;
      X             : Vis.Absolute_Int;
      Y             : Vis.Absolute_Int;
      Point         : Vis.Absolute.Vector_2d;
      Gradient      : Float := Get_Y (Direction) / Get_X (Direction);
      Node_Gradient : Float := Vis.Logic_Float (Get_Height (Extent)) /
                                 Vis.Logic_Float (Get_Width (Extent));
      Angle         : Float;
   begin
      if Displacement /= 0 then
         Angle := Trigonometry.Arctan (Gradient);
         Angle := Angle + Float (Displacement) * Default_Displacement_Angle;
         if abs (Angle - 0.5 * Ada.Numerics.Pi) < 0.000_01 then
            Gradient := Float'Safe_Last;
         elsif abs (Angle - 1.5 * Ada.Numerics.Pi) < 0.000_01 then
            Gradient := Float'Safe_First;
         else
            Gradient := Trigonometry.Tan (Angle);
         end if;
      end if;

      if abs Gradient <= Node_Gradient then
         --  intersection with vertical line
         if Get_X (Direction) > 0.0 xor Reversing then
            Sign := 1;
         else
            Sign := -1;
         end if;
         X := Get_Width (Extent) / 2 + Spacing;
         Y := Vis.Absolute_Int (Float (X) * Gradient);
         Point := Vis.Absolute.Get_Center (Extent) +
           Sign * Vis.Absolute.Combine_Vector (X, Y);
      else
         --  intersection with horizontal line
         if Get_Y (Direction) > 0.0 xor Reversing then
            Sign := 1;
         else
            Sign := -1;
         end if;
         Y := Get_Height (Extent) / 2 + Spacing;
         X := Vis.Absolute_Int (Float (Y) / Gradient);
         Point := Vis.Absolute.Get_Center (Extent) +
           Sign * Vis.Absolute.Combine_Vector (X, Y);
      end if;
      return Point;
   end Calculate_Docking_Point_Not_Vertical;

   function Calculate_Docking_Point_Vertical
     (Direction    : in Vis.Logic.Vector_2d;
      Reversing    : in Boolean;
      Extent       : in Vis.Absolute.Rectangle_2d;
      Spacing      : in Vis.Absolute_Natural;
      Displacement : in Vis.Absolute_Int)
     return Vis.Absolute.Vector_2d is
   begin
      if Get_Y (Direction) < 0.0 xor Reversing then
         return Get_Top_Center (Extent) -
           Vis.Absolute.Combine_Vector
           (Displacement * Default_Displacement_Spacing, Spacing);
      else
         return Get_Bottom_Center (Extent) +
           Vis.Absolute.Combine_Vector
           (Displacement * Default_Displacement_Spacing, Spacing);
      end if;
   end Calculate_Docking_Point_Vertical;

   procedure Calculate_Edge_Circle
     (Edge    : in     Vis_Data.Vis_Edge_Id;
      Center  : in     Vis.Absolute.Vector_2d;
      Spacing : in     Vis.Absolute_Natural;
      Radius  : in     Float) is

      Point            : Vis.Absolute.Vector_2d;
      Angle            : Float;
      Spacing_Angle    : Float :=
        Trigonometry.Arcsin (Float (Spacing) / Radius);
      Angle_Range      : Float := 1.5 * Ada.Numerics.Pi - 2.0 * Spacing_Angle;
      Number_Of_Points : Natural := Vis_Data.Get_Number_Of_Points (Edge);
      Angle_Increment  : Float := Angle_Range / Float (Number_Of_Points - 1);
   begin
      Angle := -0.5 * Ada.Numerics.Pi + Spacing_Angle;
      for Num in reverse 1 .. Number_Of_Points loop
         Point := Center + Vis.Absolute.Combine_Vector
           (X => Vis.Absolute_Int (Radius * Trigonometry.Sin (Angle)),
            Y => Vis.Absolute_Int (Radius * Trigonometry.Cos (Angle)));
         Vis_Data.Set_Point (Edge, Num, Point);

         Angle := Angle + Angle_Increment;
      end loop;
   end Calculate_Edge_Circle;

   procedure Update_Loop_Position
     (Edge         : in     Vis_Data.Vis_Edge_Id;
      Dock_Spacing : in     Vis.Absolute_Natural) is

      Source         : Vis_Data.Vis_Node_Id := Vis_Data.Get_Source (Edge);
      Extent         : Vis.Absolute.Rectangle_2d :=
        Vis_Data.Get_Extent (Source);
      Maximum_Radius : Vis.Absolute_Natural;
      Radius         : Float;
      Edge_Iterator  : Vis_Data.Vis_Edge_Lists.ListIter;
      Current_Edge   : Vis_Data.Vis_Edge_Id;
   begin
      Maximum_Radius := Vis.Absolute_Natural'Min
        (Vis.Absolute.Get_Height (Extent),
         Vis.Absolute.Get_Width (Extent));
      Radius := Float'Min (Default_Loop_Radius, Float (Maximum_Radius));
      Vis_Data.Make_Incoming_Iterator
        (Node           => Source,
         Incoming_Edges => Edge_Iterator);
      loop
         pragma Assert (Vis_Data.Vis_Edge_Lists.More (Edge_Iterator));
         Vis_Data.Vis_Edge_Lists.Next (Edge_Iterator, Current_Edge);
         exit when Vis_Data."=" (Current_Edge, Edge);
         if Vis_Data.Is_Loop (Current_Edge) then
            Radius := Radius + Default_Loop_Separation +
              Float (Vis_Data.Get_Thickness (Current_Edge));
         end if;
      end loop;

      Calculate_Edge_Circle
        (Edge    => Edge,
         Center  => Get_Bottom_Right (Vis_Data.Get_Extent (Source)),
         Spacing => Default_Dock_Spacing,
         Radius  => Radius);
   end Update_Loop_Position;

   function Calculate_Source_Displacement
     (Edge   : in     Vis_Data.Vis_Edge_Id)
     return Vis.Absolute_Int is

      Iterator : Vis_Edge_Lists.ListIter;
      Current  : Vis_Data.Vis_Edge_Id;
      Source   : Vis_Data.Vis_Node_Id := Vis_Data.Get_Source (Edge);
      Target   : Vis_Data.Vis_Node_Id := Vis_Data.Get_Target (Edge);
      Count    : Natural := 0;
      Offset   : Natural := 0;
   begin
      --  Outgoing edges towards 'Target'
      Vis_Data.Make_Outgoing_Iterator (Source, Iterator);
      while Vis_Data.Vis_Edge_Lists.More (Iterator) loop
         Vis_Data.Vis_Edge_Lists.Next (Iterator, Current);
         pragma Assert (Vis_Data."=" (Vis_Data.Get_Source (Current), Source));
         if Vis_Data."=" (Vis_Data.Get_Target (Current), Target) then
            Count := Count + 1;
         end if;
         if Vis_Data."=" (Current, Edge) then
            Offset := Count;
         end if;
      end loop;

      --  Incoming edges coming from 'Target'
      Vis_Data.Make_Incoming_Iterator (Source, Iterator);
      while Vis_Data.Vis_Edge_Lists.More (Iterator) loop
         Vis_Data.Vis_Edge_Lists.Next (Iterator, Current);
         pragma Assert (Vis_Data."=" (Vis_Data.Get_Target (Current), Source));
         if Vis_Data."=" (Vis_Data.Get_Source (Current), Target) then
            Count := Count + 1;
         end if;
      end loop;

      return Offset - (Count + 1) / 2;
   end Calculate_Source_Displacement;

   function Calculate_Target_Displacement
     (Edge   : in     Vis_Data.Vis_Edge_Id)
     return Vis.Absolute_Int is

      Iterator : Vis_Edge_Lists.ListIter;
      Current  : Vis_Data.Vis_Edge_Id;
      Source   : Vis_Data.Vis_Node_Id := Vis_Data.Get_Source (Edge);
      Target   : Vis_Data.Vis_Node_Id := Vis_Data.Get_Target (Edge);
      Count    : Natural := 0;
      Offset   : Natural := 0;
   begin
      --  Outgoing edges towards 'Target'
      Vis_Data.Make_Outgoing_Iterator (Source, Iterator);
      while Vis_Data.Vis_Edge_Lists.More (Iterator) loop
         Vis_Data.Vis_Edge_Lists.Next (Iterator, Current);
         pragma Assert (Vis_Data."=" (Vis_Data.Get_Source (Current), Source));
         if Vis_Data."=" (Vis_Data.Get_Target (Current), Target) then
            Count := Count + 1;
         end if;
         if Vis_Data."=" (Current, Edge) then
            Offset := Count;
         end if;
      end loop;

      --  Incoming edges coming from 'Target'
      Vis_Data.Make_Incoming_Iterator (Source, Iterator);
      while Vis_Data.Vis_Edge_Lists.More (Iterator) loop
         Vis_Data.Vis_Edge_Lists.Next (Iterator, Current);
         pragma Assert (Vis_Data."=" (Vis_Data.Get_Target (Current), Source));
         if Vis_Data."=" (Vis_Data.Get_Source (Current), Target) then
            Count := Count + 1;
         end if;
      end loop;

      return (Count - Offset + 1) - (Count + 1) / 2;
   end Calculate_Target_Displacement;


   ----------------
   -- Life cycle --
   ----------------

   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class;
      Zoom   : in     Vis.Zoom_Level) is
   begin
      Set_Zoom (Widget, Zoom);
   end Set_Up;

   procedure Shut_Down
     (Widget : access Graph_Widget_Record'Class) is
   begin
      --  nothing to be done.
      null;
   end Shut_Down;

   procedure Set_Zoom
     (Widget : access Graph_Widget_Record'Class;
      Zoom   : in     Vis.Zoom_Level) is
   begin
      Widget.Positioning.Transformation := Vis.Get_Transformation
        (Origin => Vis.Logic.Zero_2d,
         Zoom   => Zoom);
   end Set_Zoom;

   function Get_Zoom
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Zoom_Level is
   begin
      return Vis.Get_Zoom_Level (Widget.Positioning.Transformation);
   end Get_Zoom;


   ---------------------
   -- Transformations --
   ---------------------

   function Get_Absolute
     (Widget : access Graph_Widget_Record'Class;
      Point  : in     Vis.Logic.Vector_2d)
     return Vis.Absolute.Vector_2d is
   begin
      return Vis.Transform (Widget.Positioning.Transformation, Point);
   end Get_Absolute;

   function Get_Logic
     (Widget : access Graph_Widget_Record'Class;
      Size   : in     Vis.Absolute_Int)
     return Vis.Logic_Float is
   begin
      return Vis.Transform_Backward (Widget.Positioning.Transformation, Size);
   end Get_Logic;

   function Get_Logic
     (Widget : access Graph_Widget_Record'Class;
      Point  : in     Vis.Absolute.Vector_2d)
     return Vis.Logic.Vector_2d is
   begin
      return Vis.Transform_Backward (Widget.Positioning.Transformation, Point);
   end Get_Logic;

   function Get_Logic
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Absolute.Rectangle_2d)
     return Vis.Logic.Rectangle_2d is
   begin
      return Vis.Transform_Backward (Widget.Positioning.Transformation, Area);
   end Get_Logic;


   -----------------
   -- Positioning --
   -----------------

   procedure Update_Node_Position
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id) is

      Source : Vis.Absolute.Vector_2d;
      Target : Vis.Absolute.Vector_2d;
   begin
      Source := Drawing.Get_Node_Border_Top_Center (Widget, Node);
      Target := Vis.Transform
        (Point          => Vis_Data.Get_Position (Node),
         Transformation => Widget.Positioning.Transformation);
      Vis_Data.Move_Node (Node, Target - Source);
   end Update_Node_Position;

   procedure Update_Text_Area_Position
     (Edge   : in     Vis_Data.Vis_Edge_Id) is

      Position      : Vis.Absolute.Vector_2d;
      X             : Vis.Logic_Float;
      Y             : Vis.Logic_Float;
      Multiple_1    : Vis.Logic_Float;
      Multiple_2    : Vis.Logic_Float;
      Start_Point   : Vis.Absolute.Vector_2d :=
        Vis_Data.Get_Point (Edge, 1);
      Direction     : Vis.Absolute.Vector_2d :=
        Vis_Data.Get_Point (Edge, 2) - Start_Point;
      Text_Distance : Vis.Logic.Vector_2d :=
        Vis.To_Logic (Direction);
   begin
      X := abs Get_X (Text_Distance);
      if X /= 0.0 then
         Multiple_1 := Vis.Logic_Float'Min
           (X / 2.0, Default_Edge_Label_Distance) / X;
      else
         Multiple_1 := 0.0;
      end if;
      Y := abs Get_Y (Text_Distance);
      if Y /= 0.0 then
         Multiple_2 := Vis.Logic_Float'Min
           (Y / 2.0, Default_Edge_Label_Distance) / Y;
      else
         Multiple_2 := 0.0;
      end if;
      if Multiple_1 = 0.0 then
         Text_Distance := Multiple_2 * Text_Distance;
      elsif Multiple_2 = 0.0 then
         Text_Distance := Multiple_1 * Text_Distance;
      else
         Text_Distance := Vis.Logic_Float'Min (Multiple_1, Multiple_2) *
           Text_Distance;
      end if;

      Position := Start_Point + Vis.To_Absolute (Text_Distance);
      Vis_Data.Move_Text_Area_To
        (Edge       => Edge,
         Position   => Position,
         Align_Left => Get_X (Direction) >= 0,
         Align_Top  => Get_Y (Direction) <= 0);
   end Update_Text_Area_Position;

   procedure Update_Edge_Position
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id) is

      --  Dock_Spacing  : Vis.Absolute_Natural :=
      --    Default_Dock_Spacing + (Vis_Data.Get_Thickness (Edge) + 1) / 2;
      Source        : Vis_Data.Vis_Node_Id;
      Target        : Vis_Data.Vis_Node_Id;
      Source_Extent : Vis.Absolute.Rectangle_2d;
      Target_Extent : Vis.Absolute.Rectangle_2d;
      Source_Center : Vis.Logic.Vector_2d;
      Target_Center : Vis.Logic.Vector_2d;
      Direction     : Vis.Logic.Vector_2d;
      Start_Point   : Vis.Absolute.Vector_2d;
      End_Point     : Vis.Absolute.Vector_2d;
   begin
      if Vis_Data.Is_Loop (Edge) then
         Update_Loop_Position (Edge, Default_Dock_Spacing);
      else
         Source := Vis_Data.Get_Source (Edge);
         Target := Vis_Data.Get_Target (Edge);
         Source_Extent := Vis_Data.Get_Extent (Source);
         Target_Extent := Vis_Data.Get_Extent (Target);
         Source_Center := Vis.Logic.Get_Center (Vis.To_Logic (Source_Extent));
         Target_Center := Vis.Logic.Get_Center (Vis.To_Logic (Target_Extent));
         Direction := Target_Center - Source_Center;

         if Get_X (Direction) /= 0.0 then
            Start_Point := Calculate_Docking_Point_Not_Vertical
              (Direction    => Direction,
               Reversing    => False,
               Extent       => Source_Extent,
               Spacing      => Default_Dock_Spacing,
               Displacement => Calculate_Source_Displacement (Edge));

            End_Point := Calculate_Docking_Point_Not_Vertical
              (Direction    => Direction,
               Reversing    => True,
               Extent       => Target_Extent,
               Spacing      => Default_Dock_Spacing,
               Displacement => Calculate_Target_Displacement (Edge));
         else
            Start_Point := Calculate_Docking_Point_Vertical
              (Direction    => Direction,
               Reversing    => False,
               Extent       => Source_Extent,
               Spacing      => Default_Dock_Spacing,
               Displacement => Calculate_Source_Displacement (Edge));

            End_Point := Calculate_Docking_Point_Vertical
              (Direction    => Direction,
               Reversing    => True,
               Extent       => Target_Extent,
               Spacing      => Default_Dock_Spacing,
               Displacement => Calculate_Target_Displacement (Edge));
         end if;

         Vis_Data.Set_Point
           (Edge  => Edge,
            Num   => 1,
            Point => Start_Point);
         Vis_Data.Set_Point
           (Edge  => Edge,
            Num   => Vis_Data.Get_Number_Of_Points (Edge),
            Point => End_Point);
      end if;
      if Vis_Data.Has_Text_Area (Edge) then
         Update_Text_Area_Position (Edge);
      end if;
   end Update_Edge_Position;

   procedure Adjust_Arrow
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id) is

      Point_Count  : Natural := Vis_Data.Get_Number_Of_Points (Edge);
      Source_Point : Vis.Absolute.Vector_2d :=
        Vis_Data.Get_Point (Edge, Point_Count - 1);
      Target_Point : Vis.Absolute.Vector_2d :=
        Vis_Data.Get_Point (Edge, Point_Count);
      Distance     : Vis.Absolute.Vector_2d := Target_Point - Source_Point;
      Angle        : Float;
      Left_Angle   : Float;
      Right_Angle  : Float;
      Left_Line    : Vis.Logic.Vector_2d;
      Right_Line   : Vis.Logic.Vector_2d;
   begin
      if Distance = Vis.Absolute.Zero_2d then
         Vis_Data.Set_Left_Arrow_Point (Edge, Target_Point);
         Vis_Data.Set_Right_Arrow_Point (Edge, Target_Point);
      else
         Angle := Trigonometry.Arctan
           (Float (Get_Y (Distance)), Float (Get_X (Distance)));
         Left_Angle := Angle + Default_Edge_Arrow_Angle;
         Right_Angle := Angle - Default_Edge_Arrow_Angle;

         Left_Line := Vis.Logic.Combine_Vector
           (X => Default_Edge_Arrow_Length * Trigonometry.Cos (Left_Angle),
            Y => Default_Edge_Arrow_Length * Trigonometry.Sin (Left_Angle));
         Right_Line := Vis.Logic.Combine_Vector
           (X => Default_Edge_Arrow_Length * Trigonometry.Cos (Right_Angle),
            Y => Default_Edge_Arrow_Length * Trigonometry.Sin (Right_Angle));

         Vis_Data.Set_Left_Arrow_Point
           (Edge,
            Target_Point - Vis.To_Absolute (Left_Line));
         Vis_Data.Set_Right_Arrow_Point
           (Edge,
            Target_Point - Vis.To_Absolute (Right_Line));
      end if;
   end Adjust_Arrow;

end Giant.Graph_Widgets.Positioning;
