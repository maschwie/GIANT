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
--  $RCSfile: giant-vis.adb,v $, $Revision: 1.19 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Vis is

   use type Absolute.Vector_2d;
   use type Logic.Vector_2d;

   function To_Logic_Float
     (A : in Natural)
     return Logic_Float is
   begin
      return Logic_Float (A);
   end To_Logic_Float;

   function Logic_Float_Image
     (A : in Logic_Float)
     return String is

      Print_Minus    : Boolean     := A < 0.0;
      Positive_Value : Logic_Float := abs A;
      Trunc          : Logic_Float := Logic_Float'Truncation (Positive_Value);
      More           : Logic_Float := Logic_Float'Truncation
                                        ((Positive_Value - Trunc) * 1_000.0);
      Fore_Int       : Integer;
      Aft_Int        : Integer;
   begin
      if Logic_Float (Integer'Last) < Trunc then

         if Print_Minus then
            return "-???";
         else
            return "+???";
         end if;
      else
         Fore_Int := Integer (Trunc);
         Aft_Int  := Integer (More);
         declare
            Fore      : String          := Integer'Image (Fore_Int);
            Aft_Image : String          := Integer'Image (Aft_Int);
            Aft       : String (1 .. 3) := (others => '0');
         begin
            Aft (Aft'Last - Aft_Image'Length + 2 .. Aft'Last) :=
              Aft_Image (Aft_Image'First + 1 .. Aft_Image'Last);
            if Print_Minus then
               return "-" & Fore (Fore'First + 1 .. Fore'Last) & "." & Aft;
            else
               return Fore (Fore'First + 1 .. Fore'Last) & "." & Aft;
            end if;
         end;
      end if;
   end Logic_Float_Image;

   procedure Read_Logic_Float
     (Stream     : in     Bauhaus_IO.In_Stream_Type;
      Coordinate :    out Logic_Float) is
   begin
      Bauhaus_IO.Read_Float (Stream, Coordinate);
   end Read_Logic_Float;

   procedure Write_Logic_Float
     (Stream     : in     Bauhaus_IO.Out_Stream_Type;
      Coordinate : in     Logic_Float) is
   begin
      Bauhaus_IO.Write_Float (Stream, Coordinate);
   end Write_Logic_Float;


   function To_Absolute_Int
     (A : in Natural)
     return Absolute_Int is
   begin
      return A;
   end To_Absolute_Int;

   procedure Read_Absolute_Int
     (Stream     : in     Bauhaus_IO.In_Stream_Type;
      Coordinate :    out Absolute_Int) is
   begin
      Bauhaus_IO.Read_Integer (Stream, Coordinate);
   end Read_Absolute_Int;

   procedure Write_Absolute_Int
     (Stream     : in     Bauhaus_IO.Out_Stream_Type;
      Coordinate : in     Absolute_Int) is
   begin
      Bauhaus_IO.Write_Integer (Stream, Coordinate);
   end Write_Absolute_Int;


   procedure Write_Zoom_Level
     (Stream : in     Bauhaus_IO.Out_Stream_Type;
      Level  : in     Zoom_Level) is
   begin
      Bauhaus_IO.Write_Float (Stream, Level);
   end Write_Zoom_Level;

   procedure Read_Zoom_Level
     (Stream : in     Bauhaus_IO.In_Stream_Type;
      Level  :    out Zoom_Level) is
   begin
      Bauhaus_IO.Read_Float (Stream, Level);
   end Read_Zoom_Level;


   function To_Absolute
     (Vector : in     Logic.Vector_2d)
     return Absolute.Vector_2d is
   begin
      return Absolute.Combine_Vector
        (Absolute_Int (Logic.Get_X (Vector)),
         Absolute_Int (Logic.Get_Y (Vector)));
   end To_Absolute;

   function To_Absolute
     (Rectangle : in     Logic.Rectangle_2d)
     return Absolute.Rectangle_2d is
   begin
      return Absolute.Combine_Rectangle
        (Top_Left     => To_Absolute (Logic.Get_Top_Left (Rectangle)),
         Bottom_Right => To_Absolute (Logic.Get_Bottom_Right (Rectangle)));
   end To_Absolute;

   function To_Logic
     (Vector : in     Absolute.Vector_2d)
     return Logic.Vector_2d is
   begin
      return Logic.Combine_Vector
        (Logic_Float (Absolute.Get_X (Vector)),
         Logic_Float (Absolute.Get_Y (Vector)));
   end To_Logic;

   function To_Logic
     (Rectangle : in     Absolute.Rectangle_2d)
     return Logic.Rectangle_2d is
   begin
      return Logic.Combine_Rectangle
        (Top_Left     => To_Logic (Absolute.Get_Top_Left (Rectangle)),
         Bottom_Right => To_Logic (Absolute.Get_Top_Left (Rectangle)));
   end To_Logic;

   procedure To_Gdk
     (Vector : in     Absolute.Vector_2d;
      X      :    out Glib.Gint;
      Y      :    out Glib.Gint) is
   begin
      X := Glib.Gint (Absolute.Get_X (Vector));
      Y := Glib.Gint (Absolute.Get_Y (Vector));
   end To_Gdk;

   function Intersects_Line_Horizontal_Line_X
     (Origin         : in     Logic.Vector_2d;
      Direction      : in     Logic.Vector_2d;
      Horizontal     : in     Logic_Float)
     return Logic_Float is
   begin
      return ((Horizontal - Logic.Get_Y (Origin)) / Logic.Get_Y (Direction)) *
              Logic.Get_X (Direction) + Logic.Get_X (Origin);
   end Intersects_Line_Horizontal_Line_X;

   function Intersects_Line_Vertical_Line_Y
     (Origin         : in     Logic.Vector_2d;
      Direction      : in     Logic.Vector_2d;
      Vertical       : in     Logic_Float)
     return Logic_Float is
   begin
      return Logic.Get_Y (Direction) * (Vertical - Logic.Get_X (Origin)) /
        Logic.Get_X (Direction) + Logic.Get_Y (Origin);
   end Intersects_Line_Vertical_Line_Y;

   function Intersects_Line_Rectangle
     (From           : in     Absolute.Vector_2d;
      To             : in     Absolute.Vector_2d;
      Rectangle      : in     Absolute.Rectangle_2d)
     return Boolean is

      use Absolute;
      Left      : Vis.Absolute_Int;
      Right     : Vis.Absolute_Int;
      Top       : Vis.Absolute_Int;
      Bottom    : Vis.Absolute_Int;

      function Intersects_Horizontal
        (Horizontal : in     Absolute_Int)
        return Boolean is

         X         : Logic_Float;
         Direction : Absolute.Vector_2d := To - From;
      begin
         if Get_X (Direction) = 0 then
            X := Logic_Float (Get_X (From));
         else
            X := Intersects_Line_Horizontal_Line_X
              (Origin     => To_Logic (From),
               Direction  => To_Logic (Direction),
               Horizontal => Logic_Float (Horizontal));
         end if;
         return Logic_Float (Left) <= X and then
           X <= Logic_Float (Right);
      end Intersects_Horizontal;

      function Intersects_Vertical
        (Vertical : in     Absolute_Int)
        return Boolean is

         Y         : Logic_Float;
         Direction : Absolute.Vector_2d := To - From;
      begin
         if Get_Y (Direction) = 0 then
            Y := Logic_Float (Get_Y (From));
         else
            Y := Intersects_Line_Vertical_Line_Y
              (Origin    => To_Logic (From),
               Direction => To_Logic (Direction),
               Vertical  => Logic_Float (Vertical));
         end if;
         return Logic_Float (Top) <= Y and then
           Y <= Logic_Float (Bottom);
      end Intersects_Vertical;

   begin
      if Is_Inside (Rectangle, From) or else Is_Inside (Rectangle, To) then
         return True;
      else
         Left := Get_Left (Rectangle);
         Right := Get_Right (Rectangle);
         Top := Get_Top (Rectangle);
         Bottom := Get_Bottom (Rectangle);

         if Get_X (From) < Left then
            if Get_X (To) < Left then
               return False;
            elsif Intersects_Vertical (Left) then
               return True;
            end if;
         elsif Get_X (From) > Right then
            if Get_X (To) > Right then
               return False;
            elsif Intersects_Vertical (Right) then
               return True;
            end if;
         end if;

         if Get_Y (From) < Top then
            if Get_Y (To) < Top then
               return False;
            elsif Intersects_Horizontal (Top) then
               return True;
            end if;
         elsif Get_Y (From) > Bottom then
            if Get_Y (To) > Bottom  then
               return False;
            elsif Intersects_Horizontal (Bottom) then
               return True;
            end if;
         end if;

         return False;
      end if;
   end Intersects_Line_Rectangle;

   procedure Clip_Line_To_Rectangle
     (From      : in out Absolute.Vector_2d;
      To        : in out Absolute.Vector_2d;
      Rectangle : in     Absolute.Rectangle_2d;
      Intersect :    out Boolean) is

      use Absolute;
      use Logic;

      function Calculate_Point_Inside_Out
        (Origin     : in     Absolute.Vector_2d;
         Direction  : in     Absolute.Vector_2d;
         Horizontal : in     Absolute_Int;
         Vertical   : in     Absolute_Int)
        return Absolute.Vector_2d is

         Float_Direction    : Logic.Vector_2d;
         Horizontal_Way     : Logic_Float;
         Vertical_Way       : Logic_Float;
         Direction_Gradient : Logic_Float;
         Way_Gradient       : Logic_Float;
         Factor             : Logic_Float;
      begin
         if Get_X (Direction) = 0 then
            return Absolute.Combine_Vector
              (X => Get_X (Origin), Y => Horizontal);
         elsif Get_Y (Direction) = 0 then
            return Absolute.Combine_Vector
              (X => Vertical, Y => Get_Y (Origin));
         else
            Float_Direction := To_Logic (Direction);
            Direction_Gradient := Get_Y (Float_Direction) /
              Get_X (Float_Direction);
            Horizontal_Way := Logic_Float (Vertical - Get_X (Origin));
            Vertical_Way := Logic_Float (Horizontal - Get_Y (Origin));
            Way_Gradient := Vertical_Way / Horizontal_Way;

            if abs Way_Gradient <= abs Direction_Gradient then
               Factor := Vertical_Way /
                 Get_Y (Float_Direction);
            else
               Factor := Horizontal_Way /
                 Get_X (Float_Direction);
            end if;
            return Origin + To_Absolute (Factor * Float_Direction);
         end if;
      end Calculate_Point_Inside_Out;

      Old_From : Absolute.Vector_2d := From;
      Old_To   : Absolute.Vector_2d := To;
      Left            : Logic_Float;
      Right           : Logic_Float;
      Top             : Logic_Float;
      Bottom          : Logic_Float;
      From_Inside     : Boolean;
      To_Inside       : Boolean;
      Inside_Point    : Absolute.Vector_2d;
      Outside_Point   : Absolute.Vector_2d;
      Direction       : Absolute.Vector_2d;
      Vertical        : Absolute_Int;
      Horizontal      : Absolute_Int;
      X               : Logic_Float;
      Y               : Logic_Float;
      Left_Point      : Logic.Vector_2d;
      Right_Point     : Logic.Vector_2d;
      Origin          : Absolute.Vector_2d;
      Float_Direction : Logic.Vector_2d;
   begin
      From_Inside := Is_Inside (Rectangle, From);
      To_Inside := Is_Inside (Rectangle, To);
      if From_Inside xor To_Inside then
         --  exactly one point inside rectangle
         if To_Inside then
            Inside_Point := To;
            Outside_Point := From;
         else
            Inside_Point := From;
            Outside_Point := To;
         end if;
         Direction := Outside_Point - Inside_Point;
         if Get_X (Direction) >= 0 then
            Vertical := Get_Right (Rectangle);
         else
            Vertical := Get_Left (Rectangle);
         end if;
         if Get_Y (Direction) <= 0 then
            Horizontal := Get_Top (Rectangle);
         else
            Horizontal := Get_Bottom (Rectangle);
         end if;
         From := Inside_Point;
         To := Calculate_Point_Inside_Out
           (Origin     => Inside_Point,
            Direction  => Direction,
            Horizontal => Horizontal,
            Vertical   => Vertical);
         Intersect := True;
      elsif To_Inside then
         --  both points inside, do not need to clip
         Intersect := True;
      else
         --  no point inside
         Left := Logic_Float (Get_Left (Rectangle));
         Right := Logic_Float (Get_Right (Rectangle));
         Top := Logic_Float (Get_Top (Rectangle));
         Bottom := Logic_Float (Get_Bottom (Rectangle));
         Intersect := False;
         if Get_X (From) <= Get_X (To) then
            Left_Point := To_Logic (From);
            Right_Point := To_Logic (To);
            Direction := To - From;
            Origin := From;
         else
            Left_Point := To_Logic (To);
            Right_Point := To_Logic (From);
            Direction := From - To;
            Origin := To;
         end if;
         Float_Direction := Right_Point - Left_Point;
         if Get_X (Float_Direction) > 0.0 and then
           Get_X (Left_Point) < Left then

            Y := Intersects_Line_Vertical_Line_Y
              (Origin    => Left_Point,
               Direction => Float_Direction,
               Vertical  => Left);
            Intersect := Top <= Y and Y <= Bottom;
            if Intersect then
               From := Absolute.Combine_Vector
                 (Get_Left (Rectangle), Absolute_Int (Y));
               if Get_Y (Direction) >= 0 then
                  Horizontal := Get_Bottom (Rectangle);
               else
                  Horizontal := Get_Top (Rectangle);
               end if;
            end if;
         end if;
         if not Intersect then
            if Get_Y (Float_Direction) > 0.0 and then
              Get_Y (Left_Point) < Top then

               X := Intersects_Line_Horizontal_Line_X
                 (Origin     => Left_Point,
                  Direction  => Float_Direction,
                  Horizontal => Top);
               Intersect := Left <= X and X <= Right;
               if Intersect then
                  From := Absolute.Combine_Vector
                    (Absolute_Int (X), Get_Top (Rectangle));
                  Horizontal := Get_Bottom (Rectangle);
               end if;
            elsif Get_Y (Float_Direction) < 0.0 and then
              Get_Y (Left_Point) > Bottom then

               X := Intersects_Line_Horizontal_Line_X
                 (Origin     => Left_Point,
                  Direction  => Float_Direction,
                  Horizontal => Bottom);
               Intersect := Left <= X and X <= Right;
               if Intersect then
                  From := Absolute.Combine_Vector
                    (Absolute_Int (X), Get_Bottom (Rectangle));
                  Horizontal := Get_Top (Rectangle);
               end if;
            end if;
         end if;

         if Intersect then
            To := Calculate_Point_Inside_Out
              (Origin     => Origin,
               Direction  => Direction,
               Horizontal => Horizontal,
               Vertical   => Get_Right (Rectangle));
         end if;
      end if;
   end Clip_Line_To_Rectangle;

   function Transform
     (Size           : in     Logic_Float;
      Zoom           : in     Zoom_Level)
     return Absolute_Int is
   begin
      return Absolute_Int (Zoom * Size);
   end Transform;

   function Transform_Backward
     (Size           : in     Absolute_Int;
      Zoom           : in     Zoom_Level)
     return Logic_Float is
   begin
      if Zoom > 0.0 then
         return Logic_Float (Size) / Zoom;
      else
         return 0.0;
      end if;
   end Transform_Backward;

   function Transform
     (Point          : in     Logic.Vector_2d;
      Origin         : in     Logic.Vector_2d;
      Zoom           : in     Zoom_Level)
     return Absolute.Vector_2d is
   begin
      return To_Absolute (Zoom * (Point + Origin));
   end Transform;

   function Transform_Backward
     (Point          : in     Absolute.Vector_2d;
      Origin         : in     Logic.Vector_2d;
      Zoom           : in     Zoom_Level)
     return Logic.Vector_2d is
   begin
      if Zoom > 0.0 then
         return To_Logic (Point) / Zoom - Origin;
      else
         return Logic.Zero_2d;
      end if;
   end Transform_Backward;

   function Transform
     (Transformation : in     Transformation_Type;
      Size           : in     Logic_Float)
     return Absolute_Int is
   begin
      return Transform (Size, Transformation.Zoom);
   end Transform;

   function Transform_Backward
     (Transformation : in     Transformation_Type;
      Size           : in     Absolute_Int)
     return Logic_Float is
   begin
      return Transform_Backward (Size, Transformation.Zoom);
   end Transform_Backward;

   function Transform
     (Transformation : in     Transformation_Type;
      Point          : in     Logic.Vector_2d)
     return Absolute.Vector_2d is
   begin
      return Transform (Point, Transformation.Origin, Transformation.Zoom);
   end Transform;

   function Transform_Backward
     (Transformation : in     Transformation_Type;
      Point          : in     Absolute.Vector_2d)
     return Logic.Vector_2d is
   begin
      return Transform_Backward
        (Point, Transformation.Origin, Transformation.Zoom);
   end Transform_Backward;

   function Transform
     (Transformation : in     Transformation_Type;
      Source_Rect    : in     Logic.Rectangle_2d)
     return Absolute.Rectangle_2d is
   begin
      return Absolute.Combine_Rectangle
        (Top_Left     => Transform (Transformation,
                                    Logic.Get_Top_Left (Source_Rect)),
         Bottom_Right => Transform (Transformation,
                                    Logic.Get_Bottom_Right (Source_Rect)));
   end Transform;

   function Transform_Backward
     (Transformation : in     Transformation_Type;
      Source_Rect    : in     Absolute.Rectangle_2d)
     return Logic.Rectangle_2d is
   begin
      return Logic.Combine_Rectangle
        (Top_Left     => Transform_Backward
                            (Transformation,
                             Absolute.Get_Top_Left (Source_Rect)),
         Bottom_Right => Transform_Backward
                            (Transformation,
                             Absolute.Get_Bottom_Right (Source_Rect)));
   end Transform_Backward;

   procedure Transform_To_Gdk
     (Point          : in     Logic.Vector_2d;
      Transformation : in     Transformation_Type;
      X              :    out Glib.Gint;
      Y              :    out Glib.Gint) is

      Result_Point : Absolute.Vector_2d;
   begin
      Result_Point := Transform (Transformation, Point);
      X := Glib.Gint (Absolute.Get_X (Result_Point));
      Y := Glib.Gint (Absolute.Get_Y (Result_Point));
   end Transform_To_Gdk;

   function Get_Transformation
     (Origin         : in     Logic.Vector_2d;
      Zoom           : in     Zoom_Level)
     return Transformation_Type is
   begin
      return (Origin, Zoom);
   end Get_Transformation;

   function Get_Transformation_Rect_Into_Rect_Centered
     (Source         : in     Logic.Rectangle_2d;
      Target         : in     Absolute.Rectangle_2d)
     return Transformation_Type is

      Source_Width  : Logic_Float := Logic.Get_Width (Source);
      Source_Height : Logic_Float := Logic.Get_Height (Source);
      Target_Width  : Logic_Float :=
        Logic_Float (Absolute.Get_Width (Target));
      Target_Height : Logic_Float :=
        Logic_Float (Absolute.Get_Height (Target));

      Origin        : Logic.Vector_2d;
      X_Zoom        : Zoom_Level;
      Y_Zoom        : Zoom_Level;
      Zoom          : Zoom_Level;
   begin
      if Source_Width <= 0.0 or Source_Height <= 0.0 then
         return (Origin => Logic.Zero_2d, Zoom => 0.0);
      else
         X_Zoom := Zoom_Level (Target_Width / Source_Width);
         Y_Zoom := Zoom_Level (Target_Height / Source_Height);
         if X_Zoom <= 0.0 then
            Zoom := Y_Zoom;
         elsif Y_Zoom <= 0.0 then
            Zoom := X_Zoom;
         else
            Zoom := Zoom_Level'Min (X_Zoom, Y_Zoom);
         end if;
         if Zoom <= 0.0 then
            return (Origin => Logic.Zero_2d, Zoom => 0.0);
         else
            Origin := To_Logic (Absolute.Get_Center (Target)) / Zoom
              - Logic.Get_Center (Source);
            return (Origin, Zoom);
         end if;
      end if;
   end Get_Transformation_Rect_Into_Rect_Centered;

   function Get_Zoom_Level
     (Transformation : in     Transformation_Type)
     return Zoom_Level is
   begin
      return Transformation.Zoom;
   end Get_Zoom_Level;

end Giant.Vis;
