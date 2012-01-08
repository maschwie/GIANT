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
--  $RCSfile: giant-vis.ads,v $, $Revision: 1.19 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Glib;

with Bauhaus_IO;

with Giant.Vectors;
pragma Elaborate_All (Giant.Vectors);

package Giant.Vis is

   subtype Logic_Float is Float;

   function To_Logic_Float
     (A : in Natural)
     return Logic_Float;

   function Logic_Float_Image
     (A : in Logic_Float)
     return String;

   --  Read from Stream
   procedure Read_Logic_Float
     (Stream     : in     Bauhaus_IO.In_Stream_Type;
      Coordinate :    out Logic_Float);

   --  Write to Stream
   procedure Write_Logic_Float
     (Stream     : in     Bauhaus_IO.Out_Stream_Type;
      Coordinate : in     Logic_Float);

   package Logic is new Vectors
     (Field_Type        => Logic_Float,
      To_Field_Type     => To_Logic_Float,
      Field_Add         => "+",
      Field_Sub         => "-",
      Coordinate_Type   => Logic_Float,
      Coordinate_Zero   => 0.0,
      Point_Size        => 0.0,
      Image             => Logic_Float'Image,
      Value             => Logic_Float'Value,
      Coord_Less_Equal  => "<=",
      Coord_Negate      => "-",
      Coord_Add         => "+",
      Coord_Sub         => "-",
      Scalar_Mult_Coord => "*",
      Vector_Mult_Coord => "*",
      Scalar_Div_Coord  => "/",
      Read_Coordinate   => Read_Logic_Float,
      Write_Coordinate  => Write_Logic_Float);

   subtype Zoom_Level is Float;

   procedure Write_Zoom_Level
     (Stream : in     Bauhaus_IO.Out_Stream_Type;
      Level  : in     Zoom_Level);

   procedure Read_Zoom_Level
     (Stream : in     Bauhaus_IO.In_Stream_Type;
      Level  :    out Zoom_Level);

   subtype Absolute_Int is Integer range
     Integer (Glib.Gint'First) .. Integer (Glib.Gint'Last);

   subtype Absolute_Natural is Absolute_Int range 0 .. Absolute_Int'Last;

   function To_Absolute_Int
     (A : in Natural)
     return Absolute_Int;

   --  Read from Stream
   procedure Read_Absolute_Int
     (Stream     : in     Bauhaus_IO.In_Stream_Type;
      Coordinate :    out Absolute_Int);

   --  Write to Stream
   procedure Write_Absolute_Int
     (Stream     : in     Bauhaus_IO.Out_Stream_Type;
      Coordinate : in     Absolute_Int);

   package Absolute is new Vectors
     (Field_Type        => Absolute_Int,
      To_Field_Type     => To_Absolute_Int,
      Field_Add         => "+",
      Field_Sub         => "-",
      Coordinate_Type   => Absolute_Int,
      Coordinate_Zero   => 0,
      Point_Size        => 1,
      Image             => Absolute_Int'Image,
      Value             => Absolute_Int'Value,
      Coord_Less_Equal  => "<=",
      Coord_Negate      => "-",
      Coord_Add         => "+",
      Coord_Sub         => "-",
      Scalar_Mult_Coord => "*",
      Vector_Mult_Coord => "*",
      Scalar_Div_Coord  => "/",
      Read_Coordinate   => Read_Absolute_Int,
      Write_Coordinate  => Write_Absolute_Int);


   type Transformation_Type is private;

   function To_Absolute
     (Vector    : in     Logic.Vector_2d)
     return Absolute.Vector_2d;

   function To_Absolute
     (Rectangle : in     Logic.Rectangle_2d)
     return Absolute.Rectangle_2d;

   function To_Logic
     (Vector    : in     Absolute.Vector_2d)
     return Logic.Vector_2d;

   function To_Logic
     (Rectangle : in     Absolute.Rectangle_2d)
     return Logic.Rectangle_2d;

   procedure To_Gdk
     (Vector    : in     Absolute.Vector_2d;
      X         :    out Glib.Gint;
      Y         :    out Glib.Gint);

   --  raises Constraint_Error if Get_Y (Direction) = 0
   function Intersects_Line_Horizontal_Line_X
     (Origin         : in     Logic.Vector_2d;
      Direction      : in     Logic.Vector_2d;
      Horizontal     : in     Logic_Float)
     return Logic_Float;

   --  raises Constraint_Error if Get_X (Direction) = 0
   function Intersects_Line_Vertical_Line_Y
     (Origin         : in     Logic.Vector_2d;
      Direction      : in     Logic.Vector_2d;
      Vertical       : in     Logic_Float)
     return Logic_Float;

   function Intersects_Line_Rectangle
     (From           : in     Absolute.Vector_2d;
      To             : in     Absolute.Vector_2d;
      Rectangle      : in     Absolute.Rectangle_2d)
     return Boolean;

   procedure Clip_Line_To_Rectangle
     (From           : in out Absolute.Vector_2d;
      To             : in out Absolute.Vector_2d;
      Rectangle      : in     Absolute.Rectangle_2d;
      Intersect      :    out Boolean);

   function Transform
     (Size           : in     Logic_Float;
      Zoom           : in     Zoom_Level)
     return Absolute_Int;

   function Transform_Backward
     (Size           : in     Absolute_Int;
      Zoom           : in     Zoom_Level)
     return Logic_Float;

   function Transform
     (Point          : in     Logic.Vector_2d;
      Origin         : in     Logic.Vector_2d;
      Zoom           : in     Zoom_Level)
     return Absolute.Vector_2d;

   function Transform_Backward
     (Point          : in     Absolute.Vector_2d;
      Origin         : in     Logic.Vector_2d;
      Zoom           : in     Zoom_Level)
     return Logic.Vector_2d;

   function Transform
     (Transformation : in     Transformation_Type;
      Size           : in     Logic_Float)
     return Absolute_Int;

   function Transform_Backward
     (Transformation : in     Transformation_Type;
      Size           : in     Absolute_Int)
     return Logic_Float;

   function Transform
     (Transformation : in     Transformation_Type;
      Point          : in     Logic.Vector_2d)
     return Absolute.Vector_2d;

   function Transform_Backward
     (Transformation : in     Transformation_Type;
      Point          : in     Absolute.Vector_2d)
     return Logic.Vector_2d;

   function Transform
     (Transformation : in     Transformation_Type;
      Source_Rect    : in     Logic.Rectangle_2d)
     return Absolute.Rectangle_2d;

   function Transform_Backward
     (Transformation : in     Transformation_Type;
      Source_Rect    : in     Absolute.Rectangle_2d)
     return Logic.Rectangle_2d;

   function Get_Transformation
     (Origin         : in     Logic.Vector_2d;
      Zoom           : in     Zoom_Level)
     return Transformation_Type;

   function Get_Transformation_Rect_Into_Rect_Centered
     (Source         : in     Logic.Rectangle_2d;
      Target         : in     Absolute.Rectangle_2d)
     return Transformation_Type;

   function Get_Zoom_Level
     (Transformation : in     Transformation_Type)
     return Zoom_Level;

private

   type Transformation_Type is
      record
         Origin : Logic.Vector_2d := Logic.Zero_2d;
         Zoom   : Zoom_Level      := 1.0;
      end record;

end Giant.Vis;
