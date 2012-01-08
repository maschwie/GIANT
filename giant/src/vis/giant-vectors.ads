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
--  $RCSfile: giant-vectors.ads,v $, $Revision: 1.16 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Bauhaus_IO;

generic

   --  Field
   type Field_Type is private;

   --  Injective monotonic function Natural --> Field_Type
   with function To_Field_Type
     (A : in Natural)
     return Field_Type;

   --  Addition in Field
   with function Field_Add
     (A, B : in Field_Type)
     return Field_Type;
   --  Subtraction in Field
   with function Field_Sub
     (A, B : in Field_Type)
     return Field_Type;

   --  Coordinate for vector space
   type Coordinate_Type is private;

   --  Vector with all components = Coordinate_Zero is zero in vector space
   Coordinate_Zero : Coordinate_Type;

   --  Width and Height of a point (should be 0.0 or 1) affects width and
   --  height of rectangles
   Point_Size : Coordinate_Type;

   --  Text version of Coordinate_Type
   with function Image
     (A : in Coordinate_Type)
     return String;

   --  Coordinate_Type version of String
   --  Must raise 'Constraint_Error' if 'A' is not a correct
   --  string-representation.
   with function Value
     (A : in String)
     return Coordinate_Type;

   --  <= on Coordinate_Type
   with function Coord_Less_Equal
     (A : in Coordinate_Type; B : in Coordinate_Type)
     return Boolean;

   --  negation
   with function Coord_Negate
     (A : in Coordinate_Type)
     return Coordinate_Type;
   --  addition
   with function Coord_Add
     (A, B : in Coordinate_Type)
     return Coordinate_Type;
   --  subtraction
   with function Coord_Sub
     (A, B : in Coordinate_Type)
     return Coordinate_Type;

   --  Scalar multiplication
   with function Scalar_Mult_Coord
     (A : in Field_Type; B : in Coordinate_Type)
     return Coordinate_Type;
   --  Vector product for each coordinate
   with function Vector_Mult_Coord
     (A : in Coordinate_Type; B : in Coordinate_Type)
     return Field_Type;

   --  Scalar division for each coodinate
   with function Scalar_Div_Coord
     (A : in Coordinate_Type; B : in Field_Type)
     return Coordinate_Type;

   --  Read from Stream
   with procedure Read_Coordinate
     (Stream     : in     Bauhaus_IO.In_Stream_Type;
      Coordinate :    out Coordinate_Type);

   --  Write to Stream
   with procedure Write_Coordinate
     (Stream     : in     Bauhaus_IO.Out_Stream_Type;
      Coordinate : in     Coordinate_Type);

package Giant.Vectors is

   pragma Elaborate_Body;


   ---------------
   -- Vector_2d --
   ---------------

   type Vector_2d is private;

   type Vector_2d_Array is array (Positive range <>) of Vector_2d;

   --  Zero in vector space
   Zero_2d : constant Vector_2d;

   --  Negation
   function "-"
     (Op : in Vector_2d)
     return Vector_2d;
   pragma Inline ("-");

   --  Addition
   function "+"
     (Left  : in Vector_2d;
      Right : in Vector_2d)
     return Vector_2d;
   pragma Inline ("+");

   --  Subtraction
   function "-"
     (Left  : in Vector_2d;
      Right : in Vector_2d)
     return Vector_2d;
   pragma Inline ("-");

   --  Inner product
   function "*"
     (Left  : in Vector_2d;
      Right : in Vector_2d)
     return Field_Type;
   pragma Inline ("*");

   --  Scalar multiplication
   function "*"
     (Left  : in Field_Type;
      Right : in Vector_2d)
     return Vector_2d;
   pragma Inline ("*");

   --  Scalar division
   function "/"
     (Left  : in Vector_2d;
      Right : in Field_Type)
     return Vector_2d;
   pragma Inline ("/");

   function Get_X
     (Vector : in Vector_2d)
     return Coordinate_Type;
   pragma Inline (Get_X);

   function Get_Y
     (Vector : in Vector_2d)
     return Coordinate_Type;
   pragma Inline (Get_Y);

   function Combine_Vector
     (X      : in     Coordinate_Type;
      Y      : in     Coordinate_Type)
     return Vector_2d;
   pragma Inline (Combine_Vector);

   procedure Set_X
     (Vector : in out Vector_2d;
      X      : in     Coordinate_Type);
   pragma Inline (Set_X);

   procedure Set_Y
     (Vector : in out Vector_2d;
      Y      : in     Coordinate_Type);
   pragma Inline (Set_Y);

   function Image
     (Vector : in     Vector_2d)
     return String;

   --  Returns the value for 'Image' if 'Image' is a correct String-
   --  representation of a Vector_2d, othewise raises 'Constraint_Error'.
   --  Correct string-representations are (regular expression)
   --  s*(s*<Coord>s*,s*<Coord>s*)s*
   --  where s = ' ', <Coord> is the value of any Image (C) where C is a value
   --  of type Coordinate_Type.
   --  Raises:
   --    Constraint_Error if Image incorrect.
   function Value
     (Image  : in     String)
     return Vector_2d;

   --  Read from Stream
   procedure Read_Vector
     (Stream : in     Bauhaus_IO.In_Stream_Type;
      Vector :    out Vector_2d);

   --  Write to Stream
   procedure Write_Vector
     (Stream : in     Bauhaus_IO.Out_Stream_Type;
      Vector : in     Vector_2d);


   ------------------
   -- Rectangle_2d --
   ------------------

   type Rectangle_2d is private;

   type Rectangle_2d_Array is array (Positive range <>) of Rectangle_2d;

   function Combine_Rectangle
     (X_1 : in     Coordinate_Type;
      Y_1 : in     Coordinate_Type;
      X_2 : in     Coordinate_Type;
      Y_2 : in     Coordinate_Type)
     return Rectangle_2d;
   pragma Inline (Combine_Rectangle);

   function Combine_Rectangle
     (Top_Left     : in     Vector_2d;
      Bottom_Right : in     Vector_2d)
     return Rectangle_2d;
   pragma Inline (Combine_Rectangle);

   function Get_Top
     (Rectangle : in     Rectangle_2d)
     return Coordinate_Type;
   pragma Inline (Get_Top);

   function Get_Bottom
     (Rectangle : in     Rectangle_2d)
     return Coordinate_Type;
   pragma Inline (Get_Bottom);

   function Get_Left
     (Rectangle : in     Rectangle_2d)
     return Coordinate_Type;
   pragma Inline (Get_Left);

   function Get_Right
     (Rectangle : in     Rectangle_2d)
     return Coordinate_Type;
   pragma Inline (Get_Right);

   function Get_Top_Left
     (Rectangle : in     Rectangle_2d)
     return Vector_2d;
   pragma Inline (Get_Top_Left);

   function Get_Top_Right
     (Rectangle : in     Rectangle_2d)
     return Vector_2d;
   pragma Inline (Get_Top_Right);

   function Get_Top_Center
     (Rectangle : in     Rectangle_2d)
     return Vector_2d;
   pragma Inline (Get_Top_Center);

   function Get_Bottom_Left
     (Rectangle : in     Rectangle_2d)
     return Vector_2d;
   pragma Inline (Get_Bottom_Left);

   function Get_Bottom_Right
     (Rectangle : in     Rectangle_2d)
     return Vector_2d;
   pragma Inline (Get_Bottom_Right);

   function Get_Bottom_Center
     (Rectangle : in     Rectangle_2d)
     return Vector_2d;
   pragma Inline (Get_Bottom_Center);

   function Get_Center
     (Rectangle : in     Rectangle_2d)
     return Vector_2d;
   pragma Inline (Get_Center);

   function Get_Width
     (Rectangle : in     Rectangle_2d)
     return Coordinate_Type;
   pragma Inline (Get_Width);

   function Get_Height
     (Rectangle : in     Rectangle_2d)
     return Coordinate_Type;
   pragma Inline (Get_Height);

   function Get_Size
     (Rectangle : in     Rectangle_2d)
     return Vector_2d;
   pragma Inline (Get_Size);

   procedure Shrink
     (Rectangle : in out Rectangle_2d;
      Thickness : in     Coordinate_Type);
   pragma Inline (Shrink);

   procedure Enlarge
     (Rectangle : in out Rectangle_2d;
      Thickness : in     Coordinate_Type);
   pragma Inline (Enlarge);

   procedure Set_Top
     (Rectangle : in out Rectangle_2d;
      Top       : in     Coordinate_Type);
   pragma Inline (Set_Top);

   procedure Set_Bottom
     (Rectangle : in out Rectangle_2d;
      Bottom    : in     Coordinate_Type);
   pragma Inline (Set_Bottom);

   procedure Set_Left
     (Rectangle : in out Rectangle_2d;
      Left      : in     Coordinate_Type);
   pragma Inline (Set_Left);

   procedure Set_Right
     (Rectangle : in out Rectangle_2d;
      Right     : in     Coordinate_Type);
   pragma Inline (Set_Right);

   procedure Set_Top_Left
     (Rectangle : in out Rectangle_2d;
      Top_Left  : in     Vector_2d);
   pragma Inline (Set_Top_Left);

   procedure Set_Top_Right
     (Rectangle : in out Rectangle_2d;
      Top_Right : in     Vector_2d);
   pragma Inline (Set_Top_Right);

   procedure Set_Bottom_Left
     (Rectangle   : in out Rectangle_2d;
      Bottom_Left : in     Vector_2d);
   pragma Inline (Set_Bottom_Left);

   procedure Set_Bottom_Right
     (Rectangle    : in out Rectangle_2d;
      Bottom_Right : in     Vector_2d);
   pragma Inline (Set_Bottom_Right);

   procedure Set_Center
     (Rectangle : in out Rectangle_2d;
      Center    : in     Vector_2d);
   pragma Inline (Set_Center);

   --  Precondition:
   --    Get_X (Size) >= Point_Size and Get_Y (Size) >= Point_Size
   procedure Set_Size
     (Rectangle : in out Rectangle_2d;
      Size      : in     Vector_2d);

   procedure Move
     (Rectangle : in out Rectangle_2d;
      Offset    : in     Vector_2d);
   pragma Inline (Move);

   ----------------------------------------------------------------------------
   --  Generic procedure to move a rectangle to a specific point. The point
   --  in 'Rectangle' given through 'Get_Source_Point' will be moved onto
   --  'Target'
   --
   --  Note:
   --    One might want to create an instance like
   --    procedure Move_Center_To is new
   --      Move_To (Get_Source_Point => Get_Center)
   --  Precondition:
   --    # R := Rectangle;
   --    True
   --  Postcondition:
   --    # R' := Rectangle;
   --    Get_Size (R) = Get_Size (R') and Get_Source_Point (R) = Target
   generic
      with function Get_Source_Point
        (Rectangle : in Rectangle_2d)
        return Vector_2d;
   procedure Move_To
     (Rectangle : in out Rectangle_2d;
      Target    : in     Vector_2d);

   function Image
     (Rectangle : in     Rectangle_2d)
     return String;

   function Is_Inside
     (Rectangle : in     Rectangle_2d;
      Point     : in     Vector_2d)
     return Boolean;
   pragma Inline (Is_Inside);

   function Intersects
     (First     : in     Rectangle_2d;
      Second    : in     Rectangle_2d)
     return Boolean;
   pragma Inline (Intersects);

   function "-"
     (Left      : in     Rectangle_2d;
      Right     : in     Rectangle_2d)
     return Rectangle_2d_Array;

   function Get_Surrounding
     (Left      : in     Rectangle_2d;
      Right     : in     Rectangle_2d)
     return Rectangle_2d;

   --  Read from Stream
   procedure Read_Rectangle
     (Stream    : in     Bauhaus_IO.In_Stream_Type;
      Rectangle :    out Rectangle_2d);

   --  Write to Stream
   procedure Write_Rectangle
     (Stream    : in     Bauhaus_IO.Out_Stream_Type;
      Rectangle : in     Rectangle_2d);

private

   type Vector_2d is
      record
         X : Coordinate_Type;
         Y : Coordinate_Type;
      end record;

   Zero_2d : constant Vector_2d := Combine_Vector
     (Coordinate_Zero, Coordinate_Zero);

   type Rectangle_2d is
      record
         Top    : Coordinate_Type;
         Left   : Coordinate_Type;
         Bottom : Coordinate_Type;
         Right  : Coordinate_Type;
      end record;

end Giant.Vectors;
