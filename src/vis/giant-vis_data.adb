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
--  $RCSfile: giant-vis_data.adb,v $, $Revision: 1.42 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Deallocation;

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Vis_Data is

   package Vis_Data_Logger is new Logger
     (Name => "Giant.Vis_Data");

   package Numerics is new Ada.Numerics.Generic_Elementary_Functions
     (Float_Type => Vis.Logic_Float);


   procedure Set_Min_Max
     (Value_1 : in     Vis.Absolute_Int;
      Value_2 : in     Vis.Absolute_Int;
      Min     :    out Vis.Absolute_Int;
      Max     :    out Vis.Absolute_Int) is
   begin
      if Value_1 <= Value_2 then
         Min := Value_1;
         Max := Value_2;
      else
         Max := Value_1;
         Min := Value_2;
      end if;
   end Set_Min_Max;

   function Hits_Line
     (From      : in     Vis.Absolute.Vector_2d;
      To        : in     Vis.Absolute.Vector_2d;
      Thickness : in     Vis.Absolute_Natural;
      Point     : in     Vis.Absolute.Vector_2d;
      Tolerance : in     Vis.Absolute_Natural := 0)
     return Boolean is

      use Vis.Absolute;
      use Vis.Logic;
      Line           : Vis.Absolute.Vector_2d := To - From;
      Difference     : Vis.Logic.Vector_2d;
      Distance       : Vis.Absolute.Vector_2d;
      Spacing        : Vis.Absolute_Natural := (Thickness + 3) / 2 + Tolerance;
      Alpha          : Vis.Logic_Float;
      Profile        : Vis.Logic_Float;
      X              : Vis.Logic_Float;
      Y              : Vis.Logic_Float;
      Line_Rectangle : Vis.Absolute.Rectangle_2d;
   begin
      if Get_X (Line) = 0 and then Get_Y (Line) = 0 then
         Distance := Point - From;
         return Distance * Distance <= Spacing**2;
      else
         Difference := Vis.To_Logic (Line);

         Alpha := Numerics.Arctan (Get_Y (Difference), Get_X (Difference));

         --  Quick check if hit is possible
         Line_Rectangle := Vis.Absolute.Combine_Rectangle
           (X_1 => Get_X (From),
            Y_1 => Get_Y (From),
            X_2 => Get_X (To),
            Y_2 => Get_Y (To));
         Vis.Absolute.Enlarge (Line_Rectangle, Spacing);
         if not Vis.Absolute.Is_Inside (Line_Rectangle, Point) then
            return False;
         elsif abs Get_X (Line) > abs Get_Y (Line) then
            --  Vertical profile
            Profile := Vis.To_Logic_Float (Spacing) /
              Numerics.Cos (Alpha);
            Y := Vis.Intersects_Line_Vertical_Line_Y
              (Origin     => Vis.To_Logic (From),
               Direction  => Difference,
               Vertical   => Vis.Logic_Float (Get_X (Point)));

            return Vis.Logic_Float (Get_Y (Point) - Spacing) <= Y and then
              Vis.Logic_Float (Get_Y (Point) + Spacing) >= Y;
         else
            --  Horizontal profile
            Profile := Vis.To_Logic_Float (Spacing) /
              abs Numerics.Sin (Alpha);
            X := Vis.Intersects_Line_Horizontal_Line_X
              (Origin     => Vis.To_Logic (From),
               Direction  => Difference,
               Horizontal => Vis.Logic_Float (Get_Y (Point)));

            return Vis.Logic_Float (Get_X (Point) - Spacing) <= X and then
              Vis.Logic_Float (Get_X (Point) + Spacing) >= X;
         end if;
      end if;
   end Hits_Line;


   ------------
   -- Layers --
   ------------

   function Is_Below
     (Low  : in     Layer_Type;
      High : in     Layer_Type)
     return Boolean is
   begin
      return Low < High;
   end Is_Below;

   function Is_Below_Or_Equal
     (Low  : in     Layer_Type;
      High : in     Layer_Type)
     return Boolean is
   begin
      return Low <= High;
   end Is_Below_Or_Equal;

   procedure Free
     (Clipping : in out Layer_Clipping_Access) is

      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Object => Layer_Clipping_Type,
         Name   => Layer_Clipping_Access);
   begin
      Deallocate (Clipping);
   end Free;

   -----------------
   -- Layer_Pools --
   -----------------

   procedure Reset_Pool
     (Pool : in out Layer_Pool) is
   begin
      Pool := Bottom_Layer;
   end Reset_Pool;

   function Get_Highest_Layer
     (Pool : in     Layer_Pool)
     return Layer_Type is
   begin
      return Layer_Type (Pool);
   end Get_Highest_Layer;

   procedure Enlarge_Pool
     (Pool : in out Layer_Pool) is
   begin
      Pool := Pool + 1;
   end Enlarge_Pool;

   procedure Shrink_Pool
     (Pool : in out Layer_Pool) is
   begin
      Pool := Pool - 1;
   end Shrink_Pool;


   --------------
   -- Clipping --
   --------------

   function Is_Below
     (Left  : in     Layer_Clipping_Access;
      Right : in     Layer_Clipping_Access)
     return Boolean is
   begin
      return Is_Below (Left.Height, Right.Height);
   end Is_Below;

   procedure Destroy_Clipping_Queue
     (Queue : in out Clipping_Queue_Access) is

      procedure Free_Queue is new Ada.Unchecked_Deallocation
        (Object => Clipping_Queues.Queue_Type,
         Name   => Clipping_Queue_Access);

      Clipping : Layer_Clipping_Access;
   begin
      if Queue /= null then
         while not Clipping_Queues.Is_Empty (Queue.all) loop
            Clipping := Clipping_Queues.Get_Head (Queue.all);
            Clipping_Queues.Remove_Head (Queue.all);
            Free (Clipping);
         end loop;
         Free_Queue (Queue);
      end if;
   end Destroy_Clipping_Queue;


   -----------
   -- Flags --
   -----------

   procedure Write_Highlight_Array
     (Stream       : in     Bauhaus_IO.Out_Stream_Type;
      Highlighting : in     Highlight_Array) is

      Streamed_Value : Natural := 0;
   begin
      for I in Highlighting'Range loop
         if Highlighting (I) then
            Streamed_Value := Streamed_Value + 2**(Highlight_Type'Pos (I));
         end if;
      end loop;
      Bauhaus_IO.Write_Natural (Stream, Streamed_Value);
   end Write_Highlight_Array;

   procedure Read_Highlight_Array
     (Stream       : in     Bauhaus_IO.In_Stream_Type;
      Highlighting :    out Highlight_Array) is

      Streamed_Value : Natural;
      Current_Pos    : Natural;
   begin
      pragma Assert (Highlight_Type'Size <= Natural'Size);
      Bauhaus_IO.Read_Natural (Stream, Streamed_Value);
      Highlighting := (others => False);
      Current_Pos := 0;
      while Streamed_Value /= 0 loop
         if Streamed_Value mod 2 /= 0 then
            Highlighting (Highlight_Type'Val (Current_Pos)) := True;
         end if;
         Streamed_Value := Streamed_Value / 2;
         Current_Pos := Current_Pos + 1;
      end loop;
   end Read_Highlight_Array;


   -----------
   -- Edges --
   -----------

   function Image
     (Node  : in     Vis_Node_Id)
     return String;

   function Image
     (Edge : in     Vis_Edge_Id)
     return String is
   begin
      return "((" & Image (Get_Source (Edge)) &
        "." & Graph_Lib.Get_Edge_Tag (Get_Graph_Edge (Edge)) & ")" &
        " -> " & Image (Get_Target (Edge)) &
        ")";
   end Image;

   function Create_Edge
     (Graph_Edge  : in     Graph_Lib.Edge_Id;
      Source      : in     Vis_Node_Id;
      Target      : in     Vis_Node_Id;
      Layer       : in     Layer_Type;
      Inflections : in     Natural := 0)
     return Vis_Edge_Id is

      Edge : Vis_Edge_Id;
   begin
      Edge := new Vis_Edge_Record'
        (Number_Of_Points  => Inflections + 2,
         Edge              => Graph_Edge,
         Source            => Source,
         Target            => Target,
         Thickness         => 0,
         Text_Area         => Vis.Absolute.Combine_Rectangle
                                (X_1 => 0, X_2 => 0,
                                 Y_1 => 1, Y_2 => Vis.Absolute_Int'Last),
         Layer             => Layer,
         Points            => (1 .. Inflections + 2 => Vis.Absolute.Zero_2d),
         Left_Arrow_Point  => Vis.Absolute.Zero_2d,
         Right_Arrow_Point => Vis.Absolute.Zero_2d,
         Regions           => Region_Lists.Create,
         Flags             => (Hidden => False,
                               Obsolete => False,
                               Highlight_Type => False));

      Vis_Edge_Lists.Attach (Edge, Source.Outgoing_Edges);
      Vis_Edge_Lists.Attach (Edge, Target.Incoming_Edges);

      return Edge;
   end Create_Edge;

   procedure Destroy
     (Edge : in out Vis_Edge_Id) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Vis_Edge_Record, Name => Vis_Edge_Id);
   begin
      Region_Lists.Destroy (Edge.Regions);
      Free (Edge);
   end Destroy;

   procedure Remove_From_Nodes
     (Edge : in     Vis_Edge_Id) is
   begin
      if Edge.Source /= null then
         Vis_Edge_Lists.DeleteItem (Edge.Source.Outgoing_Edges, Edge);
         Edge.Source := null;
      end if;
      if Edge.Target /= null then
         Vis_Edge_Lists.DeleteItem (Edge.Target.Incoming_Edges, Edge);
         Edge.Target := null;
      end if;
   end Remove_From_Nodes;

   function Get_Graph_Edge
     (Edge : in     Vis_Edge_Id)
     return Graph_Lib.Edge_Id is
   begin
      return Edge.Edge;
   end Get_Graph_Edge;

   function Get_Source
     (Edge : in     Vis_Edge_Id)
     return Vis_Node_Id is
   begin
      return Edge.Source;
   end Get_Source;

   function Get_Target
     (Edge : in     Vis_Edge_Id)
     return Vis_Node_Id is
   begin
      return Edge.Target;
   end Get_Target;

   function Is_Loop
     (Edge : in     Vis_Edge_Id)
     return Boolean is
   begin
      return Edge.Source = Edge.Target;
   end Is_Loop;

   function Get_Layer
     (Edge  : in     Vis_Edge_Id)
     return Layer_Type is
   begin
      return Edge.Layer;
   end Get_Layer;

   function Get_Thickness
     (Edge : in     Vis_Edge_Id)
     return Vis.Absolute_Natural is
   begin
      return Edge.Thickness;
   end Get_Thickness;

   function Get_Number_Of_Points
     (Edge : in     Vis_Edge_Id)
     return Edge_Point_Number is
   begin
      return Edge.Points'Length;
   end Get_Number_Of_Points;

   function Get_Point
     (Edge : in     Vis_Edge_Id;
      Num  : in     Positive)
     return Vis.Absolute.Vector_2d is
   begin
      return Edge.Points (Num);
   end Get_Point;

   function Get_Left_Arrow_Point
     (Edge : in     Vis_Edge_Id)
     return Vis.Absolute.Vector_2d is
   begin
      return Edge.Left_Arrow_Point;
   end Get_Left_Arrow_Point;

   function Get_Right_Arrow_Point
     (Edge : in     Vis_Edge_Id)
     return Vis.Absolute.Vector_2d is
   begin
      return Edge.Right_Arrow_Point;
   end Get_Right_Arrow_Point;

   function Has_Text_Area
     (Edge : in     Vis_Edge_Id)
     return Boolean is
   begin
      return Vis.Absolute.Get_Height (Edge.Text_Area) < Vis.Absolute_Int'Last;
   end Has_Text_Area;

   function Get_Text_Area
     (Edge : in     Vis_Edge_Id)
     return Vis.Absolute.Rectangle_2d is
   begin
      return Edge.Text_Area;
   end Get_Text_Area;

   function Hits
     (Edge  : in     Vis_Edge_Id;
      Point : in     Vis.Absolute.Vector_2d)
     return Boolean is

      I   : Integer;
      Hit : Boolean := False;
   begin
      I := Edge.Points'First;
      while not Hit and I < Edge.Points'Last loop
         Hit := Hits_Line
           (From      => Edge.Points (I),
            To        => Edge.Points (I + 1),
            Thickness => Edge.Thickness,
            Point     => Point,
            Tolerance => 1);
         I := I + 1;
      end loop;
      Hit := Hit or else Hits_Line
        (From      => Edge.Left_Arrow_Point,
         To        => Edge.Points (Edge.Points'Last),
         Thickness => Edge.Thickness,
         Point     => Point,
         Tolerance => 1);
      Hit := Hit or else Hits_Line
        (From      => Edge.Right_Arrow_Point,
         To        => Edge.Points (Edge.Points'Last),
         Thickness => Edge.Thickness,
         Point     => Point,
         Tolerance => 1);
      return Hit;
   end Hits;

   function Intersects
     (Edge : in     Vis_Edge_Id;
      Area : in     Vis.Absolute.Rectangle_2d)
     return Boolean is

      I     : Integer;
      Found : Boolean := False;
   begin
      I := Edge.Points'First;
      while I < Edge.Points'Last and not Found loop
         Found := Vis.Intersects_Line_Rectangle
           (From      => Edge.Points (I),
            To        => Edge.Points (I + 1),
            Rectangle => Area);
         I := I + 1;
      end loop;
      return Found;
   end Intersects;

   function Is_Obsolete
     (Edge : in     Vis_Edge_Id)
     return Boolean is
   begin
      return Edge.Flags (Obsolete);
   end Is_Obsolete;

   function Is_Hidden
     (Edge : in     Vis_Edge_Id)
     return Boolean is
   begin
      return Edge.Flags (Hidden);
   end Is_Hidden;

   function Must_Be_Visible
     (Edge : in     Vis_Edge_Id)
     return Boolean is
   begin
      return Are_Incident_Visible (Get_Source (Edge)) or else
        Are_Incident_Visible (Get_Target (Edge));
   end Must_Be_Visible;

   function Get_Highlighting
     (Edge : in     Vis_Edge_Id)
     return Highlight_Array is
   begin
      return Edge.Flags (Highlight_Type'Range);
   end Get_Highlighting;

   procedure Add_Highlight_Color
     (Edge   : in     Vis_Edge_Id;
      Color  : in     Highlight_Type) is
   begin
      Edge.Flags (Color) := True;
   end Add_Highlight_Color;

   procedure Remove_Highlight_Color
     (Edge   : in     Vis_Edge_Id;
      Color  : in     Highlight_Type) is
   begin
      Edge.Flags (Color) := False;
   end Remove_Highlight_Color;

   procedure Remove_All_Highlight_Colors
     (Edge   : in     Vis_Edge_Id) is
   begin
      Edge.Flags := Edge.Flags and
        (Highlight_Type'Range => False,
         Hidden               => True,
         Obsolete             => True);
   end Remove_All_Highlight_Colors;

   procedure Set_Thickness
     (Edge      : in     Vis_Edge_Id;
      Thickness : in     Vis.Absolute_Natural) is
   begin
      pragma Assert (Region_Lists.IsEmpty (Edge.Regions));
      Edge.Thickness := Thickness;
   end Set_Thickness;

   procedure Set_Text_Area_Size
     (Edge      : in     Vis_Edge_Id;
      Size      : in     Vis.Absolute.Vector_2d) is
   begin
      pragma Assert (Region_Lists.IsEmpty (Edge.Regions));
      Vis.Absolute.Set_Size (Edge.Text_Area, Size);
   end Set_Text_Area_Size;

   procedure Move_Text_Area_To
     (Edge       : in     Vis_Edge_Id;
      Position   : in     Vis.Absolute.Vector_2d;
      Align_Left : in     Boolean;
      Align_Top  : in     Boolean) is

      generic
         with function Get_Point
           (Rectangle : in     Vis.Absolute.Rectangle_2d)
           return Vis.Absolute.Vector_2d;
      procedure Move_To;

      procedure Move_To is
         procedure Move_Point_To is new Vis.Absolute.Move_To
           (Get_Source_Point => Get_Point);
      begin
         Move_Point_To
           (Rectangle => Edge.Text_Area,
            Target    => Position);
      end Move_To;

      procedure Move_Top_Left_To is new Move_To
        (Get_Point => Vis.Absolute.Get_Top_Left);
      procedure Move_Top_Right_To is new Move_To
        (Get_Point => Vis.Absolute.Get_Top_Right);
      procedure Move_Bottom_Left_To is new Move_To
        (Get_Point => Vis.Absolute.Get_Bottom_Left);
      procedure Move_Bottom_Right_To is new Move_To
        (Get_Point => Vis.Absolute.Get_Bottom_Right);
   begin
      if Align_Top then
         if Align_Left then
            Move_Top_Left_To;
         else
            Move_Top_Right_To;
         end if;
      else
         if Align_Left then
            Move_Bottom_Left_To;
         else
            Move_Bottom_Right_To;
         end if;
      end if;
   end Move_Text_Area_To;

   procedure Move_Text_Area_Top_Left_To
     (Edge      : in     Vis_Edge_Id;
      Top_Left  : in     Vis.Absolute.Vector_2d) is

      procedure Move_Top_Left_To is new Vis.Absolute.Move_To
        (Get_Source_Point => Vis.Absolute.Get_Top_Left);
   begin
      Move_Top_Left_To
        (Rectangle => Edge.Text_Area,
         Target    => Top_Left);
   end Move_Text_Area_Top_Left_To;

   procedure Move_Text_Area_Bottom_Left_To
     (Edge        : in     Vis_Edge_Id;
      Bottom_Left : in     Vis.Absolute.Vector_2d) is

      procedure Move_Bottom_Left_To is new Vis.Absolute.Move_To
        (Get_Source_Point => Vis.Absolute.Get_Bottom_Left);
   begin
      Move_Bottom_Left_To
        (Rectangle => Edge.Text_Area,
         Target    => Bottom_Left);
   end Move_Text_Area_Bottom_Left_To;

   procedure Remove_Text_Area
     (Edge      : in     Vis_Edge_Id) is
   begin
      Vis.Absolute.Set_Bottom (Edge.Text_Area, Vis.Absolute_Int'Last);
      Vis.Absolute.Set_Top (Edge.Text_Area, 1);
   end Remove_Text_Area;

   procedure Set_Point
     (Edge      : in     Vis_Edge_Id;
      Num       : in     Positive;
      Point     : in     Vis.Absolute.Vector_2d) is
   begin
      Edge.Points (Num) := Point;
   end Set_Point;

   procedure Set_Left_Arrow_Point
     (Edge      : in     Vis_Edge_Id;
      Point     : in     Vis.Absolute.Vector_2d) is
   begin
      Edge.Left_Arrow_Point := Point;
   end Set_Left_Arrow_Point;

   procedure Set_Right_Arrow_Point
     (Edge      : in     Vis_Edge_Id;
      Point     : in     Vis.Absolute.Vector_2d) is
   begin
      Edge.Right_Arrow_Point := Point;
   end Set_Right_Arrow_Point;

   procedure Set_Obsolete
     (Edge  : in     Vis_Edge_Id;
      State : in     Boolean) is
   begin
      Edge.Flags (Obsolete) := State;
   end Set_Obsolete;

   procedure Set_Hidden
     (Edge  : in     Vis_Edge_Id;
      State : in     Boolean) is
   begin
      Edge.Flags (Hidden) := State;
   end Set_Hidden;

   procedure Set_Layer
     (Edge  : in     Vis_Edge_Id;
      Layer : in     Layer_Type) is
   begin
      Edge.Layer := Layer;
   end Set_Layer;

   function Is_Edge_Below
     (Left  : in     Vis_Edge_Id;
      Right : in     Vis_Edge_Id)
     return Boolean is
   begin
      return Is_Below (Get_Layer (Left), Get_Layer (Right));
   end Is_Edge_Below;


   -----------
   -- Nodes --
   -----------

   function Image
     (Node       : in     Vis_Node_Id)
     return String is
   begin
      return Graph_Lib.Node_Id_Image (Get_Graph_Node (Node));
   end Image;

   function Create_Node
     (Graph_Node : in     Graph_Lib.Node_Id;
      Layer      : in     Layer_Type)
     return Vis_Node_Id is

   begin
      return new Vis_Node_Record'
        (Position       => Vis.Logic.Zero_2d,
         Node           => Graph_Node,
         Extent         => Vis.Absolute.Combine_Rectangle (0, 0, 0, 0),
         Layer          => Layer,
         Incoming_Edges => Vis_Edge_Lists.Create,
         Outgoing_Edges => Vis_Edge_Lists.Create,
         Regions        => Region_Lists.Create,
         Flags          => (Hidden => False, Obsolete => False,
                            Annotated => False,
                            Sized => False, Locked => False,
                            Incident_Visible => False,
                            Highlight_Type => False));
   end Create_Node;

   procedure Destroy
     (Node  : in out Vis_Node_Id) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Vis_Node_Record, Name => Vis_Node_Id);
   begin
      Vis_Edge_Lists.Destroy (Node.Incoming_Edges);
      Vis_Edge_Lists.Destroy (Node.Outgoing_Edges);
      Region_Lists.Destroy (Node.Regions);
      Free (Node);
   end Destroy;

   procedure Remove_From_Edges
     (Node  : in     Vis_Node_Id) is

      Edge : Vis_Edge_Id;
   begin
      while not Vis_Edge_Lists.IsEmpty (Node.Outgoing_Edges) loop
         Edge := Vis_Edge_Lists.FirstValue (Node.Outgoing_Edges);
         Vis_Edge_Lists.DeleteHead (Node.Outgoing_Edges);
         Edge.Source := null;
      end loop;
      while not Vis_Edge_Lists.IsEmpty (Node.Incoming_Edges) loop
         Edge := Vis_Edge_Lists.FirstValue (Node.Incoming_Edges);
         Vis_Edge_Lists.DeleteHead (Node.Incoming_Edges);
         Edge.Target := null;
      end loop;
   end Remove_From_Edges;

   function Get_Layer
     (Node  : in     Vis_Node_Id)
     return Layer_Type is
   begin
      return Node.Layer;
   end Get_Layer;

   function Get_Graph_Node
     (Node : in     Vis_Node_Id)
     return Graph_Lib.Node_Id is
   begin
      return Node.Node;
   end Get_Graph_Node;

   function Get_Position
     (Node : in     Vis_Node_Id)
     return Vis.Logic.Vector_2d is
   begin
      return Node.Position;
   end Get_Position;

   function Get_Top_Center
     (Node : in     Vis_Node_Id)
     return Vis.Absolute.Vector_2d is
   begin
      return Vis.Absolute.Get_Top_Center (Node.Extent);
   end Get_Top_Center;

   function Get_Center
     (Node : in     Vis_Node_Id)
     return Vis.Absolute.Vector_2d is
   begin
      return Vis.Absolute.Get_Center (Node.Extent);
   end Get_Center;

   function Get_Extent
     (Node : in     Vis_Node_Id)
     return Vis.Absolute.Rectangle_2d is
   begin
      return Node.Extent;
   end Get_Extent;

   function Hits
     (Node  : in     Vis_Node_Id;
      Point : in     Vis.Absolute.Vector_2d)
     return Boolean is
   begin
      return Vis.Absolute.Is_Inside
        (Rectangle => Get_Extent (Node),
         Point     => Point);
   end Hits;

   function Intersects
     (Node  : in     Vis_Node_Id;
      Area  : in     Vis.Absolute.Rectangle_2d)
     return Boolean is
   begin
      return Vis.Absolute.Intersects (Get_Extent (Node), Area);
   end Intersects;

   procedure Make_Incoming_Iterator
     (Node           : in     Vis_Node_Id;
      Incoming_Edges :    out Vis_Edge_Lists.ListIter) is
   begin
      Incoming_Edges := Vis_Edge_Lists.MakeListIter (Node.Incoming_Edges);
   end Make_Incoming_Iterator;

   procedure Make_Outgoing_Iterator
     (Node           : in     Vis_Node_Id;
      Outgoing_Edges :    out Vis_Edge_Lists.ListIter) is
   begin
      Outgoing_Edges := Vis_Edge_Lists.MakeListIter (Node.Outgoing_Edges);
   end Make_Outgoing_Iterator;

   function Is_Obsolete
     (Node : in    Vis_Node_Id)
     return Boolean is
   begin
      return Node.Flags (Obsolete);
   end Is_Obsolete;

   function Is_Hidden
     (Node : in     Vis_Node_Id)
     return Boolean is
   begin
      return Node.Flags (Hidden);
   end Is_Hidden;

   function Is_Annotated
     (Node : in     Vis_Node_Id)
     return Boolean is
   begin
      return Node.Flags (Annotated);
   end Is_Annotated;

   function Is_Sized
     (Node : in     Vis_Node_Id)
     return Boolean is
   begin
     return Node.Flags (Sized);
   end Is_Sized;

   function Is_Locked
     (Node : in     Vis_Node_Id)
     return Boolean is
   begin
      return Node.Flags (Locked);
   end Is_Locked;

   function Are_Incident_Visible
     (Node : in     Vis_Node_Id)
     return Boolean is
   begin
      return Node.Flags (Incident_Visible);
   end Are_Incident_Visible;

   function Get_Highlighting
     (Node : in     Vis_Node_Id)
     return Highlight_Array is
   begin
      return Node.Flags (Highlight_Type'Range);
   end Get_Highlighting;

   procedure Set_Position
     (Node     : in     Vis_Node_Id;
      Position : in     Vis.Logic.Vector_2d) is
   begin
      Node.Position := Position;
   end Set_Position;

   procedure Set_Node_Size
     (Node : in     Vis_Node_Id;
      Size : in     Vis.Absolute.Vector_2d) is
   begin
      pragma Assert (Region_Lists.IsEmpty (Node.Regions));
      Vis.Absolute.Set_Size (Node.Extent, Size);
   end Set_Node_Size;

   procedure Move_Node
     (Node   : in     Vis_Node_Id;
      Offset : in     Vis.Absolute.Vector_2d) is
   begin
      pragma Assert (Region_Lists.IsEmpty (Node.Regions));
      Vis.Absolute.Move (Node.Extent, Offset);
   end Move_Node;

   procedure Set_Layer
     (Node   : in     Vis_Node_Id;
      Layer  : in     Layer_Type) is
   begin
      Node.Layer := Layer;
   end Set_Layer;

   procedure Add_Highlight_Color
     (Node   : in     Vis_Node_Id;
      Color  : in     Highlight_Type) is
   begin
      Node.Flags (Color) := True;
   end Add_Highlight_Color;

   procedure Remove_Highlight_Color
     (Node   : in     Vis_Node_Id;
      Color  : in     Highlight_Type) is
   begin
      Node.Flags (Color) := False;
   end Remove_Highlight_Color;

   procedure Remove_All_Highlight_Colors
     (Node   : in     Vis_Node_Id) is
   begin
      Node.Flags := Node.Flags and
        (Highlight_Type'Range => False,
         Hidden               => True,
         Obsolete             => True,
         Annotated            => True,
         Sized                => True,
         Locked               => True,
         Incident_Visible     => True);
   end Remove_All_Highlight_Colors;

   procedure Set_Annotated
     (Node  : in     Vis_Node_Id;
      State : in     Boolean) is
   begin
      Node.Flags (Annotated) := State;
   end Set_Annotated;

   procedure Set_Sized
     (Node   : in     Vis_Node_Id;
      State  : in     Boolean) is
   begin
      Node.Flags (Sized) := State;
   end Set_Sized;

   procedure Set_Obsolete
     (Node   : in     Vis_Node_Id;
      State  : in     Boolean) is
   begin
      Node.Flags (Obsolete) := State;
   end Set_Obsolete;

   procedure Set_Hidden
     (Node   : in     Vis_Node_Id;
      State  : in     Boolean) is
   begin
      Node.Flags (Hidden) := State;
   end Set_Hidden;

   procedure Set_Locked
     (Node   : in     Vis_Node_Id;
      State  : in     Boolean) is
   begin
      Node.Flags (Locked) := State;
   end Set_Locked;

   procedure Set_Incident_Visible
     (Node  : in     Vis_Node_Id;
      State : in     Boolean) is
   begin
      Node.Flags (Incident_Visible) := State;
   end Set_Incident_Visible;

   function Is_Node_Below
     (Left  : in     Vis_Node_Id;
      Right : in     Vis_Node_Id)
     return Boolean is
   begin
      return Is_Below (Get_Layer (Left), Get_Layer (Right));
   end Is_Node_Below;


   -------------
   -- Regions --
   -------------

   function Hash_Region_Position
     (Key : in Region_Position)
     return Integer is

      Vector : Vis.Absolute.Vector_2d := Vis.Absolute.Vector_2d (Key);
      X      : Vis.Absolute_Natural   := abs Vis.Absolute.Get_X (Vector);
      Y      : Vis.Absolute_Natural   := abs Vis.Absolute.Get_Y (Vector);
      Value  : Natural range 0 .. 16#7FFF_FFFF#;
   begin
      -- use lower 2 bytes of each coordinate
      Value := 16#1# * (X mod 16#100#) + 16#100# * (Y mod 16#100#);
      X := X / 16#100#;
      Y := Y / 16#100#;
      Value := Value + 16#1_0000# * (X mod 16#100#)
        + 16#100_0000# * (Y mod 16#80#);
      return Value;
   end Hash_Region_Position;

   function Order_Position
     (Left  : in    Region_Position;
      Right : in    Region_Position)
     return Boolean is
   begin
      if Vis.Absolute.Get_X (Vis.Absolute.Vector_2d (Left)) =
        Vis.Absolute.Get_X (Vis.Absolute.Vector_2d (Right)) then

         return Vis.Absolute.Get_Y (Vis.Absolute.Vector_2d (Left)) <
           Vis.Absolute.Get_Y (Vis.Absolute.Vector_2d (Right));
      else
         return Vis.Absolute.Get_X (Vis.Absolute.Vector_2d (Left)) <
           Vis.Absolute.Get_X (Vis.Absolute.Vector_2d (Right));
      end if;
   end Order_Position;

   function Create_Region
     (Extent : Vis.Absolute.Rectangle_2d)
     return Region_Id is

      Region : Region_Id := new Region_Record;
   begin
      Region.Extent := Extent;
      Region.Polluted_Edge := null;
      Region.Polluted_Node := null;
      Region.Background_Polluted := True;
      Region.Edges := Vis_Edge_Sets.Empty_Set;
      Region.Nodes := Vis_Node_Sets.Empty_Set;
      return Region;
   end Create_Region;

   procedure Destroy_Region
     (Region : in out Region_Id) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Region_Record,
         Name   => Region_Id);

   begin
      if Region /= null then
         Vis_Edge_Sets.Destroy (Region.Edges);
         Vis_Node_Sets.Destroy (Region.Nodes);
         Free (Region);
      end if;
   end Destroy_Region;

   procedure Add_Background_Pollution
     (Region : access Region_Record) is
   begin
      Region.Polluted_Edge := null;
      Region.Polluted_Node := null;
      Region.Background_Polluted := True;
   end Add_Background_Pollution;

   procedure Add_Edge_Pollution
     (Region : access Region_Record;
      Edge   : in     Vis_Edge_Id) is
   begin
      if not Region.Background_Polluted then
         if Region.Polluted_Edge /= null then
            if Is_Edge_Below (Edge, Region.Polluted_Edge) then
               Region.Polluted_Edge := Edge;
            end if;
         else
            Region.Polluted_Edge := Edge;
            Region.Polluted_Node := null;
         end if;
      end if;
   end Add_Edge_Pollution;

   procedure Add_Node_Pollution
     (Region : access Region_Record;
      Node   : in     Vis_Node_Id) is
   begin
      if not Region.Background_Polluted
        and then Region.Polluted_Edge = null
        and then (Region.Polluted_Node = null or else
                  Is_Node_Below (Node, Region.Polluted_Node))
      then
         Region.Polluted_Node := Node;
      end if;
   end Add_Node_Pollution;

   procedure Remove_Background_Pollution
     (Region : access Region_Record) is
   begin
      if Region.Background_Polluted then
         Region.Background_Polluted := False;
         if not Vis_Edge_Sets.Is_Empty (Region.Edges) then
            Region.Polluted_Edge := Vis_Edge_Sets.First (Region.Edges);
         elsif not Vis_Node_Sets.Is_Empty (Region.Nodes) then
            Region.Polluted_Node := Vis_Node_Sets.First (Region.Nodes);
         end if;
      end if;
   end Remove_Background_Pollution;

   procedure Remove_Foreground_Pollution
     (Region : access Region_Record) is
   begin
      Region.Polluted_Edge := null;
      Region.Polluted_Node := null;
   end Remove_Foreground_Pollution;

   procedure Remove_Edge_Pollution
     (Region : access Region_Record) is
   begin
      if Region.Polluted_Edge /= null then
         Region.Polluted_Edge := null;
         if not Vis_Node_Sets.Is_Empty (Region.Nodes) then
            Region.Polluted_Node := Vis_Node_Sets.First (Region.Nodes);
         end if;
      end if;
   end Remove_Edge_Pollution;

   procedure Remove_Node_Pollution
     (Region : access Region_Record) is
   begin
      Region.Polluted_Node := null;
   end Remove_Node_Pollution;

   procedure Remove_All_Pollution
     (Region : access Region_Record) is
   begin
      Region.Polluted_Node := null;
      Region.Polluted_Edge := null;
      Region.Background_Polluted := False;
   end Remove_All_Pollution;

   function Is_Background_Polluted
     (Region : access Region_Record)
     return Boolean is
   begin
      return Region.Background_Polluted;
   end Is_Background_Polluted;

   --  Iterator must be destroyed.
   function Get_Polluted_Edges
     (Region : access Region_Record)
     return Vis_Edge_Sets.Iterator is

      Iterator : Vis_Edge_Sets.Iterator;
   begin
      if Region.Polluted_Edge /= null then
         --  Start from first polluted edge
         Iterator := Vis_Edge_Sets.Make_Iterator_On_Element
           (Region.Edges, Region.Polluted_Edge);
      elsif Region.Background_Polluted then
         --  All edges are polluted
         Iterator := Vis_Edge_Sets.Make_Iterator (Region.Edges);
      else
         --  Make empty Iterator
         Iterator := Vis_Edge_Sets.Make_Iterator (Vis_Edge_Sets.Empty_Set);
      end if;
      return Iterator;
   end Get_Polluted_Edges;

   --  Iterator must be destroyed.
   function Get_Polluted_Nodes
     (Region : access Region_Record)
     return Vis_Node_Sets.Iterator is

      Iterator : Vis_Node_Sets.Iterator;
   begin
      if Region.Polluted_Node /= null then
         --  Start at first polluted node
         Iterator := Vis_Node_Sets.Make_Iterator_On_Element
           (Region.Nodes, Region.Polluted_Node);
      elsif Region.Polluted_Edge /= null or Region.Background_Polluted then
         --  All nodes are polluted
         Iterator := Vis_Node_Sets.Make_Iterator (Region.Nodes);
      else
         --  Make empty iterator
         Iterator := Vis_Node_Sets.Make_Iterator (Vis_Node_Sets.Empty_Set);
      end if;
      return Iterator;
   end Get_Polluted_Nodes;

   procedure Add_Node_To_Region
     (Region : access Region_Record;
      Node   : in     Vis_Node_Id) is
   begin
      pragma Assert (Node /= null);
      Vis_Node_Sets.Insert (Region.Nodes, Node);
   end Add_Node_To_Region;

   procedure Add_Edge_To_Region
     (Region : access Region_Record;
      Edge   : in     Vis_Edge_Id) is
   begin
      pragma Assert (Edge /= null);
      Vis_Edge_Sets.Insert (Region.Edges, Edge);
   end Add_Edge_To_Region;

   procedure Remove_Edge_From_Region
     (Region : access Region_Record;
      Edge   : in     Vis_Edge_Id) is

      Iterator : Vis_Edge_Sets.Iterator;
   begin
      pragma Assert (Edge /= null);
      if Edge = Region.Polluted_Edge then
         Iterator := Vis_Edge_Sets.Make_Iterator_On_Element
           (A_Set   => Region.Edges,
            Element => Edge);
         Vis_Edge_Sets.Next (Iterator);
         if Vis_Edge_Sets.More (Iterator) then
            Region.Polluted_Edge := Vis_Edge_Sets.Current (Iterator);
         else
            Region.Polluted_Edge := null;
            if not Vis_Node_Sets.Is_Empty (Region.Nodes) then
               Region.Polluted_Node := Vis_Node_Sets.First (Region.Nodes);
            end if;
         end if;
         Vis_Edge_Sets.Destroy (Iterator);
      end if;
      Vis_Edge_Sets.Remove_If_Exists
        (A_Set   => Region.Edges,
         Element => Edge);
   end Remove_Edge_From_Region;

   procedure Remove_Node_From_Region
     (Region : access Region_Record;
      Node   : in     Vis_Node_Id) is

      Iterator : Vis_Node_Sets.Iterator;
   begin
      pragma Assert (Node /= null);
      if Node = Region.Polluted_Node then
         Iterator := Vis_Node_Sets.Make_Iterator_On_Element
           (A_Set   => Region.Nodes,
            Element => Node);
         Vis_Node_Sets.Next (Iterator);
         if Vis_Node_Sets.More (Iterator) then
            Region.Polluted_Node := Vis_Node_Sets.Current (Iterator);
         else
            Region.Polluted_Node := null;
         end if;
         Vis_Node_Sets.Destroy (Iterator);
      end if;
      Vis_Node_Sets.Remove_If_Exists
        (A_Set   => Region.Nodes,
         Element => Node);
   end Remove_Node_From_Region;

   function Get_Region_Extent
     (Region : access Region_Record)
     return Vis.Absolute.Rectangle_2d is
   begin
      return Region.Extent;
   end Get_Region_Extent;


   ---------------------
   -- Region_Managers --
   ---------------------

   procedure Set_Up
     (Manager : in out Region_Manager) is
   begin
      Manager.Region_Width := Default_Region_Width;
      Manager.Region_Height := Default_Region_Height;
      Manager.Regions := Region_Mappings.Create;
   end Set_Up;

   procedure Destroy
     (Manager : in out Region_Manager) is
   begin
      Region_Mappings.Destroy (Manager.Regions);
   end Destroy;

   procedure Init_Region_Manager
     (Manager         : in out Region_Manager;
      Size            : in     Vis.Absolute.Vector_2d;
      Number_Of_Nodes : in     Natural) is
   begin
      if Region_Mappings.Is_Empty (Manager.Regions) then
         Vis_Data_Logger.Debug ("Init_Region_Manager not implemented.");
      else
         Vis_Data_Logger.Debug ("Unnecessary call to Init_Region_Manager.");
      end if;
   end Init_Region_Manager;

   function Get_Position_X_At_X
     (Manager : in     Region_Manager;
      Point_X : in     Vis.Absolute_Int)
     return Vis.Absolute_Int is

      Position_X : Vis.Absolute_Int;
   begin
      Position_X := Point_X / Manager.Region_Width;
      if Point_X < 0 and then Point_X mod Manager.Region_Width /= 0 then
         Position_X := Position_X - 1;
      end if;
      return Position_X;
   end Get_Position_X_At_X;
   pragma Inline (Get_Position_X_At_X);

   function Get_Position_Y_At_Y
     (Manager : in     Region_Manager;
      Point_Y : in     Vis.Absolute_Int)
     return Vis.Absolute_Int is

      Position_Y : Vis.Absolute_Int := Point_Y / Manager.Region_Height;
   begin
      if Point_Y < 0 and then Point_Y mod Manager.Region_Height /= 0 then
         Position_Y := Position_Y - 1;
      end if;
      return Position_Y;
   end Get_Position_Y_At_Y;
   pragma Inline (Get_Position_Y_At_Y);


   function Get_Region_Position
     (Manager : in     Region_Manager;
      Point   : in     Vis.Absolute.Vector_2d)
     return Region_Position is

      Point_X    : Vis.Absolute_Int := Vis.Absolute.Get_X (Point);
      Point_Y    : Vis.Absolute_Int := Vis.Absolute.Get_Y (Point);
      Position_X : Vis.Absolute_Int := Get_Position_X_At_X (Manager, Point_X);
      Position_Y : Vis.Absolute_Int := Get_Position_Y_At_Y (Manager, Point_Y);
   begin
      return Region_Position
        (Vis.Absolute.Combine_Vector (Position_X, Position_Y));
   end Get_Region_Position;

   function Get_Region_Extent
     (Manager  : in     Region_Manager;
      Position : in     Region_Position)
     return Vis.Absolute.Rectangle_2d is

      X : Vis.Absolute_Int := Vis.Absolute.Get_X
        (Vis.Absolute.Vector_2d (Position));
      Y : Vis.Absolute_Int := Vis.Absolute.Get_Y
        (Vis.Absolute.Vector_2d (Position));
   begin
      return Vis.Absolute.Combine_Rectangle
        (X * Manager.Region_Width,
         Y * Manager.Region_Height,
         (X + 1) * Manager.Region_Width - 1,
         (Y + 1) * Manager.Region_Height - 1);
   end Get_Region_Extent;

   --  Gets the region for a position. If that region does not exist yet,
   --  then null will be returned.
   function Get_Region_If_Exists
     (Manager  : in     Region_Manager;
      Position : in     Region_Position)
     return Region_Id is
   begin
      if Region_Mappings.Is_Bound (Manager.Regions, Position) then
         return Region_Mappings.Fetch (Manager.Regions, Position);
      else
         return null;
      end if;
   end Get_Region_If_Exists;

   --  Gets the region for a position. If that region does not exist yet then
   --  it will be created.
   procedure Get_Region
     (Manager  : in out Region_Manager;
      Position : in     Region_Position;
      Region   :    out Region_Id) is
   begin
      if Region_Mappings.Is_Bound (Manager.Regions, Position) then
         Region := Region_Mappings.Fetch (Manager.Regions, Position);
      else
         Region := Create_Region (Get_Region_Extent (Manager, Position));
         Region_Mappings.Bind (Manager.Regions, Position, Region);
      end if;
   end Get_Region;

   procedure Optimize_Drawing_Area
     (Manager : in     Region_Manager;
      Area    : in out Vis.Absolute.Rectangle_2d) is

      Top_Left_Position     : Region_Position;
      Bottom_Right_Point    : Vis.Absolute.Vector_2d;
      Bottom_Right_Position : Region_Position;
      Top_Left_Extent       : Vis.Absolute.Rectangle_2d;
      Bottom_Right_Extent   : Vis.Absolute.Rectangle_2d;
   begin
      --  Find top-left and bottom-right regions
      Top_Left_Position := Get_Region_Position
        (Manager, Vis.Absolute.Get_Top_Left (Area));
      Top_Left_Extent := Get_Region_Extent (Manager, Top_Left_Position);

      Bottom_Right_Point := Vis.Absolute."-"
        (Vis.Absolute."+"
          (Vis.Absolute.Get_Bottom_Right (Top_Left_Extent),
           Vis.Absolute.Get_Size (Area)),
         Vis.Absolute.Combine_Vector (-1, -1));
      Bottom_Right_Position := Get_Region_Position
        (Manager, Bottom_Right_Point);
      Bottom_Right_Extent := Get_Region_Extent
        (Manager, Bottom_Right_Position);
      --  Take top-left and bottom-right points of these regions
      Area := Vis.Absolute.Combine_Rectangle
        (Top_Left     => Vis.Absolute.Get_Top_Left (Top_Left_Extent),
         Bottom_Right => Vis.Absolute.Get_Bottom_Right (Bottom_Right_Extent));
   end Optimize_Drawing_Area;

   generic
      with procedure Hit
        (Position : in     Region_Position);
   procedure Add_Lines_Positions
     (Manager     : in     Region_Manager;
      Thickness   : in     Vis.Absolute_Natural;
      Start_Point : in     Vis.Absolute.Vector_2d;
      End_Point   : in     Vis.Absolute.Vector_2d);

   procedure Add_Lines_Positions
     (Manager     : in     Region_Manager;
      Thickness   : in     Vis.Absolute_Natural;
      Start_Point : in     Vis.Absolute.Vector_2d;
      End_Point   : in     Vis.Absolute.Vector_2d) is

      use Vis.Absolute;
      use Vis.Logic;

      procedure Add_Line_Bottom_To_Top
        (Bottom_Point : in     Vis.Absolute.Vector_2d;
         Top_Point    : in     Vis.Absolute.Vector_2d;
         Space_Around : in     Vis.Absolute_Natural) is

         Line             : Vis.Logic.Vector_2d;
         Middle_Line_From : Vis.Logic.Vector_2d;
         Middle_Line_To   : Vis.Logic.Vector_2d;
         Inner_Line_From  : Vis.Logic.Vector_2d;
         Inner_Line_To    : Vis.Logic.Vector_2d;
         Outer_Line_From  : Vis.Logic.Vector_2d;
         Outer_Line_To    : Vis.Logic.Vector_2d;
         Direction        : Vis.Logic.Vector_2d;
         Co_Direction     : Vis.Logic.Vector_2d;
         Current_Top      : Vis.Logic_Float;
         Intersect_X      : Vis.Logic_Float;
         Outer_X          : Vis.Absolute_Int;
         Inner_X          : Vis.Absolute_Int;
         Min_X            : Vis.Absolute_Int;
         Max_X            : Vis.Absolute_Int;
         Current_Y        : Vis.Absolute_Int;
         Spacing          : Vis.Logic_Float := Vis.Logic_Float (Space_Around);
      begin
         pragma Assert (Get_Y (Bottom_Point) > Get_Y (Top_Point));
         Middle_Line_From := Vis.To_Logic (Bottom_Point);
         Middle_Line_To   := Vis.To_Logic (Top_Point);
         Line := Middle_Line_To - Middle_Line_From;
         Direction := Line / Numerics.Sqrt (Line * Line);
         --  Rotated to the right by Pi/2
         Co_Direction := Combine_Vector
           (X => Get_Y (Direction),
            Y => -Get_X (Direction));

         --  Add Spacing to middle line
         Middle_Line_From := Middle_Line_From - Spacing * Direction;
         Middle_Line_To   := Middle_Line_To   + Spacing * Direction;
         Line := Middle_Line_To - Middle_Line_From;

         --  Calculate outer and inner line in distance 'Spacing' to the
         --  middle line
         if Get_X (Direction) < 0.0 then
            Inner_Line_From := Middle_Line_From + Spacing * Co_Direction;
            Outer_Line_From := Middle_Line_From - Spacing * Co_Direction;
         else
            pragma Assert (Get_X (Direction) > 0.0);
            Inner_Line_From := Middle_Line_From - Spacing * Co_Direction;
            Outer_Line_From := Middle_Line_From + Spacing * Co_Direction;
         end if;
         pragma Assert (Get_Y (Outer_Line_From) <= Get_Y (Inner_Line_From));
         Inner_Line_To := Inner_Line_From + Line;
         Outer_Line_To := Outer_Line_From + Line;

         Outer_X := Get_Position_X_At_X
           (Manager,
            Vis.Absolute_Int (Get_X (Outer_Line_From)));
         Current_Y := Get_Position_Y_At_Y
           (Manager,
            Vis.Absolute_Int (Get_Y (Inner_Line_From)));
         loop
            Current_Top := Vis.Logic_Float
              (Get_Top (Get_Region_Extent
                          (Manager,
                           Combine_Vector (Outer_X, Current_Y)))) - 0.5;
            if Current_Top > Get_Y (Inner_Line_To) then
               --  inner line has intersection with Current_Top
               Intersect_X := Vis.Intersects_Line_Horizontal_Line_X
                 (Origin     => Inner_Line_From,
                  Direction  => Inner_Line_To - Inner_Line_From,
                  Horizontal => Current_Top);
               Inner_X := Get_Position_X_At_X
                 (Manager,
                  Vis.Absolute_Int (Intersect_X));
            else
               --  inner line does not intersect Current_Top
               --  use last X touched by inner line
               Inner_X := Get_Position_X_At_X
                 (Manager,
                  Vis.Absolute_Int (Get_X (Inner_Line_To)));
            end if;

            -- add Inner_X .. Outer_X, Current_Y
            Set_Min_Max
              (Value_1 => Inner_X,
               Value_2 => Outer_X,
               Min     => Min_X,
               Max     => Max_X);
            for X in Min_X .. Max_X loop
               Hit (Combine_Vector (X, Current_Y));
            end loop;

            --  Finish when outer line completely below Current_Top
            exit when Current_Top <= Get_Y (Outer_Line_To);

            if Current_Top < Get_Y (Outer_Line_From) then
               --  outer line has intersection with Current_Bottom
               Intersect_X := Vis.Intersects_Line_Horizontal_Line_X
                 (Origin     => Outer_Line_From,
                  Direction  => Outer_Line_To - Outer_Line_From,
                  Horizontal => Current_Top);
               Outer_X := Get_Position_X_At_X
                 (Manager,
                  Vis.Absolute_Int (Intersect_X));
            else
               --  outer line starts above Current_Top
               --  use old value of Outer_X
               null;
            end if;

            Current_Y := Current_Y - 1;
         end loop;
      end Add_Line_Bottom_To_Top;

      Pool     : Position_Pool;
      Iterator : Position_Iterator;
      Area     : Vis.Absolute.Rectangle_2d;
      Min_X    : Vis.Absolute_Int;
      Max_X    : Vis.Absolute_Int;
      Min_Y    : Vis.Absolute_Int;
      Max_Y    : Vis.Absolute_Int;
   begin
      if Vis.Absolute.Get_X (Start_Point) = Vis.Absolute.Get_X (End_Point) then
         --  Vertical line can be treated as rectangle
         Set_Min_Max
           (Value_1 => Vis.Absolute.Get_Y (Start_Point),
            Value_2 => Vis.Absolute.Get_Y (End_Point),
            Min     => Min_Y,
            Max     => Max_Y);
         Area := Vis.Absolute.Combine_Rectangle
           (X_1 => Vis.Absolute.Get_X (Start_Point) - (Thickness + 3) / 2,
            X_2 => Vis.Absolute.Get_X (Start_Point) + (Thickness + 3) / 2,
            Y_1 => Min_Y - (Thickness + 3) / 2,
            Y_2 => Max_Y + (Thickness + 3) / 2);

         Pool := Create_Position_Pool_From_Area (Manager, Area);
         Make_Position_Iterator (Pool, Iterator);
         while Has_More (Iterator) loop
            Hit (Get_Current (Iterator));
            Next (Iterator);
         end loop;
      elsif Vis.Absolute.Get_Y (Start_Point) = Vis.Absolute.Get_Y (End_Point)
      then
         --  Horizontal line can be treated as rectangle
         Set_Min_Max
           (Value_1 => Vis.Absolute.Get_X (Start_Point),
            Value_2 => Vis.Absolute.Get_X (End_Point),
            Min     => Min_X,
            Max     => Max_X);
         Area := Vis.Absolute.Combine_Rectangle
           (X_1 => Min_X - (Thickness + 3) / 2,
            X_2 => Max_X + (Thickness + 3) / 2,
            Y_1 => Vis.Absolute.Get_Y (Start_Point) - (Thickness + 3) / 2,
            Y_2 => Vis.Absolute.Get_Y (Start_Point) + (Thickness + 3) / 2);

         Pool := Create_Position_Pool_From_Area (Manager, Area);
         Make_Position_Iterator (Pool, Iterator);
         while Has_More (Iterator) loop
            Hit (Get_Current (Iterator));
            Next (Iterator);
         end loop;

      elsif Vis.Absolute.Get_Y (Start_Point) > Vis.Absolute.Get_Y (End_Point)
      then
         --  Ascending line
         Add_Line_Bottom_To_Top (Start_Point, End_Point, (Thickness + 3) / 2);
      else
         --  Descending line
         Add_Line_Bottom_To_Top (End_Point, Start_Point, (Thickness + 3) / 2);
      end if;
   end Add_Lines_Positions;

   procedure Insert_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id) is

      Positions : Position_Sets.Set := Position_Sets.Empty_Set;

      procedure Hit_Position
        (Position : in     Region_Position) is

         Inserted  : Boolean;
         Region    : Region_Id;
      begin
         Position_Sets.Insert
           (A_Set   => Positions,
            Element => Position,
            Is_New  => Inserted);

         if Inserted then
            Get_Region
              (Manager  => Manager,
               Position => Position,
               Region   => Region);

            Region_Lists.Attach (Region, Edge.Regions);
            Add_Edge_To_Region (Region, Edge);
            Add_Edge_Pollution (Region, Edge);
         end if;
      end Hit_Position;

      procedure Add_Edge_Line_Positions is new Add_Lines_Positions
        (Hit => Hit_Position);

      Pool     : Position_Pool;
      Iterator : Position_Iterator;
   begin
      for I in Edge.Points'First .. Edge.Points'Last - 1 loop
         Add_Edge_Line_Positions
           (Manager     => Manager,
            Thickness   => Edge.Thickness,
            Start_Point => Edge.Points (I),
            End_Point   => Edge.Points (I + 1));
      end loop;
      Add_Edge_Line_Positions
        (Manager     => Manager,
         Thickness   => Edge.Thickness,
         Start_Point => Edge.Left_Arrow_Point,
         End_Point   => Edge.Points (Edge.Points'Last));
      Add_Edge_Line_Positions
        (Manager     => Manager,
         Thickness   => Edge.Thickness,
         Start_Point => Edge.Right_Arrow_Point,
         End_Point   => Edge.Points (Edge.Points'Last));

      if Has_Text_Area (Edge) then
         Pool := Create_Position_Pool_From_Area (Manager, Edge.Text_Area);
         Make_Position_Iterator (Pool, Iterator);
         while Has_More (Iterator) loop
            Hit_Position (Get_Current (Iterator));
            Next (Iterator);
         end loop;
      end if;

      Position_Sets.Destroy (Positions);
   end Insert_Edge;

   function Has_Manager
     (Edge    : in     Vis_Edge_Id)
     return Boolean is
   begin
      return not Region_Lists.IsEmpty (Edge.Regions);
   end Has_Manager;

   procedure Change_Layer
     (Manager   : in out Region_Manager;
      Edge      : in     Vis_Edge_Id;
      New_Layer : in     Layer_Type) is

      Iterator : Region_Lists.ListIter;
      Region   : Region_Id;
   begin
      Iterator := Region_Lists.MakeListIter (Edge.Regions);
      while Region_Lists.More (Iterator) loop
         Region_Lists.Next (Iterator, Region);
         Remove_Edge_From_Region (Region, Edge);
      end loop;

      Set_Layer (Edge, New_Layer);

      Iterator := Region_Lists.MakeListIter (Edge.Regions);
      while Region_Lists.More (Iterator) loop
         Region_Lists.Next (Iterator, Region);
         Add_Edge_To_Region (Region, Edge);
      end loop;

      Pollute_Edge (Manager, Edge);
   end Change_Layer;

   procedure Drop_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id) is

      Region : Region_Id;
   begin
      while not Region_Lists.IsEmpty (Edge.Regions) loop
         Region := Region_Lists.FirstValue (Edge.Regions);
         Remove_Edge_From_Region (Region, Edge);
         Add_Background_Pollution (Region);
         Region_Lists.DeleteHead (Edge.Regions);
      end loop;
   end Drop_Edge;

   function Get_Edge_At
     (Manager : in     Region_Manager;
      Point   : in     Vis.Absolute.Vector_2d)
     return Vis_Edge_Id is

      Region     : Region_Id;
      Found_Edge : Vis_Edge_Id := null;
      Edges      : Vis_Edge_Sets.Iterator;
   begin
      Region := Get_Region_If_Exists
        (Manager, Get_Region_Position (Manager, Point));
      if Region /= null then
         Edges := Vis_Edge_Sets.Make_Reverse_Iterator (Region.Edges);
         while Found_Edge = null and then Vis_Edge_Sets.More (Edges) loop
            Found_Edge := Vis_Edge_Sets.Current (Edges);
            if not Hits (Found_Edge, Point) then
               Found_Edge := null;
            end if;
            Vis_Edge_Sets.Previous (Edges);
         end loop;
         Vis_Edge_Sets.Destroy (Edges);
      end if;
      return Found_Edge;
   end Get_Edge_At;

   procedure Insert_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id) is

      Pool     : Position_Pool;
      Iterator : Position_Iterator;
      Region   : Region_Id;
   begin
      Pool := Create_Position_Pool_From_Area (Manager, Get_Extent (Node));
      Make_Position_Iterator (Pool, Iterator);
      while Has_More (Iterator) loop
         Get_Region
           (Manager  => Manager,
            Position => Get_Current (Iterator),
            Region   => Region);
         Next (Iterator);
         Region_Lists.Attach (Region, Node.Regions);
         Add_Node_To_Region (Region, Node);
         Add_Node_Pollution (Region, Node);
      end loop;
   end Insert_Node;

   function Has_Manager
     (Node    : in     Vis_Node_Id)
     return Boolean is
   begin
      return not Region_Lists.IsEmpty (Node.Regions);
   end Has_Manager;

   procedure Change_Layer
     (Manager   : in out Region_Manager;
      Node      : in     Vis_Node_Id;
      New_Layer : in     Layer_Type) is

      Iterator : Region_Lists.ListIter;
      Region   : Region_Id;
   begin
      Iterator := Region_Lists.MakeListIter (Node.Regions);
      while Region_Lists.More (Iterator) loop
         Region_Lists.Next (Iterator, Region);
         Remove_Node_From_Region (Region, Node);
      end loop;

      Set_Layer (Node, New_Layer);

      Iterator := Region_Lists.MakeListIter (Node.Regions);
      while Region_Lists.More (Iterator) loop
         Region_Lists.Next (Iterator, Region);
         Add_Node_To_Region (Region, Node);
      end loop;

      Pollute_Node (Manager, Node);
   end Change_Layer;

   procedure Drop_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id) is

      Region : Region_Id;
   begin
      while not Region_Lists.IsEmpty (Node.Regions) loop
         Region := Region_Lists.FirstValue (Node.Regions);
         Remove_Node_From_Region (Region, Node);
         Add_Background_Pollution (Region);
         Region_Lists.DeleteHead (Node.Regions);
      end loop;
   end Drop_Node;

   function Get_Node_At
     (Manager : in     Region_Manager;
      Point   : in     Vis.Absolute.Vector_2d)
     return Vis_Node_Id is

      Region     : Region_Id;
      Found_Node : Vis_Node_Id := null;
      Nodes      : Vis_Node_Sets.Iterator;
   begin
      Region := Get_Region_If_Exists
        (Manager, Get_Region_Position (Manager, Point));
      if Region /= null then
         Nodes := Vis_Node_Sets.Make_Reverse_Iterator (Region.Nodes);
         while Found_Node = null and then Vis_Node_Sets.More (Nodes) loop
            Found_Node := Vis_Node_Sets.Current (Nodes);
            if not Hits (Found_Node, Point) then
               Found_Node := null;
            end if;
            Vis_Node_Sets.Previous (Nodes);
         end loop;
         Vis_Node_Sets.Destroy (Nodes);
      end if;
      return Found_Node;
   end Get_Node_At;

   procedure Pollute_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id) is

      Iterator       : Region_Lists.ListIter := Region_Lists.MakeListIter
        (Edge.Regions);
      Current_Region : Region_Id;
   begin
      while Region_Lists.More (Iterator) loop
         Current_Region := Region_Lists.CellValue (Iterator);
         Add_Edge_Pollution (Current_Region, Edge);
         Region_Lists.Forward (Iterator);
      end loop;
   end Pollute_Edge;

   procedure Pollute_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id) is

      Iterator       : Region_Lists.ListIter := Region_Lists.MakeListIter
        (Node.Regions);
      Current_Region : Region_Id;
   begin
      while Region_Lists.More (Iterator) loop
         Current_Region := Region_Lists.CellValue (Iterator);
         Add_Node_Pollution (Current_Region, Node);
         Region_Lists.Forward (Iterator);
      end loop;
   end Pollute_Node;

   procedure Pollute_Area
     (Manager : in out Region_Manager;
      Area    : in     Vis.Absolute.Rectangle_2d) is

      Pool     : Position_Pool;
      Iterator : Position_Iterator;
      Position : Region_Position;
      Region   : Region_Id;
   begin
      Pool := Create_Position_Pool_From_Area (Manager, Area);
      Make_Position_Iterator (Pool, Iterator);
      while Has_More (Iterator) loop
         Position := Get_Current (Iterator);
         Next (Iterator);
         Region := Get_Region_If_Exists (Manager, Position);
         --  If region exists then add pollution, else it is polluted by
         --  default --> do nothing
         if Region /= null then
            Add_Background_Pollution (Region);
         end if;
      end loop;
   end Pollute_Area;

   generic
      with procedure Action
        (Region : in     Region_Id);
   procedure Process_Regions_In_Area_Difference
     (Manager  : in     Region_Manager;
      Old_Area : in     Vis.Absolute.Rectangle_2d;
      New_Area : in     Vis.Absolute.Rectangle_2d);

   procedure Process_Regions_In_Area_Difference
     (Manager  : in     Region_Manager;
      Old_Area : in     Vis.Absolute.Rectangle_2d;
      New_Area : in     Vis.Absolute.Rectangle_2d) is

      Positions     : Position_Sets.Set := Position_Sets.Empty_Set;

      procedure Add_Positions_In_Area
        (Area      : in     Vis.Absolute.Rectangle_2d) is

         Pool     : Position_Pool;
         Iterator : Position_Iterator;
      begin
         Pool := Create_Position_Pool_From_Area
           (Manager  => Manager,
            Area     => Area);
         Make_Position_Iterator
           (Pool     => Pool,
            Iterator => Iterator);
         while Has_More (Iterator) loop
            Position_Sets.Insert (Positions, Get_Current (Iterator));
            Next (Iterator);
         end loop;
      end Add_Positions_In_Area;

      Iterator      : Position_Sets.Iterator;
      Region        : Region_Id;
   begin
      declare
         Areas_1 : Vis.Absolute.Rectangle_2d_Array :=
           Vis.Absolute."-" (New_Area, Old_Area);
         Areas_2 : Vis.Absolute.Rectangle_2d_Array :=
           Vis.Absolute."-" (Old_Area, New_Area);
      begin
         for I in Areas_1'Range loop
            Add_Positions_In_Area (Areas_1 (I));
         end loop;
         for I in Areas_2'Range loop
            Add_Positions_In_Area (Areas_2 (I));
         end loop;
      end;

      Iterator := Position_Sets.Make_Iterator (Positions);
      while Position_Sets.More (Iterator) loop
         Region := Get_Region_If_Exists
           (Manager  => Manager,
            Position => Position_Sets.Current (Iterator));
         if Region /= null then
            Action (Region);
         end if;
         Position_Sets.Next (Iterator);
      end loop;
      Position_Sets.Destroy (Iterator);
      Position_Sets.Destroy (Positions);
   end Process_Regions_In_Area_Difference;

   procedure Update_Area_Content_Merged_Edges
     (Edge_Merger  : access Edge_Update_Iterators.Merger_Type;
      Old_Area     : in     Vis.Absolute.Rectangle_2d;
      New_Area     : in     Vis.Absolute.Rectangle_2d;
      Add_Edges    :    out Vis_Edge_Lists.List;
      Remove_Edges :    out Vis_Edge_Lists.List) is

      Edge      : Vis_Edge_Id;
      Is_In_Old : Boolean;
      Is_In_New : Boolean;
   begin
      Add_Edges := Vis_Edge_Lists.Create;
      Remove_Edges := Vis_Edge_Lists.Create;

      while Edge_Update_Iterators.Has_More (Edge_Merger) loop
         Edge := Edge_Update_Iterators.Get_Current (Edge_Merger);

         Is_In_Old := Intersects (Edge, Old_Area);
         Is_In_New := Intersects (Edge, New_Area);

         if Is_In_Old and not Is_In_New then
            Vis_Edge_Lists.Attach (Edge, Remove_Edges);
         elsif not Is_In_Old and Is_In_New then
            Vis_Edge_Lists.Attach (Edge, Add_Edges);
         end if;

         Edge_Update_Iterators.Forward (Edge_Merger);
      end loop;
   end Update_Area_Content_Merged_Edges;

   procedure Update_Area_Content_Merged_Nodes
     (Node_Merger  : access Node_Update_Iterators.Merger_Type;
      Old_Area     : in     Vis.Absolute.Rectangle_2d;
      New_Area     : in     Vis.Absolute.Rectangle_2d;
      Add_Nodes    :    out Vis_Node_Lists.List;
      Remove_Nodes :    out Vis_Node_Lists.List) is

      Node      : Vis_Node_Id;
      Is_In_Old : Boolean;
      Is_In_New : Boolean;
   begin
      Add_Nodes := Vis_Node_Lists.Create;
      Remove_Nodes := Vis_Node_Lists.Create;

      while Node_Update_Iterators.Has_More (Node_Merger) loop
         Node := Node_Update_Iterators.Get_Current (Node_Merger);

         Is_In_Old := Intersects (Node, Old_Area);
         Is_In_New := Intersects (Node, New_Area);

         if Is_In_Old and not Is_In_New then
            Vis_Node_Lists.Attach (Node, Remove_Nodes);
         elsif not Is_In_Old and Is_In_New then
            Vis_Node_Lists.Attach (Node, Add_Nodes);
         end if;

         Node_Update_Iterators.Forward (Node_Merger);
      end loop;
   end Update_Area_Content_Merged_Nodes;

   procedure Update_Area_Content
     (Manager      : in     Region_Manager;
      Old_Area     : in     Vis.Absolute.Rectangle_2d;
      New_Area     : in     Vis.Absolute.Rectangle_2d;
      Add_Edges    :    out Vis_Edge_Lists.List;
      Remove_Edges :    out Vis_Edge_Lists.List;
      Add_Nodes    :    out Vis_Node_Lists.List;
      Remove_Nodes :    out Vis_Node_Lists.List) is

      package Edge_Iterator_Lists renames Edge_Update_Iterators.Iterator_Lists;
      package Node_Iterator_Lists renames Node_Update_Iterators.Iterator_Lists;
      All_Edge_Iterators : Edge_Iterator_Lists.List :=
        Edge_Iterator_Lists.Create;
      All_Node_Iterators : Node_Iterator_Lists.List :=
        Node_Iterator_Lists.Create;

      procedure Add_Region
        (Region : in     Region_Id) is
      begin
         Edge_Iterator_Lists.Attach
           (Vis_Edge_Sets.Make_Iterator (Region.Edges), All_Edge_Iterators);
         Node_Iterator_Lists.Attach
           (Vis_Node_Sets.Make_Iterator (Region.Nodes), All_Node_Iterators);
      end Add_Region;

      procedure Process_Regions is new Process_Regions_In_Area_Difference
        (Action => Add_Region);

      Edge_Merger : Edge_Update_Iterators.Merger_Access;
      Node_Merger : Node_Update_Iterators.Merger_Access;
   begin
      Process_Regions (Manager, Old_Area, New_Area);

      Edge_Merger := Edge_Update_Iterators.Create (All_Edge_Iterators);
      Edge_Iterator_Lists.Destroy (All_Edge_Iterators);
      Update_Area_Content_Merged_Edges
        (Edge_Merger, Old_Area, New_Area, Add_Edges, Remove_Edges);
      Edge_Update_Iterators.Destroy (Edge_Merger);

      Node_Merger := Node_Update_Iterators.Create (All_Node_Iterators);
      Node_Iterator_Lists.Destroy (All_Node_Iterators);
      Update_Area_Content_Merged_Nodes
        (Node_Merger, Old_Area, New_Area, Add_Nodes, Remove_Nodes);
      Node_Update_Iterators.Destroy (Node_Merger);
   end Update_Area_Content;

   procedure Update_Area_Content_Nodes
     (Manager      : in     Region_Manager;
      Old_Area     : in     Vis.Absolute.Rectangle_2d;
      New_Area     : in     Vis.Absolute.Rectangle_2d;
      Add_Nodes    :    out Vis_Node_Lists.List;
      Remove_Nodes :    out Vis_Node_Lists.List) is

      package Node_Iterator_Lists renames Node_Update_Iterators.Iterator_Lists;
      All_Node_Iterators : Node_Iterator_Lists.List :=
        Node_Iterator_Lists.Create;

      procedure Add_Region
        (Region : in     Region_Id) is
      begin
         Node_Iterator_Lists.Attach
           (Vis_Node_Sets.Make_Iterator (Region.Nodes), All_Node_Iterators);
      end Add_Region;

      procedure Process_Regions is new Process_Regions_In_Area_Difference
        (Action => Add_Region);

      Node_Merger : Node_Update_Iterators.Merger_Access;
   begin
      Process_Regions (Manager, Old_Area, New_Area);

      Node_Merger := Node_Update_Iterators.Create (All_Node_Iterators);
      Node_Iterator_Lists.Destroy (All_Node_Iterators);
      Update_Area_Content_Merged_Nodes
        (Node_Merger, Old_Area, New_Area, Add_Nodes, Remove_Nodes);
      Node_Update_Iterators.Destroy (Node_Merger);
   end Update_Area_Content_Nodes;

   procedure Start_Refresh_Operation
     (Manager         : in out Region_Manager;
      Refresh_Area    : in     Vis.Absolute.Rectangle_2d;
      Command         :    out Refresh_Command_Type;
      Refresh_Pending : in     Boolean) is

      procedure Add_Region
        (Region : in     Region_Id) is

         First_Edge      : Vis_Data.Vis_Edge_Id;
         First_Node      : Vis_Data.Vis_Node_Id;
         Clipping_Opened : Boolean := False;
         Edge_Iterator   : Vis_Edge_Sets.Iterator :=
           Get_Polluted_Edges (Region);
         Node_Iterator   : Vis_Node_Sets.Iterator :=
           Get_Polluted_Nodes (Region);
      begin
         if Is_Background_Polluted (Region) then
            --  Background must be refreshed
            Rectangle_2d_Lists.Attach
              (Get_Region_Extent (Region),
               Command.Reset);
            Clipping_Opened := True;
         end if;

         if Vis_Edge_Sets.More (Edge_Iterator) then
            if not Clipping_Opened then
               --  Clipping not opened by background --> must open now
               First_Edge := Vis_Edge_Sets.Current (Edge_Iterator);
               Clipping_Queues.Insert
                 (Queue => Command.Edge_Clipping.all,
                  Item  => new Layer_Clipping_Type'
                             (Height => Get_Layer (First_Edge),
                              Area   => Get_Region_Extent (Region)));
               Clipping_Opened := True;
            end if;

            Edge_Update_Iterators.Add_Iterator
              (Merger   => Command.Edges.all,
               Iterator => Edge_Iterator);
         else
            Vis_Edge_Sets.Destroy (Edge_Iterator);
         end if;

         if Vis_Node_Sets.More (Node_Iterator) then
            if not Clipping_Opened then
               --  Clipping not opened by lower item --> must open now
               First_Node := Vis_Node_Sets.Current (Node_Iterator);
               Clipping_Queues.Insert
                 (Queue => Command.Node_Clipping.all,
                  Item  => new Layer_Clipping_Type'
                             (Height => Get_Layer (First_Node),
                              Area   => Get_Region_Extent (Region)));
               Clipping_Opened := True;
            end if;

            Node_Update_Iterators.Add_Iterator
              (Merger   => Command.Nodes.all,
               Iterator => Node_Iterator);
         else
            Vis_Node_Sets.Destroy (Node_Iterator);
         end if;

         if not Clipping_Opened then
            --  No clipping has opened --> area unchanged
            Rectangle_2d_Lists.Attach
              (Get_Region_Extent (Region),
               Command.Unchanged);
         end if;

         if Refresh_Pending then
            Remove_All_Pollution (Region);
         end if;
      end Add_Region;

      Iterator     : Position_Iterator;
      Position     : Region_Position;
      Region       : Region_Id;
      Pool         : Position_Pool := Create_Position_Pool_From_Area
        (Manager, Refresh_Area);
      Region_Count : Natural := Get_Position_Pool_Size (Pool);
   begin
      Command :=
        (Reset         => Rectangle_2d_Lists.Create,
         Edge_Clipping => new Clipping_Queues.Queue_Type (Region_Count),
         Edges         => new Edge_Update_Iterators.Merger_Type (Region_Count),
         Node_Clipping => new Clipping_Queues.Queue_Type (Region_Count),
         Nodes         => new Node_Update_Iterators.Merger_Type (Region_Count),
         Unchanged     => Rectangle_2d_Lists.Create);

      Make_Position_Iterator (Pool, Iterator);
      while Has_More (Iterator) loop
         Position := Get_Current (Iterator);
         Next (Iterator);
         Region := Get_Region_If_Exists (Manager, Position);
         if Region /= null then
            Add_Region (Region);
         else
            --  'Positions' without 'Region_Id's will be refreshed by default
            Rectangle_2d_Lists.Attach
              (Get_Region_Extent (Manager, Position),
               Command.Reset);
         end if;
      end loop;

      Edge_Update_Iterators.Start_Iteration (Command.Edges.all);
      Node_Update_Iterators.Start_Iteration (Command.Nodes.all);
   end Start_Refresh_Operation;

   procedure End_Refresh_Operation
     (Command         : in out Refresh_Command_Type) is
   begin
      Rectangle_2d_Lists.Destroy (Command.Reset);
      Destroy_Clipping_Queue (Command.Edge_Clipping);
      Edge_Update_Iterators.Destroy (Command.Edges);
      Destroy_Clipping_Queue (Command.Node_Clipping);
      Node_Update_Iterators.Destroy (Command.Nodes);
      Rectangle_2d_Lists.Destroy (Command.Unchanged);
   end End_Refresh_Operation;


   -----------------------
   -- Region_Position,  --
   -- Position_Pool,    --
   -- Position_Iterator --
   -----------------------

   function Create_Position_Pool_From_Area
     (Manager      : in     Region_Manager;
      Area         : in     Vis.Absolute.Rectangle_2d)
     return Position_Pool is
   begin
      return Create_Position_Pool
        (Top_Left     => Get_Region_Position
                           (Manager, Vis.Absolute.Get_Top_Left (Area)),
         Bottom_Right => Get_Region_Position
                           (Manager, Vis.Absolute.Get_Bottom_Right (Area)));
   end Create_Position_Pool_From_Area;

   function Create_Position_Pool
     (Top_Left     : in     Region_Position;
      Bottom_Right : in     Region_Position)
     return Position_Pool is
   begin
      return Position_Pool (Vis.Absolute.Combine_Rectangle
        (Top_Left     => Vis.Absolute.Vector_2d (Top_Left),
         Bottom_Right => Vis.Absolute.Vector_2d (Bottom_Right)));
   end Create_Position_Pool;

   function Get_Position_Pool_Area
     (Manager      : in     Region_Manager;
      Pool         : in     Position_Pool)
     return Vis.Absolute.Rectangle_2d is

      Top_Left_Position : Region_Position     := Region_Position
        (Vis.Absolute.Get_Top_Left (Vis.Absolute.Rectangle_2d (Pool)));
      Bottom_Right_Position : Region_Position := Region_Position
        (Vis.Absolute.Get_Bottom_Right (Vis.Absolute.Rectangle_2d (Pool)));
   begin
      return Vis.Absolute.Combine_Rectangle
        (Top_Left     => Vis.Absolute.Get_Top_Left
           (Get_Region_Extent (Manager, Top_Left_Position)),
         Bottom_Right => Vis.Absolute.Get_Bottom_Right
           (Get_Region_Extent (Manager, Bottom_Right_Position)));
   end Get_Position_Pool_Area;

   function Get_Position_Pool_Size
     (Pool         : in     Position_Pool)
     return Natural is
   begin
      return Vis.Absolute.Get_Width (Vis.Absolute.Rectangle_2d (Pool)) *
        Vis.Absolute.Get_Height (Vis.Absolute.Rectangle_2d (Pool));
   end Get_Position_Pool_Size;

   procedure Make_Position_Iterator
     (Pool     : in     Position_Pool;
      Iterator :    out Position_Iterator) is
   begin
      Iterator :=
        (Current => Region_Position
          (Vis.Absolute.Get_Top_Left (Vis.Absolute.Rectangle_2d (Pool))),
         Pool    => Pool);
   end Make_Position_Iterator;

   function Has_More
     (Iterator : in     Position_Iterator)
     return Boolean is
   begin
      return Vis.Absolute.Get_X (Vis.Absolute.Vector_2d (Iterator.Current))
        <= Vis.Absolute.Get_Right (Vis.Absolute.Rectangle_2d (Iterator.Pool));
   end Has_More;

   function Get_Current
     (Iterator : in     Position_Iterator)
     return Region_Position is
   begin
      pragma Assert (Has_More (Iterator));
      return Iterator.Current;
   end Get_Current;

   procedure Next
     (Iterator : in out Position_Iterator) is

      Current_X : Vis.Absolute_Int := Vis.Absolute.Get_X
        (Vis.Absolute.Vector_2d (Iterator.Current));
      Current_Y : Vis.Absolute_Int := Vis.Absolute.Get_Y
        (Vis.Absolute.Vector_2d (Iterator.Current));
   begin
      if Current_X < Vis.Absolute.Get_Right
           (Vis.Absolute.Rectangle_2d (Iterator.Pool)) or else
         Current_Y >= Vis.Absolute.Get_Bottom
           (Vis.Absolute.Rectangle_2d (Iterator.Pool))
      then

         Vis.Absolute.Set_X
           (Vector => Vis.Absolute.Vector_2d (Iterator.Current),
            X      => Current_X + 1);
      else
         Vis.Absolute.Set_X
           (Vector => Vis.Absolute.Vector_2d (Iterator.Current),
            X      => Vis.Absolute.Get_Left
                        (Vis.Absolute.Rectangle_2d (Iterator.Pool)));
         Vis.Absolute.Set_Y
           (Vector => Vis.Absolute.Vector_2d (Iterator.Current),
            Y      => Current_Y + 1);
      end if;
   end Next;

end Giant.Vis_Data;
