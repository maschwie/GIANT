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
--  $RCSfile: giant-graph_widgets-states.adb,v $, $Revision: 1.16 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Gtk.Widget;
with Gdk.Window;

package body Giant.Graph_Widgets.States is


   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Cursors :=
        (Default => Get_Default_Default_Cursor,
         Waiting => Get_Default_Waiting_Cursor,
         Action  => Get_Default_Action_Cursor);
   end Set_Up;

   procedure Shut_Down
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Lock_Sets.Destroy (Widget.States.Locks);
   end Shut_Down;

   procedure Realized
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Update_Cursor (Widget => Widget, Force_Set => True);
   end Realized;


   -----------
   -- Areas --
   -----------

   procedure Logic_Area_Changed
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Logic_Area_Changed := True;
   end Logic_Area_Changed;

   procedure Logic_Area_Updated
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Logic_Area_Changed := False;
   end Logic_Area_Updated;

   function Must_Update_Logic_Area
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Logic_Area_Changed and then
        not Is_Locked (Widget);
   end Must_Update_Logic_Area;


   procedure Visual_Area_Changed
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Visual_Area_Changed := True;
   end Visual_Area_Changed;

   procedure Visual_Area_Updated
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Visual_Area_Changed := False;
   end Visual_Area_Updated;

   function Must_Update_Visual_Area
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Visual_Area_Changed and then
        not Is_Locked (Widget);
   end Must_Update_Visual_Area;


   ---------------------
   -- Drawing Ability --
   ---------------------

   procedure Enable_Drawing
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Drawing_Ready := True;
   end Enable_Drawing;

   procedure Disable_Drawing
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Drawing_Ready := False;
   end Disable_Drawing;


   -----------------
   -- Action Mode --
   -----------------

   procedure Enable_Action_Mode
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Action_Mode := True;
      Update_Cursor (Widget);
   end Enable_Action_Mode;

   procedure Disable_Action_Mode
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Action_Mode := False;
      Update_Cursor (Widget);
   end Disable_Action_Mode;

   function Is_Action_Mode_Current
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Action_Mode;
   end Is_Action_Mode_Current;


   -----------
   -- Locks --
   -----------

   procedure Create_New_Lock
     (Widget : access Graph_Widget_Record'Class;
      Lock   :    out Lock_Type) is
   begin
      Widget.States.Highest_Lock := Widget.States.Highest_Lock + 1;
      Lock := Widget.States.Highest_Lock;
      Lock_Sets.Insert
        (A_Set   => Widget.States.Locks,
         Element => Lock);
      Update_Cursor (Widget);
   end Create_New_Lock;

   procedure Destroy_Lock
     (Widget : access Graph_Widget_Record'Class;
      Lock   : in     Lock_Type) is

      Lock_Removed : Boolean;
   begin
      Lock_Sets.Remove_If_Exists
        (A_Set   => Widget.States.Locks,
         Element => Lock,
         Found   => Lock_Removed);
      if Lock_Sets.Is_Empty (Widget.States.Locks) then
         Widget.States.Highest_Lock := 0;
         if Lock_Removed then
            Widget.States.Lock_Flush_Pending := True;
         end if;
      end if;
   end Destroy_Lock;

   function Is_Locked
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return not Lock_Sets.Is_Empty (Widget.States.Locks);
   end Is_Locked;

   function Is_Valid_Lock
     (Widget : access Graph_Widget_Record'Class;
      Lock   : in     Lock_Type)
     return Boolean is
   begin
      return Lock_Sets.Is_Member (Widget.States.Locks, Lock);
   end Is_Valid_Lock;

   function Must_Flush_Locked_Content
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Lock_Flush_Pending and then
        Gtk.Widget.Realized_Is_Set (Widget);
   end Must_Flush_Locked_Content;

   procedure Flush_Locked_Content
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Lock_Flush_Pending := False;
      Update_Cursor (Widget);
   end Flush_Locked_Content;


   -------------------------
   --  Pollution tracking --
   -------------------------

   procedure Changed_Visual
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Visual_Polluted := True;
      Widget.States.Temporary_Changed := True;
   end Changed_Visual;

   procedure Updated_Visual
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Visual_Polluted := False;
   end Updated_Visual;

   procedure Changed_Temporary
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Temporary_Changed := True;
   end Changed_Temporary;

   procedure Updated_Temporary
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Temporary_Changed := False;
   end Updated_Temporary;


   -------------
   -- Cursors --
   -------------

   procedure Set_Default_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor) is
   begin
      Widget.States.Cursors (Default) := Cursor;
      Update_Cursor (Widget => Widget, Force_Set => True);
   end Set_Default_Cursor;

   procedure Set_Waiting_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor) is
   begin
      Widget.States.Cursors (Waiting) := Cursor;
      Update_Cursor (Widget => Widget, Force_Set => True);
   end Set_Waiting_Cursor;

   procedure Set_Action_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor) is
   begin
      Widget.States.Cursors (Action) := Cursor;
      Update_Cursor (Widget => Widget, Force_Set => True);
   end Set_Action_Cursor;


   procedure Update_Cursor
     (Widget    : access Graph_Widget_Record'Class;
      Force_Set : in     Boolean := False) is

      New_State : Cursor_State_Type;
   begin
      --  can set cursor only if there is a Gdk_Window
      if Gtk.Widget.Realized_Is_Set (Widget) then
         if Widget.States.Action_Mode then
            New_State := Action;
         elsif Is_Locked (Widget) then
            New_State := Waiting;
         else
            New_State := Default;
         end if;

         if Force_Set or New_State /= Widget.States.Current_Cursor then
            Widget.States.Current_Cursor := New_State;
            Gdk.Window.Set_Cursor
              (Get_Window (Widget),
               Widget.States.Cursors (Widget.States.Current_Cursor));
         end if;
      end if;
   end Update_Cursor;


   --------------------
   -- Mouse Handling --
   --------------------

   procedure Begin_Click_On_Background
     (Widget : access Graph_Widget_Record'Class;
      Point  : in     Vis.Absolute.Vector_2d) is
   begin
      Widget.States.Mouse_State := Clicking;
      Widget.States.Mouse_Origin := Point;
      Widget.States.Mouse_On_Edge := null;
      Widget.States.Mouse_On_Node := null;
   end Begin_Click_On_Background;

   procedure Begin_Click_On_Edge
     (Widget : access Graph_Widget_Record'Class;
      Point  : in     Vis.Absolute.Vector_2d;
      Edge   : in     Vis_Data.Vis_Edge_Id) is
   begin
      Widget.States.Mouse_State := Clicking;
      Widget.States.Mouse_Origin := Point;
      Widget.States.Mouse_On_Edge := Edge;
      Widget.States.Mouse_On_Node := null;
   end Begin_Click_On_Edge;

   procedure Begin_Click_On_Node
     (Widget : access Graph_Widget_Record'Class;
      Point  : in     Vis.Absolute.Vector_2d;
      Node   : in     Vis_Data.Vis_Node_Id) is
   begin
      Widget.States.Mouse_State := Clicking;
      Widget.States.Mouse_Origin := Point;
      Widget.States.Mouse_On_Edge := null;
      Widget.States.Mouse_On_Node := Node;
   end Begin_Click_On_Node;

   procedure End_Click
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Mouse_State := None;
      Widget.States.Mouse_On_Edge := null;
      Widget.States.Mouse_On_Node := null;
   end End_Click;

   function Get_Click_Edge
     (Widget : access Graph_Widget_Record'Class)
     return Vis_Data.Vis_Edge_Id is
   begin
      return Widget.States.Mouse_On_Edge;
   end Get_Click_Edge;

   function Get_Click_Node
     (Widget : access Graph_Widget_Record'Class)
     return Vis_Data.Vis_Node_Id is
   begin
      return Widget.States.Mouse_On_Node;
   end Get_Click_Node;

   function Get_Click_Point
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute.Vector_2d is
   begin
      return Widget.States.Mouse_Origin;
   end Get_Click_Point;

   function Is_Click_Current
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Mouse_State = Clicking;
   end Is_Click_Current;

   procedure Set_Mouse_Modifiers
     (Widget    : access Graph_Widget_Record'Class;
      Modifiers : in     Gdk.Types.Gdk_Modifier_Type) is
   begin
      Widget.States.Mouse_Modifiers := Modifiers;
   end Set_Mouse_Modifiers;

   function Get_Mouse_Modifiers
     (Widget    : access Graph_Widget_Record'Class)
     return Gdk.Types.Gdk_Modifier_Type is
   begin
      return Widget.States.Mouse_Modifiers;
   end Get_Mouse_Modifiers;

   function Get_Mouse_Move_Distance
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute.Vector_2d is
   begin
      return Vis.Absolute."-"
        (Widget.States.Mouse_Position, Widget.States.Mouse_Origin);
   end Get_Mouse_Move_Distance;

   procedure Set_Mouse_Position
     (Widget : access Graph_Widget_Record'Class;
      Point  : in     Vis.Absolute.Vector_2d) is
   begin
      Widget.States.Mouse_Position := Point;
   end Set_Mouse_Position;

   function Get_Mouse_Position
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute.Vector_2d is
   begin
      return Widget.States.Mouse_Position;
   end Get_Mouse_Position;


   procedure Begin_Rectangle
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Mouse_State := Rectangling;
   end Begin_Rectangle;

   procedure End_Rectangle
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Mouse_State := None;
   end End_Rectangle;

   function Is_Rectangle_Current
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Mouse_State = Rectangling;
   end Is_Rectangle_Current;


   procedure Begin_Drag
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Mouse_State := Dragging;
   end Begin_Drag;

   procedure End_Drag
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Mouse_State := None;
   end End_Drag;

   function Is_Drag_Current
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Mouse_State = Dragging;
   end Is_Drag_Current;


   procedure Begin_Move
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Mouse_State := Moving;
   end Begin_Move;

   procedure End_Move
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Mouse_State := None;
   end End_Move;

   function Is_Move_Current
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Mouse_State = Moving;
   end Is_Move_Current;



   procedure Begin_Auto_Scrolling
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Auto_Scrolling := True;
   end Begin_Auto_Scrolling;

   procedure End_Auto_Scrolling
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Auto_Scrolling := False;
   end End_Auto_Scrolling;

   function Is_Auto_Scrolling
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Auto_Scrolling;
   end Is_Auto_Scrolling;


   ---------------------
   -- State Inquiries --
   ---------------------

   function Must_Queue_Draw
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return not Is_Locked (Widget) and then
        (Has_Temporary_Changed (Widget) or else
         Has_Display_Changed (Widget));
   end Must_Queue_Draw;

   function Has_Display_Changed
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Visual_Polluted;
   end Has_Display_Changed;

   function Has_Temporary_Changed
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Temporary_Changed;
   end Has_Temporary_Changed;

   function Can_Resize
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Drawing_Ready and then
        Gtk.Widget.Realized_Is_Set (Widget);
   end Can_Resize;

   function Can_Move
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Drawing_Ready and then
        Gtk.Widget.Realized_Is_Set (Widget);
   end Can_Move;


   Default_Default_Cursor : Gdk.Cursor.Gdk_Cursor := Gdk.Cursor.Null_Cursor;
   Default_Waiting_Cursor : Gdk.Cursor.Gdk_Cursor := Gdk.Cursor.Null_Cursor;
   Default_Action_Cursor  : Gdk.Cursor.Gdk_Cursor := Gdk.Cursor.Null_Cursor;

   function Get_Default_Default_Cursor
     return Gdk.Cursor.Gdk_Cursor is
   begin
      if Gdk.Cursor."=" (Default_Waiting_Cursor, Gdk.Cursor.Null_Cursor) then
         Gdk.Cursor.Gdk_New
           (Widget      => Default_Waiting_Cursor,
            Cursor_Type => Gdk.Cursor.Left_Ptr);
      end if;
      return Default_Waiting_Cursor;
   end Get_Default_Default_Cursor;

   function Get_Default_Waiting_Cursor
     return Gdk.Cursor.Gdk_Cursor is
   begin
      if Gdk.Cursor."=" (Default_Action_Cursor, Gdk.Cursor.Null_Cursor) then
         Gdk.Cursor.Gdk_New
           (Widget      => Default_Action_Cursor,
            Cursor_Type => Gdk.Cursor.Watch);
      end if;
      return Default_Waiting_Cursor;
   end Get_Default_Waiting_Cursor;

   function Get_Default_Action_Cursor
     return Gdk.Cursor.Gdk_Cursor is
   begin
      if Gdk.Cursor."=" (Default_Action_Cursor, Gdk.Cursor.Null_Cursor) then
         Gdk.Cursor.Gdk_New
           (Widget      => Default_Action_Cursor,
            Cursor_Type => Gdk.Cursor.Crosshair);
      end if;
      return Default_Action_Cursor;
   end Get_Default_Action_Cursor;

end Giant.Graph_Widgets.States;
