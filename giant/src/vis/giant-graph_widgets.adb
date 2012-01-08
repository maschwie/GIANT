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
--  $RCSfile: giant-graph_widgets.adb,v $, $Revision: 1.58 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Ada.Unchecked_Deallocation;
with System;

with Glib;
with Glib.Object;
with Gtk.Object;
with Gtk.Widget;
pragma Elaborate_All (Gtk.Widget);

with Giant.Graph_Widgets.Callbacks;
with Giant.Graph_Widgets.Drawing;
with Giant.Graph_Widgets.Handlers;
with Giant.Graph_Widgets.Notifications;
with Giant.Graph_Widgets.Positioning;
with Giant.Graph_Widgets.Settings;
with Giant.Graph_Widgets.States;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);
with Giant.Simple_Priority_Queues;
pragma Elaborate_All (Giant.Simple_Priority_Queues);

package body Giant.Graph_Widgets is

   function "="
     (Left  : Vis_Data.Vis_Edge_Id;
      Right : Vis_Data.Vis_Edge_Id)
     return Boolean renames Vis_Data."=";

   function "="
     (Left  : Vis_Data.Vis_Node_Id;
      Right : Vis_Data.Vis_Node_Id)
     return Boolean renames Vis_Data."=";


   package Graph_Widget_Logger is new Logger
     (Name => "Giant.Graph_Widgets");


   package Graph_Edge_Sets renames Graph_Lib.Edge_Id_Sets;
   package Graph_Node_Sets renames Graph_Lib.Node_Id_Sets;


   ----------------------------------------------------------------------------
   --  Calls 'Action (Widget, I)' for each Item 'I' in 'Set'.
   generic
      type Object_Type is private;
      with function "<"
        (Left  : in Object_Type;
         Right : in Object_Type)
        return Boolean;
      with function "="
        (Left  : in Object_Type;
         Right : in Object_Type)
        return Boolean;
      with package Object_Sets is new Ordered_Sets
        (Item_Type => Object_Type,
         "<"       => "<",
         "="       => "=");
      with procedure Action
        (Widget : access Graph_Widget_Record'Class;
         Object : in     Object_Type);
   procedure For_All
     (Widget : access Graph_Widget_Record'Class;
      Set    : in     Object_Sets.Set);

   --  implementation
   procedure For_All
     (Widget : access Graph_Widget_Record'Class;
      Set    : in     Object_Sets.Set) is

      procedure For_One
        (Item : in     Object_Type) is
      begin
         Action (Widget, Item);
      end For_One;

      procedure Apply_For_All is new Object_Sets.Apply
        (Execute => For_One);

   begin
      Apply_For_All (Set);
   end For_All;


   ----------------------------------------------------------------------------
   --  Uses 'Convert' on each element in 'Set' and calls 'Action' for each
   --  conversion result
   generic
      type Object_Type is private;
      with function "<"
        (Left  : in Object_Type;
         Right : in Object_Type)
        return Boolean;
      with function "="
        (Left  : in Object_Type;
         Right : in Object_Type)
        return Boolean;
      with package Object_Sets is new Ordered_Sets
        (Item_Type => Object_Type,
         "<"       => "<",
         "="       => "=");
      type Converted_Type is private;
      with function Convert
        (Widget : access Graph_Widget_Record'Class;
         Source : in     Object_Type)
        return Converted_Type;
      with procedure Action
        (Widget : access Graph_Widget_Record'Class;
         Edge   : in     Converted_Type);
   procedure For_All_Conversion
     (Widget : access Graph_Widget_Record'Class;
      Set    : in     Object_Sets.Set);

   --  implementation
   procedure For_All_Conversion
     (Widget : access Graph_Widget_Record'Class;
      Set    : in     Object_Sets.Set) is

      procedure Call_Action
        (Widget : access Graph_Widget_Record'Class;
         Object : in     Object_Type) is
      begin
         Action (Widget, Convert (Widget, Object));
      end Call_Action;

      procedure Process is new For_All
        (Object_Type, "<", "=", Object_Sets, Call_Action);
   begin
      Process (Widget, Set);
   end For_All_Conversion;


   ----------------------------------------------------------------------------
   --  Looks up the 'Vis_Edge_Id's for the 'Edge_Id's an calls 'Action'
   --  on each.
   generic
      --  Edge may be null!
      with procedure Action
        (Widget : access Graph_Widget_Record'Class;
         Edge   : in     Vis_Data.Vis_Edge_Id);
   procedure For_All_Graph_Edges
     (Widget : access Graph_Widget_Record'Class;
      Edges  : in     Graph_Lib.Edge_Id_Sets.Set);

   --  implementation
   procedure For_All_Graph_Edges
     (Widget : access Graph_Widget_Record'Class;
      Edges  : in     Graph_Lib.Edge_Id_Sets.Set) is

      procedure Process is new For_All_Conversion
        (Object_Type    => Graph_Lib.Edge_Id,
         "<"            => Graph_Lib."<",
         "="            => Graph_Lib."=",
         Object_Sets    => Graph_Lib.Edge_Id_Sets,
         Converted_Type => Vis_Data.Vis_Edge_Id,
         Convert        => Look_Up,
         Action         => Action);

   begin
      Process (Widget, Edges);
   end For_All_Graph_Edges;

   ----------------------------------------------------------------------------
   --  Looks up the 'Vis_Node_Id's for the 'Node_Id's an calls 'Action'
   --  on each.
   generic
      --  Node may be null!
      with procedure Action
        (Widget : access Graph_Widget_Record'Class;
         Node   : in     Vis_Data.Vis_Node_Id);
   procedure For_All_Graph_Nodes
     (Widget : access Graph_Widget_Record'Class;
      Nodes  : in     Graph_Lib.Node_Id_Sets.Set);

   --  implementation
   procedure For_All_Graph_Nodes
     (Widget : access Graph_Widget_Record'Class;
      Nodes  : in     Graph_Lib.Node_Id_Sets.Set) is

      procedure Process is new For_All_Conversion
        (Object_Type    => Graph_Lib.Node_Id,
         "<"            => Graph_Lib."<",
         "="            => Graph_Lib."=",
         Object_Sets    => Graph_Lib.Node_Id_Sets,
         Converted_Type => Vis_Data.Vis_Node_Id,
         Convert        => Look_Up,
         Action         => Action);

   begin
      Process (Widget, Nodes);
   end For_All_Graph_Nodes;

   ----------------------------------------------------------------------------
   --  Calls 'Action' on every 'Edge' with 'Get_Target (Edge) = Node' or
   --  'Get_Source (Edge) = Node'.
   generic
      with procedure Action
        (Edge : in     Vis_Data.Vis_Edge_Id);
   procedure For_All_Incident_Edges
     (Node   : in     Vis_Data.Vis_Node_Id);

   --  implementation
   procedure For_All_Incident_Edges
     (Node   : in     Vis_Data.Vis_Node_Id) is

      Iterator : Vis_Edge_Lists.ListIter;
      Edge     : Vis_Data.Vis_Edge_Id;
   begin
      Vis_Data.Make_Incoming_Iterator (Node, Iterator);
      while Vis_Edge_Lists.More (Iterator) loop
         Vis_Edge_Lists.Next (Iterator, Edge);
         Action (Edge);
      end loop;
      Vis_Data.Make_Outgoing_Iterator (Node, Iterator);
      while Vis_Edge_Lists.More (Iterator) loop
         Vis_Edge_Lists.Next (Iterator, Edge);
         Action (Edge);
      end loop;
   end For_All_Incident_Edges;



   -------------------------------
   -- Construction, Destruction --
   -------------------------------

   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;

   ---------------------------------------------------------------------------
   --  Initializes the data structure. More initialization is done after the
   --  "realize" signal has been emitted on 'Widget'
   procedure Initialize
     (Widget       : access Graph_Widget_Record'Class;
      Style        : in     Config.Vis_Styles.Visualisation_Style_Access;
      Pool         : in     Node_Annotations.Node_Annotation_Access) is

      --  Needed since GNAT 3.15p. In 3.13p and 3.14p "or" worked on
      --  any two constants declared like
      --  Some_Constant : constant := 2**3;
      --  In 3.15p this was removed, thus must import the operation from 'Glib'
      function "or"
        (Left  : in     Glib.Guint32;
         Right : in     Glib.Guint32)
        return Glib.Guint32
        renames Glib."or";

   begin
      Gtk.Drawing_Area.Initialize (Widget);
      Gtk.Object.Initialize_Class_Record
        (Object                    => Widget,
         Signals                   => Handlers.Get_Signal_Array,
         Class_Record              => Class_Record,
         Type_Name                 => "Giant_Graph_Widget",
         Parameters                => Handlers.Get_Signal_Parameters);
      Gtk.Widget.Set_Scroll_Adjustments_Signal
        (Widget => Class_Record,
         Signal => Handlers.Set_Scroll_Adjustments_Signal);

      Set_Flags
        (Object => Widget,
         Flags  => Gtk.Widget.Can_Default or
                   Gtk.Widget.Can_Focus or
                   Gtk.Widget.Receives_Default);

      Widget.Logic_Area := Vis.Logic.Combine_Rectangle
        (Top_Left     => Vis.Logic.Zero_2d,
         Bottom_Right => Vis.Logic.Zero_2d);

      Widget.Selected_Edges := Vis_Edge_Sets.Empty_Set;
      Widget.Selected_Nodes := Vis_Node_Sets.Empty_Set;

      Widget.Unsized_Edges := Vis_Edge_Sets.Empty_Set;

      Widget.Locked_Nodes := Vis_Node_Lists.Create;
      Widget.Unsized_Nodes := Vis_Node_Lists.Create;

      Vis_Data.Set_Up (Widget.Manager);
      Widget.Edge_Map := Edge_Id_Mappings.Create;
      Vis_Data.Reset_Pool (Widget.Edge_Layers);
      Widget.Node_Map := Node_Id_Mappings.Create;
      Vis_Data.Reset_Pool (Widget.Node_Layers);

      Callbacks.Connect_All_Callbacks (Widget);

      States.Set_Up (Widget);
      Positioning.Set_Up (Widget, Default_Zoom_Level);
      --  Cannot set up yet, but must set values.
      Settings.Set_Style (Widget, Style);
      Settings.Set_Annotation_Pool (Widget, Pool);
   end Initialize;

   procedure Create
     (Widget      :    out Graph_Widget;
      Style       : in     Config.Vis_Styles.Visualisation_Style_Access :=
        Config.Vis_Styles.Get_Default_Vis_Style;
      Annotations : in     Node_Annotations.Node_Annotation_Access      :=
        Node_Annotations.Create_Empty) is
   begin
      Widget := new Graph_Widget_Record;
      Initialize (Widget, Style, Annotations);
   end Create;

   procedure Shut_Down_Graph_Widget
     (Widget : access Graph_Widget_Record'Class) is

      Edges : Edge_Id_Mappings.Values_Iter;
      Edge  : Vis_Data.Vis_Edge_Id;
      Nodes : Node_Id_Mappings.Values_Iter;
      Node  : Vis_Data.Vis_Node_Id;
   begin
      Vis_Data.Destroy (Widget.Manager);
      Positioning.Shut_Down (Widget);
      States.Shut_Down (Widget);

      Edges := Edge_Id_Mappings.Make_Values_Iter (Widget.Edge_Map);
      while Edge_Id_Mappings.More (Edges) loop
         Edge_Id_Mappings.Next (Edges, Edge);
         Vis_Data.Destroy (Edge);
      end loop;
      Edge_Id_Mappings.Destroy (Widget.Edge_Map);

      Nodes := Node_Id_Mappings.Make_Values_Iter (Widget.Node_Map);
      while Node_Id_Mappings.More (Nodes) loop
         Node_Id_Mappings.Next (Nodes, Node);
         Vis_Data.Destroy (Node);
      end loop;
      Node_Id_Mappings.Destroy (Widget.Node_Map);

      Vis_Node_Lists.Destroy (Widget.Locked_Nodes);
      Vis_Node_Lists.Destroy (Widget.Unsized_Nodes);

      Vis_Edge_Sets.Destroy (Widget.Unsized_Edges);

      Vis_Node_Sets.Destroy (Widget.Selected_Nodes);
      Vis_Edge_Sets.Destroy (Widget.Selected_Edges);
   end Shut_Down_Graph_Widget;

   procedure Read_Graph_Widget
     (Stream      : in     Bauhaus_IO.In_Stream_Type;
      Widget      :    out Graph_Widget;
      Style       : in     Config.Vis_Styles.Visualisation_Style_Access;
      Annotations : in     Node_Annotations.Node_Annotation_Access) is

      Lock           : Lock_Type;

      procedure Read_Node is
         Graph_Node   : Graph_Lib.Node_Id;
         Node         : Vis_Data.Vis_Node_Id;
         Hidden       : Boolean;
         Highlighting : Vis_Data.Highlight_Array;
         Position     : Vis.Logic.Vector_2d;
      begin
         Graph_Lib.Read_Node_Id (Stream, Graph_Node);
         Find_Or_Create (Widget, Graph_Node, Node);
         Bauhaus_IO.Read_Boolean (Stream, Hidden);
         Vis_Data.Set_Hidden (Node, Hidden);
         Vis_Data.Read_Highlight_Array (Stream, Highlighting);
         for Color in Highlighting'Range loop
            if Highlighting (Color) then
               Add_Node_Highlighting (Widget, Node, Color);
               if Vis_Data."=" (Color, Vis_Data.Current_Local) then
                  Add_Node_To_Selection (Widget, Node);
               end if;
            end if;
         end loop;
         Vis.Logic.Read_Vector (Stream, Position);
         Set_Top_Middle (Widget, Graph_Node, Position, Lock);
      end Read_Node;

      procedure Read_Edge is
         Graph_Edge : Graph_Lib.Edge_Id;
         Edge       : Vis_Data.Vis_Edge_Id;
         Hidden     : Boolean;
         Highlighting : Vis_Data.Highlight_Array;
      begin
         Graph_Lib.Read_Edge_Id (Stream, Graph_Edge);
         Find_Or_Create (Widget, Graph_Edge, Edge);
         Bauhaus_IO.Read_Boolean (Stream, Hidden);
         Vis_Data.Set_Hidden (Edge, Hidden);
         Vis_Data.Read_Highlight_Array (Stream, Highlighting);
         for Color in Highlighting'Range loop
            if Highlighting (Color) then
               Add_Edge_Highlighting (Widget, Edge, Color);
               if Vis_Data."=" (Color, Vis_Data.Current_Local) then
                  Add_Edge_To_Selection (Widget, Edge);
               end if;
            end if;
         end loop;
      end Read_Edge;

      Version_Number : Integer;
      Location       : Vis.Logic.Vector_2d;
      Level          : Vis.Zoom_Level;
      Edge_Count     : Natural;
      Node_Count     : Natural;
   begin
      Create (Widget, Style, Annotations);
      Bauhaus_IO.Read_Integer (Stream, Version_Number);

      Lock_All_Content (Widget, Lock);
      case Version_Number is
         when Current_Version_Number =>
            Vis.Logic.Read_Vector (Stream, Location);
            Vis.Read_Zoom_Level (Stream, Level);
            Bauhaus_IO.Read_Natural (Stream, Node_Count);
            for I in 1 .. Node_Count loop
               Read_Node;
            end loop;
            Bauhaus_IO.Read_Natural (Stream, Edge_Count);
            for I in 1 .. Edge_Count loop
               Read_Edge;
            end loop;
            Set_Location_And_Zoom_Level (Widget, Location, Level);
         when others =>
            Graph_Widget_Logger.Fatal
              ("Cannot read Graph Widget. Version number incorrect, expected" &
               Natural'Image (Current_Version_Number) & ", found " &
               Natural'Image (Version_Number) & ". Create empty Graph Widget"
               & " instead.");
      end case;
      Release_Lock (Widget, Lock);
   end Read_Graph_Widget;

   procedure Write_Graph_Widget
     (Stream : in     Bauhaus_IO.Out_Stream_Type;
      Widget : access Graph_Widget_Record) is

      package Vis_Edge_Priority_Queues is new Simple_Priority_Queues
        (Item_Type           => Vis_Data.Vis_Edge_Id,
         Has_Higher_Priority => Vis_Data.Is_Edge_Below);

      type Vis_Edge_Priority_Queue_Access is
        access Vis_Edge_Priority_Queues.Queue_Type;

      procedure Free is new Ada.Unchecked_Deallocation
        (Name   => Vis_Edge_Priority_Queue_Access,
         Object => Vis_Edge_Priority_Queues.Queue_Type);

      package Vis_Node_Priority_Queues is new Simple_Priority_Queues
        (Item_Type           => Vis_Data.Vis_Node_Id,
         Has_Higher_Priority => Vis_Data.Is_Node_Below);

      type Vis_Node_Priority_Queue_Access is
        access Vis_Node_Priority_Queues.Queue_Type;

      procedure Free is new Ada.Unchecked_Deallocation
        (Name   => Vis_Node_Priority_Queue_Access,
         Object => Vis_Node_Priority_Queues.Queue_Type);

      procedure Write_Node
        (Node : in     Vis_Data.Vis_Node_Id) is

         Only_Local_Mask : constant Vis_Data.Highlight_Array :=
           (Vis_Data.Local_Highlight_Type => True,
            others => False);
         Highlighting    : Vis_Data.Highlight_Array :=
           Vis_Data."and" (Vis_Data.Get_Highlighting (Node), Only_Local_Mask);
      begin
         Graph_Lib.Write_Node_Id
           (Stream, Vis_Data.Get_Graph_Node (Node));
         Bauhaus_IO.Write_Boolean
           (Stream, Vis_Data.Is_Hidden (Node));
         Vis_Data.Write_Highlight_Array
           (Stream, Highlighting);
         Vis.Logic.Write_Vector (Stream, Vis_Data.Get_Position (Node));
      end Write_Node;

      procedure Write_Edge
        (Edge : in     Vis_Data.Vis_Edge_Id) is

         Only_Local_Mask : constant Vis_Data.Highlight_Array :=
           (Vis_Data.Local_Highlight_Type => True,
            others => False);
         Highlighting    : Vis_Data.Highlight_Array :=
           Vis_Data."and" (Vis_Data.Get_Highlighting (Edge), Only_Local_Mask);
      begin
         Graph_Lib.Write_Edge_Id
           (Stream, Vis_Data.Get_Graph_Edge (Edge));
         Bauhaus_IO.Write_Boolean
           (Stream, Vis_Data.Is_Hidden (Edge));
         Vis_Data.Write_Highlight_Array
           (Stream, Highlighting);
      end Write_Edge;

   begin
      Bauhaus_IO.Write_Integer (Stream, Current_Version_Number);

      Vis.Logic.Write_Vector (Stream, Get_Location (Widget));
      Vis.Write_Zoom_Level (Stream, Get_Zoom_Level (Widget));

      declare
         Node          : Vis_Data.Vis_Node_Id;
         Node_Iterator : Node_Id_Mappings.Values_Iter;
         Node_Queue    : Vis_Node_Priority_Queue_Access :=
           new Vis_Node_Priority_Queues.Queue_Type
           (Node_Id_Mappings.Size (Widget.Node_Map));
      begin
         Node_Iterator := Node_Id_Mappings.Make_Values_Iter (Widget.Node_Map);
         while Node_Id_Mappings.More (Node_Iterator) loop
            Node_Id_Mappings.Next (Node_Iterator, Node);
            if not Vis_Data.Is_Obsolete (Node) then
               Vis_Node_Priority_Queues.Insert (Node_Queue.all, Node);
            end if;
         end loop;

         Bauhaus_IO.Write_Natural
           (Stream, Vis_Node_Priority_Queues.Get_Size (Node_Queue.all));
         while not Vis_Node_Priority_Queues.Is_Empty (Node_Queue.all) loop
            Node := Vis_Node_Priority_Queues.Get_Head (Node_Queue.all);
            Vis_Node_Priority_Queues.Remove_Head (Node_Queue.all);
            Write_Node (Node);
         end loop;

         Free (Node_Queue);
      end;

      declare
         Edge          : Vis_Data.Vis_Edge_Id;
         Edge_Iterator : Edge_Id_Mappings.Values_Iter;
         Edge_Queue    : Vis_Edge_Priority_Queue_Access :=
           new Vis_Edge_Priority_Queues.Queue_Type
           (Edge_Id_Mappings.Size (Widget.Edge_Map));
      begin
         Edge_Iterator := Edge_Id_Mappings.Make_Values_Iter (Widget.Edge_Map);
         while Edge_Id_Mappings.More (Edge_Iterator) loop
            Edge_Id_Mappings.Next (Edge_Iterator, Edge);
            if not Vis_Data.Is_Obsolete (Edge) then
               Vis_Edge_Priority_Queues.Insert (Edge_Queue.all, Edge);
            end if;
         end loop;

         Bauhaus_IO.Write_Natural
           (Stream, Vis_Edge_Priority_Queues.Get_Size (Edge_Queue.all));
         while not Vis_Edge_Priority_Queues.Is_Empty (Edge_Queue.all) loop
            Edge := Vis_Edge_Priority_Queues.Get_Head (Edge_Queue.all);
            Vis_Edge_Priority_Queues.Remove_Head (Edge_Queue.all);

            Write_Edge (Edge);
         end loop;

         Free (Edge_Queue);
      end;
   end Write_Graph_Widget;


   -------------------
   -- Configuration --
   -------------------

   procedure Set_Node_Annotations
     (Widget : access Graph_Widget_Record'Class;
      Pool   : in     Node_Annotations.Node_Annotation_Access) is

      Node      : Vis_Data.Vis_Node_Id;
      Old_State : Boolean;
      New_State : Boolean;
      Iterator  : Node_Id_Mappings.Values_Iter;
      Lock      : Lock_Type;
   begin
      Lock_All_Content (Widget, Lock);
      Settings.Set_Annotation_Pool (Widget, Pool);
      --  Update on all nodes
      Iterator := Node_Id_Mappings.Make_Values_Iter (Widget.Node_Map);
      while Node_Id_Mappings.More (Iterator) loop
         Node_Id_Mappings.Next (Iterator, Node);
         Old_State := Vis_Data.Is_Annotated (Node);
         New_State := Settings.Has_Annotation (Widget, Node);
         if Old_State /= New_State then
            Vis_Data.Set_Annotated (Node, New_State);
            Vis_Data.Pollute_Node (Widget.Manager, Node);
            States.Changed_Visual (Widget);
         end if;
      end loop;
      Release_Lock (Widget, Lock);
   end Set_Node_Annotations;

   procedure Set_Default_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor) is
   begin
      if Gdk.Cursor."/=" (Cursor, Gdk.Cursor.Null_Cursor) then
         States.Set_Default_Cursor (Widget, Cursor);
      end if;
   end Set_Default_Cursor;

   procedure Set_Waiting_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor) is
   begin
      if Gdk.Cursor."/=" (Cursor, Gdk.Cursor.Null_Cursor) then
         States.Set_Waiting_Cursor (Widget, Cursor);
      end if;
   end Set_Waiting_Cursor;

   procedure Set_Action_Mode_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor) is
   begin
      if Gdk.Cursor."/=" (Cursor, Gdk.Cursor.Null_Cursor) then
         States.Set_Action_Cursor (Widget, Cursor);
      end if;
   end Set_Action_Mode_Cursor;


   --------------------------------------------
   -- Insertion, Deletion of Edges and Nodes --
   --------------------------------------------

   function Contains
     (Widget   : access Graph_Widget_Record'Class;
      Edge     : in     Graph_Lib.Edge_Id)
     return Boolean is

      Vis_Edge : Vis_Data.Vis_Edge_Id;
   begin
      if Edge_Id_Mappings.Is_Bound (Widget.Edge_Map, Edge) then
         Vis_Edge := Edge_Id_Mappings.Fetch (Widget.Edge_Map, Edge);
         return not Vis_Data.Is_Obsolete (Vis_Edge);
      else
         return False;
      end if;
   end Contains;

   function Contains
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Graph_Lib.Node_Id)
     return Boolean is

      Vis_Node : Vis_Data.Vis_Node_Id;
   begin
      if Node_Id_Mappings.Is_Bound (Widget.Node_Map, Node) then
         Vis_Node := Node_Id_Mappings.Fetch (Widget.Node_Map, Node);
         return not Vis_Data.Is_Obsolete (Vis_Node);
      else
         return False;
      end if;
   end Contains;

   function Get_Content
     (Widget    : access Graph_Widget_Record'Class;
      Name      : in     String := "")
     return Graph_Lib.Subgraphs.Subgraph is

      package Subgraphs renames Graph_Lib.Subgraphs;
      Subgraph      : Subgraphs.Subgraph := Subgraphs.Create (Name);
      Edge_Iterator : Edge_Id_Mappings.Values_Iter;
      Edge          : Vis_Data.Vis_Edge_Id;
      Node_Iterator : Node_Id_Mappings.Values_Iter;
      Node          : Vis_Data.Vis_Node_Id;
   begin
      Node_Iterator := Node_Id_Mappings.Make_Values_Iter (Widget.Node_Map);
      while Node_Id_Mappings.More (Node_Iterator) loop
         Node_Id_Mappings.Next (Node_Iterator, Node);
         if not Vis_Data.Is_Obsolete (Node) then
            Subgraphs.Add_Node (Subgraph, Vis_Data.Get_Graph_Node (Node));
         end if;
      end loop;
      Edge_Iterator := Edge_Id_Mappings.Make_Values_Iter (Widget.Edge_Map);
      while Edge_Id_Mappings.More (Edge_Iterator) loop
         Edge_Id_Mappings.Next (Edge_Iterator, Edge);
         if not Vis_Data.Is_Obsolete (Edge) then
            Subgraphs.Add_Edge (Subgraph, Vis_Data.Get_Graph_Edge (Edge));
         end if;
      end loop;
      return Subgraph;
   end Get_Content;

   procedure Insert_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in     Graph_Lib.Selections.Selection;
      Lock      :    out Lock_Type) is

      Node_Iterator : Graph_Node_Sets.Iterator;
      Graph_Node    : Graph_Lib.Node_Id;
      Node          : Vis_Data.Vis_Node_Id;
      Edge_Iterator : Graph_Edge_Sets.Iterator;
      Graph_Edge    : Graph_Lib.Edge_Id;
      Edge          : Vis_Data.Vis_Edge_Id;
   begin
      States.Create_New_Lock (Widget, Lock);
      States.Changed_Visual (Widget);

      Node_Iterator := Graph_Node_Sets.Make_Iterator
        (Graph_Lib.Selections.Get_All_Nodes (Selection));
      while Graph_Node_Sets.More (Node_Iterator) loop
         Graph_Node_Sets.Next (Node_Iterator, Graph_Node);
         Find_Or_Create (Widget, Graph_Node, Node);
         --  In case the node was contained before and was queued for
         --  removal, that request must be dropped, but only after the
         --  removal request is propagated to all incident edges.
         --  A node queued for removal is always on the unsized queue and
         --  must not put onto it for a second time.
         if Vis_Data.Is_Obsolete (Node) then
            Mark_Incident_Obsolete (Widget, Node);
            Vis_Data.Set_Obsolete (Node, False);
         elsif Vis_Data.Has_Manager (Node) then
            Add_Node_To_Locked (Widget, Node);
         else
            Move_Node_To_Unsized (Widget, Node);
         end if;
      end loop;
      Graph_Node_Sets.Destroy (Node_Iterator);

      Edge_Iterator := Graph_Edge_Sets.Make_Iterator
        (Graph_Lib.Selections.Get_All_Edges (Selection));
      while Graph_Edge_Sets.More (Edge_Iterator) loop
         Graph_Edge_Sets.Next (Edge_Iterator, Graph_Edge);
         Find_Or_Create (Widget, Graph_Edge, Edge);
         if Edge /= null then
            --  In case the edge was queued for removal, that request must
            --  be dropped.
            Vis_Data.Set_Obsolete (Edge, False);
            if Vis_Data.Has_Manager (Edge) then
               Add_Edge_To_Locked (Widget, Edge);
            else
               --  Move_Edge_To_Unsized (Widget, Edge);
               --  Perform lazy assignment of size
               null;
            end if;
         else
            Graph_Widget_Logger.Error
              ("Could not insert edge because incident nodes are not "
               & "contained in graph widget. Ignoring...");

         end if;
      end loop;
   end Insert_Selection;

   procedure Insert_Selection_Difference
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in out Graph_Lib.Selections.Selection;
      Lock      :    out Lock_Type) is

      generic
         type Object_Type is private;
         with function "<"
           (Left  : in Object_Type;
            Right : in Object_Type)
           return Boolean;
         with function "="
           (Left  : in Object_Type;
            Right : in Object_Type)
           return Boolean;
         with package Object_Sets is new Ordered_Sets
           (Item_Type => Object_Type,
            "<"       => "<",
            "="       => "=");
         with function Is_Known
           (Widget : access Graph_Widget_Record'Class;
            Object : Object_Type)
           return Boolean;
         with procedure Remove_Set
           (Selection : in Graph_Lib.Selections.Selection;
            Set       : in Object_Sets.Set);
      procedure Remove_Known
        (Selection : in Graph_Lib.Selections.Selection;
         Set       : in Object_Sets.Set);

      procedure Remove_Known
        (Selection : in Graph_Lib.Selections.Selection;
         Set       : in Object_Sets.Set) is

         Known    : Object_Sets.Set      := Object_Sets.Empty_Set;
         Iterator : Object_Sets.Iterator := Object_Sets.Make_Iterator (Set);
         Current  : Object_Type;
      begin
         while Object_Sets.More (Iterator) loop
            Object_Sets.Next (Iterator, Current);
            if Is_Known (Widget, Current) then
               Object_Sets.Insert
                 (A_Set   => Known,
                  Element => Current);
            end if;
         end loop;
         Object_Sets.Destroy (Iterator);
         Remove_Set (Selection, Set);
         Object_Sets.Destroy (Known);
      end Remove_Known;

      procedure Remove_Edges is new Remove_Known
        (Object_Type  => Graph_Lib.Edge_Id,
         "="          => Graph_Lib."=",
         "<"          => Graph_Lib."<",
         Object_Sets  => Graph_Lib.Edge_Id_Sets,
         Is_Known     => Contains,
         Remove_Set   => Graph_Lib.Selections.Remove_Edge_Set);
      procedure Remove_Nodes is new Remove_Known
        (Object_Type  => Graph_Lib.Node_Id,
         "="          => Graph_Lib."=",
         "<"          => Graph_Lib."<",
         Object_Sets  => Graph_Lib.Node_Id_Sets,
         Is_Known     => Contains,
         Remove_Set   => Graph_Lib.Selections.Remove_Node_Set);

   begin
      Remove_Edges (Selection, Graph_Lib.Selections.Get_All_Edges (Selection));
      Remove_Nodes (Selection, Graph_Lib.Selections.Get_All_Nodes (Selection));
      Insert_Selection (Widget, Selection, Lock);
   end Insert_Selection_Difference;

   procedure Remove_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in     Graph_Lib.Selections.Selection) is

      Selection_Changed : Boolean := False;

      procedure Mark_Edge_For_Deletion
        (Widget : access Graph_Widget_Record'Class;
         Edge   : in     Vis_Data.Vis_Edge_Id) is
      begin
         if Edge /= null then
            if Is_Edge_In_Selection (Widget, Edge) then
               Remove_Edge_From_Selection (Widget, Edge);
               Selection_Changed := True;
            end if;
            Mark_Edge_Obsolete (Widget, Edge);
         end if;
      end Mark_Edge_For_Deletion;

      procedure Mark_Node_For_Deletion
        (Widget : access Graph_Widget_Record'Class;
         Node   : in     Vis_Data.Vis_Node_Id) is
      begin
         if Node /= null then
            if Is_Node_In_Selection (Widget, Node) then
               Remove_Node_From_Selection (Widget, Node);
               Selection_Changed := True;
            end if;
            Mark_Node_Obsolete (Widget, Node);
         end if;
      end Mark_Node_For_Deletion;

      procedure Mark_Edges_For_Deletion is new For_All_Graph_Edges
        (Action => Mark_Edge_For_Deletion);
      procedure Mark_Nodes_For_Deletion is new For_All_Graph_Nodes
        (Action => Mark_Node_For_Deletion);

      Lock : Lock_Type;
   begin
      Lock_All_Content (Widget, Lock);
      Mark_Edges_For_Deletion
        (Widget, Graph_Lib.Selections.Get_All_Edges (Selection));
      Mark_Nodes_For_Deletion
        (Widget, Graph_Lib.Selections.Get_All_Nodes (Selection));
      Release_Lock (Widget, Lock);
      Notify_Selection_Change (Widget);
   end Remove_Selection;

   procedure Clear
     (Widget : access Graph_Widget_Record'Class) is

      Selection : Graph_Lib.Selections.Selection;
   begin
      Selection := Graph_Lib.Subgraphs.Convert_To_Selection
        (Get_Content (Widget));
      Remove_Selection (Widget, Selection);
      Graph_Lib.Selections.Destroy (Selection);
   end Clear;


   -----------------
   -- Action Mode --
   -----------------

   procedure Start_Action_Mode
     (Widget : access Graph_Widget_Record'Class) is
   begin
      States.Enable_Action_Mode (Widget);
   end Start_Action_Mode;

   procedure Cancel_Action_Mode
     (Widget : access Graph_Widget_Record'Class) is
   begin
      States.Disable_Action_Mode (Widget);
   end Cancel_Action_Mode;

   function Is_Action_Mode_Active
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return States.Is_Action_Mode_Current (Widget);
   end Is_Action_Mode_Active;


   ------------
   -- Layout --
   ------------

   procedure Lock_All_Content
     (Widget    : access Graph_Widget_Record'Class;
      Lock      :    out Lock_Type) is
   begin
      States.Create_New_Lock (Widget, Lock);
   end Lock_All_Content;

   procedure Lock_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in     Graph_Lib.Selections.Selection;
      Lock      :    out Lock_Type) is
   begin
      --  Locking only one special set of edges and nodes is not yet possible.
      States.Create_New_Lock (Widget, Lock);
   end Lock_Selection;

   procedure Set_Top_Middle
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Graph_Lib.Node_Id;
      Location  : in     Vis.Logic.Vector_2d;
      Lock      : in     Lock_Type) is

      Vis_Node : Vis_Data.Vis_Node_Id;
   begin
      pragma Assert
        (Vis.Logic_Float'Safe_First <= Vis.Logic.Get_X (Location) and
         Vis.Logic.Get_X (Location) <= Vis.Logic_Float'Safe_Last);
      pragma Assert
        (Vis.Logic_Float'Safe_First <= Vis.Logic.Get_Y (Location) and
         Vis.Logic.Get_Y (Location) <= Vis.Logic_Float'Safe_Last);

      if States.Is_Locked (Widget) then
         --  Graph_Widget_Logger.Debug
         --    ("Set_Top_Middle: Node =" &
         --     Graph_Lib.Node_Id_Image (Node) &
         --     ", Location = " & Vis.Logic.Image (Location));
         pragma Assert (States.Is_Valid_Lock (Widget, Lock));
         Vis_Node := Look_Up (Widget, Node);
         if Vis_Node /= null then
            Add_Node_To_Locked (Widget, Vis_Node);
            Vis_Data.Set_Position (Vis_Node, Location);
            Add_Logic_Position (Widget, Location);
         else
            Graph_Widget_Logger.Fatal
              ("Set_Top_Middle called for unknown node: " &
               Graph_Lib.Node_Id_Image (Node) & "Request ignored.");
         end if;
      else
         Graph_Widget_Logger.Fatal
           ("Set_Top_Middle called while graph widget not locked! " &
            "Illegal Lock:" & Natural'Image (Natural (Lock)) &
            ". Request ignored.");
      end if;
   end Set_Top_Middle;
   pragma Inline (Set_Top_Middle);

   function Get_Top_Middle
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Graph_Lib.Node_Id)
     return Vis.Logic.Vector_2d is

      Vis_Node : Vis_Data.Vis_Node_Id := Look_Up (Widget, Node);
   begin
      if Vis_Node /= null then
         return Vis_Data.Get_Position (Vis_Node);
      else
         raise Unknown_Node_Id;
      end if;
   end Get_Top_Middle;
   pragma Inline (Get_Top_Middle);

   function Get_Current_Maximum_Node_Width
     (Widget    : access Graph_Widget_Record'Class)
     return Vis.Logic_Float is
   begin
      return Positioning.Get_Logic
        (Widget,
         Settings.Get_Node_Width (Widget) +
           2 * Drawing.Get_Maximum_Node_Highlight_Width (Widget));
   end Get_Current_Maximum_Node_Width;

   function Get_Current_Node_Width
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Graph_Lib.Node_Id)
     return Vis.Logic_Float is

      Vis_Node : Vis_Data.Vis_Node_Id := Look_Up (Widget, Node);
   begin
      if Vis_Node /= null then
         if Gtk.Widget.Realized_Is_Set (Widget) then
            if not Vis_Data.Is_Sized (Vis_Node) then
               --  must set size
               Drawing.Update_Node_Size (Widget, Vis_Node);
            end if;
            return Positioning.Get_Logic
              (Widget,
               Vis.Absolute.Get_Width (Vis_Data.Get_Extent (Vis_Node)));
         else
            --  Cannot determine correct size, give an estimate.
            return Positioning.Get_Logic
              (Widget, Settings.Get_Node_Width (Widget));
         end if;
      else
         raise Unknown_Node_Id;
      end if;
   end Get_Current_Node_Width;

   function Get_Current_Node_Height
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Graph_Lib.Node_Id)
     return Vis.Logic_Float is

      Vis_Node : Vis_Data.Vis_Node_Id := Look_Up (Widget, Node);
   begin
      if Vis_Node /= null then
         if Gtk.Widget.Realized_Is_Set (Widget) then
            if not Vis_Data.Is_Sized (Vis_Node) then
               --  must set size
               Drawing.Update_Node_Size (Widget, Vis_Node);
            end if;
            return Positioning.Get_Logic
              (Widget,
               Vis.Absolute.Get_Height (Vis_Data.Get_Extent (Vis_Node))) +
                 2.0 * Vis.Logic_Float
                 (Drawing.Get_Maximum_Node_Highlight_Width (Widget));
         else
            --  Cannot determine correct size, give an estimate
            return Positioning.Get_Logic
              (Widget,
               Settings.Get_Node_Attribute_Count (Widget, Vis_Node) * 16 + 99);
         end if;
      else
         raise Unknown_Node_Id;
      end if;
   end Get_Current_Node_Height;

   procedure Update_Node_Size
     (Widget    : access Graph_Widget_Record'Class;
      Node_List : in     Vis_Node_Lists.List) is

      Iterator : Vis_Node_Lists.ListIter;
      Node     : Vis_Data.Vis_Node_Id;
   begin
      Iterator := Vis_Node_Lists.MakeListIter (Node_List);
      while Vis_Node_Lists.More (Iterator) loop
         Vis_Node_Lists.Next (Iterator, Node);
         Drawing.Update_Node_Size (Widget, Node);
         Vis_Data.Set_Sized (Node, True);
      end loop;
   end Update_Node_Size;


   --  Settings must have been Set_Up
   procedure Flush_Locked
     (Widget : access Graph_Widget_Record'Class) is

      Node           : Vis_Data.Vis_Node_Id;
      Deferred_Nodes : Vis_Node_Lists.List := Vis_Node_Lists.Create;
      Edge_Set       : Vis_Edge_Sets.Set;
      Newly_Appeared : Vis_Edge_Sets.Set;
      Edge_Iterator  : Vis_Edge_Sets.Iterator;
      Edge           : Vis_Data.Vis_Edge_Id;
      Lock           : Lock_Type;
      Node_List      : Vis_Node_Lists.List;
   begin
      States.Flush_Locked_Content (Widget);
      States.Changed_Visual (Widget);

      Update_Node_Size
        (Widget    => Widget,
         Node_List => Widget.Unsized_Nodes);

      Node_List := Vis_Node_Lists.Attach
        (Widget.Unsized_Nodes, Widget.Locked_Nodes);
      Widget.Unsized_Nodes := Vis_Node_Lists.Create;
      Widget.Locked_Nodes := Vis_Node_Lists.Create;

      while not Vis_Node_Lists.IsEmpty (Node_List) loop
         Node := Vis_Node_Lists.FirstValue (Node_List);
         Vis_Node_Lists.DeleteHead (Node_List);

         if Vis_Data.Is_Obsolete (Node) then
            --  Apply deferred deletion
            Mark_Incident_Obsolete (Widget, Node);
            Destroy_Node (Widget, Node);
         else
            Positioning.Update_Node_Position (Widget, Node);
            Add_Logic_Area_Rectangle
              (Widget,
               Positioning.Get_Logic (Widget, Vis_Data.Get_Extent (Node)));
            Vis_Data.Insert_Node (Widget.Manager, Node);
            Vis_Data.Set_Locked (Node, False);

            if Is_In_Visible_Area (Widget, Node) then
               --  Must process incident edges, cannot do so before node
               --  positioning is completed.
               Vis_Data.Set_Incident_Visible (Node, True);
               Vis_Node_Lists.Attach (Node, Deferred_Nodes);
            else
               Vis_Data.Set_Incident_Visible (Node, False);
            end if;
         end if;
      end loop;
      Vis_Node_Lists.Destroy (Node_List);

      Switch_Incident_Visible (Widget, Deferred_Nodes, Newly_Appeared);
      Vis_Node_Lists.Destroy (Deferred_Nodes);

      Edge_Set := Vis_Edge_Sets.Copy (Newly_Appeared);

      --  Add the unsized queue
      Edge_Iterator := Vis_Edge_Sets.Make_Iterator (Widget.Unsized_Edges);
      while Vis_Edge_Sets.More (Edge_Iterator) loop
         Vis_Edge_Sets.Next (Edge_Iterator, Edge);

         if Vis_Data.Is_Obsolete (Edge) then
            --  Apply deferred deletion
            Destroy_Edge (Widget, Edge);
         elsif Vis_Data.Must_Be_Visible (Edge) and then
           not Vis_Data.Has_Manager (Edge) then
            --  If an edge was on unsized queue and then appeared, then
            --  it is still on unsized queue and in the region manager.
            --  In this case this edge must not be inserted again.

            Vis_Edge_Sets.Insert (Edge_Set, Edge);
         end if;

         --pragma Assert
         --  (Vis_Data.Must_Be_Visible (Edge) or
         --   not Vis_Data.Has_Manager (Edge));

      end loop;
      Vis_Edge_Sets.Destroy (Edge_Iterator);

      Insert_Incident_Edges
        (Widget => Widget,
         Edges  => Edge_Set);
      Vis_Edge_Sets.Remove_All (Widget.Unsized_Edges);
      Vis_Edge_Sets.Destroy (Edge_Set);

      if not Vis_Edge_Sets.Is_Empty (Newly_Appeared) then
         --  New edges have appeared. Must flush again.
         Lock_All_Content (Widget, Lock);
         Callbacks.Edges_Appeared (Widget, Newly_Appeared);
         Vis_Edge_Sets.Destroy (Newly_Appeared);
         Release_Lock (Widget, Lock);
         --  Exit now because this function will be called recursively.
         --  Recursion terminates if 'Callbacks.Edges_Appeared' does not
         --  again make new edges appear, which it doesn't (hopefully)
      else
         --  No new edges have appeared
         Vis_Edge_Sets.Destroy (Newly_Appeared);
         Redraw (Widget);
      end if;
   end Flush_Locked;

   procedure Release_Lock
     (Widget    : access Graph_Widget_Record'Class;
      Lock      : in     Lock_Type) is

      Must_Update_Scrollbars : Boolean := False;
   begin
      States.Destroy_Lock (Widget, Lock);
      if States.Must_Flush_Locked_Content (Widget) then
         Flush_Locked (Widget);
      end if;
      if States.Must_Update_Logic_Area (Widget) then
         Notifications.Logical_Area_Changed (Widget, Widget.Logic_Area);
         States.Logic_Area_Updated (Widget);
         Must_Update_Scrollbars := True;
      end if;
      if States.Must_Update_Visual_Area (Widget) then
         Notifications.Visible_Area_Changed
           (Widget, Get_Visible_Area (Widget));
         States.Visual_Area_Updated (Widget);
         Must_Update_Scrollbars := True;
      end if;
      if Must_Update_Scrollbars then
         Callbacks.Update_Scrollbars (Widget);
      end if;
   end Release_Lock;


   ---------------------------------------
   -- Layout manipulations without lock --
   ---------------------------------------

   --  'Node' must be locked
   procedure Move_Node
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id;
      Offset : in     Vis.Logic.Vector_2d) is

      Old_Position : Vis.Logic.Vector_2d := Vis_Data.Get_Position (Node);
      New_Position : Vis.Logic.Vector_2d :=
        Vis.Logic."+" (Old_Position, Offset);
   begin
      Add_Node_To_Locked (Widget, Node);
      Vis_Data.Set_Position (Node, New_Position);
      Add_Logic_Position (Widget, New_Position);
   end Move_Node;

   procedure Move_Nodes
     (Widget : access Graph_Widget_Record'Class;
      Nodes  : in     Vis_Node_Sets.Set;
      Offset : in     Vis.Logic.Vector_2d) is

      procedure Move_One
        (Widget : access Graph_Widget_Record'Class;
         Node   : in     Vis_Data.Vis_Node_Id) is
      begin
         Move_Node (Widget, Node, Offset);
      end Move_One;

      procedure Move_All is new For_All
        (Object_Type => Vis_Data.Vis_Node_Id,
         "<"         => Vis_Data.Is_Node_Below,
         "="         => Vis_Data."=",
         Object_Sets => Vis_Node_Sets,
         Action      => Move_One);

      Lock : Lock_Type;
   begin
      Lock_All_Content (Widget, Lock);
      Move_All
        (Widget => Widget,
         Set    => Nodes);
      Release_Lock (Widget, Lock);
   end Move_Nodes;

   procedure Make_Room
     (Widget    : access Graph_Widget_Record'Class;
      Center    : in     Vis.Logic.Vector_2d;
      Width     : in     Vis.Logic_Float;
      Height    : in     Vis.Logic_Float) is

      use Vis.Logic;
      Lock            : Lock_Type;
      Graph_Node      : Graph_Lib.Node_Id;
      Node_Iterator   : Node_Id_Mappings.Keys_Iter;
      Corner_Gradient : Vis.Logic_Float;
      Gradient        : Vis.Logic_Float;
      F               : Vis.Logic_Float;
      Position        : Vis.Logic.Vector_2d;
      Distance        : Vis.Logic.Vector_2d;
      Away            : Vis.Logic.Vector_2d;
   begin
      if Width <= 0.0 or else Height <= 0.0 then
         return;
      end if;

      Lock_All_Content (Widget, Lock);

      Corner_Gradient := abs (Height / Width);

      Node_Iterator := Node_Id_Mappings.Make_Keys_Iter (Widget.Node_Map);
      while Node_Id_Mappings.More (Node_Iterator) loop
         Node_Id_Mappings.Next (Node_Iterator, Graph_Node);

         Position := Get_Top_Middle (Widget, Graph_Node);
         Distance := Position - Center;
         if Get_X (Distance) = 0.0 then
            if Get_Y (Distance) >= 0.0 then
               Away := Combine_Vector (0.0, Height / 2.0);
            else
               Away := Combine_Vector (0.0, -Height / 2.0);
            end if;
         elsif Get_Y (Distance) = 0.0 then
            if Get_X (Distance) > 0.0 then
               Away := Combine_Vector (Width / 2.0, 0.0);
            else
               Away := Combine_Vector (-Width / 2.0, 0.0);
            end if;
         else
            Gradient := Get_Y (Distance) / Get_X (Distance);
            if abs Gradient >= Corner_Gradient then
               --  vertical
               F := abs (Height / (2.0 * Get_Y (Distance)));
            else
               --  horizontal
               F := abs (Width / (2.0 * Get_X (Distance)));
            end if;
            Away := F * Distance;
         end if;

         Set_Top_Middle (Widget, Graph_Node, Position + Away, Lock);
      end loop;

      Release_Lock (Widget, Lock);
   end Make_Room;

   procedure Move_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in     Graph_Lib.Selections.Selection;
      Move      : in     Vis.Logic.Vector_2d) is

      procedure Move_One
        (Widget : access Graph_Widget_Record'Class;
         Node   : in     Vis_Data.Vis_Node_Id) is
      begin
         Move_Node (Widget, Node, Move);
      end Move_One;

      procedure Move_All is new For_All_Graph_Nodes (Action => Move_One);

      Lock : Lock_Type;
   begin
      Lock_All_Content (Widget, Lock);
      Move_All
        (Widget => Widget,
         Nodes  => Graph_Lib.Selections.Get_All_Nodes (Selection));
      Release_Lock (Widget, Lock);
   end Move_Selection;


   --------------------------
   -- Visualization Styles --
   --------------------------

   function Get_Vis_Style
     (Widget     : access Graph_Widget_Record'Class)
      return Config.Vis_Styles.Visualisation_Style_Access is
   begin
      return Settings.Get_Style (Widget);
   end Get_Vis_Style;

   procedure Set_Vis_Style
     (Widget     : access Graph_Widget_Record'Class;
      Style      : in     Config.Vis_Styles.Visualisation_Style_Access) is

      Lock : Lock_Type;
   begin
      Lock_All_Content (Widget, Lock);
      Move_All_Nodes_To_Unsized (Widget);
      --  Move_All_Edges_To_Unsized (Widget);
      Settings.Set_Style (Widget, Style);
      Release_Lock (Widget, Lock);
   end Set_Vis_Style;


   ------------------
   -- Highlighting --
   ------------------

   function To_Local_Highlight_Type
     (Color      : in     Config.Global_Data.Selection_High_Light_ID)
     return Vis_Data.Local_Highlight_Type is
   begin
      case Color is
         when Config.Global_Data.Current_Selection =>
            return Vis_Data.Current_Local;
         when Config.Global_Data.Color_1 =>
            return Vis_Data.First_Local;
         when Config.Global_Data.Color_2 =>
            return Vis_Data.Second_Local;
         when Config.Global_Data.Color_3 =>
            return Vis_Data.Third_Local;
      end case;
   end To_Local_Highlight_Type;

   function To_Global_Highlight_Type
     (Color      : in     Config.Global_Data.Subgraph_High_Light_ID)
     return Vis_Data.Global_Highlight_Type is
   begin
      case Color is
         when Config.Global_Data.Color_1 =>
            return Vis_Data.First_Global;
         when Config.Global_Data.Color_2 =>
            return Vis_Data.Second_Global;
         when Config.Global_Data.Color_3 =>
            return Vis_Data.Third_Global;
      end case;
   end To_Global_Highlight_Type;

   --  Precondition:
   --    'Edge' must be locked
   procedure Add_Edge_Highlighting
     (Widget    : access Graph_Widget_Record'Class;
      Edge      : in     Vis_Data.Vis_Edge_Id;
      Highlight : in     Vis_Data.Highlight_Type) is

      Current : Vis_Data.Highlight_Array;
   begin
      pragma Assert (States.Is_Locked (Widget));
      Current := Vis_Data.Get_Highlighting (Edge);
      if not Current (Highlight) then
         Move_Edge_To_Unsized (Widget, Edge);
         Vis_Data.Add_Highlight_Color (Edge, Highlight);
      end if;
   end Add_Edge_Highlighting;

   --  Precondition:
   --    'Node' must be locked
   procedure Add_Node_Highlighting
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Vis_Data.Vis_Node_Id;
      Highlight : in     Vis_Data.Highlight_Type) is

      Current : Vis_Data.Highlight_Array;
   begin
      pragma Assert (States.Is_Locked (Widget));
      Current := Vis_Data.Get_Highlighting (Node);
      if not Current (Highlight) then
         if Highlight in Vis_Data.Global_Highlight_Type then
            Move_Node_To_Unsized (Widget, Node);
            Vis_Data.Add_Highlight_Color (Node, Highlight);
         else
            Vis_Data.Add_Highlight_Color (Node, Highlight);
            Vis_Data.Pollute_Node (Widget.Manager, Node);
         end if;
      end if;
   end Add_Node_Highlighting;

   --  Precondition:
   --    'Edge' must be locked
   procedure Remove_Edge_Highlighting
     (Widget    : access Graph_Widget_Record'Class;
      Edge      : in     Vis_Data.Vis_Edge_Id;
      Highlight : in     Vis_Data.Highlight_Type) is

      Current : Vis_Data.Highlight_Array;
   begin
      pragma Assert (States.Is_Locked (Widget));
      Current := Vis_Data.Get_Highlighting (Edge);
      if Current (Highlight) then
         Move_Edge_To_Unsized (Widget, Edge);
         Vis_Data.Remove_Highlight_Color (Edge, Highlight);
      end if;
   end Remove_Edge_Highlighting;

   --  Precondition:
   --    'Node' must be locked
   procedure Remove_Node_Highlighting
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Vis_Data.Vis_Node_Id;
      Highlight : in     Vis_Data.Highlight_Type) is

      Current : Vis_Data.Highlight_Array;
   begin
      pragma Assert (States.Is_Locked (Widget));
      Current := Vis_Data.Get_Highlighting (Node);
      if Current (Highlight) then
         if Highlight in Vis_Data.Local_Highlight_Type then
            Vis_Data.Remove_Highlight_Color (Node, Highlight);
            Vis_Data.Pollute_Node (Widget.Manager, Node);
         else
            Move_Node_To_Unsized (Widget, Node);
            Vis_Data.Remove_Highlight_Color (Node, Highlight);
         end if;
      end if;
   end Remove_Node_Highlighting;

   procedure Add_Local_Highlighting
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in     Graph_Lib.Selections.Selection;
      Color     : in     Config.Global_Data.Selection_High_Light_ID) is

      Highlight : constant Vis_Data.Local_Highlight_Type :=
        To_Local_Highlight_Type (Color);

      procedure Add_Local_Edge_Highlighting
        (Widget : access Graph_Widget_Record'Class;
         Edge   : in     Vis_Data.Vis_Edge_Id) is
      begin
         if Edge /= null then
            Add_Edge_Highlighting (Widget, Edge, Highlight);
         --  else ignore
         end if;
      end Add_Local_Edge_Highlighting;

      procedure Add_Local_Node_Highlighting
        (Widget : access Graph_Widget_Record'Class;
         Node   : in     Vis_Data.Vis_Node_Id) is
      begin
         if Node /= null then
            Add_Node_Highlighting (Widget, Node, Highlight);
         --  else ignore
         end if;
      end Add_Local_Node_Highlighting;

      procedure Add_Edge_To_Selection_Selective
        (Widget : access Graph_Widget_Record'Class;
         Edge   : in     Vis_Data.Vis_Edge_Id) is
      begin
         if Edge /= null then
            Add_Edge_To_Selection (Widget, Edge);
         --  else ignore
         end if;
      end Add_Edge_To_Selection_Selective;

      procedure Add_Node_To_Selection_Selective
        (Widget : access Graph_Widget_Record'Class;
         Node   : in     Vis_Data.Vis_Node_Id) is
      begin
         if Node /= null then
            Add_Node_To_Selection (Widget, Node);
         --  else ignore
         end if;
      end Add_Node_To_Selection_Selective;

      procedure Local_Highlight_Edges is new For_All_Graph_Edges
        (Action => Add_Local_Edge_Highlighting);
      procedure Local_Highlight_Nodes is new For_All_Graph_Nodes
        (Action => Add_Local_Node_Highlighting);

      procedure Register_Selected_Edges is new For_All_Graph_Edges
        (Action => Add_Edge_To_Selection_Selective);
      procedure Register_Selected_Nodes is new For_All_Graph_Nodes
        (Action => Add_Node_To_Selection_Selective);

      Lock : Lock_Type;
   begin
      Lock_All_Content (Widget, Lock);
      Local_Highlight_Nodes
        (Widget => Widget,
         Nodes  => Graph_Lib.Selections.Get_All_Nodes (Selection));
      Local_Highlight_Edges
        (Widget => Widget,
         Edges  => Graph_Lib.Selections.Get_All_Edges (Selection));

      if Vis_Data."=" (Highlight, Vis_Data.Current_Local) then
         --  Update selection if correct color set.
         Register_Selected_Edges
           (Widget => Widget,
            Edges  => Graph_Lib.Selections.Get_All_Edges (Selection));
         Register_Selected_Nodes
           (Widget => Widget,
            Nodes  => Graph_Lib.Selections.Get_All_Nodes (Selection));
      end if;
      Release_Lock (Widget, Lock);
   end Add_Local_Highlighting;

   procedure Remove_Local_Highlighting
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in     Graph_Lib.Selections.Selection;
      Color     : in     Config.Global_Data.Selection_High_Light_ID) is

      Highlight : constant Vis_Data.Local_Highlight_Type :=
        To_Local_Highlight_Type (Color);

      procedure Remove_Local_Edge_Highlighting
        (Widget : access Graph_Widget_Record'Class;
         Edge   : in     Vis_Data.Vis_Edge_Id) is
      begin
         if Edge /= null then
            Remove_Edge_Highlighting (Widget, Edge, Highlight);
         --  else ignore
         end if;
      end Remove_Local_Edge_Highlighting;

      procedure Remove_Local_Node_Highlighting
        (Widget : access Graph_Widget_Record'Class;
         Node   : in     Vis_Data.Vis_Node_Id) is
      begin
         if Node /= null then
            Remove_Node_Highlighting (Widget, Node, Highlight);
         --  else ignore
         end if;
      end Remove_Local_Node_Highlighting;

      procedure Remove_Edge_From_Selection_Selective
        (Widget : access Graph_Widget_Record'Class;
         Edge   : in     Vis_Data.Vis_Edge_Id) is
      begin
         if Edge /= null then
            Remove_Edge_From_Selection (Widget, Edge);
         --  else ignore
         end if;
      end Remove_Edge_From_Selection_Selective;

      procedure Remove_Node_From_Selection_Selective
        (Widget : access Graph_Widget_Record'Class;
         Node   : in     Vis_Data.Vis_Node_Id) is
      begin
         if Node /= null then
            Remove_Node_From_Selection (Widget, Node);
         --  else ignore
         end if;
      end Remove_Node_From_Selection_Selective;

      procedure Local_Unhighlight_Edges is new For_All_Graph_Edges
        (Action => Remove_Local_Edge_Highlighting);
      procedure Local_Unhighlight_Nodes is new For_All_Graph_Nodes
        (Action => Remove_Local_Node_Highlighting);

      procedure Unregister_Selected_Edges is new For_All_Graph_Edges
        (Action => Remove_Edge_From_Selection_Selective);
      procedure Unregister_Selected_Nodes is new For_All_Graph_Nodes
        (Action => Remove_Node_From_Selection_Selective);

      Lock : Lock_Type;
   begin
      Lock_All_Content (Widget, Lock);
      Local_Unhighlight_Nodes
        (Widget => Widget,
         Nodes  => Graph_Lib.Selections.Get_All_Nodes (Selection));
      Local_Unhighlight_Edges
        (Widget => Widget,
         Edges  => Graph_Lib.Selections.Get_All_Edges (Selection));

      if Vis_Data."=" (Highlight, Vis_Data.Current_Local) then
         --  Update selection if correct color set.
         Unregister_Selected_Edges
           (Widget => Widget,
            Edges  => Graph_Lib.Selections.Get_All_Edges (Selection));
         Unregister_Selected_Nodes
           (Widget => Widget,
            Nodes  => Graph_Lib.Selections.Get_All_Nodes (Selection));
      end if;
      Release_Lock (Widget, Lock);
   end Remove_Local_Highlighting;

   procedure Add_Global_Highlighting
     (Widget   : access Graph_Widget_Record'Class;
      Subgraph : in     Graph_Lib.Subgraphs.Subgraph;
      Color    : in     Config.Global_Data.Subgraph_High_Light_ID) is

      Highlight : constant Vis_Data.Global_Highlight_Type :=
        To_Global_Highlight_Type (Color);

      procedure Add_Global_Edge_Highlighting
        (Widget : access Graph_Widget_Record'Class;
         Edge   : in     Vis_Data.Vis_Edge_Id) is
      begin
         if Edge /= null then
            Add_Edge_Highlighting (Widget, Edge, Highlight);
         --  else ignore
         end if;
      end Add_Global_Edge_Highlighting;

      procedure Add_Global_Node_Highlighting
        (Widget : access Graph_Widget_Record'Class;
         Node   : in     Vis_Data.Vis_Node_Id) is
      begin
         if Node /= null then
            Add_Node_Highlighting (Widget, Node, Highlight);
         --  else ignore
         end if;
      end Add_Global_Node_Highlighting;

      procedure Global_Highlight_Edges is new For_All_Graph_Edges
        (Action => Add_Global_Edge_Highlighting);
      procedure Global_Highlight_Nodes is new For_All_Graph_Nodes
        (Action => Add_Global_Node_Highlighting);

      Lock : Lock_Type;
   begin
      Lock_All_Content (Widget, Lock);
      Global_Highlight_Nodes
        (Widget => Widget,
         Nodes  => Graph_Lib.Subgraphs.Get_All_Nodes (Subgraph));
      Global_Highlight_Edges
        (Widget => Widget,
         Edges  => Graph_Lib.Subgraphs.Get_All_Edges (Subgraph));
      Release_Lock (Widget, Lock);
   end Add_Global_Highlighting;

   procedure Remove_Global_Highlighting
     (Widget   : access Graph_Widget_Record'Class;
      Subgraph : in     Graph_Lib.Subgraphs.Subgraph;
      Color    : in     Config.Global_Data.Subgraph_High_Light_ID) is

      Highlight : constant Vis_Data.Global_Highlight_Type :=
        To_Global_Highlight_Type (Color);

      procedure Remove_Global_Edge_Highlighting
        (Widget : access Graph_Widget_Record'Class;
         Edge   : in     Vis_Data.Vis_Edge_Id) is
      begin
         if Edge /= null then
            Remove_Edge_Highlighting (Widget, Edge, Highlight);
         --  else ignore
         end if;
      end Remove_Global_Edge_Highlighting;

      procedure Remove_Global_Node_Highlighting
        (Widget : access Graph_Widget_Record'Class;
         Node   : in     Vis_Data.Vis_Node_Id) is
      begin
         if Node /= null then
            Remove_Node_Highlighting (Widget, Node, Highlight);
         --  else ignore
         end if;
      end Remove_Global_Node_Highlighting;

      procedure Global_Unhighlight_Edges is new For_All_Graph_Edges
        (Action => Remove_Global_Edge_Highlighting);
      procedure Global_Unhighlight_Nodes is new For_All_Graph_Nodes
        (Action => Remove_Global_Node_Highlighting);

      Lock : Lock_Type;
   begin
      Lock_All_Content (Widget, Lock);
      Global_Unhighlight_Nodes
        (Widget => Widget,
         Nodes  => Graph_Lib.Subgraphs.Get_All_Nodes (Subgraph));
      Global_Unhighlight_Edges
        (Widget => Widget,
         Edges  => Graph_Lib.Subgraphs.Get_All_Edges (Subgraph));
      Release_Lock (Widget, Lock);
   end Remove_Global_Highlighting;

   procedure Clear_Highlighting
     (Widget     : access Graph_Widget_Record'Class) is
   begin
      null;
   end Clear_Highlighting;


   -----------------------
   -- Visual Attributes --
   -----------------------

   function Is_Hidden
     (Widget     : access Graph_Widget_Record'Class;
      Edge       : in     Graph_Lib.Edge_Id)
     return Boolean is

      Vis_Edge : Vis_Data.Vis_Edge_Id := Look_Up (Widget, Edge);
   begin
      if Vis_Edge = null then
         raise Unknown_Edge_Id;
      end if;
      return Vis_Data.Is_Hidden (Vis_Edge);
   end Is_Hidden;

   function Is_Hidden
     (Widget     : access Graph_Widget_Record'Class;
      Node       : in     Graph_Lib.Node_Id)
     return Boolean is

      Vis_Node : Vis_Data.Vis_Node_Id := Look_Up (Widget, Node);
   begin
      if Vis_Node = null then
         raise Unknown_Node_Id;
      end if;
      return Vis_Data.Is_Hidden (Vis_Node);
   end Is_Hidden;

   procedure Set_Hidden
     (Widget     : access Graph_Widget_Record'Class;
      Selection  : in     Graph_Lib.Selections.Selection;
      Hidden     : in     Boolean) is
   begin
      raise Unimplemented;
   end Set_Hidden;

   procedure Unhide_All
     (Widget     : access Graph_Widget_Record'Class) is
   begin
      raise Unimplemented;
   end Unhide_All;

   procedure Change_Annotation_State
     (Widget     : access Graph_Widget_Record'Class;
      Node       : in     Graph_Lib.Node_Id) is

      Old_State : Boolean;
      New_State : Boolean;
      Vis_Node  : Vis_Data.Vis_Node_Id := Look_Up (Widget, Node);
   begin
      if Vis_Node /= null then
         Old_State := Vis_Data.Is_Annotated (Vis_Node);
         New_State := Settings.Has_Annotation (Widget, Vis_Node);
         if Old_State /= New_State then
            Vis_Data.Set_Annotated (Vis_Node, New_State);
            Vis_Data.Pollute_Node (Widget.Manager, Vis_Node);
            States.Changed_Visual (Widget);
         end if;
         Redraw (Widget);
      end if;
   end Change_Annotation_State;


   ------------------------
   -- Zooming and Moving --
   ------------------------

   function Get_Maximum_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Zoom_Level is

      Target_Rect : Vis.Absolute.Rectangle_2d;
      Lesser      : Vis.Absolute_Int;
   begin
      Lesser := (Vis.Absolute_Int'Last / 3) * 2;
      Target_Rect := Vis.Absolute.Combine_Rectangle
         (X_1 => Vis.Absolute_Int'First + Lesser,
          Y_1 => Vis.Absolute_Int'First + Lesser,
          X_2 => Vis.Absolute_Int'Last - Lesser,
          Y_2 => Vis.Absolute_Int'Last - Lesser);
      return Vis.Get_Zoom_Level
        (Vis.Get_Transformation_Rect_Into_Rect_Centered
         (Source => Get_Logical_Area (Widget),
          Target => Target_Rect));
   end Get_Maximum_Zoom_Level;

   function Get_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Zoom_Level is
   begin
      return Positioning.Get_Zoom (Widget);
   end Get_Zoom_Level;

   procedure Set_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class;
      Zoom       : in     Vis.Zoom_Level) is
   begin
      Set_Location_And_Zoom_Level
        (Widget   => Widget,
         Location => Get_Location (Widget),
         Zoom     => Zoom);
   end Set_Zoom_Level;

   procedure Zoom_To_Rectangle
     (Widget     : access Graph_Widget_Record'Class;
      Rectangle  : in     Vis.Logic.Rectangle_2d) is

      Transformation : Vis.Transformation_Type;
   begin
      Transformation := Vis.Get_Transformation_Rect_Into_Rect_Centered
        (Source => Rectangle,
         Target => Drawing.Get_Visible_Area (Widget));
      Set_Location_And_Zoom_Level
        (Widget   => Widget,
         Location => Vis.Logic.Get_Center (Rectangle),
         Zoom     => Vis.Get_Zoom_Level (Transformation));
   end Zoom_To_Rectangle;

   procedure Zoom_To_Edge
     (Widget     : access Graph_Widget_Record'Class;
      Edge       : in     Graph_Lib.Edge_Id) is

      Rectangle      : Vis.Absolute.Rectangle_2d;
      Transformation : Vis.Transformation_Type;
      Vis_Edge       : Vis_Data.Vis_Edge_Id := Look_Up (Widget, Edge);
   begin
      if Vis_Data."/=" (Vis_Edge, null) then
         Rectangle := Vis.Absolute.Get_Surrounding
           (Left  => Vis_Data.Get_Extent (Vis_Data.Get_Source (Vis_Edge)),
            Right => Vis_Data.Get_Extent (Vis_Data.Get_Target (Vis_Edge)));
         Zoom_To_Rectangle
           (Widget,
            Positioning.Get_Logic (Widget, Rectangle));
      else
         raise Unknown_Edge_Id;
      end if;
   end Zoom_To_Edge;

   procedure Zoom_To_Selection
     (Widget     : access Graph_Widget_Record'Class;
      Selection  : in     Graph_Lib.Selections.Selection) is


      Rectangle  : Vis.Absolute.Rectangle_2d;
      Iterator   : Graph_Node_Sets.Iterator;
      Node       : Vis_Data.Vis_Node_Id;
      Graph_Node : Graph_Lib.Node_Id;
      Nodes      : Graph_Node_Sets.Set :=
        Graph_Lib.Selections.Get_All_Nodes (Selection);
   begin
      Iterator := Graph_Node_Sets.Make_Iterator (Nodes);
      if Graph_Node_Sets.More (Iterator) then
         Graph_Node_Sets.Next (Iterator, Graph_Node);
         Node := Look_Up (Widget, Graph_Node);
         if Vis_Data."/=" (Node, null) then
            Rectangle := Vis_Data.Get_Extent (Node);

            while Graph_Node_Sets.More (Iterator) loop
               Graph_Node_Sets.Next (Iterator, Graph_Node);
               Node := Look_Up (Widget, Graph_Node);
               if Vis_Data."/=" (Node, null) then
                  Rectangle := Vis.Absolute.Get_Surrounding
                    (Rectangle, Vis_Data.Get_Extent (Node));
               else
                  raise Unknown_Node_Id;
               end if;
            end loop;

            Zoom_To_Rectangle
              (Widget,
               Positioning.Get_Logic (Widget, Rectangle));
         else
            raise Unknown_Node_Id;
         end if;
      end if;
   end Zoom_To_Selection;

   procedure Zoom_To_All
     (Widget     : access Graph_Widget_Record'Class) is
   begin
      Zoom_To_Rectangle (Widget, Get_Logical_Area (Widget));
   end Zoom_To_All;

   function Get_Logical_Area
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Logic.Rectangle_2d is
   begin
      return Widget.Logic_Area;
   end Get_Logical_Area;

   function Get_Visible_Area
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Logic.Rectangle_2d is
   begin
      return Positioning.Get_Logic (Widget, Drawing.Get_Visible_Area (Widget));
   end Get_Visible_Area;

   function Get_Location
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Logic.Vector_2d is
   begin
      return Positioning.Get_Logic
        (Widget, Vis.Absolute.Get_Center (Drawing.Get_Visible_Area (Widget)));
   end Get_Location;

   procedure Set_Visible_Center
     (Widget     : access Graph_Widget_Record'Class;
      Center     : in     Vis.Absolute.Vector_2d) is

      use type Vis.Absolute.Vector_2d;
      Point        : Vis.Absolute.Vector_2d;
      Old_Area     : Vis.Absolute.Rectangle_2d;
      Lock         : Lock_Type;
  begin
      Lock_All_Content (Widget, Lock);
      Point := States.Get_Mouse_Position (Widget);
      Point := Point + Center -
        Vis.Absolute.Get_Center (Drawing.Get_Visible_Area (Widget));

      Old_Area := Drawing.Get_Visible_Area (Widget);
      Drawing.Move_Visible_Area_To
        (Widget => Widget,
         Point  => Center);

      Callbacks.Mouse_Pointer_Moved_To (Widget, Point);

      Make_Edges_Appear (Widget, Old_Area);

      Release_Lock (Widget, Lock);
   end Set_Visible_Center;

   procedure Set_Location
     (Widget     : access Graph_Widget_Record'Class;
      Location   : in     Vis.Logic.Vector_2d) is
   begin
      Set_Visible_Center (Widget, Positioning.Get_Absolute (Widget, Location));
   end Set_Location;

   procedure Set_Location_And_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class;
      Location   : in     Vis.Logic.Vector_2d;
      Zoom       : in     Vis.Zoom_Level) is

      Lock        : Lock_Type;
      Actual_Zoom : Vis.Zoom_Level;
      Max_Zoom    : Vis.Zoom_Level := Get_Maximum_Zoom_Level (Widget);
   begin
      Actual_Zoom := Vis.Zoom_Level'Max (0.0, Zoom);
      Actual_Zoom := Vis.Zoom_Level'Min (Actual_Zoom, Max_Zoom);
      Lock_All_Content (Widget, Lock);
      Move_All_Nodes_To_Unsized (Widget);
      --  Move_All_Edges_To_Unsized (Widget);
      Settings.Set_Zoom (Widget, Actual_Zoom);
      Positioning.Set_Zoom (Widget, Actual_Zoom);
      Set_Location (Widget, Location);
      if Zoom >= Default_Minimum_Precise_Zoom then
         --  Recalculation of logic area will be done on release anyway, so
         --  might as well use it to resize the logic area-estimate and thus
         --  update mini map and scroll bars.
         Widget.Logic_Area := Vis.Logic.Combine_Rectangle
           (Top_Left     => Vis.Logic.Zero_2d,
            Bottom_Right => Vis.Logic.Zero_2d);
      end if;
      Release_Lock (Widget, Lock);
   end Set_Location_And_Zoom_Level;


   ---------------
   -- Selection --
   ---------------

   function Is_Edge_In_Selection
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id)
     return Boolean is
   begin
      return Vis_Edge_Sets.Is_Member (Widget.Selected_Edges, Edge);
   end Is_Edge_In_Selection;

   function Is_Node_In_Selection
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id)
     return Boolean is
   begin
      return Vis_Node_Sets.Is_Member (Widget.Selected_Nodes, Node);
   end Is_Node_In_Selection;

   function Get_Selected_Edges
     (Widget : access Graph_Widget_Record'Class)
     return Vis_Edge_Sets.Set is
   begin
      return Widget.Selected_Edges;
   end Get_Selected_Edges;

   function Get_Selected_Nodes
     (Widget : access Graph_Widget_Record'Class)
     return Vis_Node_Sets.Set is
   begin
      return Widget.Selected_Nodes;
   end Get_Selected_Nodes;

   procedure Add_Edge_To_Selection
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id) is
   begin
      Vis_Edge_Sets.Insert (Widget.Selected_Edges, Edge);
   end Add_Edge_To_Selection;

   procedure Add_Node_To_Selection
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id) is
   begin
      Vis_Node_Sets.Insert (Widget.Selected_Nodes, Node);
   end Add_Node_To_Selection;

   procedure Remove_Edge_From_Selection
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id) is
   begin
      Vis_Edge_Sets.Remove_If_Exists (Widget.Selected_Edges, Edge);
   end Remove_Edge_From_Selection;

   procedure Remove_Node_From_Selection
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id) is
   begin
      Vis_Node_Sets.Remove_If_Exists (Widget.Selected_Nodes, Node);
   end Remove_Node_From_Selection;

   procedure Modify_Selection
     (Widget : access Graph_Widget_Record'Class;
      Edges  : in out Vis_Edge_Lists.List;
      Nodes  : in out Vis_Node_Lists.List;
      Mode   : in     Internal_Selection_Modify_Type) is

      Actual_Mode   : Internal_Selection_Modify_Type := Mode;
      Lock          : Lock_Type;
      Current_Edge  : Vis_Data.Vis_Edge_Id;
      Current_Node  : Vis_Data.Vis_Node_Id;
   begin
      Lock_All_Content (Widget, Lock);
      if Actual_Mode = Change then
         Clear_Selection (Widget);
         Actual_Mode := Add;
      end if;

      while not Vis_Edge_Lists.IsEmpty (Edges) loop
         Current_Edge := Vis_Edge_Lists.FirstValue (Edges);
         Vis_Edge_Lists.DeleteHead (Edges);
         if Actual_Mode = Remove or else
           (Actual_Mode = Toggle and then
            Is_Edge_In_Selection (Widget, Current_Edge)) then

            Remove_Edge_From_Selection (Widget, Current_Edge);
            Remove_Edge_Highlighting
              (Widget    => Widget,
               Edge      => Current_Edge,
               Highlight => Vis_Data.Current_Local);
         else
            Add_Edge_To_Selection (Widget, Current_Edge);
            Add_Edge_Highlighting
              (Widget    => Widget,
               Edge      => Current_Edge,
               Highlight => Vis_Data.Current_Local);
         end if;
      end loop;
      Vis_Edge_Lists.Destroy (Edges);

      while not Vis_Node_Lists.IsEmpty (Nodes) loop
         Current_Node := Vis_Node_Lists.FirstValue (Nodes);
         Vis_Node_Lists.DeleteHead (Nodes);
         if Actual_Mode = Remove or else
           (Actual_Mode = Toggle and then
            Is_Node_In_Selection (Widget, Current_Node)) then

            Remove_Node_From_Selection (Widget, Current_Node);
            Remove_Node_Highlighting
              (Widget    => Widget,
               Node      => Current_Node,
               Highlight => Vis_Data.Current_Local);
         else
            Add_Node_To_Selection (Widget, Current_Node);
            Add_Node_Highlighting
              (Widget    => Widget,
               Node      => Current_Node,
               Highlight => Vis_Data.Current_Local);
         end if;
      end loop;
      Vis_Node_Lists.Destroy (Nodes);

      Release_Lock (Widget, Lock);
   end Modify_Selection;

   procedure Clear_Selection
     (Widget : access Graph_Widget_Record'Class) is

      Lock          : Lock_Type;
      Edge_Iterator : Vis_Edge_Sets.Iterator;
      Current_Edge  : Vis_Data.Vis_Edge_Id;
      Node_Iterator : Vis_Node_Sets.Iterator;
      Current_Node  : Vis_Data.Vis_Node_Id;
   begin
      Lock_All_Content (Widget, Lock);

      Edge_Iterator := Vis_Edge_Sets.Make_Iterator (Widget.Selected_Edges);
      while Vis_Edge_Sets.More (Edge_Iterator) loop
         Vis_Edge_Sets.Next (Edge_Iterator, Current_Edge);
         Remove_Edge_Highlighting
           (Widget    => Widget,
            Edge      => Current_Edge,
            Highlight => Vis_Data.Current_Local);
      end loop;
      Vis_Edge_Sets.Destroy (Edge_Iterator);
      Vis_Edge_Sets.Remove_All (Widget.Selected_Edges);

      Node_Iterator := Vis_Node_Sets.Make_Iterator (Widget.Selected_Nodes);
      while Vis_Node_Sets.More (Node_Iterator) loop
         Vis_Node_Sets.Next (Node_Iterator, Current_Node);
         Remove_Node_Highlighting
           (Widget    => Widget,
            Node      => Current_Node,
            Highlight => Vis_Data.Current_Local);
      end loop;
      Vis_Node_Sets.Destroy (Node_Iterator);
      Vis_Node_Sets.Remove_All (Widget.Selected_Nodes);

      Release_Lock (Widget, Lock);
   end Clear_Selection;

   procedure Notify_Selection_Change
     (Widget : access Graph_Widget_Record'Class) is

      Selection : Graph_Lib.Selections.Selection;

      procedure Add_Edge
        (Edge : in     Vis_Data.Vis_Edge_Id) is
      begin
         Graph_Lib.Selections.Add_Edge
           (The_Selection => Selection,
            Edge          => Vis_Data.Get_Graph_Edge (Edge));
      end Add_Edge;

      procedure Add_Node
        (Node : in     Vis_Data.Vis_Node_Id) is
      begin
         Graph_Lib.Selections.Add_Node
           (The_Selection => Selection,
            Node          => Vis_Data.Get_Graph_Node (Node));
      end Add_Node;

      procedure Add_All_Edges is new Vis_Edge_Sets.Apply
        (Execute => Add_Edge);
      procedure Add_All_Nodes is new Vis_Node_Sets.Apply
        (Execute => Add_Node);
   begin
      Selection := Graph_Lib.Selections.Create ("");
      Add_All_Edges (Widget.Selected_Edges);
      Add_All_Nodes (Widget.Selected_Nodes);
      Notifications.Selection_Changed
        (Widget     => Widget,
         Action     => Change,
         Difference => Selection);
      Graph_Lib.Selections.Destroy (Selection);
   end Notify_Selection_Change;

   procedure Modify_Selection_With_Edge_And_Notify
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id;
      Mode   : in     Selection_Modify_Type) is

      Action    : Selection_Change_Type;
      Edge_List : Vis_Edge_Lists.List := Vis_Edge_Lists.Create;
      Node_List : Vis_Node_Lists.List := Vis_Node_Lists.Create;
      Selection : Graph_Lib.Selections.Selection :=
        Graph_Lib.Selections.Create ("");
   begin
      Vis_Edge_Lists.Attach (Edge, Edge_List);
      Graph_Lib.Selections.Add_Edge
        (Selection, Vis_Data.Get_Graph_Edge (Edge));
      case Mode is
         when Add =>
            Action := Insert;
         when Toggle =>
            if Is_Edge_In_Selection (Widget, Edge) then
               Action := Remove;
            else
               Action := Insert;
            end if;
         when Change =>
            Action := Change;
      end case;

      Modify_Selection
        (Widget => Widget,
         Edges  => Edge_List,
         Nodes  => Node_List,
         Mode   => Mode);
      Notifications.Selection_Changed
        (Widget     => Widget,
         Action     => Action,
         Difference => Selection);
      Graph_Lib.Selections.Destroy (Selection);
   end Modify_Selection_With_Edge_And_Notify;

   procedure Modify_Selection_With_Node_And_Notify
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id;
      Mode   : in     Selection_Modify_Type) is

      Action    : Selection_Change_Type;
      Edge_List : Vis_Edge_Lists.List := Vis_Edge_Lists.Create;
      Node_List : Vis_Node_Lists.List := Vis_Node_Lists.Create;
      Selection : Graph_Lib.Selections.Selection :=
        Graph_Lib.Selections.Create ("");
   begin
      Vis_Node_Lists.Attach (Node, Node_List);
      Graph_Lib.Selections.Add_Node
        (Selection, Vis_Data.Get_Graph_Node (Node));
      case Mode is
         when Add =>
            Action := Insert;
         when Toggle =>
            if Is_Node_In_Selection (Widget, Node) then
               Action := Remove;
            else
               Action := Insert;
            end if;
         when Change =>
            Action := Change;
      end case;

      Modify_Selection
        (Widget => Widget,
         Edges  => Edge_List,
         Nodes  => Node_List,
         Mode   => Mode);
      Notifications.Selection_Changed
        (Widget     => Widget,
         Action     => Action,
         Difference => Selection);
      Graph_Lib.Selections.Destroy (Selection);
   end Modify_Selection_With_Node_And_Notify;

   procedure Clear_Selection_And_Notify
     (Widget : access Graph_Widget_Record'Class) is

      Empty_Selection : Graph_Lib.Selections.Selection :=
        Graph_Lib.Selections.Create ("");
   begin
      Clear_Selection (Widget);
      Notifications.Selection_Changed
        (Widget     => Widget,
         Action     => Clear,
         Difference => Empty_Selection);
      Graph_Lib.Selections.Destroy (Empty_Selection);
   end Clear_Selection_And_Notify;


   -------------
   -- Helpers --
   -------------

   procedure Mark_Edge_Obsolete
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id) is
   begin
      Vis_Data.Set_Obsolete (Edge, True);
      Move_Edge_To_Unsized (Widget, Edge);
   end Mark_Edge_Obsolete;

   procedure Mark_Node_Obsolete
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id) is
   begin
      Vis_Data.Set_Obsolete (Node, True);
      if Vis_Data.Is_Sized (Node) then
         Move_Node_To_Unsized (Widget, Node);
      end if;
   end Mark_Node_Obsolete;

   procedure Mark_Incident_Obsolete
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id) is

      procedure Mark_One_Obsolete_Marshaller
        (Edge : in     Vis_Data.Vis_Edge_Id) is
      begin
         Mark_Edge_Obsolete (Widget, Edge);
      end Mark_One_Obsolete_Marshaller;

      procedure Mark_All_Obsolete is new For_All_Incident_Edges
        (Action => Mark_One_Obsolete_Marshaller);

   begin
      Mark_All_Obsolete (Node);
   end Mark_Incident_Obsolete;

   procedure Make_Edges_Appear
     (Widget           : access Graph_Widget_Record'Class;
      Old_Visible_Area : in     Vis.Absolute.Rectangle_2d) is

      Add_Queue    : Vis_Node_Lists.List;
      Remove_Queue : Vis_Node_Lists.List;
      Edges        : Vis_Edge_Sets.Set;
      Lock         : Lock_Type;
   begin
      Lock_All_Content (Widget, Lock);

      Vis_Data.Update_Area_Content_Nodes
        (Manager      => Widget.Manager,
         Old_Area     => Old_Visible_Area,
         New_Area     => Drawing.Get_Visible_Area (Widget),
         Add_Nodes    => Add_Queue,
         Remove_Nodes => Remove_Queue);
      Switch_Incident_Visible (Widget, Add_Queue, Edges);
      Vis_Node_Lists.Destroy (Add_Queue);

      Insert_Incident_Edges (Widget, Edges);
      Check_Remove_Incident_Edges (Widget, Remove_Queue);

      Callbacks.Edges_Appeared (Widget, Edges);

      Vis_Edge_Sets.Destroy (Edges);

      Release_Lock (Widget, Lock);
   end Make_Edges_Appear;

   function Is_In_Visible_Area
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id)
     return Boolean is
   begin
      return Vis_Data.Intersects (Node, Drawing.Get_Visible_Area (Widget));
   end Is_In_Visible_Area;

   procedure Switch_Incident_Visible
     (Widget     : access Graph_Widget_Record'Class;
      Nodes      : in     Vis_Node_Lists.List;
      Incident   :    out Vis_Edge_Sets.Set) is

      procedure Add_One_Edge
        (Edge : in Vis_Data.Vis_Edge_Id) is
      begin
         if not Vis_Data.Has_Manager (Edge) then
            Vis_Edge_Sets.Insert (Incident, Edge);
         end if;
      end Add_One_Edge;

      procedure Collect_Incident_Unmanaged_Edges is new For_All_Incident_Edges
        (Action => Add_One_Edge);

      Iterator : Vis_Node_Lists.ListIter;
      Node     : Vis_Data.Vis_Node_Id;
   begin
      Iterator := Vis_Node_Lists.MakeListIter (Nodes);
      while Vis_Node_Lists.More (Iterator) loop
         Vis_Node_Lists.Next (Iterator, Node);
         Vis_Data.Set_Incident_Visible (Node, True);
         Collect_Incident_Unmanaged_Edges (Node);
      end loop;
   end Switch_Incident_Visible;

   procedure Insert_Incident_Edges
     (Widget : access Graph_Widget_Record'Class;
      Edges  : in     Vis_Edge_Sets.Set) is

      procedure Size_And_Position_One
        (Widget : access Graph_Widget_Record'Class;
         Edge   : in     Vis_Data.Vis_Edge_Id) is
      begin
         Drawing.Update_Edge_Size (Widget, Edge);
         Positioning.Update_Edge_Position (Widget, Edge);
      end Size_And_Position_One;

      procedure Size_And_Position_All is new For_All
        (Object_Type => Vis_Data.Vis_Edge_Id,
         "<"         => Vis_Data.Is_Edge_Below,
         "="         => Vis_Data."=",
         Object_Sets => Vis_Edge_Sets,
         Action      => Size_And_Position_One);

      procedure Adjust_Arrow_And_Insert_One
        (Widget : access Graph_Widget_Record'Class;
         Edge   : in     Vis_Data.Vis_Edge_Id) is
      begin
         Positioning.Adjust_Arrow (Widget, Edge);
         Vis_Data.Insert_Edge (Widget.Manager, Edge);
      end Adjust_Arrow_And_Insert_One;

      procedure Adjust_Arrow_And_Insert_All is new For_All
        (Object_Type => Vis_Data.Vis_Edge_Id,
         "<"         => Vis_Data.Is_Edge_Below,
         "="         => Vis_Data."=",
         Object_Sets => Vis_Edge_Sets,
         Action      => Adjust_Arrow_And_Insert_One);

      Edge_Set      : Vis_Edge_Sets.Set := Vis_Edge_Sets.Empty_Set;
      Iterator      : Vis_Node_Lists.ListIter;
   begin
      --  Position edges, directly docking
      Size_And_Position_All (Widget, Edges);

      --  Position arrow heads
      Adjust_Arrow_And_Insert_All (Widget, Edges);

      Vis_Edge_Sets.Destroy (Edge_Set);
   end Insert_Incident_Edges;

   procedure Check_Remove_Incident_Edges
     (Widget : access Graph_Widget_Record'Class;
      Nodes  : in out Vis_Node_Lists.List) is

      procedure Check_Remove_One_Edge
        (Edge : in    Vis_Data.Vis_Edge_Id) is
      begin
         if not Vis_Data.Must_Be_Visible (Edge) and then
           Vis_Data.Has_Manager (Edge) then

            Vis_Data.Drop_Edge (Widget.Manager, Edge);
         end if;
      end Check_Remove_One_Edge;

      procedure Check_Incident is new For_All_Incident_Edges
        (Action => Check_Remove_One_Edge);

      Node     : Vis_Data.Vis_Node_Id;
   begin
      while not Vis_Node_Lists.IsEmpty (Nodes) loop
         Node := Vis_Node_Lists.FirstValue (Nodes);
         Vis_Node_Lists.DeleteHead (Nodes);

         Vis_Data.Set_Incident_Visible (Node, False);
         Check_Incident (Node);
      end loop;
      Vis_Node_Lists.Destroy (Nodes);
   end Check_Remove_Incident_Edges;

   --  Either 'Edge' must not be part of selection or all edges in selection
   --  must be raised in correct order as part of an atomic operation
   procedure Raise_Edge
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id) is

      Is_Selected : Boolean := False;
      Is_Locked   : Boolean := False;
      Is_Unsized  : Boolean := False;
      New_Layer   : Vis_Data.Layer_Type;
   begin
      --  if not Vis_Data.Can_Enlarge (Widget.Edge_Layers) then
      --     Compactify_Edge_Layers (Widget);
      --  end if;
      Vis_Data.Enlarge_Pool (Widget.Edge_Layers);
      New_Layer := Vis_Data.Get_Highest_Layer (Widget.Edge_Layers);

      Vis_Edge_Sets.Remove_If_Exists
        (A_Set   => Widget.Selected_Edges,
         Element => Edge,
         Found   => Is_Selected);

      if Vis_Data.Has_Manager (Edge) then
         Vis_Data.Change_Layer (Widget.Manager, Edge, New_Layer);
      else
         Vis_Data.Set_Layer (Edge, New_Layer);
      end if;

      if Is_Selected then
         Vis_Edge_Sets.Insert (Widget.Selected_Edges, Edge);
      end if;
   end Raise_Edge;

   procedure Raise_Node
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id) is

      Is_Selected : Boolean := False;
      Is_Locked   : Boolean := False;
      Is_Unsized  : Boolean := False;
      New_Layer   : Vis_Data.Layer_Type;
   begin
      --  if not Vis_Data.Can_Enlarge (Widget.Node_Layers) then
      --     Compactify_Node_Layers (Widget);
      --  end if;
      Vis_Data.Enlarge_Pool (Widget.Node_Layers);
      New_Layer := Vis_Data.Get_Highest_Layer (Widget.Node_Layers);

      Vis_Node_Sets.Remove_If_Exists
        (A_Set   => Widget.Selected_Nodes,
         Element => Node,
         Found   => Is_Selected);

      if Vis_Data.Has_Manager (Node) then
         Vis_Data.Change_Layer (Widget.Manager, Node, New_Layer);
         pragma Assert (Vis_Data.Is_Sized (Node));
         pragma Assert (not Vis_Data.Is_Locked (Node));
      else
         Vis_Data.Set_Layer (Node, New_Layer);
      end if;

      if Is_Selected then
         Vis_Node_Sets.Insert (Widget.Selected_Nodes, Node);
      end if;
   end Raise_Node;

   procedure Raise_Floating
     (Widget : access Graph_Widget_Record'Class) is

      type Edge_Array_Type is array (Integer range <>) of Vis_Data.Vis_Edge_Id;
      type Node_Array_Type is array (Integer range <>) of Vis_Data.Vis_Node_Id;

      package Edge_Arrays is new Vis_Edge_Sets.Arrays
        (Item_Array => Edge_Array_Type);
      package Node_Arrays is new Vis_Node_Sets.Arrays
        (Item_Array => Node_Array_Type);
   begin
      if States.Is_Drag_Current (Widget) then
         declare
            Nodes : Node_Array_Type := Node_Arrays.To_Array
              (Widget.Selected_Nodes);
         begin
            Vis_Node_Sets.Destroy (Widget.Selected_Nodes);
            Widget.Selected_Nodes := Vis_Node_Sets.Empty_Set;
            for I in Nodes'Range loop
               Raise_Node (Widget, Nodes (I));
            end loop;
            Vis_Node_Sets.Destroy (Widget.Selected_Nodes);
            Widget.Selected_Nodes := Node_Arrays.To_Set (Nodes);
         end;

         declare
            Edges : Edge_Array_Type := Edge_Arrays.To_Array
              (Widget.Selected_Edges);
         begin
            Vis_Edge_Sets.Destroy (Widget.Selected_Edges);
            Widget.Selected_Edges := Vis_Edge_Sets.Empty_Set;
            for I in Edges'Range loop
               Raise_Edge (Widget, Edges (I));
            end loop;
            Vis_Edge_Sets.Destroy (Widget.Selected_Edges);
            Widget.Selected_Edges := Edge_Arrays.To_Set (Edges);
         end;
      end if;
   end Raise_Floating;

   function Get_Floating_Nodes
     (Widget : access Graph_Widget_Record'Class)
     return Vis_Node_Sets.Set is
   begin
      if States.Is_Drag_Current (Widget) then
         return Widget.Selected_Nodes;
      else
         return Vis_Node_Sets.Empty_Set;
      end if;
   end Get_Floating_Nodes;

   procedure Add_Edge_To_Locked
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id) is
   begin
      Vis_Data.Drop_Edge (Widget.Manager, Edge);
   end Add_Edge_To_Locked;

   procedure Add_Edges_To_Locked
     (Widget : access Graph_Widget_Record'Class;
      Edges  : in out Vis_Edge_Lists.ListIter) is

      Current : Vis_Data.Vis_Edge_Id;
   begin
      while Vis_Edge_Lists.More (Edges) loop
         Vis_Edge_Lists.Next (Edges, Current);
         Add_Edge_To_Locked (Widget, Current);
      end loop;
   end Add_Edges_To_Locked;

   procedure Add_Node_To_Locked
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id) is

      Edge_Iterator : Vis_Edge_Lists.ListIter;
   begin
      if Vis_Data.Has_Manager (Node) then
         Vis_Data.Drop_Node (Widget.Manager, Node);
      end if;
      if Vis_Data.Is_Sized (Node) and then
        not Vis_Data.Is_Locked (Node) then

         Vis_Data.Set_Locked (Node, True);
         Vis_Node_Lists.Attach (Node, Widget.Locked_Nodes);

         --  Drop all incident
         Vis_Data.Make_Outgoing_Iterator (Node, Edge_Iterator);
         Add_Edges_To_Locked (Widget, Edge_Iterator);
         Vis_Data.Make_Incoming_Iterator (Node, Edge_Iterator);
         Add_Edges_To_Locked (Widget, Edge_Iterator);
      end if;
   end Add_Node_To_Locked;

   procedure Move_Edge_To_Unsized
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id) is
   begin
      if Vis_Data.Has_Manager (Edge) then
         Vis_Data.Drop_Edge (Widget.Manager, Edge);
      end if;
      --  Multiple insertions are avoided by 'Vis_Edge_Sets'-implementation
      Vis_Edge_Sets.Insert (Widget.Unsized_Edges, Edge);
   end Move_Edge_To_Unsized;

   procedure Move_Edges_To_Unsized
     (Widget : access Graph_Widget_Record'Class;
      Edges  : in     Vis_Edge_Lists.ListIter) is

      Iterator : Vis_Edge_Lists.ListIter := Edges;
      Edge     : Vis_Data.Vis_Edge_Id;
   begin
      while Vis_Edge_Lists.More (Iterator) loop
         Vis_Edge_Lists.Next (Iterator, Edge);
         Move_Edge_To_Unsized (Widget, Edge);
      end loop;
   end Move_Edges_To_Unsized;

   procedure Move_Node_To_Unsized
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id) is

      Edge_Iterator : Vis_Edge_Lists.ListIter;
   begin
      if Vis_Data.Has_Manager (Node) then
         Vis_Data.Drop_Node (Widget.Manager, Node);
      end if;
      Vis_Data.Set_Locked (Node, False);
      Vis_Data.Set_Sized (Node, False);
      Vis_Node_Lists.Attach (Node, Widget.Unsized_Nodes);
      --  add all incident
      Vis_Data.Make_Outgoing_Iterator (Node, Edge_Iterator);
      Add_Edges_To_Locked (Widget, Edge_Iterator);
      Vis_Data.Make_Incoming_Iterator (Node, Edge_Iterator);
      Add_Edges_To_Locked (Widget, Edge_Iterator);
   end Move_Node_To_Unsized;

   procedure Move_All_Edges_To_Unsized
     (Widget : access Graph_Widget_Record'Class) is

      Edge     : Vis_Data.Vis_Edge_Id;
      Iterator : Edge_Id_Mappings.Values_Iter :=
        Edge_Id_Mappings.Make_Values_Iter (Widget.Edge_Map);
   begin
      while Edge_Id_Mappings.More (Iterator) loop
         Edge_Id_Mappings.Next (Iterator, Edge);
         Move_Edge_To_Unsized (Widget, Edge);
      end loop;
   end Move_All_Edges_To_Unsized;

   procedure Move_All_Nodes_To_Unsized
     (Widget : access Graph_Widget_Record'Class) is

      Node     : Vis_Data.Vis_Node_Id;
      Iterator : Node_Id_Mappings.Values_Iter :=
        Node_Id_Mappings.Make_Values_Iter (Widget.Node_Map);
   begin
      while Node_Id_Mappings.More (Iterator) loop
         Node_Id_Mappings.Next (Iterator, Node);
         Move_Node_To_Unsized (Widget, Node);
      end loop;
   end Move_All_Nodes_To_Unsized;

   procedure Add_Logic_Position
     (Widget   : access Graph_Widget_Record'Class;
      Position : in     Vis.Logic.Vector_2d) is

      Bottom_Right : Vis.Logic.Vector_2d;
      X            : Vis.Logic_Float;
      Y            : Vis.Logic_Float;
      Changed      : Boolean := False;
   begin
      if Vis.Logic."=" (Vis.Logic.Get_Top_Left (Widget.Logic_Area),
                        Vis.Logic.Zero_2d)
        and then Vis.Logic."=" (Vis.Logic.Get_Bottom_Right (Widget.Logic_Area),
                                Vis.Logic.Zero_2d) then

         --  No point in the logical area
         if Vis.Logic."=" (Position, Vis.Logic.Zero_2d) then
            --  If (0.0, 0.0) is the only point then it must be treated
            --  differently to the case in which there is no point
            Bottom_Right := Vis.Logic.Combine_Vector
              (X => Vis.Logic_Float'Succ (0.0),
               Y => Vis.Logic_Float'Succ (0.0));
         else
            Bottom_Right := Position;
         end if;
         Widget.Logic_Area := Vis.Logic.Combine_Rectangle
           (Top_Left     => Position,
            Bottom_Right => Bottom_Right);
         Changed := True;
      else
         X := Vis.Logic.Get_X (Position);
         if X > Vis.Logic.Get_Right (Widget.Logic_Area) then
            Changed := True;
            Vis.Logic.Set_Right (Widget.Logic_Area, X);
         elsif X < Vis.Logic.Get_Left (Widget.Logic_Area) then
            Changed := True;
            Vis.Logic.Set_Left (Widget.Logic_Area, X);
         end if;

         Y := Vis.Logic.Get_Y (Position);
         if Y > Vis.Logic.Get_Bottom (Widget.Logic_Area) then
            Changed := True;
            Vis.Logic.Set_Bottom (Widget.Logic_Area, Y);
         elsif Y < Vis.Logic.Get_Top (Widget.Logic_Area) then
            Changed := True;
            Vis.Logic.Set_Top (Widget.Logic_Area, Y);
         end if;
      end if;
      if Changed then
         States.Logic_Area_Changed (Widget);
      end if;
   end Add_Logic_Position;

   procedure Add_Logic_Area_Rectangle
     (Widget    : access Graph_Widget_Record'Class;
      Rectangle : in     Vis.Logic.Rectangle_2d) is

      function Is_Empty
        (Rectangle : in     Vis.Logic.Rectangle_2d)
        return Boolean is
      begin
         return Vis.Logic."="
           (Vis.Logic.Get_Top_Left (Rectangle),
            Vis.Logic.Get_Bottom_Right (Rectangle));
      end Is_Empty;

      X       : Vis.Logic_Float;
      Y       : Vis.Logic_Float;
      Changed : Boolean := False;
   begin
      if Get_Zoom_Level (Widget) >= Default_Minimum_Precise_Zoom then
         if Is_Empty (Widget.Logic_Area) then
            Changed := True;
            Widget.Logic_Area := Rectangle;
         else
            X := Vis.Logic.Get_Left (Rectangle);
            if X < Vis.Logic.Get_Left (Widget.Logic_Area) then
               Changed := True;
               Vis.Logic.Set_Left (Widget.Logic_Area, X);
            end if;
            X := Vis.Logic.Get_Right (Rectangle);
            if X > Vis.Logic.Get_Right (Widget.Logic_Area) then
               Changed := True;
               Vis.Logic.Set_Right (Widget.Logic_Area, X);
            end if;
            Y := Vis.Logic.Get_Top (Rectangle);
            if Y < Vis.Logic.Get_Top (Widget.Logic_Area) then
               Changed := True;
               Vis.Logic.Set_Top (Widget.Logic_Area, Y);
            end if;
            Y := Vis.Logic.Get_Bottom (Rectangle);
            if Y > Vis.Logic.Get_Bottom (Widget.Logic_Area) then
               Changed := True;
               Vis.Logic.Set_Bottom (Widget.Logic_Area, Y);
            end if;
         end if;
         if Changed then
            States.Logic_Area_Changed (Widget);
         end if;
      end if;
   end Add_Logic_Area_Rectangle;

   procedure Resize_Graph_Widget
     (Widget : access Graph_Widget_Record'Class;
      Size   : in     Vis.Absolute.Vector_2d) is

      Old_Area : Vis.Absolute.Rectangle_2d :=
        Drawing.Get_Visible_Area (Widget);
   begin
      Drawing.Resize_Display (Widget);
      Make_Edges_Appear (Widget, Old_Area);
      Callbacks.Update_Scrollbars (Widget);
      Notifications.Visible_Area_Changed (Widget, Get_Visible_Area (Widget));
   end Resize_Graph_Widget;

   procedure Find_Or_Create
     (Widget     : access Graph_Widget_Record'Class;
      Graph_Edge : in     Graph_Lib.Edge_Id;
      Edge       :    out Vis_Data.Vis_Edge_Id) is

      Source      : Vis_Data.Vis_Node_Id;
      Target      : Vis_Data.Vis_Node_Id;
      Inflections : Natural;
   begin
      Edge := Look_Up (Widget, Graph_Edge);
      if Edge = null then
         Source := Look_Up (Widget, Graph_Lib.Get_Source_Node (Graph_Edge));
         Target := Look_Up (Widget, Graph_Lib.Get_Target_Node (Graph_Edge));
         if Source /= null and then Target /= null then
            if Source = Target then
               Inflections := 4;
            else
               Inflections := 0;
            end if;
            Vis_Data.Enlarge_Pool (Widget.Edge_Layers);
            Edge := Vis_Data.Create_Edge
              (Graph_Edge  => Graph_Edge,
               Source      => Source,
               Target      => Target,
               Layer       => Vis_Data.Get_Highest_Layer (Widget.Edge_Layers),
               Inflections => Inflections);
            Edge_Id_Mappings.Bind
              (Map         => Widget.Edge_Map,
               Key         => Graph_Edge,
               Value       => Edge);
         end if;
      end if;
   end Find_Or_Create;

   procedure Find_Or_Create
     (Widget     : access Graph_Widget_Record'Class;
      Graph_Node : in     Graph_Lib.Node_Id;
      Node       :    out Vis_Data.Vis_Node_Id) is
   begin
      Node := Look_Up (Widget, Graph_Node);
      if Node = null then
         Vis_Data.Enlarge_Pool (Widget.Node_Layers);
         Node := Vis_Data.Create_Node
           (Graph_Node  => Graph_Node,
            Layer       => Vis_Data.Get_Highest_Layer (Widget.Node_Layers));
         Vis_Data.Set_Annotated
           (Node        => Node,
            State       => Settings.Has_Annotation (Widget, Node));
         Node_Id_Mappings.Bind
           (Map         => Widget.Node_Map,
            Key         => Graph_Node,
            Value       => Node);
      end if;
   end Find_Or_Create;

   function Look_Up
     (Widget     : access Graph_Widget_Record'Class;
      Graph_Edge : in     Graph_Lib.Edge_Id)
     return Vis_Data.Vis_Edge_Id is
   begin
      return Edge_Id_Mappings.Fetch (Widget.Edge_Map, Graph_Edge);
   exception
      when Edge_Id_Mappings.Not_Bound =>
         return null;
   end Look_Up;

   function Look_Up
     (Widget     : access Graph_Widget_Record'Class;
      Graph_Node : in     Graph_Lib.Node_Id)
     return Vis_Data.Vis_Node_Id is
   begin
      return Node_Id_Mappings.Fetch (Widget.Node_Map, Graph_Node);
   exception
      when Node_Id_Mappings.Not_Bound =>
         return null;
   end Look_Up;

   procedure Destroy_Edge
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in out Vis_Data.Vis_Edge_Id) is

      Graph_Edge : Graph_Lib.Edge_Id := Vis_Data.Get_Graph_Edge (Edge);
   begin
      Edge_Id_Mappings.Unbind (Widget.Edge_Map, Graph_Edge);
      Vis_Data.Remove_From_Nodes (Edge);
      Vis_Data.Destroy (Edge);
   end Destroy_Edge;

   procedure Destroy_Node
     (Widget : access Graph_Widget_Record'Class;
      Node   : in out Vis_Data.Vis_Node_Id) is

      Graph_Node : Graph_Lib.Node_Id := Vis_Data.Get_Graph_Node (Node);
   begin
      Node_Id_Mappings.Unbind (Widget.Node_Map, Graph_Node);
      Vis_Data.Remove_From_Edges (Node);
      Vis_Data.Destroy (Node);
   end Destroy_Node;

   procedure Redraw
     (Widget : access Graph_Widget_Record'Class) is
   begin
      if States.Must_Queue_Draw (Widget) then
         Queue_Draw (Widget);
      end if;
   end Redraw;

end Giant.Graph_Widgets;
