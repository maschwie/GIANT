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
--  First Author: Oliver Kopp
--
--  $RCSfile: giant-tree_layouts.adb,v $, $Revision: 1.32 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--  Variables are named according to the paper
--    http://citeseer.nj.nec.com/buchheim02improving.html
--    Revised version at http://www.zaik.uni-koeln.de/~paper/preprints.html
--      ?show=zaik2002-431&preprint_session=877c83a63d7134d8123dd3e6dd0ab004

with Ada.Unchecked_Deallocation;

with Giant.Matrix_Layouts;

with Giant.Config;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Tree_Layouts is

   package Logger is new Giant.Logger ("Tree_Layout");

   ---------------------------------------------------------------------------
   function Initialize
     (Widget                   : in Graph_Widgets.Graph_Widget;
      Widget_Lock              : in Graph_Widgets.Lock_Type;
      Selection_To_Layout      : in Graph_Lib.Selections.Selection;
      Target_Position          : in Vis.Logic.Vector_2d;
      Root_Node                : in Graph_Lib.Node_Id;
      Meta_Class_Set_To_Layout : in Config.Class_Sets.Meta_Class_Set_Access;
      Process_Edges_Reverse    : in Boolean)
     return Tree_Layout
   is
      Res : Tree_Layout;
   begin
      pragma Assert (Graph_Lib.Node_Id_Sets.Is_Member
        (Graph_Lib.Selections.Get_All_Nodes (Selection_To_Layout),
         Root_Node));

      Res                 := new Tree_Layout_Record;
      Res.Widget          := Widget;
      Res.Widget_Lock     := Widget_Lock;
      Res.Nodes_To_Layout := Graph_Lib.Node_Id_Sets.Copy
        (Graph_Lib.Selections.Get_All_Nodes
         (Selection_To_Layout));
      Res.Target_Position := Target_Position;
      Res.Root_Node       := Root_Node;
      Res.Meta_Class_Set  := Meta_Class_Set_To_Layout;
      Res.Process_Edges_Reverse := Process_Edges_Reverse;
      Res.State           := Init_Start;

      --  Evolutions.Initialize
      --  TBD: complexity, can be estimated, but I don't know how by now
      Initialize (Res);

      return Res;
   end Initialize;

   ---------------------------------------------------------------------------
   procedure Finish
     (Layout   : access Tree_Layout_Record;
      Canceled : in     Boolean)
   is

      ---------------------------------------------------------------------
      --  Removes all Node_Layout_Data out of the memory
      --
      --  Processes Layout.Tree_Root preorder iteratively
      --
      --  Called by Clean_Everything_Up
      procedure Remove_Node_Layout_Data is

         -----------------------------------------------------------------
         procedure Free_Node_Layout_Data is new Ada.Unchecked_Deallocation
           (Node_Layout_Data_Record, Node_Layout_Data);

         Stack : Node_Layout_Data_Stacks.Stack;
         Data  : Node_Layout_Data;
         I     : Node_Layout_Data;

      begin
         Stack := Node_Layout_Data_Stacks.Create;

         Node_Layout_Data_Stacks.Push (Stack, Layout.Tree_Root);
         while not Node_Layout_Data_Stacks.Is_Empty (Stack) loop
            Node_Layout_Data_Stacks.Pop (Stack, Data);

            --  Push children onto stack
            I := Data.Leftmost_Child;
            while I /= null loop
               Node_Layout_Data_Stacks.Push (Stack, I);
               I := I.Right_Silbling;
            end loop;

            --  free (self)
            Free_Node_Layout_Data (Data);
         end loop;

         Node_Layout_Data_Stacks.Destroy (Stack);
      end Remove_Node_Layout_Data;

      ----------------------------------------------------------------------
      --  Cleans up stuff allocated in Init_Calculation_Start
      procedure Clean_Everything_Up
      is

      begin
         Remove_Node_Layout_Data;

         Node_Layout_Data_Lists.Destroy (Layout.Queue);

         Level_Mappings.Destroy (Layout.Level_Heights);

         FirstWalk_Stacks.Destroy (Layout.FirstWalk_Stack);

         SecondWalk_Stacks.Destroy (Layout.SecondWalk_Stack);
      end Clean_Everything_Up;

   begin
      Graph_Widgets.Release_Lock (Layout.Widget, Layout.Widget_Lock);
      Graph_Lib.Node_Id_Sets.Destroy (Layout.Nodes_To_Layout);

      if Canceled then
         case Layout.State is

            when Init_Start =>
               -- It was cancelled before Evolutions.Synchronized_Step
               -- Therefore no memory was allocated --> nothing has to be freed
               null;

            when Init_Run_Part_One | Init_Run_Part_Two=>
               Clean_Everything_Up;

            when
              FirstWalk_Start | FirstWalk_Run =>
               Clean_Everything_Up;

            --  the layout is nearly finished, the remaining nodes could
            --    be layouted
            when
              SecondWalk_Start | SecondWalk_Run | Matrix =>
               Clean_Everything_Up;

         end case;
      else
         if Layout.Count_Nodes_To_Layout > 1 then
            --  0, 1 nodes: trivial layout
            --  2 or more nodes: non-trivial algorithm used
            --    data structures were allocated etc.
            --    they have to be destroyed
            Clean_Everything_Up;
         end if;
      end if;
   end Finish;

   ---------------------------------------------------------------------------
   procedure Step
     (Layout      : access Tree_Layout_Record;
      Next_Action :    out Evolutions.Evolution_Action)
   is

      ------------------------------------------------------------------------
      procedure Start_Matrix_Layout
      is
         The_Matrix_Layout      : Matrix_Layouts.Matrix_Layout;
         The_Selection          : Graph_Lib.Selections.Selection;
         Matrix_Target_Position : Vis.Logic.Vector_2d;
      begin
         --  Create selection used as parameter for the layout
         --  a speed-optimization could be done here
         --  Selections.Create_Temp_Selection (Set, Set)
         --    which creates a wrapper-selection, which doesn't
         --    copy given sets and which doesn't destroy them either
         --    at Destroy
         The_Selection := Graph_Lib.Selections.Create ("tmp");
         Graph_Lib.Selections.Add_Node_Set
           (The_Selection, Layout.Nodes_To_Layout);

--           Logger.Debug ("Matrix: " & Natural'Image (Graph_Lib.Selections.Get_Node_Count (The_Selection)));

         Matrix_Target_Position :=
           Vis.Logic.Combine_Vector
           (Layout.Max_X + 2.0 * Layout.X_Distance,
            Vis.Logic.Get_Y (Layout.Target_Position));
         --  2.0 * Layout.X_Distance is used to see, exactly, where the tree
         --  ends

         The_Matrix_Layout := Matrix_Layouts.Initialize
           (Widget              => Layout.Widget,
            Widget_Lock         => Layout.Widget_Lock,
            Release_Widget_Lock => False,
            Selection_To_Layout => The_Selection,
            Target_Position     => Matrix_Target_Position);

         Graph_Lib.Selections.Destroy (The_Selection);

         --  do the matrix layout and return to here again
         Evolutions.Start_Sub_Calculation (Layout, The_Matrix_Layout);
         Next_Action := Evolutions.Finish;
      end Start_Matrix_Layout;

      ------------------------------------------------------------------------
      procedure FirstWalk is
         Nodes_Processed : Natural := 0;

         Data            : FirstWalk_Data_Record;

      begin
         while (Nodes_Processed <= Max_Nodes_In_One_Run) and
           not FirstWalk_Stacks.Is_Empty
           (Layout.FirstWalk_Stack) loop

            --  Fetch Top of stack
            --    it is POPped, since it has to be either
            --    modified (i.e. POPed / PUSHed) or only POPped
            FirstWalk_Stacks.Pop
              (Layout.FirstWalk_Stack, Data);

            --  Logger.Debug ("FirstWalk: Seen: " &
            --              Graph_Lib.Node_Id_Image (Data.V.Node));

            if Data.V.Leftmost_Child = null then
               --  V is a leaf
               declare
                  W : Node_Layout_Data;
               begin
                  W := Data.V.Left_Silbling;
                  if W /= null then
                     --  V has a left silbling W
                     Data.V.Prelim := W.Prelim + Layout.X_Distance;
                  else
                     Data.V.Prelim := 0.0;
                  end if;
               end;
            else
               if Data.W = null then
                  --  V was not processed at all, since the index
                  --    of the first processed child is null
                  --    This is valid, since this trunc of the if-condition
                  --    is reached only if V has a child

                  --  1. Adjust data of self, self has to be processed again
                  --     Defaultancestor was already set by
                  --       FirstWalk_Stacks_Initial_Push
                  Data.W := Data.V.Leftmost_Child;
                  --  Push is valid, since Data was POPped at the beginning of
                  --    this routine
                  FirstWalk_Stacks.Push (Layout.FirstWalk_Stack, Data);

                  --  2. Child has to be processed next
                  FirstWalk_Stack_Initial_Push
                    (Layout.FirstWalk_Stack,
                     Data.V.Leftmost_Child);
               else
                  --  Data.W was already "FirstWalked", but not yet
                  --    "Apported"
                  Apportion
                    (V               => Data.W,
                     DefaultAncestor => Data.DefaultAncestor,
                     X_Distance      => Layout.X_Distance);

                  Data.W := Data.W.Right_Silbling;

                  if Data.W /= null then
                     --  processed child (i.e. Data.W) has a left silbing
                     --  i.e. we have not yet processed all children

                     --  Adjust own values on the stack
                     --    i.e. Data.W and Data.DefaultAncestor
                     --      Data.Ancestor was modified by Apportion
                     --      Data.W by "Data.W := Data.W.Left_Silbling"
                     --
                     --  We have to push Data again, since Data was POPped
                     --  at the beginning of this routine
                     FirstWalk_Stacks.Push (Layout.FirstWalk_Stack, Data);

                     --  Push child
                     FirstWalk_Stack_Initial_Push
                       (Layout.FirstWalk_Stack,
                        Data.W);
                  else
                     FirstWalk_Visit_Self
                       (V          => Data.V,
                        X_Distance => Layout.X_Distance);
                  end if;
               end if;
            end if;

            Nodes_Processed := Nodes_Processed + 1;
         end loop;

         Evolutions.Advance_Progress (Layout, Nodes_Processed);

         if FirstWalk_Stacks.Is_Empty
           (Layout.FirstWalk_Stack) then
            Layout.State := SecondWalk_Start;
            Next_Action  := Evolutions.Run;
         else
            Next_Action  := Evolutions.Run;
         end if;
      end FirstWalk;

      procedure SecondWalk
      is
         Nodes_Processed       : Natural := 0;

         SecondWalk_Data       : SecondWalk_Data_Record;
         New_SecondWalk_Data   : SecondWalk_Data_Record;

         W                     : Node_Layout_Data;

         New_Relative_Position : Vis.Logic.Vector_2d;
         New_Position          : Vis.Logic.Vector_2d;

         X                     : Vis.Logic_Float;
         Y                     : Vis.Logic_Float;

      begin
         while (Nodes_Processed <= Max_Nodes_In_One_Run) and
           not SecondWalk_Stacks.Is_Empty (Layout.SecondWalk_Stack) loop
            SecondWalk_Stacks.Pop
              (Layout.SecondWalk_Stack, SecondWalk_Data);

            X := SecondWalk_Data.V.Prelim + SecondWalk_Data.M;

            --  After a call to Init_Calculation_Part_Two, in
            --    Layout.Level_Heights the position meassured from 0.0
            --    is stored
            if SecondWalk_Data.V.Level = 1 then
               Y := 0.0;
            else
               --  take value of one level above, since the TOP and not the
               --    bottom of the current node is set
               Y := Level_Mappings.Fetch (Layout.Level_Heights,
                                          SecondWalk_Data.V.Level - 1);
            end if;

            New_Relative_Position := Vis.Logic.Combine_Vector
              (X => X,
               Y => Y);

            New_Position :=
               Vis.Logic."+" (New_Relative_Position,
                              Layout.Target_Position);

--              Logger.Debug ("Self:  " & Graph_Lib.Node_Id_Image
--                             (SecondWalk_Data.V.Node));
--              if SecondWalk_Data.V.Parent /= null then
--                 Logger.Debug ("Parent:  " & Graph_Lib.Node_Id_Image
--                               (SecondWalk_Data.V.Parent.Node));
--              end if;

--              Logger.Debug ("HasRightSilbling: " & Boolean'Image
--                            (SecondWalk_Data.V.Right_Silbling /= null));
--              Logger.Debug ("HasChildren: " & Boolean'Image
--                            (SecondWalk_Data.V.Leftmost_Child /= null));
--              Logger.Debug ("Prelim:" & Float'Image (SecondWalk_Data.V.Prelim));
--              Logger.Debug ("M:     " & Float'Image (SecondWalk_Data.M));

--              Logger.Debug ("X:     " & Float'Image (X));
--              Logger.Debug ("Level: " & Natural'Image (SecondWalk_Data.V.Level));
--              Logger.Debug ("Y:     " & Float'Image (Y));

--              Logger.Debug ("RelPos " & Vis.Logic.Image (New_Relative_Position));
--              Logger.Debug ("  TPos " & Vis.Logic.Image (New_Position));

            Graph_Widgets.Set_Top_Middle
              (Layout.Widget,
               SecondWalk_Data.V.Node,
               New_Position,
               Layout.Widget_Lock);

           if Layout.Max_X < Vis.Logic.Get_X (New_Position) then
              Layout.Max_X := Vis.Logic.Get_X (New_Position);
           end if;

            W := SecondWalk_Data.V.Leftmost_Child;
            while W /= null loop
               New_SecondWalk_Data.V := W;
               New_SecondWalk_Data.M :=
                 SecondWalk_Data.M + SecondWalk_Data.V.Modf;
               SecondWalk_Stacks.Push
                 (Layout.SecondWalk_Stack,
                  New_SecondWalk_Data);

               W := W.Right_Silbling;
            end loop;

            Nodes_Processed := Nodes_Processed + 1;
         end loop;

         Evolutions.Advance_Progress (Layout, Nodes_Processed);

         if SecondWalk_Stacks.Is_Empty
           (Layout.SecondWalk_Stack) then
            Layout.State := Matrix;
            Next_Action  := Evolutions.Run;
         else
            Next_Action  := Evolutions.Run;
         end if;

      end SecondWalk;

   begin
      case Layout.State is
         when Init_Start =>
            --  Logger.Debug ("State: Step: Init_Start");
            --  Depending on the amount of nodes to layout,
            --    we can do it now (0, 1) or we have to a non-trivial layout
            --    (>=2)
            Layout.Count_Nodes_To_Layout :=
              Graph_Lib.Node_Id_Sets.Size (Layout.Nodes_To_Layout);
            case Layout.Count_Nodes_To_Layout is
               when 0 =>
                  Next_Action := Evolutions.Finish;
               when 1 =>
                  declare
                     Node : Graph_Lib.Node_Id;
                  begin
                     Graph_Lib.Node_Id_Sets.Remove_First
                       (Layout.Nodes_To_Layout, Node);
                     Graph_Widgets.Set_Top_Middle
                       (Layout.Widget,
                        Node,
                        Layout.Target_Position,
                        Layout.Widget_Lock);
                  end;
                  Next_Action := Evolutions.Finish;
               when others =>
                  --  Initialization can only be done in a synchronized
                  --    environment. With following statement, it is
                  --    jumped to there
                  Next_Action  := Evolutions.Synchronize;
            end case;

         when Init_Run_Part_One =>
            --  Logger.Debug ("State: Step: Init_Run_Part_One");
            Logger.Error ("unreachable code executed: Init_Run_Part_One");
            Layout.State := Init_Run_Part_Two;
            Next_Action  := Evolutions.Synchronize;

         when Init_Run_Part_Two =>
            --  Logger.Debug ("State: Step: Init_Run_Part_Two");
            Logger.Error ("unreachable code executed: Init_Run_Part_Two");
            Layout.State := FirstWalk_Start;
            Next_Action  := Evolutions.Run;

         when FirstWalk_Start =>
            --  Logger.Debug ("State: Step: FirstWalk_Start");
            FirstWalk_Stack_Initial_Push
              (Layout.FirstWalk_Stack,
               Layout.Tree_Root);

            Layout.State := FirstWalk_Run;
            Next_Action  := Evolutions.Run;

         when FirstWalk_Run =>
            --  Logger.Debug ("State: Step: FirstWalk_Run");
            FirstWalk;

         when SecondWalk_Start =>
            --  Logger.Debug ("State: Step: SecondWalk_Start");

            SecondWalk_Stacks.Push
              (Layout.SecondWalk_Stack,
               (V => Layout.Tree_Root,
                M => - Layout.Tree_Root.Prelim));

            --  Target_Position is a good start for Max_X, since
            --    it is the middle of the tree
            --  0.0 is not used here, since negative values could also
            --    have been used as Target_Position
            Layout.Max_X := Vis.Logic.Get_X (Layout.Target_Position);

            Layout.State := SecondWalk_Run;
            Next_Action  := Evolutions.Run;

         when SecondWalk_Run =>
            --  Logger.Debug ("State: Step: SecondWalk_Run");
            SecondWalk;

         when Matrix =>
            --  Logger.Debug ("State: Step: Matrix");
            Start_Matrix_Layout;
      end case;
   end Step;

   procedure Synchronized_Step
     (Layout      : access Tree_Layout_Record;
      Next_Action :    out Evolutions.Evolution_Action)
   is

      ------------------------------------------------------------------------
      --  Initializes one Node_Layout_Data-Item
      --
      --  Parameters:
      --    Node              : the belongig graph_lib-node
      --    Level             : states, to which level this node belongs to
      --    Leftmost_Silbling : The leftmost silbling of this node
      --                        null means, that the newly created node
      --                        is the leftmost silbing for itself
      function Generate_Node_Layout_Data
        (Node              : in Graph_Lib.Node_Id;
         Level             : in Positive;
         Leftmost_Silbling : in Node_Layout_Data := null;
         Parent            : in Node_Layout_Data := null)
        return Node_Layout_Data
      is
         Data : Node_Layout_Data;
      begin
         Data := new Node_Layout_Data_Record;
         --  from the paper
         Data.Modf     := 0.0;
         Data.Thread   := null;
         Data.Ancestor := Data;

         --  not initialized in the paper, but needs to be initialized
         Data.Change   := 0.0;
         Data.Shift    := 0.0;

         --  needed for the definitve layout
         Data.Level    := Level;
         Data.Node     := Node;

         --  build the necessary structure to know, where the node resides
         --    in the tree
         if Leftmost_Silbling = null then
            Data.Leftmost_Silbling := Data;
         else
            Data.Leftmost_Silbling := Leftmost_Silbling;
         end if;

         Data.Parent         := Parent;

         Data.Leftmost_Child := null;

         --  all others will be initialized later

         return Data;
      end Generate_Node_Layout_Data;

      ------------------------------------------------------------------
      --  add/adjustcurrent_node_height in mapping having always the
      --    highest height of the level
      procedure Adjust_Level_Height
        (Node  : in Graph_Lib.Node_Id;
         Level : in Positive) is

         Cur_Height    : Vis.Logic_Float;
         Stored_Height : Vis.Logic_Float;

      begin
         Cur_Height := Graph_Widgets.Get_Current_Node_Height
           (Layout.Widget, Node);

         if Level_Mappings.Is_Bound (Layout.Level_Heights, Level) then
            Stored_Height := Level_Mappings.Fetch
              (Layout.Level_Heights, Level);
            if Stored_Height < Cur_Height then
               Level_Mappings.Unbind (Layout.Level_Heights, Level);
               Level_Mappings.Bind   (Layout.Level_Heights, Level, Cur_Height);
            end if;
         else
            Level_Mappings.Bind (Layout.Level_Heights, Level, Cur_Height);
         end if;
      end Adjust_Level_Height;

      ------------------------------------------------------------------------
      --  Inits the Init_Calculation, so that a break in the initialization
      --    is possible
      procedure Init_Calculation_Start
      is
      begin
         --  Root node is the first to be layouted
         Graph_Lib.Node_Id_Sets.Remove
           (Layout.Nodes_To_Layout, Layout.Root_Node);

         --  Generate layout-data for root node
         Layout.Tree_Root := Generate_Node_Layout_Data
           (Node  => Layout.Root_Node,
            Level => 1);

         --  Generate queue having root as first and only element
         Layout.Queue :=
           Node_Layout_Data_Lists.MakeList (Layout.Tree_Root);
         Layout.Queue_Last := Layout.Queue;

         Layout.Level_Heights := Level_Mappings.Create;
         Adjust_Level_Height (Layout.Root_Node, 1);

         Layout.FirstWalk_Stack  := FirstWalk_Stacks.Create;
         Layout.SecondWalk_Stack := SecondWalk_Stacks.Create;

         --  we have to use 1+X_Distance, since the nodes have not 0 width
         Layout.X_Distance :=
           Graph_Widgets.Get_Current_Maximum_Node_Width (Layout.Widget) *
           (1.0+X_Distance);

         --  next step is the "normal" initialize of the treelayout
         Layout.State := Init_Run_Part_One;
         Next_Action  := Evolutions.Synchronize;
      end Init_Calculation_Start;

      ------------------------------------------------------------------------
      --  Converts Nodes_To_Layout to Layout_Tree
      procedure Init_Calculation_Part_One
      is

         ---------------------------------------------------------------------
         --  Creates Node_Layout_Data of all children of given node
         --  Children = all nodes reachable by parent and not yet having
         --             a respective Node_Layout_Data_Record
         --
         --  Nodes which are childs of Parent_Data will be removed from
         --    Layout.Nodes_To_Layous
         procedure Create_Children
           (Parent_Data : in Node_Layout_Data)
         is

            ------------------------------------------------------------------
            --  Appends given element to Layout.Queue in O(1)
            --
            --  a simple "Append" of Node_Layout_Data_Lists can't be used,
            --    since it needs O(n)
            procedure Append_To_Queue
              (Data : in Node_Layout_Data)
            is
               Last_Element : Node_Layout_Data_Lists.List;
            begin
               Last_Element := Node_Layout_Data_Lists.MakeList (Data);
               Node_Layout_Data_Lists.Attach
                 (Layout.Queue_Last, Last_Element);
               Layout.Queue_Last := Last_Element;
            end Append_To_Queue;

            --  Stores the current amount of Silblings
            Silbling_Count     : Natural;

            --  indicates, if node was member in Layout.Nodes_To_Layout
            Found              : Boolean;

            --  holds the last created Node_Layout_Data
            Data               : Node_Layout_Data;

            Last_Silbling      : Node_Layout_Data;
            Leftmost_Silbling  : Node_Layout_Data;

            ----------------------------------------
            --  Data of the new Node_Layout_Data  --
            ----------------------------------------
            Node               : Graph_Lib.Node_Id;

            procedure Process_Current_Child is
            begin
               if Found then
                     --  node is reachable, via a valid edge
                     --  is member of the class sets and is to be layouted

                     Silbling_Count := Silbling_Count + 1;

                     Adjust_Level_Height
                       (Node, Parent_Data.Level + 1);

                     Data := Generate_Node_Layout_Data
                       (Node              => Node,
                        Level             => Parent_Data.Level + 1,
                        Leftmost_Silbling => Leftmost_Silbling,
                        Parent            => Parent_Data);

                     Data.Silbling_Number := Silbling_Count;

                     if Silbling_Count = 1 then
                        --  Data is the first child of the node
                        Leftmost_Silbling := Data;
                        Data.Leftmost_Silbling := Data;
                     else
                        Last_Silbling.Right_Silbling := Data;
                        Data.Left_Silbling := Last_Silbling;
                     end if;

                     Last_Silbling := Data;

                     --  the children of Data have to be created, too
                     --  --> append Data to queue
                     Append_To_Queue (Data);
               end if;
            end Process_Current_Child;

         begin
            Silbling_Count    := 0;
            Last_Silbling     := null;
            Leftmost_Silbling := null;
            Data              := null;

            if Layout.Process_Edges_Reverse then
               declare
                  Incoming_Edges     : Graph_Lib.Edge_Id_Array :=
                    Graph_Lib.Get_Incoming_Edges (Parent_Data.Node);
               begin
                  for I in Incoming_Edges'Range loop
                     if Config.Class_Sets.Is_Empty
                       (Layout.Meta_Class_Set) or else
                       Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
                       (Layout.Meta_Class_Set,
                        Graph_Lib.Get_Edge_Class_Id (Incoming_Edges (I))) then
                        Node := Graph_Lib.Get_Source_Node (Incoming_Edges (I));
                        Graph_Lib.Node_Id_Sets.Remove_If_Exists
                          (Layout.Nodes_To_Layout, Node, Found);
                        Process_Current_Child;
                     end if;
                  end loop;
               end;
            else
               declare
                  Outgoing_Edges     : Graph_Lib.Edge_Id_Array :=
                    Graph_Lib.Get_Outgoing_Edges (Parent_Data.Node);
               begin
                  for I in Outgoing_Edges'Range loop
                     if Config.Class_Sets.Is_Empty
                       (Layout.Meta_Class_Set) or else
                       Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
                       (Layout.Meta_Class_Set,
                        Graph_Lib.Get_Edge_Class_Id (Outgoing_Edges (I))) then
                        Node := Graph_Lib.Get_Target_Node (Outgoing_Edges (I));
                        Graph_Lib.Node_Id_Sets.Remove_If_Exists
                          (Layout.Nodes_To_Layout, Node, Found);
                        Process_Current_Child;
                     end if;
                  end loop;
               end;
            end if;

            --  there is no Data.Rightmost_Silbing, therefore
            --    there's no need to adjust Data, which
            --    is the Rightmost_Silbing now

            if Silbling_Count > 0 then
               --  there was at least one data created
               --  adjust child values of parent
               Parent_Data.Leftmost_Child  := Leftmost_Silbling;
               Parent_Data.Rightmost_Child := Data;
            end if;
         end Create_Children;

         Current_Data            : Node_Layout_Data;
         Number_Nodes_To_Process : Natural;

      begin
         Number_Nodes_To_Process := Natural'Min
           (Node_Layout_Data_Lists.Length (Layout.Queue),
            Max_Nodes_In_One_Run);

         for I in 1..Number_Nodes_To_Process loop
            Current_Data := Node_Layout_Data_Lists.FirstValue
              (Layout.Queue);
            Create_Children (Current_Data);
            Node_Layout_Data_Lists.DeleteHead (Layout.Queue);
         end loop;

         Evolutions.Advance_Progress (Layout, Number_Nodes_To_Process);

         if Graph_Lib.Node_Id_Sets.Is_Empty (Layout.Nodes_To_Layout) or
           Node_Layout_Data_Lists.IsEmpty (Layout.Queue) then
            --  there are no more nodes to convert to Node_Layout_Data
            --    (i.e. in Layout.Queue are only leaves)
            --  or there are no more Node_Layout_Datas to process
            --  --> proceed to next step
            Layout.State := Init_Run_Part_Two;
            Next_Action  := Evolutions.Synchronize;
         else
            Next_Action := Evolutions.Synchronize;
         end if;
      end Init_Calculation_Part_One;

      ----------------------------------------------------------------------
      --  Adjust level heights, so that for each level the height meassured
      --    from 0.0 is stored
      procedure Init_Calculation_Part_Two
      is
         Level       : Positive;
         Cur_Height  : Vis.Logic_Float;
         Last_Height : Vis.Logic_Float := 0.0;
      begin
         Level := 1;

         --  could be implemented more effienctly using the height of the tree
         --  (i.e. for Level in 1..Layout.Tree_Height loop)
         --  But Layout.Tree_Height is not available and would be needed only
         --  for this
         while Level_Mappings.Is_Bound (Layout.Level_Heights, Level) loop
            --  Get height for current row
            Cur_Height := Level_Mappings.Fetch  (Layout.Level_Heights, Level);

            --  It will be inserted a new one -- remove current one at hashmap
            Level_Mappings.Unbind (Layout.Level_Heights, Level);

            --  Adjust current height to have a certain distance between the
            --    lines
            Cur_Height  := Cur_Height * (1.0 + Y_Distance);

            --  Calculate new total height, meassured from Target_Position
            Cur_Height  := Cur_Height + Last_Height;

            --  Store for next caluclation run
            Last_Height := Cur_Height;

            --  Store for SecondWalk_Run
            Level_Mappings.Bind (Layout.Level_Heights, Level, Cur_Height);

            --  Next step means next level
            Level := Level + 1;
         end loop;

         Evolutions.Advance_Progress (Layout, Level - 1);

         Layout.State := FirstWalk_Start;
         Next_Action  := Evolutions.Run;
      end Init_Calculation_Part_Two;

   begin
      case Layout.State is
         when Init_Start =>
            --  Logger.Debug ("State: Sync: Init_Start");
            Init_Calculation_Start;
         when Init_Run_Part_One =>
            --  Logger.Debug ("State: Sync: Init_Run_Part_One");
            Init_Calculation_Part_One;
         when Init_Run_Part_Two =>
            --  Logger.Debug ("State: Sync: Init_Run_Part_Two");
            Init_Calculation_Part_Two;
         when others =>
            --  Logger.Debug ("State: Sync: Others");
            Next_Action  := Evolutions.Run;
      end case;
   end Synchronized_Step;

   ---------------------------------------------------------------------------
   function Are_Silblings
     (First  : in Node_Layout_Data;
      Second : in Node_Layout_Data)
     return Boolean
   is
   begin
      return (First.Leftmost_Silbling = Second.Leftmost_Silbling);
   end Are_Silblings;

   ---------------------------------------------------------------------------
   function Id (P : in Positive) return Positive
   is
   begin
      return P;
   end Id;

   --------------------------------------------------------------------------
   procedure FirstWalk_Stack_Initial_Push
     (Stack : in out FirstWalk_Stacks.Stack;
      V     : in     Node_Layout_Data)
   is
   begin
      FirstWalk_Stacks.Push
        (Stack,
         (V => V,
          W => null,
          DefaultAncestor => V.Leftmost_Child));
   end FirstWalk_Stack_Initial_Push;

   ---------------------------------------------------------------
   --  1:1 - Implementations of routines of Walker's algorithm  --
   ---------------------------------------------------------------

   --------------------------------------------------------------------------
   procedure ExecuteShifts (V : in Node_Layout_Data)
   is
      Shift  : Vis.Logic_Float;
      Change : Vis.Logic_Float;
      W      : Node_Layout_Data;
   begin
      Shift  := 0.0;
      Change := 0.0;
      W := V.Rightmost_Child;
      while W /= null loop
         W.Prelim := W.Prelim + Shift;
         W.Modf   := W.Modf   + Shift;
         Change   := Change   + W.Change;
         Shift    := Shift    + W.Shift + Change;

         W := W.Left_Silbling;
      end loop;
   end ExecuteShifts;

   ------------------------------------------------------------------
   function NextLeft (V : in Node_Layout_Data)
                     return Node_Layout_Data
   is
   begin
      if V.Leftmost_Child /= null then
         return V.Leftmost_Child;
      else
         return V.Thread;
      end if;
   end NextLeft;

   ------------------------------------------------------------------
   function NextRight (V : in Node_Layout_Data)
                      return Node_Layout_Data
   is
   begin
      if V.Rightmost_Child /= null then
         return V.Rightmost_Child;
      else
         return V.Thread;
      end if;
   end NextRight;

   ------------------------------------------------------------------
   procedure MoveSubtree
     (WM    : in Node_Layout_Data;
      WP    : in Node_Layout_Data;
      Shift : in Vis.Logic_Float)
   is
      Subtrees : Vis.Logic_Float;
   begin
      Subtrees  := Vis.Logic_Float
        (WP.Silbling_Number - WM.Silbling_Number);
      WP.Change := WP.Change - Shift/Subtrees;
      WP.Shift  := WP.Shift  + Shift;
      WM.Change := WM.Change + Shift/Subtrees;
      WP.Prelim := WP.Prelim + Shift;
      WP.Modf   := WP.Modf   + Shift;
   end MoveSubtree;

   ------------------------------------------------------------------
   function Ancestor
     (VIM             : in Node_Layout_Data;
      V               : in Node_Layout_Data;
      DefaultAncestor : in Node_Layout_Data)
     return Node_Layout_Data
   is
   begin
      if Are_Silblings (VIM.Ancestor, V) then
         return VIM.Ancestor;
      else
         return DefaultAncestor;
      end if;
   end Ancestor;

   ---------------------------------------------------------------------
   procedure Apportion
     (V               : in     Node_Layout_Data;
      DefaultAncestor : in out Node_Layout_Data;
      X_Distance      : in     Vis.Logic_Float)
   is

      VIP, VOP, VIM, VOM : Node_Layout_Data;
      SIP, SOP, SIM, SOM : Vis.Logic_Float;

      Shift              : Vis.Logic_Float;

      W                  : Node_Layout_Data;

   begin
      W := V.Left_Silbling;
      if W /= null then
         VIP := V;
         VOP := V;
         VIM := W;
         VOM := VIP.Leftmost_Silbling;
         SIP := VIP.Modf;
         SOP := VOP.Modf;
         SIM := VIM.Modf;
         SOM := VOM.Modf;
         while (NextRight (VIM) /= null) and
           (NextLeft (VOP) /= null) loop
            VIM := NextRight (VIM);
            VIP := NextLeft (VIP);
            VOM := NextLeft (VOM);
            VOP := NextRight (VOP);
            VOP.Ancestor := V;
            Shift := (VIM.Prelim + SIM) - (VIP.Prelim + SIP) + X_Distance;
            if Shift > 0.0 then
               MoveSubtree
                 (Ancestor (VIM, V, DefaultAncestor),
                  V,
                  Shift);
               SIP := SIP + Shift;
               SOP := SOP + Shift;
            end if;
            SIM := SIM + VIM.Modf;
            SIP := SIP + VIP.Modf;
            SOM := SOM + VOM.Modf;
            SOP := SOP + VOP.Modf;
         end loop;

         if (NextRight (VIM) /= null) and (NextRight (VOP) = null) then
            VOP.Thread := NextRight (VIM);
            VOP.Modf   := VOP.Modf + SIM - SOP;
         end if;

         if (NextLeft (VIP) /= null) and (NextLeft (VOM) = null) then
            VOM.Thread := NextLeft (VIP);
            VOM.Modf   := VOM.Modf + SIP - SOM;
            DefaultAncestor := V;
         end if;
      end if;
   end Apportion;

   ------------------------------------------------------------------------
   procedure FirstWalk_Visit_Self
     (V          : in Node_Layout_Data;
      X_Distance : in Vis.Logic_Float)
   is
      MidPoint : Vis.Logic_Float;
      W        : Node_Layout_Data;
   begin
      --  Logger.Debug ("FirstWalk_Visist_Self: Seen: " &
      --                Graph_Lib.Node_Id_Image (V.Node));

      ExecuteShifts (V);
      MidPoint :=
        (V.Leftmost_Child.Prelim + V.Rightmost_Child.Prelim) / 2.0;
      W := V.Left_Silbling;
      if W /= null then
         V.Prelim := W.Prelim + X_Distance;
         V.Modf   := V.Prelim - MidPoint;
      else
         V.Prelim := MidPoint;
      end if;
   end FirstWalk_Visit_Self;

end Giant.Tree_Layouts;

