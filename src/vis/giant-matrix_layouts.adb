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
--  $RCSfile: giant-matrix_layouts.adb,v $, $Revision: 1.16 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--

with Ada.Numerics.Generic_Elementary_Functions;

with Giant.Graph_Lib;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Matrix_Layouts is

   package Logger is new Giant.Logger("giant.matrix_layouts");

   ---------------------------------------------------------------------------
   function Initialize
     (Widget              : in Giant.Graph_Widgets.Graph_Widget;
      Widget_Lock         : in Giant.Graph_Widgets.Lock_Type;
      Release_Widget_Lock : in Boolean;
      Selection_To_Layout : in Giant.Graph_Lib.Selections.Selection;
      Target_Position     : in Giant.Vis.Logic.Vector_2d)
     return Matrix_Layout
   is
      Res : Matrix_Layout;
   begin
      Res                     := new Matrix_Layout_Record;
      Res.Widget              := Widget;
      Res.Widget_Lock         := Widget_Lock;
      Res.Release_Widget_Lock := Release_Widget_Lock;
      Res.Nodes_To_Layout     := Graph_Lib.Node_Id_Sets.Copy
        (Graph_Lib.Selections.Get_All_Nodes
         (Selection_To_Layout));
      Res.Target_Position     := Target_Position;
      Res.State               := Init;

      --  Evolutions.Initialize
      Initialize
        (Individual => Res,
         Complexity => Graph_Lib.Node_Id_Sets.Size (Res.Nodes_To_Layout));

      return Res;
   end Initialize;

   ---------------------------------------------------------------------------
   procedure Finish
     (Layout   : access Matrix_Layout_Record;
      Canceled : in     Boolean)
   is
   begin
      if Layout.Release_Widget_Lock then
         --  Logger.Debug ("Release_Lock.");
         Graph_Widgets.Release_Lock (Layout.Widget, Layout.Widget_Lock);
         --  Logger.Debug ("Release_Lock done.");
      end if;
   end Finish;

   ---------------------------------------------------------------------------
   procedure Step
     (Layout      : access Matrix_Layout_Record;
      Next_Action :    out Evolutions.Evolution_Action)
   is

      procedure Init_Calculation
      is
        package Float_Functions is new
          Ada.Numerics.Generic_Elementary_Functions (Float_Type => Float);
      begin
         Layout.Matrix_Width := Natural
           (Float_Functions.Sqrt
            (Float (Graph_Lib.Node_Id_Sets.Size
                    (Layout.Nodes_To_Layout))));
         Layout.Current_Column     := 1;
         Layout.Current_Position   := Layout.Target_Position;
         Layout.Current_Row_Height := 0.0;
         Layout.Max_Node_Width     :=
           Graph_Widgets.Get_Current_Maximum_Node_Width (Layout.Widget);
         Layout.X_Distance := Layout.Max_Node_Width * (X_Distance);

         --  Logger.Debug ("Width: " & Natural'Image (Layout.Matrix_Width));
      end Init_Calculation;

      procedure Do_Calculation
      is

         Number_Nodes_To_Layout : Natural;
         Node                   : Graph_Lib.Node_Id;

      begin
         --  Logger.Debug ("Do_Calculation: Start");
         Number_Nodes_To_Layout := Natural'Min
           (Layout.Matrix_Width - Layout.Current_Column + 1,
            Max_Nodes_In_One_Run);
         Number_Nodes_To_Layout := Natural'Min
           (Number_Nodes_To_Layout,
            Graph_Lib.Node_Id_Sets.Size (Layout.Nodes_To_Layout));

         --  Logger.Debug ("Nodes_To_Layout: " & Natural'Image
         --                (Number_Nodes_To_Layout));
         --  Logger.Debug ("Set.Size:        " & Natural'Image
         --                (Graph_Lib.Node_Id_Sets.Size
         --                 (Layout.Nodes_To_Layout)));

         --  Logger.Debug ("CurCol:          " & Natural'Image
         --                (Layout.Current_Column));
         --  Logger.Debug ("Loop until:      " & Natural'Image
         --                (Layout.Current_Column +
         --                 Number_Nodes_To_Layout - 1));

         for I in Layout.Current_Column ..
           Layout.Current_Column + Number_Nodes_To_Layout - 1 loop

            --  Logger.Debug ("I: " & Natural'Image (I));

            --  TBD: find next node by a breadth-first-search
            --       (according to spec)

            --  Set position of first node in queue
            Graph_Lib.Node_Id_Sets.Remove_First
              (Layout.Nodes_To_Layout, Node);
            Graph_Widgets.Set_Top_Middle
              (Layout.Widget,
               Node,
               Layout.Current_Position,
               Layout.Widget_Lock);

            --  determine height of current row
            Layout.Current_Row_Height := Float'Max
              (Layout.Current_Row_Height,
               Graph_Widgets.Get_Current_Node_Height
               (Layout.Widget, Node));

            --  Adjust coordinates for next node
            Layout.Current_Column := Layout.Current_Column + 1;
            Vis.Logic.Set_X
              (Layout.Current_Position,
               Vis.Logic.Get_X (Layout.Current_Position) +
               Layout.Max_Node_Width +
               Layout.X_Distance);
         end loop;

         --  Logger.Debug ("CurCol:          " & Natural'Image
         --                (Layout.Current_Column));
         --  Logger.Debug ("Width: " & Natural'Image (Layout.Matrix_Width));

         if Layout.Current_Column > Layout.Matrix_Width then
            --  we have reached EOL, go to beginning of line
            Layout.Current_Column := 1;
            Layout.Current_Position :=
              Vis.Logic.Combine_Vector
              (Vis.Logic.Get_X (Layout.Target_Position),
               Vis.Logic.Get_Y (Layout.Current_Position) +
               Layout.Current_Row_Height * (1.0+Y_Distance));
            Layout.Current_Row_Height := 0.0;
         end if;

         --  TBD: state says 51482 of 53398, but layout is finished
         --       don't think, it's this routine's fault
         Evolutions.Advance_Progress (Layout, Number_Nodes_To_Layout);

         --  Logger.Debug ("Set.Size:        " & Natural'Image
         --                (Graph_Lib.Node_Id_Sets.Size
         --                 (Layout.Nodes_To_Layout)));

         if Graph_Lib.Node_Id_Sets.Is_Empty (Layout.Nodes_To_Layout) then
            Next_Action := Evolutions.Finish;
         else
            Next_Action := Evolutions.Run;
         end if;

         --  Logger.Debug ("Do_Calculation: Finished");
      end Do_Calculation;

   begin
      --  Logger.Debug ("Begin: Step");

      case Layout.State is
         when Init =>
            case Graph_Lib.Node_Id_Sets.Size (Layout.Nodes_To_Layout) is
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
                  Init_Calculation;
                  Layout.State := Calc;
                  Next_Action  := Evolutions.Run;
            end case;
         when Calc =>
            Do_Calculation;
      end case;

      --  Logger.Debug ("End: Step");
   end Step;

end Giant.Matrix_Layouts;
