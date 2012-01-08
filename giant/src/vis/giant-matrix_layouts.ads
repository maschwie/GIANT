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
--  $RCSfile: giant-matrix_layouts.ads,v $, $Revision: 1.16 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
--  Contains the matrixlayout-algorithm
--

with Giant.Evolutions;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Graph_Widgets;
with Giant.Vis;

package Giant.Matrix_Layouts is

   --  Theoreticcaly, it could also be Concurrent, but since
   --    Set_Top_Middle is not multitasking-capable, the code has to be
   --    rewritten
   type Matrix_Layout_Record is
     new Evolutions.Iterative_Evolution with private;

   type Matrix_Layout is access all Matrix_Layout_Record'Class;

   ---------------------------------------------------------------------------
   --  Maximum number of nost to be processed in one run
   Max_Nodes_In_One_Run : constant := 1000;

   ---------------------------------------------------------------------------
   --  X-Distance of two neighbours as proportion of
   --    Get_Current_Maximum_Node_Width
   X_Distance           : constant := 0.07;

   ---------------------------------------------------------------------------
   --  Y-Distance of two neighbours as proportion of
   --    The height of predecessing row
   Y_Distance           : constant := 0.07;

   ---------------------
   --  Initilization  --
   ---------------------

   ---------------------------------------------------------------------------
   --  Initializes the tree-layout-algorithm
   --
   --  Parameters (cp. specification 11.2.2: Matrixlayout / Parameter):
   --
   --    Selection_To_Layout:
   --      Selection containing the nodes to layout
   --      Edges are ignored
   --
   --    Widget:
   --      Graph_Widet where the nodes to layout reside
   --
   --    Widget_Lock:
   --      Lock of the widget
   --
   --    Release_Widget_Lock
   --      true  = release lock at the end of the layout
   --      false = don't release
   --
   --    Target_Position:
   --      Position on window, where the upper-left corner of the matrix has to
   --      be placed
   --
   --  Returns:
   --    derived Evolutions-Object to do the layout
   --
   function Initialize
     (Widget              : in Giant.Graph_Widgets.Graph_Widget;
      Widget_Lock         : in Giant.Graph_Widgets.Lock_Type;
      Release_Widget_Lock : in Boolean;
      Selection_To_Layout : in Giant.Graph_Lib.Selections.Selection;
      Target_Position     : in Giant.Vis.Logic.Vector_2d)
     return Matrix_Layout;

   -------------------
   --  Calculation  --
   -------------------

   ---------------------------------------------------------------------------
   procedure Finish
     (Layout   : access Matrix_Layout_Record;
      Canceled : in     Boolean);

   ---------------------------------------------------------------------------
   procedure Step
     (Layout      : access Matrix_Layout_Record;
      Next_Action :    out Evolutions.Evolution_Action);

private
   type Layout_State is (Init, Calc);

   type Matrix_Layout_Record is
     new Evolutions.Iterative_Evolution with record

        --------------------------
        --  Init by Initialize  --
        --------------------------

        Widget              : Graph_Widgets.Graph_Widget;
        Widget_Lock         : Graph_Widgets.Lock_Type;
        Release_Widget_Lock : Boolean;

        --  TBD: convert to array or something like that
        --       to gain even more speed
        Nodes_To_Layout     : Graph_Lib.Node_Id_Set;

        Target_Position     : Vis.Logic.Vector_2d;
        State               : Layout_State;

        -------------------------------------
        --  Init by Step.Init_Calculation  --
        -------------------------------------

        --  Distance between two nodes used at FirstWalk
        X_Distance         : Vis.Logic_Float;

        --  Amount of nodes in a row
        Matrix_Width       : Positive;

        --  Column, where the next node will reside
        Current_Column     : Positive;

        --  Position of next node
        Current_Position   : Vis.Logic.Vector_2d;

        Current_Row_Height : Vis.Logic_Float;

        Max_Node_Width     : Vis.Logic_Float;
     end record;

end Giant.Matrix_Layouts;
