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
--  $RCSfile: giant-layout_factory.ads,v $, $Revision: 1.14 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
--  Used as interface between the controller and the layout-algorithms
--  This package is used to offer the parameter-parsing and the calling
--    of the layout-algorithms
--

with Giant.Evolutions;
with Giant.Graph_Widgets;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Vis;

package Giant.Layout_Factory is

   Invalid_Format             : exception;
   Unknown_Algorithm          : exception;

   --  string indicating, that edges have to be processed reverse
   Process_Edges_Reverse      : constant String := "Reverse_Edges";

   ----------------------------------------------------------------------------
   --  Initialises an layout-algorithm
   --
   --  If an error occurs, an exception is risen
   --    if Additional_Parameters_Error is risen, Additional
   --
   --
   --  The resulting object has to be destroyed after usage
   --
   --  Parameters:
   --
   --    Algorithm:
   --      String, which describes the algorithm to be used
   --
   --    Selection_To_Layout:
   --      Selection containing the nodes to be layouted
   --      The selection will not be modified during the run
   --
   --    Widget:
   --      Graph_Widget where the nodes to layout reside
   --
   --    Widget_Lock:
   --      Lock for the widget - to be relased after the layout has finished
   --      it is not released, if an error at the production of an algorithm
   --      occurs
   --      The selection is locked by the caller (i.e. the lock is set and
   --        the factory has not to care about setting the lock)
   --
   --   Target_Position:
   --      Position of a certain node, meaning is depending on the
   --        layout-algorithm
   --
   --    Additonal_Parameters:
   --       String containing special parameters
   --       for the given layout algorithm. The layout
   --       algorithm for itself is responsible
   --       to parse them.
   --
   --    Layout_Evolution':
   --       Access to an Initialized Evolution-Object
   --       which does the layout null if unsuccessful
   --
   --
   --  Raises:
   --    Invalid_Format - if the string couldn't be parsed properly
   --                     The description of the exception contains the
   --                     error description
   --
   --  Currently implemented algorithms with their additional parameters:
   --
   --  "matrix":
   --     no additional parameters needed
   --
   --     Target_Position: Position of the upper left corner
   --
   --  "tree":
   --     Format:  [<Root_Node_ID>]; <List_Of_Class_Set_Names>[; Reverse_Edges]
   --     Example: "5; Aber, Hallo"
   --              "; Ja, genau"
   --              It is not possible to use " or ; or , in a classsetname.
   --              Brackets are possible.
   --
   --     Meaning:
   --       Root_Node_Id    : The root-node of the tree to layout
   --                         If not given, the root-node is searched
   --                         If there's more than one possible root-node-id,
   --                         the result is random.
   --       Class_Set_Names : Names of ClassSet containing node-classes
   --                           and edge-classes to layout
   --       Reverse_Edges   : If given, edges are reversed
   --         Normally, an outgoing edge indicates the target to be the child.
   --         If this parameter is given, the source of an incoming edge is
   --         regarded as child
   --         Useful for parent-edges
   --
   --       Target_Position : Position on window, where the root-node has to
   --                            be placed
   procedure Create
     (Algorithm             : in     String;
      Selection_To_Layout   : in     Graph_Lib.Selections.Selection;
      Widget                : in     Graph_Widgets.Graph_Widget;
      Widget_Lock           : in     Graph_Widgets.Lock_Type;
      Target_Position       : in     Giant.Vis.Logic.Vector_2d;
      Additional_Parameters : in     String;
      Layout_Evolution      :    out Evolutions.Evolution_Class_Access);

private

end Giant.Layout_Factory;
