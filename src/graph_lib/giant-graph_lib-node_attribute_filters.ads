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
--  $RCSfile: giant-graph_lib-node_attribute_filters.ads,v $, $Revision: 1.11 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
------------------------------------------------------------------------------
--
--  Offers filtering used for visulaization
--

with Ada.Unchecked_Deallocation;

--  from Bauhaus
with String_Lists;

package Giant.Graph_Lib.Node_Attribute_Filters is

   type Filter is private;
   type Filtered_Iterator is private;

   ------------------------------------
   --  Create & Destroy of a filter  --
   ------------------------------------

   ---------------------------------------------------------------------------
   --  If a given attribute does not exist in Node_Class of the given filter,
   --    it is IGNORED
   --  It is implemented like that to simplify implementation at the
   --    project-management
   --
   --  Params:
   --    Node_Class:                The node class the filter created for
   --    Ndoe_Attribute_Names_List: The list of classes, which should be shown
   --                               (i.e. NOT filtered!)
   --  Returns:
   --    The corresponding filter
   --
   --  Raises:
   --    Node_Attribute_Does_Not_Exist:
   --      If a Node_Attribute given by given names does not exist
   function Create
     (Node_Class                : in     Node_Class_Id;
      Node_Attribute_Names_List : in     String_Lists.List)
     return Filter;

   ---------------------------------------------------------------------------
   procedure Destroy
      (Node_Attribute_Filter  : in out Filter);

   ---------------------------------------------------------------------------
   --  Returns:
   --    Amount of stored attributes
   function Size
     (Node_Attribute_Filter  : in Filter)
     return Natural;

   ---------------------------------------------------------------------------
   --  Sets the Iterator to the first element
   function Make_Filtered_Iter
     (Node_Attribute_Filter : in Filter)
     return Filtered_Iterator;

   ---------------------------------------------------------------------------
   --  Pre:
   --    More (Iter)
   function More
     (Iter   : in     Filtered_Iterator)
     return Boolean;

   ---------------------------------------------------------------------------
   procedure Next
     (Iter   : in out Filtered_Iterator;
      Attrib :    out Node_Attribute_Id);

   ---------------------------------------------------------------------------
   --  Sets the iterator to the first element
   procedure Reset
     (Iter : in out Filtered_Iterator);

private
   subtype Filter_Type is Node_Attribute_Id_Array;

   type Filter is access Filter_Type;

   type Filtered_Iterator is record
      Used_Filter      : Filter;
      Current_Position : Positive;
   end record;

   ---------------------------------------------------------------------------
   procedure Free_Filter is new Ada.Unchecked_Deallocation
     (Filter_Type, Filter);

   --  taken from mail from keulsn, 20030426
   --  Ideas how to deal and use some ADA-specialities
   --
   --  [...Zoom_Level... not to be implemented, 13-06-2003 keulsn]
   --  type Attribute_Filter_Type is array (Positive range <>) of Boolean;
   --  pragma Pack (Attribute_Filter_Type);
   --  [Since there is a Node_Attribute_Id, implementation as makes far
   --   more sense, 13-06-2003 keulsn]
   --  type Attribute_Filter_Access is access Attribute_Filter_Type;
   --  [...Filter_For_Zoom... not to be implemented, 13-06-2003 keulsn]
end Giant.Graph_Lib.Node_Attribute_Filters;
