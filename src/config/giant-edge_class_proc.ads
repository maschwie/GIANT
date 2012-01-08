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
--  First Author: Martin Schwienbacher
--
--  $RCSfile: giant-edge_class_proc.ads,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
--  -----
--  This package others functionality to process edge class entries in
--  xml files that may hold vildcards ("*").
--
with Giant.Graph_Lib; -- from GIANT

with DOM.Core.Nodes;  -- from xmlada

package Giant.Edge_Class_Proc is

   ---------------------------------------------------------------------------
   --  This function returns all edge classes a xml edge class entry 
   --  that may hold vildcards stands for.
   --
   --  Note  the node describing the edge class must have the follwing
   --  attributes:
   --
   --  <!ATTLIST edge_class
   --    start_node_class CDATA #REQUIRED
   --    attribute_name   CDATA #REQUIRED
   --  >
   --
   --  The following values are allowed:
   --  
   --  1. start_node_class = "node_class" attribute_name = "attribute_name"
   --  -> Will return exactly one edge class
   --  
   --  2. start_node_class = "*" attribute_name = "attribute_name"
   --  -> Will return all edge classes described by the given
   --     attribute_name, regardeless of the node class.
   --
   --  3. start_node_class = "node_class" attribute_name = "*"
   --  -> Will return all edge classes that belong to the node class
   --     "node_class".
   --  
   --  Parameters:
   --    XML_Node - A XML Node that describes an edge class.
   --  Returns:
   --    A set holding all appropriate edge classes, if none are found an
   --    empty set will be returned.
   function Process_Edge_Class_Entry 
     (XML_Node : in DOM.Core.Node) 
     return Graph_Lib.Edge_Class_Id_Set;

end Giant.Edge_Class_Proc;
