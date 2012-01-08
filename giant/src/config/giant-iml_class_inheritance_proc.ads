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
--  $RCSfile: giant-iml_class_inheritance_proc.ads,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
--  -----
--  This package others functionality needed to process node and 
--  edge class definitions that are based on an inheritance hierarchy.
--
with Giant.Graph_Lib; -- from GIANT

with DOM.Core.Nodes;  -- from xmlada

package Giant.IML_Class_Inheritance_Proc is

   ---------------------------------------------------------------------------
   --  Returns all node classes that inherit from a given one (incl.
   --  the Upper_Class itself).
   --
   --
   --  Note  the xml node describing the node class must have the follwing
   --  attributes:
   --
   --  <!ATTLIST super_node_class
   --    super_node_class_name CDATA #REQUIRED
   --  >  
   --
   --  Parameters:
   --    XML_Node - A XML Node that describes a node class
   --  Returns:
   --    A set holding the node class described by the passed xml node
   function Get_All_Sub_Node_Classes 
     (XML_Node : in DOM.Core.Node) 
     return Graph_Lib.Node_Class_Id_Set;
             
   ---------------------------------------------------------------------------
   --  Retrun all edge classes that belong to the passed node class or
   --  to one of the node classes 
   --
   --
   --  <!ATTLIST super_edge_class
   --    super_start_node_class CDATA #REQUIRED
   --    attribute_name         CDATA #REQUIRED
   --  >
   --
   --  Allowed Values
   --    1. super_start_node_class = "node_class_name" 
   --       attribute_name         = "attribute_name"
   --
   --    2. super_start_node_class = "node_class_name" 
   --       attribute_name         = "*"
   --
   -- Returns a set holding all Edge_Classes with the "attribute_name"
   -- that belong to the node class "super_start_node_class" or one
   -- of its sublacces. If attribute_name ="*" then all edge classes
   -- that belong to the node class or one of its subclasses will
   -- be returned.         
   function Get_All_Sub_Edge_Classes 
     (XML_Node : in DOM.Core.Node) 
     return Graph_Lib.Edge_Class_Id_Set;

end Giant.IML_Class_Inheritance_Proc;
