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
--  $RCSfile: giant-edge_class_proc.adb,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
with DOM.Core.Elements;  -- from xmlada

package body Giant.Edge_Class_Proc is

   ---------------------------------------------------------------------------
   -- Passed node must have the following attributes specified in DTD:
   --
   -- May return an empty set.
   --
   -- <!ATTLIST edge_class
   --   start_node_class CDATA #REQUIRED
   --   attribute_name   CDATA #REQUIRED
   -- >
   function Process_Edge_Class_Entry (XML_Node : in DOM.Core.Node) return
     Graph_Lib.Edge_Class_Id_Set is

      New_Edge_Class_Set : Graph_Lib.Edge_Class_Id_Set :=
        Graph_Lib.Edge_Class_Id_Sets.Empty_Set;

      A_Node_Class_ID : Graph_Lib.Node_Class_Id;
      A_Node_Attribute_ID : Graph_Lib.Node_Attribute_Id;

      A_Edge_Class_ID : Graph_Lib.Edge_Class_Id;
   begin

      --  Case 1 - one single edge class specified by the xml node
      if (DOM.Core.Elements.Get_Attribute
          (XML_Node, "start_node_class") /="*") and
        (DOM.Core.Elements.Get_Attribute
         (XML_Node, "attribute_name") /="*") then

         --  check whether edge class and node class name
         --  do exist
         if Graph_Lib.Does_Node_Class_Exist
           (DOM.Core.Elements.Get_Attribute (XML_Node, "start_node_class"))
           and Graph_Lib.Does_Node_Attribute_Exist
           (DOM.Core.Elements.Get_Attribute (XML_Node, "start_node_class"),
            DOM.Core.Elements.Get_Attribute (XML_Node, "attribute_name")) then

            A_Node_Class_ID := Graph_Lib.Convert_Node_Class_Name_To_Id
              (DOM.Core.Elements.Get_Attribute
                (XML_Node, "start_node_class"));

            A_Node_Attribute_ID := Graph_Lib.Convert_Node_Attribute_Name_To_Id
              (DOM.Core.Elements.Get_Attribute (XML_Node, "start_node_class"),
               DOM.Core.Elements.Get_Attribute (XML_Node, "attribute_name"));


            --  Check whether this attribute is an edge
            if Graph_Lib.Does_Edge_Class_Exist
              (A_Node_Class_ID, A_Node_Attribute_ID) then

               A_Edge_Class_ID :=
                 Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
                 (A_Node_Class_ID, A_Node_Attribute_ID);

               Graph_Lib.Edge_Class_Id_Sets.Insert
                 (New_Edge_Class_Set, A_Edge_Class_ID);

               -- Holds one edge class id
               return New_Edge_Class_Set;
            end if;
         end if;

      --  Case 2 - one node class, vildcard for attributes
      elsif (DOM.Core.Elements.Get_Attribute
             (XML_Node, "start_node_class") /="*") and
        (DOM.Core.Elements.Get_Attribute
         (XML_Node, "attribute_name") ="*") then

         if Graph_Lib.Does_Node_Class_Exist
           (DOM.Core.Elements.Get_Attribute (XML_Node, "start_node_class"))
         then

            A_Node_Class_ID := Graph_Lib.Convert_Node_Class_Name_To_Id
              (DOM.Core.Elements.Get_Attribute (XML_Node, "start_node_class"));

            return Graph_Lib.Get_All_Edge_Class_Ids_For_Node_Class
              (A_Node_Class_ID);
         end if;

      --  Case 3 - vildcard for node classes, one single attribute.
      elsif (DOM.Core.Elements.Get_Attribute
             (XML_Node, "start_node_class") ="*") and
        (DOM.Core.Elements.Get_Attribute
         (XML_Node, "attribute_name") /="*") then

            return Graph_Lib.Get_All_Edge_Class_Ids_For_Node_Attribute
              (DOM.Core.Elements.Get_Attribute (XML_Node, "attribute_name"));
      end if;

      --  Return an empty set
      --  (statement only reachable if an empty set is returned).
      return New_Edge_Class_Set;
   end Process_Edge_Class_Entry;
  
end Giant.Edge_Class_Proc;
