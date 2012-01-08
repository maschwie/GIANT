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
--  $RCSfile: giant-iml_class_inheritance_proc.adb,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
with DOM.Core.Elements;  -- from xmlada

with Giant.Logger;  -- from GIANT
pragma Elaborate_All (Giant.Logger);

package body Giant.IML_Class_Inheritance_Proc is

   package Logger is new Giant.Logger("Giant.IML_Class_Inheritance_Proc");

   ---------------------------------------------------------------------------
   --  <!ATTLIST super_node_class
   --    super_node_class_name CDATA #REQUIRED
   --  >
   function Get_All_Sub_Node_Classes
     (XML_Node : in DOM.Core.Node)
     return Graph_Lib.Node_Class_Id_Set is

     New_Node_Class_Set : Graph_Lib.Node_Class_Id_Set;
     A_Node_Class_ID    : Graph_Lib.Node_Class_Id;

   begin

      A_Node_Class_ID := Graph_Lib.Convert_Node_Class_Name_To_Id
        (DOM.Core.Elements.Get_Attribute
          (XML_Node, "super_node_class_name"));

      New_Node_Class_Set := Graph_Lib.Get_Successors
        (Node_Class     => A_Node_Class_ID,
         Include_Parent => True);

      return New_Node_Class_Set;
   end Get_All_Sub_Node_Classes;

   ---------------------------------------------------------------------------
   --  <!ATTLIST super_edge_class
   --    super_start_node_class CDATA #REQUIRED
   --    attribute_name         CDATA #REQUIRED
   --  >
   function Get_All_Sub_Edge_Classes
     (XML_Node : in DOM.Core.Node)
     return Graph_Lib.Edge_Class_Id_Set is

      Node_Class_Set      : Graph_Lib.Node_Class_Id_Set;
      Node_Class_Set_Iter : Graph_Lib.Node_Class_Id_Sets.Iterator;
      A_Node_Class_ID     : Graph_Lib.Node_Class_Id;
      A_Node_Attribute_ID : Graph_Lib.Node_Attribute_Id;

      A_Edge_Class_ID     : Graph_Lib.Edge_Class_id;
      New_Edge_Class_Set  : Graph_Lib.Edge_Class_Id_Set;
      Temp_Edge_Class_Set : Graph_Lib.Edge_Class_Id_Set;
   begin

      A_Node_Class_ID := Graph_Lib.Convert_Node_Class_Name_To_Id
        (DOM.Core.Elements.Get_Attribute
          (XML_Node, "super_start_node_class"));

      Node_Class_Set := Graph_Lib.Get_Successors
        (Node_Class     => A_Node_Class_ID,
         Include_Parent => True);
      Node_Class_Set_Iter := Graph_Lib.Node_Class_Id_Sets.Make_Iterator
        (Node_Class_Set);

      New_Edge_Class_Set := Graph_Lib.Edge_Class_Id_Sets.Empty_Set;

      while Graph_Lib.Node_Class_Id_Sets.More (Node_Class_Set_Iter) loop

         Graph_Lib.Node_Class_Id_Sets.Next
           (Node_Class_Set_Iter, A_Node_Class_ID);

         -- case 1 (node_class_name, edge_attribute_name)
         if (DOM.Core.Elements.Get_Attribute
           (XML_Node, "attribute_name") /= "*") then

            A_Node_Attribute_ID := Graph_lib.Convert_Node_Attribute_Name_To_Id
              (Node_Class          => A_Node_Class_ID,
               Node_Attribute_Name => DOM.Core.Elements.Get_Attribute
                 (XML_Node, "attribute_name"));

            --  Check whether this attribute is an edge
            if Graph_Lib.Does_Edge_Class_Exist
              (A_Node_Class_ID, A_Node_Attribute_ID) then

               A_Edge_Class_ID :=
                 Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
                    (A_Node_Class_ID, A_Node_Attribute_ID);

               Graph_Lib.Edge_Class_Id_Sets.Insert
                 (New_Edge_Class_Set, A_Edge_Class_ID);
            end if;

         -- case 2 (node_class_name, "*")
         else

            Temp_Edge_Class_Set :=
              Graph_Lib.Get_All_Edge_Class_Ids_For_Node_Class
                (A_Node_Class_ID);
            Graph_Lib.Edge_Class_Id_Sets.Union
              (New_Edge_Class_Set, Temp_Edge_Class_Set);
            Graph_Lib.Edge_Class_Id_Sets.Destroy (Temp_Edge_Class_Set);
         end if;

      end loop;

      Graph_Lib.Node_Class_Id_Sets.Destroy (Node_Class_Set_Iter);
      Graph_Lib.Node_Class_Id_Sets.Destroy (Node_Class_Set);

      return New_Edge_Class_Set;
   end Get_All_Sub_Edge_Classes;

end Giant.IML_Class_Inheritance_Proc;
