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
--  $RCSfile: giant-layout_factory.adb,v $, $Revision: 1.26 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Giant.Config;
with Giant.Config.Class_Sets;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);
with Giant.Matrix_Layouts;
with Giant.Tree_Layouts;
with Giant.String_Split;

with Lists;
pragma Elaborate_All (Lists);
with String_Lists;

package body Giant.Layout_Factory is

   ---------------------------------------------------------------------------
   package Logger is new Giant.Logger ("Layout_Factory");

   ---------------------------------------------------------------------------
   procedure Create
     (Algorithm             : in     String;
      Selection_To_Layout   : in     Graph_Lib.Selections.Selection;
      Widget                : in     Graph_Widgets.Graph_Widget;
      Widget_Lock           : in     Graph_Widgets.Lock_Type;
      Target_Position       : in     Giant.Vis.Logic.Vector_2d;
      Additional_Parameters : in     String;
      Layout_Evolution      :    out Evolutions.Evolution_Class_Access)
   is

      -------------------------------------------------------------------------
      --  Parameters:
      --    Data: aren't parsed, since matrix doesn't get
      --          any additional parameters
      --          It is got here to ensure consistency with the calling of
      --          the other layouts
      procedure Parse_Matrix_Parameters
        (Data : in String)
      is
      begin
         Layout_Evolution := Giant.Evolutions.Evolution_Class_Access
           (Matrix_Layouts.Initialize
            (Widget              => Widget,
             Widget_Lock         => Widget_Lock,
             Release_Widget_Lock => True,
             Selection_To_Layout => Selection_To_Layout,
             Target_Position     => Target_Position));
      end Parse_Matrix_Parameters;

      -------------------------------------------------------------------------
      --  Attention! - ";" may not be used within parameters!
      --    "Hello; you, you 2" is invalid, but "Hello you, you 2"
      --                                        is valid
      procedure Parse_Tree_Parameters
        (Data : in String)
      is

         ----------------------------------------------------------------------
         --  Parameters:
         --    Data - a string in the format "([<class set>[,<class set>]*]"
         --
         --  Returns:
         --    A meta class set containing all class sets
         --
         --  Precondition:
         --    Config.Class_Sets has to be initialized
         --    (i.e. routines there don't raise
         --     Class_Sets_ADO_Not_Initialized_Exception)
         function Convert_String_To_Meta_Class_Set
           (Data : in String)
           return Config.Class_Sets.Meta_Class_Set_Access
         is

            ------------------------------------------------------------------
            Class_Sets_String_List : String_Lists.List;
            String_Iter            : String_Lists.ListIter;

            Cur_String             : Ada.Strings.Unbounded.Unbounded_String;
            Cur_Class_Set          : Config.Class_Sets.Class_Set_Access;

            Class_Sets_List        : Config.Class_Sets.Class_Sets_Lists.List;

            Res                    : Config.Class_Sets.Meta_Class_Set_Access;

         begin
            Logger.Debug ("Got class-sets (String) " & Data);

            Class_Sets_String_List := String_Split.Split_String
              (Source  => Data,
               Pattern => ",",
               Trim    => true);

            --  convert String_List into a list of Class_Set_Accesses

            Logger.Debug ("These are " &
                          Integer'Image (String_Lists.Length
                                         (Class_Sets_String_List)) & " " &
                          "strings containing a class set");

            Class_Sets_List := Config.Class_Sets.Class_Sets_Lists.Create;

            String_Iter := String_Lists.MakeListIter (Class_Sets_String_List);
            while String_Lists.More (String_Iter) loop
               String_Lists.Next (String_Iter, Cur_String);
               declare
                  Cur_Class_Set_Name : String :=
                    Ada.Strings.Unbounded.To_String (Cur_String);
               begin
                  Cur_Class_Set := Config.Class_Sets.Get_Class_Set_Access
                    (Cur_Class_Set_Name);
                  --  exception risen here, if Class_Set could not be found

                  --  If no exception was risen, add converted class_set
                  Config.Class_Sets.Class_Sets_Lists.Attach
                    (Class_Sets_List, Cur_Class_Set);
               exception
                  when Config.Class_Sets.Class_Set_Does_Not_Exist_Exception =>
                     Logger.Error ("Class_Set " & Cur_Class_Set_Name &
                                   " not found");
               end;
            end loop;

            Logger.Debug ("From that list, " &
                          Integer'Image
                          (Config.Class_Sets.Class_Sets_Lists.Length
                           (Class_Sets_List)) & " " &
                          "sets could be converted to IDs");

            --  Build returns an empty class-set, if given list is empty
            Res := Config.Class_Sets.Build (Class_Sets_List);

            Config.Class_Sets.Class_Sets_Lists.Destroy (Class_Sets_List);
            String_Lists.Destroy (Class_Sets_String_List);

            --  Logger.Debug ("After building, " &
            --                "<TBD> " &
            --                "Class Sets are remaining");
            --  Logger.Debug ("They could not be converted from string to ID");

            return Res;
         end Convert_String_To_Meta_Class_Set;

         --  needed by Get_Root_Node
         Meta_Class_Set    : Config.Class_Sets.Class_Set_Access;
         Reverse_Edges     : Boolean;

         ----------------------------------------------------------------------
         --  Extracts Root_Node_Id out of given string
         --
         --  if an empty string is given, the root is searched in the tree
         --
         --  Raises:
         --    Invalid_Format, if root node can't be found
         function Get_Root_Node
           (Id : in String)
           return Graph_Lib.Node_Id
         is
            Node            : Graph_Lib.Node_Id;

            Current_Node    : Graph_Lib.Node_Id;
            Edges           : Graph_Lib.Edge_Id_Set;

            Nodes_To_Layout : Graph_Lib.Node_Id_Set;
            Node_Iterator   : Graph_Lib.Node_Id_Sets.Iterator;

            Current_Edge    : Graph_Lib.Edge_Id;
            Edge_Iterator   : Graph_Lib.Edge_Id_Sets.Iterator;

            Predecessor_Found : Boolean;
            Root_Found        : Boolean;

            Seen            : Graph_Lib.Node_Id_Set;
         begin
            if Id'Length = 0 then
               --  no root was given --> search for root-node in tree

               --  first element of set is starting point for search
               Node_Iterator := Graph_Lib.Node_Id_Sets.Make_Iterator
                 (Graph_Lib.Selections.Get_All_Nodes (Selection_To_Layout));
               if Graph_Lib.Node_Id_Sets.More (Node_Iterator) then
                  Current_Node := Graph_Lib.Node_Id_Sets.Current
                    (Node_Iterator);
               else
                  --  nothing to layout
                  Current_Node := Graph_Lib.Invalid_Node_Id;
               end if;
               Graph_Lib.Node_Id_Sets.Destroy (Node_Iterator);

               if Graph_Lib."=" (Current_Node, Graph_Lib.Invalid_Node_Id) then
                  --  there is nothing to layout, cancel finding
                  --    of root of tree
                  return Graph_Lib.Invalid_Node_Id;
               end if;

               Seen := Graph_Lib.Node_Id_Sets.Empty_Set;
               Graph_Lib.Node_Id_Sets.Insert (Seen, Current_Node);

               loop
                  if Reverse_Edges then
                     Edges := Graph_Lib.Get_Outgoing_Edges
                       (Current_Node);
                  else
                     Edges := Graph_Lib.Get_Incoming_Edges
                       (Current_Node);
                  end if;
                  Edge_Iterator := Graph_Lib.Edge_Id_Sets.Make_Iterator
                    (Edges);

                  --  algorithm analogue to
                  --  Tree_Layouts.Init_Calculation_Part_One

                  Root_Found := False;
                  Predecessor_Found := False;

                  while not Predecessor_Found
                    and then Graph_Lib.Edge_Id_Sets.More (Edge_Iterator) loop
                     Graph_Lib.Edge_Id_Sets.Next (Edge_Iterator, Current_Edge);

                     if Config.Class_Sets.Is_Empty
                       (Meta_Class_Set) or else
                       Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
                       (Meta_Class_Set,
                        Graph_Lib.Get_Edge_Class_Id (Current_Edge)) then

--                          Logger.Debug ("Looking at edge " & Graph_Lib.Edge_Id_Image (Current_Edge));

                        if Reverse_Edges then
                           Node := Graph_Lib.Get_Target_Node (Current_Edge);
                        else
                           Node := Graph_Lib.Get_Source_Node (Current_Edge);
                        end if;

                        if Graph_Lib.Node_Id_Sets.Is_Member
                          (Graph_Lib.Selections.Get_All_Nodes
                           (Selection_To_Layout), Node) then
                           Predecessor_Found := True;

                           Current_Node := Node;

                           if Graph_Lib.Node_Id_Sets.Is_Member
                             (Seen, Node) then
                              --  there is a circle in the graph.

--                                Logger.Debug ("Circle");

                              Root_Found := True;
                           else
                              Graph_Lib.Node_Id_Sets.Insert (Seen, Node);
                              Root_Found := False;

--                                Logger.Debug ("no circle");
                           end if;
                        end if;
                     end if;
                  end loop;
                  Graph_Lib.Edge_Id_Sets.Destroy (Edge_Iterator);

                  Graph_Lib.Edge_Id_Sets.Destroy (Edges);

                  exit when not Predecessor_Found or Root_Found;
               end loop;
--                 Logger.Debug (Boolean'Image (Predecessor_Found) & " " & Boolean'Image (Root_Found));

               Graph_Lib.Node_Id_Sets.Destroy (Seen);

--                 Logger.Debug ("Found root " & Graph_Lib.Node_Id_Image (Current_Node));

               --  current node is the root of the tree
               return Current_Node;
            else
               begin
                  return Graph_Lib.Node_Id_Value (Id);
               exception
                  when Graph_Lib.Node_Does_Not_Exist =>
                     Layout_Evolution := null;
                     Ada.Exceptions.Raise_Exception (Invalid_Format'Identity,
                                                     "Invalid root node");
               end;
            end if;
         end Get_Root_Node;

         Root_Node         : Giant.Graph_Lib.Node_Id;

         Parameters        : String_Lists.List;
         Parameters_Iter   : String_Lists.ListIter;
         Current_Parameter : Ada.Strings.Unbounded.Unbounded_String;

         --  It is not possible to convert the paramter to a Root_Node-Id,
         --    because of the automatic searching, where a class-set is needed
         Root_Node_String  : Ada.Strings.Unbounded.Unbounded_String;

      begin
         --  Split parameters
         Parameters := String_Split.Split_String (Data, ";");

         --  Check amount of parameters
         if String_Lists.Length (Parameters) < 2 then
            Layout_Evolution := null;
            Ada.Exceptions.Raise_Exception (Invalid_Format'Identity,
                                            "Not enough parameters");
         end if;

         Parameters_Iter := String_Lists.MakeListIter (Parameters);

         --  Get String for geeting Root-Id
         --    The getting itself is done later because of the possible
         --    automatic calculation of the Id
         String_Lists.Next (Parameters_Iter, Current_Parameter);
         Root_Node_String := Current_Parameter;

         --  Class_Sets
         String_Lists.Next (Parameters_Iter, Current_Parameter);
         Meta_Class_Set := Convert_String_To_Meta_Class_Set
           (Ada.Strings.Unbounded.To_String (Current_Parameter));

         --  Reverse_Edges?
         if String_Lists.More (Parameters_Iter) then
            String_Lists.Next (Parameters_Iter, Current_Parameter);
            Reverse_Edges :=
              (Ada.Strings.Unbounded.To_String (Current_Parameter) =
               Process_Edges_Reverse);
         else
            Reverse_Edges := False;
         end if;

         --  Class_Sets are initialized now,
         --    the Root-Node can be extracted now
         Root_Node := Get_Root_Node
           (Ada.Strings.Unbounded.To_String (Root_Node_String));

         --  Assure that Root_Node is in Selection
         if not Graph_Lib.Node_Id_Sets.Is_Member
           (Graph_Lib.Selections.Get_All_Nodes (Selection_To_Layout),
            Root_Node) then
            Layout_Evolution := null;

            --  this exception is chosen q&d
            Ada.Exceptions.Raise_Exception (Invalid_Format'Identity,
                                            "Invalid root node");
         end if;

         Layout_Evolution := Giant.Evolutions.Evolution_Class_Access
           (Tree_Layouts.Initialize
            (Widget,
             Widget_Lock,
             Selection_To_Layout,
             Target_Position,
             Root_Node,
             Meta_Class_Set,
             Reverse_Edges));
      end Parse_Tree_Parameters;

      Trimmed_Parameters : String :=
        Ada.Strings.Fixed.Trim (Additional_Parameters, Ada.Strings.Both);

   begin
      --  case for algorithms
      --  the called functions also set the out-parameters
      if Algorithm = "matrix" then
         Parse_Matrix_Parameters (Trimmed_Parameters);
      elsif Algorithm = "tree" then
         Parse_Tree_Parameters (Trimmed_Parameters);
      else
         Layout_Evolution := null;
         raise Unknown_Algorithm;
      end if;
   end Create;

end Giant.Layout_Factory;
