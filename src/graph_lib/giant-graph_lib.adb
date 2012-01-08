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
--  $RCSfile: giant-graph_lib.adb,v $, $Revision: 1.78 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $

--  from ADA
with Ada.Unchecked_Deallocation;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Tags;

--  from Bauhaus
with Tagged_Ptr_Hash;
pragma Elaborate_All (Tagged_Ptr_Hash);
with Tagged_Constant_Ptr_Ops;
with Untagged_Ptr_Ops;

with SLocs;
with Storables;
with IML_Classes;
with IML_Graphs;
with IML.IO;
with IML_Reflection;
with Literal_Types;
with Lists;

--  from Giant
with Giant.Ptr_Normal_Hashs;
pragma Elaborate_All (Giant.Ptr_Normal_Hashs);
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);


package body Giant.Graph_Lib is

   package Logger is new Giant.Logger("giant.graph_lib");

   Invalid_Node_Attribute_Id : constant Node_Attribute_Id := null;

   --  reference to loaded IML-Graph
   --  Created in "Create"
   --  Destroyed in "Destroy"
   IML_Graph : IML_Graphs.IML_Graph;

   package Edge_Id_Array_Routines is new Edge_Id_Sets.Arrays
     (Item_Array => Edge_Id_Array);

   ---------------------
   --  Miscellaneous  --
   ---------------------

   ---------------------------------------------------------------------------
   function "<"
     (Left  : Node_Id;
      Right : Node_Id)
     return Boolean
   is

      package Compare is new Untagged_Ptr_Ops
        (T     => Node_Record,
         T_Ptr => Node_Id);

   begin
      return Compare.Less (Left, Right);
   end "<";

   ---------------------------------------------------------------------------
   function "<"
     (Left  : Node_Class_Id;
      Right : Node_Class_Id)
      return Boolean
   is

      package Compare is new Tagged_Constant_Ptr_Ops
        (T     => IML_Reflection.Abstract_Class'Class,
         T_Ptr => Node_Class_Id);

   begin
      return Compare.Less (Left, Right);
   end "<";

   ---------------------------------------------------------------------------
   function "<"
     (Left  : Edge_Id;
      Right : Edge_Id)
      return Boolean
   is

      package Compare is new Untagged_Ptr_Ops
        (T     => Edge_Record,
         T_Ptr => Edge_Id);

   begin
      return Compare.Less (Left, Right);
   end "<";

   ---------------------------------------------------------------------------
   --  Maybe this routine has to be rewritten, since it contains a partial
   --    ordering and not an absolute one. The latter could be achieved using
   --    the pointer's value in the memory, but I currently do not know how
   --    to do that in a clean way
   function "<"
     (Left  : Edge_Class_Id;
      Right : Edge_Class_Id)
      return Boolean
   is

      package Compare is new Untagged_Ptr_Ops
        (T     => Edge_Class,
         T_Ptr => Edge_Class_Id);

   begin
      return Compare.Less (Left, Right);
   end "<";

   ---------------------------------------------------------------------------
   function "<"
      (Left  : Node_Attribute_Id;
       Right : Node_Attribute_Id)
      return Boolean
   is

      package Compare is new Tagged_Constant_Ptr_Ops
        (T     => IML_Reflection.Field'Class,
         T_Ptr => Node_Attribute_Id);

   begin
      return Compare.Less (Left, Right);
   end "<";

   ---------------------------------------------------------------------------
   function "="
     (Left  : Node_Class_Id;
      Right : Node_Class_Id)
      return Boolean
   is
   begin
      return IML_Reflection."=" (Left, Right);
   end "=";

   ---------------------------------------------------------------------------
   function Convert_Edge_Class_Tag_To_Id
     (Edge_Class_Tag : in String)
     return Edge_Class_Id
   is
      Node_Class : Node_Class_Id;
      Attribute  : Node_Attribute_Id;
      Point_Pos  : Natural;
   begin
      Point_Pos := Ada.Strings.Fixed.Index
        (Edge_Class_Tag, ".");
      declare
         Node_Class_Name : String := Edge_Class_Tag
           (Edge_Class_Tag'First..Point_Pos-Edge_Class_Tag'First);
         Attribute_Name  : String := Edge_Class_Tag
           (Edge_Class_Tag'First+Point_Pos..Edge_Class_Tag'Last);
      begin
         Node_Class := Convert_Node_Class_Name_To_Id
           (Node_Class_Name);
         Attribute  := Convert_Node_Attribute_Name_To_Id
           (Node_Class, Attribute_Name);
         return Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
           (Node_Class, Attribute);
      end;
   end Convert_Edge_Class_Tag_To_Id;

   ---------------------------------------------------------------------------
   function Convert_Node_Attribute_Class_Id_To_Name
     (Node_Attribute_Class : in Node_Attribute_Class_Id)
     return String
   is
   begin
      case Node_Attribute_Class is
         when Class_Node_Id =>
           return "Node_Id";
         when Class_Node_Id_List =>
           return "Node_Id_List";
         when Class_Node_Id_Set =>
           return "Node_Id_Set";
         when Class_String =>
           return "String";
         when Class_SLoc =>
           return "SLoc";
         when Class_Boolean =>
           return "Boolean";
         when Class_Natural =>
            return "Natural";
         when Class_Identifier =>
            return "Identifier";
         when Class_Invalid =>
            return "INVALID";
      end case;
   end Convert_Node_Attribute_Class_Id_To_Name;

   ---------------------------------------------------------------------------
   function Convert_Node_Attribute_Id_To_Name
     (Node_Attribute : in Node_Attribute_Id)
      return String
   is
   begin
      return Node_Attribute.Name;
   end Convert_Node_Attribute_Id_To_Name;

   ---------------------------------------------------------------------------
   function Convert_Node_Attribute_Name_To_Id
     (Node_Class_Name     : in String;
      Node_Attribute_Name : in String)
     return Node_Attribute_Id
   is
      Node_Class     : Node_Class_Id;
   begin
      Node_Class := Convert_Node_Class_Name_To_Id (Node_Class_Name);

      return Convert_Node_Attribute_Name_To_Id
        (Node_Class, Node_Attribute_Name);
   end Convert_Node_Attribute_Name_To_Id;

   ---------------------------------------------------------------------------
   --  Performance can be increased if no linear search, but a hasing is used
   --    (In create() the hasing should be instantiated)
   --
   --  Case-senstive search
   function Convert_Node_Attribute_Name_To_Id
     (Node_Class          : in Node_Class_Id;
      Node_Attribute_Name : in String)
     return Node_Attribute_Id
   is
      I         : Integer;
   begin
      I := Node_Class.Fields'First;
      while
        (I <= Node_Class.Fields'Last) and then
        (Node_Class.Fields (I).Name /= Node_Attribute_Name) loop
         I := I + 1;
      end loop;

      if I <= Node_Class.Fields'Last then
         return Node_Class.Fields(I);
      else
         raise Node_Attribute_Does_Not_Exist;
      end if;
   end Convert_Node_Attribute_Name_To_Id;

   ---------------------------------------------------------------------------
   function Convert_Node_Class_Name_To_Id
     (Node_Class_Name : in String)
      return Node_Class_Id
   is
      I           : Integer;
   begin
      --  straightforward-implementation, since this function is only
      --  needed during the initialization

      I := All_Node_Classes'First;
      while
        (I <= All_Node_Classes'Last) and then
        (All_Node_Classes (I).Name /= Node_Class_Name) loop
         I := I+1;
      end loop;

      if I <= All_Node_Classes'Last then
         return All_Node_Classes (I);
      else
         raise Node_Class_Does_Not_Exist;
      end if;
   end Convert_Node_Class_Name_To_Id;

   ---------------------------------------------------------------------------
   function Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
     (Node_Class     : in Node_Class_Id;
      Node_Attribute : in Node_Attribute_Id)
      return Edge_Class_Id
   is
      Node_Class_Data : Node_Class_Id_Hash_Data_Access;
      Edge_Class      : Edge_Class_Id;
   begin
      begin
         Node_Class_Data := Node_Class_Id_Hashed_Mappings.Fetch
           (Node_Class_Id_Mapping, Node_Class);
      exception
         when Node_Class_Id_Hashed_Mappings.Uninitialized_Mapping =>
            --  TBD: replace with own exception
            raise Constraint_Error;
         when Node_Class_Id_Hashed_Mappings.Not_Bound =>
            raise Node_Class_Does_Not_Exist;
            --  return Invalid_Edge_Class_Id;
      end;

      begin
         Edge_Class := Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.Fetch
           (Node_Class_Data.Node_Attribute_Id_Mapping, Node_Attribute);
      exception
         when Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
           Uninitialized_Mapping =>
            --  TBD: replace with own exception
            raise Constraint_Error;
         when Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.Not_Bound =>
            raise Node_Attribute_Does_Not_Exist;
      end;

      return Edge_Class;
   end Convert_Node_Class_Node_Attribute_To_Edge_Class_Id;

   -----------------------------
   --  Basical init-routines  --
   -----------------------------

   ---------------------------------------------------------------------------
   procedure Initialize
   is
      Cur_Class   : Node_Class_Id;
      Hash_Data   : Node_Class_Id_Hash_Data_Access;
   begin
      Node_Class_Id_Mapping := Node_Class_Id_Hashed_Mappings.Create;

      Initialize_All_Node_Classes;

      for I in All_Node_Classes'Range loop
         Cur_Class := All_Node_Classes (I);

         Hash_Data := new Node_Class_Id_Hash_Data;
         Hash_Data.Node_Attribute_Id_Mapping :=
           Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.Create;

         Node_Class_Id_Hashed_Mappings.Bind
              (Node_Class_Id_Mapping,
               Cur_Class,
               Hash_Data);

         for J in Cur_Class.Fields'Range loop
            declare
               Cur_Field : IML_Reflection.Field_Id := Cur_Class.Fields (J);
            begin
               if (Cur_Field.all in IML_Reflection.Edge_Field) or
                 (Cur_Field.all in IML_Reflection.List_Field) or
                 (Cur_Field.all in IML_Reflection.Set_Field) then
                  declare
                     New_Edge_Class : Edge_Class_Id;
                  begin
                     New_Edge_Class := new Edge_Class;
                     New_Edge_Class.Source_Node_Class := Cur_Class;
                     New_Edge_Class.Source_Node_Attribute := Cur_Field;

                     Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.Bind
                       (Hash_Data.Node_Attribute_Id_Mapping,
                        Cur_Field,
                        New_Edge_Class);
                  end;
               end if;
            end;
         end loop;

      end loop;
   end Initialize;

   ---------------------------------------------------------------------------
   procedure Initialize_All_Node_Classes is
   begin
      All_Node_Classes := IML_Classes.Get_All_Classes;
   end Initialize_All_Node_Classes;

   ---------------------------------------------------------------------------
   procedure Destroy
   is
   begin
      --  Destroy_Node_Class_Id_Mapping
      --  TBD: deep-destroy!
      null;
   end Destroy;

   -----------------------------------------------------------
   --  Create the internal representation of the IML-Graph  --
   -----------------------------------------------------------
   procedure Load
     (Path_To_IML_File : in String;
      Individual       : in Basic_Evolutions.Basic_Evolution_Access := null)
     is

      Node_Count : Natural := 0;
      Cancel : Boolean := False;

      -----------------------------------------------------------
      --  Data-Structore for temporary graph used for loading  --
      -----------------------------------------------------------

      package Load_Nodes is

         type Node_Record;
         type Node_Access is access all Node_Record;

         type Edge_Record is record
            Source     : Node_Access;
            Target     : Node_Access;

            --  like in "outer" Edge_Record
            Attribute                : Node_Attribute_Id;
            Attribute_Element_Number : Natural;
         end record;

         type Edge_Access is access Edge_Record;

         package Edge_Lists is new Lists
           (ItemType => Edge_Access);

         type Node_Record is record
            Edges_In      : Edge_Lists.List;
            Edges_Out     : Edge_Lists.List;
            IML_Node      : IML_Roots.IML_Root;

            --  Used at conversion from temporary structure to
            --  graph_lib-internal structure

            --  reference to the "real" (i.e. in the running used) node
            Internal_Node : Node_Id;

            --  Last used index of Node_Record.Incoming_Edges
            --    Node_Record: "real" Node_Record
            Last_Incoming_Edge : Natural := Node_Edges_First_Index - 1;
         end record;

         --  Creates an edge in the package-internal structure
         procedure Create_Edge
           (Edge_Source              : in Node_Access;
            Edge_Target              : in Node_Access;
            Attribute                : in Node_Attribute_Id;
            Attribute_Element_Number : in Natural);

         --  Creates a node in the internal structure
         --  they are NOT collected in an internal list
         --  The caller has to handle the destroyage
         function Create_Node
           (IMLNode : IML_Roots.IML_Root)
           return Node_Access;

         --  Destroys the node and all outgoing edges
         --  ! The edges pointing to this node are /not/ destroyed
         procedure Destroy_Node (NodeToDestroy : in out Node_Access);

         package Node_Queues is new Lists (ItemType => Node_Access);
         subtype Node_Queue is Node_Queues.List;

      end Load_Nodes;

      package body Load_Nodes is

         procedure Create_Edge
           (Edge_Source : in Node_Access;
            Edge_Target : in Node_Access;
            Attribute   : in Node_Attribute_Id;
            Attribute_Element_Number : in Natural) is

            Edge : Edge_Access := new Edge_Record;
         begin
            --  initialize edge
            Edge.Source      := Edge_Source;
            Edge.Target      := Edge_Target;
            Edge.Attribute   := Attribute;
            Edge.Attribute_Element_Number := Attribute_Element_Number;

            --  make it known to connected nodes
            Edge_Lists.Attach (Edge, Edge_Target.Edges_In);
            Edge_Lists.Attach (Edge, Edge_Source.Edges_Out);
         end Create_Edge;

         function Create_Node
           (IMLNode : IML_Roots.IML_Root)
           return Node_Access is
            New_Node : Node_Access := new Node_Record;
         begin
            New_Node.IML_Node  := IMLNode;
            New_Node.Edges_In  := Edge_Lists.Create;
            New_Node.Edges_Out := Edge_Lists.Create;
            Node_Count := Node_Count + 1;
            return New_Node;
         end Create_Node;

         procedure Destroy_Node (NodeToDestroy : in out Node_Access) is

            procedure Free_Node is new Ada.Unchecked_Deallocation
              (Node_Record, Node_Access);

            procedure Free_Edge is new Ada.Unchecked_Deallocation
              (Edge_Record, Edge_Access);

            procedure DestroyDeep_Edges is new Edge_Lists.DestroyDeep
              (Dispose => Free_Edge);

         begin
            DestroyDeep_Edges (NodeToDestroy.Edges_Out);
            Free_Node (NodeToDestroy);
         end Destroy_Node;

      end Load_Nodes;


      -----------------------------
      --  Hashing for IML_Nodes  --
      -----------------------------

      --  Oriented on vis_test.IML_graph_loader
      package IML_Node_Mapper is
         procedure Create;
         procedure Destroy;

         ---------------------------------------------------------------------
         --  Gets an node out of the hashtable
         --  Creates it, if it doesn't exist
         --
         --  Parameters:
         --    Created': True,  if a mapping was created
         --              False, if a mapping already existed
         procedure Get
           (IML_Node : in     IML_Roots.IML_Root;
            Node     :    out Load_Nodes.Node_Access;
            Created  :    out Boolean);
      end IML_Node_Mapper;

      package body IML_Node_Mapper is

         function IML_Roots_Hash is new Tagged_Ptr_Hash
           (T     => IML_Roots.IML_Root_Class,
            T_Ptr => IML_Roots.IML_Root);

         package Mapping_IML_Load_Nodes is new Hashed_Mappings
           (Key_Type   => IML_Roots.IML_Root,
            Hash       => IML_Roots_Hash,
            Value_Type => Load_Nodes.Node_Access);

         Mapping : Mapping_IML_Load_Nodes.Mapping;

         procedure Create is
         begin
            Mapping := Mapping_IML_Load_Nodes.Create (4194300);
         end Create;

         procedure Destroy is
         begin
            Mapping_IML_Load_Nodes.Destroy (Mapping);
         end Destroy;

         procedure Get
           (IML_Node : in     IML_Roots.IML_Root;
            Node     :    out Load_Nodes.Node_Access;
            Created  :    out Boolean) is
         begin
            if Mapping_IML_Load_Nodes.Is_Bound (Mapping, IML_Node) then
               Node := Mapping_IML_Load_Nodes.Fetch (Mapping, IML_Node);
               Created := False;
            else
               Node := Load_Nodes.Create_Node (IML_Node);
               Mapping_IML_Load_Nodes.Bind (Mapping, IML_Node, Node);
               Created := True;
            end if;
         end Get;

      end IML_Node_Mapper;

      ------------------------------------------------------------------------
      --  Parameters:
      --    Queue' : All generated nodes will be stored there
      procedure Convert_IML_Graph_To_Temp_Structure
        (Queue : out Load_Nodes.Node_Queue)
      is

         procedure Process_Queue is

            ------------------------------------------------------------------
            --  used for getting a much better performance
            --    at Node_Queues.Attach
            Queue_Tail     : Load_Nodes.Node_Queues.List;

            ------------------------------------------------------------------
            --  Does all necessary things, if there's an edge from
            --    a Node to an IML_Node
            --
            --  * Checks if target is valid
            --    if yes:
            --      * Creates/fetches target node
            --      * Creates edge
            procedure Process_Edge
              (Source_Node              : in Load_Nodes.Node_Access;
               Target                   : in Storables.Storable;
               Attribute                : in Node_Attribute_Id;
               Attribute_Element_Number : in Natural) is

               Created     : Boolean;
               Target_Node : Load_Nodes.Node_Access;

               New_Queue_Tail : Load_Nodes.Node_Queues.List;

            begin
               --  Logger.Debug ("Begin: Process_Edge");

               if (Storables."/=" (Target, null)) and then
                 (Is_IML_Root (Target)) then
                  IML_Node_Mapper.Get
                    (IML_Roots.IML_Root (Target), Target_Node, Created);

                  --  Logger.Debug ("Fetched Target_Node");

                  if Created then
                     New_Queue_Tail := Load_Nodes.Node_Queues.MakeList
                       (Target_Node);
                     Load_Nodes.Node_Queues.Attach
                       (Queue_Tail, New_Queue_Tail);
                     Queue_Tail := New_Queue_Tail;
                     --  Logger.Debug ("Attached Target_Node");
                  end if;

                  Load_Nodes.Create_Edge
                    (Source_Node,
                     Target_Node,
                     Attribute,
                     Attribute_Element_Number);

                  --  Logger.Debug ("Edge created");
               end if;

               --  Logger.Debug ("End: Process_Edge");
            end Process_Edge;

            Node  : Load_Nodes.Node_Access;

            ------------------------------------------------------------------
            procedure Process_Attribute
              (IML_Node  : in IML_Roots.IML_Root;
               Attribute : in IML_Reflection.Field_ID) is
            begin
               --  Logger.Debug ("Begin: Process_Attribute");

               if Attribute.all in IML_Reflection.Edge_Field'Class then
                  declare
                     IML_Edge  : IML_Reflection.Edge_Field
                       := IML_Reflection.Edge_Field (Attribute.all);
                     Target    : Storables.Storable;
                  begin
                     Target   := IML_Edge.Get_Target (IML_Node);
                     if Storables."/=" (Target, null) then
                        Process_Edge (Node, Target, Attribute, 0);
                        --  else
                        --    Logger.Info
                        --      ("Edge_Field with null target ignored");
                     end if;
                  end;
               elsif Attribute.all in IML_Reflection.List_Field'Class then
                  declare
                     IML_List  : IML_Reflection.List_Field
                       := IML_Reflection.List_Field (Attribute.all);
                     Iter      : IML_Reflection.List_Iterator;
                     Target    : Storables.Storable;
                     I         : Natural := 0;
                  begin
                     Iter     := IML_List.Make_Iterator (IML_Node);

                     while IML_Reflection.More (Iter) loop
                        IML_Reflection.Next (Iter, Target);
                        I := I+1;

                        Process_Edge (Node, Target, Attribute, I);
                     end loop;

                     IML_Reflection.Destroy (Iter);
                  end;
               elsif Attribute.all in IML_Reflection.Set_Field'Class then
                  declare
                     IML_Set : IML_Reflection.Set_Field
                       := IML_Reflection.Set_Field (Attribute.all);
                     Iter    : IML_Reflection.Set_Iterator;
                     Target  : Storables.Storable;
                     I       : Natural := 0;
                  begin
                     Iter    := IML_Set.Make_Iterator (IML_Node);

                     while IML_Reflection.More (Iter) loop
                        IML_Reflection.Next (Iter, Target);
                        I := I+1;

                        Process_Edge (Node, Target, Attribute, I);
                     end loop;

                     IML_Reflection.Destroy (Iter);
                  end;
               elsif
                 (Attribute.all in
                  IML_Reflection.Identifier_Field'Class) or
                 (Attribute.all in
                  IML_Reflection.Builtin_Field'Class) then
                  null;
               else
                  Logger.Error (Attribute.Name);
                  Logger.Error ("Unknown IML_Reflection.Field");
               end if;

               --  Logger.Debug ("End: Process_Attribute");
            end Process_Attribute;

            Iter      : Load_Nodes.Node_Queues.ListIter;

            Class     : IML_Reflection.Class_ID;
            Attribute : IML_Reflection.Field_ID;

         begin
            Iter := Load_Nodes.Node_Queues.MakeListIter (Queue);

            Queue_Tail := Queue;

            while Load_Nodes.Node_Queues.More (Iter) loop
               Node := Load_Nodes.Node_Queues.CellValue (Iter);

               --  Logger.Debug ("Processing: " &
               --                     IML_Node_Ids.Image
               --        (Storables.Get_Node_Id (Node.IML_Node)));

               if Is_IML_Root (Node.IML_Node) then
                  --  we process only nodes below IML_Root and no other
                  --  storables

                  Class := IML_Roots.Get_Class_ID
                    (IML_Roots.IML_Root (Node.IML_Node));

                  for I in Class.Fields'Range loop
                     Attribute := Class.Fields (I);
                     Process_Attribute (Node.IML_Node, Attribute);
                  end loop;

               end if;
               Load_Nodes.Node_Queues.Forward (Iter);
            end loop;
         end Process_Queue;

         Root_Node : IML_Roots.IML_Root;
         Created   : Boolean;
         Node      : Load_Nodes.Node_Access;

      begin
         Logger.Debug ("Begin: Convert_IML_Graph_To_Temp_Structure");

         Queue := Load_Nodes.Node_Queues.Create;

         Root_Node := IML_Graphs.Get_Raw_Graph (IML_Graph);

         IML_Node_Mapper.Get (Root_Node, Node, Created);

         Load_Nodes.Node_Queues.Attach (Queue, Node);

         Process_Queue;
      end Convert_IML_Graph_To_Temp_Structure;

      -------------------------------------------------------------------------
      --  Converts generated temporary structure to the structure
      --  which is defined by Node_Record etc.
      --
      --  Parameters:
      --    Queue: Queue containing all nodes in temporary structure
      procedure Convert_Temp_Structure_To_Used_Structure
        (Queue : in Load_Nodes.Node_Queue) is

         ---------------------------------------------------------------------
         --  Converts temporary nodes to internal nodes
         --
         --  Parameters:
         --    IML_Node_ID_Mapping': Contains mapping from IML to
         --                          internal nodes created in this procedure
         procedure Convert_Nodes
           (IML_Node_ID_Mapping : out IML_Node_Id_Hashed_Mappings.Mapping)
         is

            Node_Iter : Load_Nodes.Node_Queues.ListIter;
            Cur_Node  : Load_Nodes.Node_Access; --  of the temporary structure
            New_Node  : Node_Id; --  of the internal structure

         begin
            Logger.Debug ("Begin: Convert_Nodes");

            --  Mapping's initial size is about 4 million nodes
            IML_Node_ID_Mapping := IML_Node_ID_Hashed_Mappings.Create
              (4194300);

            Basic_Evolutions.Set_Total_Complexity (Individual, Node_Count);
            Basic_Evolutions.Set_Text
              (Individual, -"Converting Node %v of %u");

            Node_Iter := Load_Nodes.Node_Queues.MakeListIter (Queue);
            while Load_Nodes.Node_Queues.More (Node_Iter) loop
               Load_Nodes.Node_Queues.Next (Node_Iter, Cur_Node);

               New_Node := new Node_Record
                 (Number_Of_Incoming_Edges =>
                    Load_Nodes.Edge_Lists.Length (Cur_Node.Edges_In),
                  Number_Of_Outgoing_Edges =>
                    Load_Nodes.Edge_Lists.Length (Cur_Node.Edges_Out)
                  );

               New_Node.IML_Node := Cur_Node.IML_Node;

               IML_Node_ID_Hashed_Mappings.Bind
                 (IML_Node_ID_Mapping,
                  Storables.Get_Node_ID (New_Node.IML_Node),
                  New_Node);

               --  set variable to enable edge-conversion
               Cur_Node.Internal_Node := New_Node;
               Cancel := Basic_Evolutions.Step (Individual);
            end loop;
         end Convert_Nodes;

         ----------------------------------------------------------------------
         --  In this step, all Edge_Records are created and linked to the
         --    temporary edges
         --
         --  Parameters:
         --    All_Edges_Set': Contains all created edges
         procedure Convert_Edges
           (All_Edges_Set : out Edge_Id_Sets.Set)
         is

            --  up to now, we haven't used any index
            --  later, the index ist /first/ incremented and then set
            --  therefore it is started by one index before the first index
            --  of the All_Edges-Array.
            Last_Internal_Id : Integer := All_Edges_First_Index - 1;

            -------------------------------------------------------------------
            --  * Creates all outgoing edges of a node
            --  * Adds this edges to the corresponding target-node as
            --      incoming edges
            --  * Modifies "All_Edges_Set"
            --      adds created edges to "All_Edges_Set"
            --
            --  Precondition:
            --    Size of TargtArray == Length(Source_List)
            --
            --  Parameters:
            --    Source_List  : List of all outgoing edges
            --    Target_Array : Array to store outgoing edges
            procedure Convert_Edges_Of_A_Node
              (Source_List  : in     Load_Nodes.Edge_Lists.List;
               Target_Array :    out Edge_Id_Array) is

               Edge_Iter : Load_Nodes.Edge_Lists.ListIter;
               Cur_Edge  : Load_Nodes.Edge_Access;

            begin
               pragma Assert (Target_Array'Length =
                                Load_Nodes.Edge_Lists.Length (Source_List));

               Edge_Iter := Load_Nodes.Edge_Lists.MakeListIter (Source_List);

               for I in Target_Array'Range loop
                  Load_Nodes.Edge_Lists.Next (Edge_Iter, Cur_Edge);

                  --  Every new edge gets a higher id
                  Last_Internal_Id := Last_Internal_Id + 1;

                  Target_Array (I) :=
                    new Edge_Record;
                  Target_Array (I).Internal_Id :=
                    Last_Internal_Id;
                  Target_Array (I).Source_Node :=
                    Cur_Edge.Source.Internal_Node;
                  Target_Array (I).Target_Node :=
                    Cur_Edge.Target.Internal_Node;
                  Target_Array (I).Attribute   :=
                    Cur_Edge.Attribute;
                  Target_Array (I).Attribute_Element_Number :=
                    Cur_Edge.Attribute_Element_Number;

                  --  Add edge as incoming edge to target
                  Cur_Edge.Target.Last_Incoming_Edge :=
                    Cur_Edge.Target.Last_Incoming_Edge + 1;
                  Cur_Edge.Target.Internal_Node.Incoming_Edges
                    (Cur_Edge.Target.Last_Incoming_Edge) :=
                    Target_Array (I);

                  --  add it to procedure-internal set holding all edges
                  --  of the graph
                  Edge_Id_Sets.Insert (All_Edges_Set, Target_Array (I));
               end loop;
            end Convert_Edges_Of_A_Node;

            Node_Iter : Load_Nodes.Node_Queues.ListIter;
            Cur_Node  : Load_Nodes.Node_Access; --  of the temporary structure
            New_Node  : Node_Id; --  of the graph_lib-internal structure

         begin
            Logger.Debug ("Begin: Convert_Edges");

            All_Edges_Set := Edge_Id_Sets.Empty_Set;

            Basic_Evolutions.Set_Total_Complexity (Individual, Node_Count);
            Basic_Evolutions.Set_Text
              (Individual, -"Converting Edge %v of %u");

            Node_Iter := Load_Nodes.Node_Queues.MakeListIter (Queue);
            while Load_Nodes.Node_Queues.More (Node_Iter) loop
               Load_Nodes.Node_Queues.Next (Node_Iter, Cur_Node);

               New_Node := Cur_Node.Internal_Node;

               Convert_Edges_Of_A_Node
                 (Cur_Node.Edges_Out,
                  New_Node.Outgoing_Edges);
               Cancel := Basic_Evolutions.Step (Individual);
            end loop;
         end Convert_Edges;

         ----------------------------------------------------------------------
         --  Convert set containing all edges to "public" array of all edges
         --
         --  The conversion cannot be done by Edge_Id_Array_Routines.To_Array
         --    since this stores the array on the stack, which could be
         --    too small (cp Revision 1.36)
         --
         --  Parameters:
         --     First_Index - specifies the index to be used as first
         --                   in the resulting array
         --                   Is needed, because we need this "magic" index
         --                   for the "Edge_Id_Sets.array-routines"
         function Convert_Edge_Set_To_Edge_Array
           (Edges_Set   : in Edge_Id_Sets.Set;
            First_Index : in Integer)
           return Edge_Id_Array_Access
         is

            Edge_Array         : Edge_Id_Array_Access;
            Current_Edge_Index : Integer := First_Index;

            procedure Execute (Edge : Edge_Id)
            is
            begin
               Edge_Array (Current_Edge_Index) := Edge;
               Current_Edge_Index := Current_Edge_Index + 1;
            end Execute;

            procedure Apply is new Edge_Id_Sets.Apply
              (Execute => Execute);

            Last_Index : Integer :=
              First_Index + Edge_Id_Sets.Size (Edges_Set) - 1;
          begin
             Edge_Array := new Edge_Id_Array (First_Index .. Last_Index);
             Apply (Edges_Set);
             return Edge_Array;
          end Convert_Edge_Set_To_Edge_Array;

      begin
         Logger.Debug ("Begin: Convert_Temp_Structure_To_Used_Structure");

         Convert_Nodes (IML_Node_ID_Mapping);

         --  all nodes are existing now
         --  create edges
         declare
            All_Edges_Set : Edge_Id_Sets.Set;
         begin
            Convert_Edges (All_Edges_Set);

            --  Convert set containing all edges to "public" array of all edges
            Logger.Debug ("Begin: Convert_Edge_Set_To_Edge_Array");
            All_Edges := Convert_Edge_Set_To_Edge_Array
              (All_Edges_Set, All_Edges_First_Index);

            Logger.Debug ("Begin: Destroy");
            Edge_Id_Sets.Destroy (All_Edges_Set);
         end;
      end Convert_Temp_Structure_To_Used_Structure;

      -------------------------------------------------------------------------
      --  Destroys the temporary structure,
      --  frees all memory
      --  Queue is deallocated, too
      procedure Destroy_Temp_Structure
        (Queue : in out Load_Nodes.Node_Queue) is

         procedure Dispose
            (Node  : in out Load_Nodes.Node_Access) is
         begin
            Load_Nodes.Destroy_Node (Node);
         end Dispose;

         procedure DestroyDeep_Nodes is new
           Load_Nodes.Node_Queues.DestroyDeep (Dispose => Dispose);

      begin
         Logger.Debug ("Begin: Destroy_Temp_Structure");
         DestroyDeep_Nodes (Queue);
      end Destroy_Temp_Structure;

   begin
      --  Load Graph into memory
      Cancel := Basic_Evolutions.Set_Percentage
        (Individual, 0.1, "Loading IML file");
      Basic_Evolutions.Set_Cancel_Enabled (Individual, False);
      begin
         IML_Graph := IML.IO.Load (Path_To_IML_File);
      exception
         when Storables.Load_Failure =>
            raise Load_Error;
      end;

      Cancel := Basic_Evolutions.Set_Percentage
        (Individual, 0.3, "Creating IML Mapper");
      IML_Node_Mapper.Create;

      declare
         Queue : Load_Nodes.Node_Queue;
      begin
         Cancel := Basic_Evolutions.Set_Percentage
           (Individual, 0.5, "Creating Temporary Structure");
         Convert_IML_Graph_To_Temp_Structure (Queue);

         Cancel := Basic_Evolutions.Set_Percentage
           (Individual, 0.7, "Converting Temporary Structure");
         Convert_Temp_Structure_To_Used_Structure (Queue);

         Cancel := Basic_Evolutions.Set_Percentage
           (Individual, 0.9, "Cleaning up");
         Destroy_Temp_Structure (Queue);
      end;

      IML_Node_Mapper.Destroy;
      Cancel := Basic_Evolutions.Set_Percentage
        (Individual, 1.0, "Done");
   end Load;

   ---------------------------------------------------------------------------
   procedure Unload is

      procedure Destroy_Internal_Graph is

         procedure Free_Edge_Id is new Ada.Unchecked_Deallocation
           (Edge_Record,
            Edge_Id);

         procedure Free_Node_Id is new Ada.Unchecked_Deallocation
           (Node_Record,
            Node_Id);

         Iter    : IML_Node_ID_Hashed_Mappings.Values_Iter;
         Cur_Node : Node_Id;
      begin
         --  There is no Hashed_Mappings.DestroyDeep, therefore we have to do
         --  this by hand

         Iter := IML_Node_ID_Hashed_Mappings.Make_Values_Iter
           (IML_Node_ID_Mapping);

         while IML_Node_ID_Hashed_Mappings.More (Iter) loop
            IML_Node_ID_Hashed_Mappings.Next (Iter, Cur_Node);

            --  remove all the edges out of the memory
            for I in Cur_Node.Outgoing_Edges'Range loop
               Free_Edge_Id (Cur_Node.Outgoing_Edges (I));
            end loop;

            Free_Node_Id (Cur_Node);

         end loop;

         --  The mapping was created at
         --    Convert_Temp_Structure_To_Used_Structure
         IML_Node_ID_Hashed_Mappings.Destroy (IML_Node_ID_Mapping);
      end Destroy_Internal_Graph;

   begin
      Destroy_Internal_Graph;

      --  Unload IML_Graph from memory
      --  FIXME:  IML_Graphs.Destroy (IML_Graph);
      --          crashes if used
   end Unload;

   ---------------------------------------------------------------------------
   function Does_Edge_Class_Exist
     (Node_Class     : in Node_Class_Id;
      Node_Attribute : in Node_Attribute_Id)
      return Boolean
   is
      Hash_Data : Node_Class_Id_Hash_Data_Access;
   begin
      if not Node_Class_Id_Hashed_Mappings.Is_Bound
        (Node_Class_Id_Mapping,
         Node_Class) then
         return False;
      else
         Hash_Data := Node_Class_Id_Hashed_Mappings.Fetch
           (Node_Class_Id_Mapping,
            Node_Class);

         return Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.Is_Bound
           (Hash_Data.Node_Attribute_Id_Mapping,
            Node_Attribute);
      end if;
   end Does_Edge_Class_Exist;

   ---------------------------------------------------------------------------
   function Does_Node_Attribute_Exist
     (Node_Class          : in Node_Class_Id;
      Node_Attribute_Name : in String)
      return Boolean
   is
      Attribute : Node_Attribute_Id;
   begin
      begin
         Attribute := Convert_Node_Attribute_Name_To_Id
           (Node_Class,
            Node_Attribute_Name);

         --  No exception: Attribute exists
         return True;
      exception
         when Node_Attribute_Does_Not_Exist =>
            return False;
      end;
   end Does_Node_Attribute_Exist;

   ---------------------------------------------------------------------------
   function Does_Node_Attribute_Exist
     (Node_Class_Name     : in String;
      Node_Attribute_Name : in String)
      return Boolean
   is
      Attribute : Node_Attribute_Id;
   begin
      begin
         Attribute := Convert_Node_Attribute_Name_To_Id
           (Node_Class_Name,
            Node_Attribute_Name);

         --  No exception: Attribute exists
         return True;
      exception
         when Node_Attribute_Does_Not_Exist =>
            return False;
      end;
   end Does_Node_Attribute_Exist;

   ---------------------------------------------------------------------------
   function Does_Node_Class_Exist
     (Node_Class_Name : in String)
      return Boolean
   is
   begin
      declare
         Node_Class : Node_Class_Id;
      begin
         Node_Class := Convert_Node_Class_Name_To_Id (Node_Class_Name);

         --  no exception risen: class exists
         return True;
      exception
         when Node_Class_Does_Not_Exist =>
            return False;
      end;
   end Does_Node_Class_Exist;

   ---------------------------------------------------------------------------
   --  Loops over the hashtables storing the Edgeclasses and returns all
   --    of them
   function Get_All_Edge_Class_Ids return Edge_Class_Id_Set is
      Set      : Edge_Class_Id_Set;

      IterData : Node_Class_Id_Hashed_Mappings.Values_Iter;
      CurData  : Node_Class_Id_Hash_Data_Access;

      --  used for the inner loop
      IterAttrib : Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
        Values_Iter;
      Cur_Edge_Class  : Edge_Class_Id;

   begin
      Set      := Edge_Class_Id_Sets.Empty_Set;
      IterData := Node_Class_Id_Hashed_Mappings.Make_Values_Iter
        (Node_Class_Id_Mapping);

      while Node_Class_Id_Hashed_Mappings.More (IterData) loop
         Node_Class_Id_Hashed_Mappings.Next (IterData, CurData);

         IterAttrib := Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
           Make_Values_Iter
           (CurData.Node_Attribute_Id_Mapping);

         while Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings
           .More (IterAttrib) loop
            Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
              Next (IterAttrib, Cur_Edge_Class);

            Edge_Class_Id_Sets.Insert (Set, Cur_Edge_Class);
         end loop;
      end loop;

      return Set;
   end Get_All_Edge_Class_Ids;

   ---------------------------------------------------------------------------
   function Get_Node_Class_Tag
     (Node_Class : in Node_Class_Id)
     return String
   is
   begin
      return Node_Class.Name;
   end Get_Node_Class_Tag;

   ---------------------------------------------------------------------------
   --  implemented like Get_Successors
   function Get_Predecessors
     (Node_Class    : in Node_Class_Id;
      Include_Child : in Boolean)
     return Node_Class_Id_Set
   is

      ------------------------------------------------------------------------
      --  Recursivly implemented, since it doesn't need to be fast.
      procedure Get_Predecessors
        (Node_Class : in     Node_Class_Id;
         Res        : in out Node_Class_Id_Set)
      is
      begin
         for I in All_Node_Classes'Range loop
            if All_Node_Classes (I) = Node_Class.Super then
               Node_Class_Id_Sets.Insert (Res, All_Node_Classes (I));
               Get_Predecessors (All_Node_Classes (I), Res);
            end if;
         end loop;
      end Get_Predecessors;

      Res : Node_Class_Id_Set;
   begin
      Res := Node_Class_Id_Sets.Empty_Set;

      if Include_Child then
         Node_Class_Id_Sets.Insert (Res, Node_Class);
      end if;

      Get_Predecessors (Node_Class, Res);

      return Res;
   end Get_Predecessors;

   ---------------------------------------------------------------------------
   function Get_Successors
     (Node_Class     : in Node_Class_Id;
      Include_Parent : in Boolean)
     return Node_Class_Id_Set
   is

      ------------------------------------------------------------------------
      --  Recursivly implemented, since it doesn't need to be fast.
      procedure Get_Successors
        (Node_Class : in     Node_Class_Id;
         Res        : in out Node_Class_Id_Set)
      is
      begin
         for I in All_Node_Classes'Range loop
            if All_Node_Classes (I).Super = Node_Class then
               Node_Class_Id_Sets.Insert (Res, All_Node_Classes (I));
               Get_Successors (All_Node_Classes (I), Res);
            end if;
         end loop;
      end Get_Successors;

      Res : Node_Class_Id_Set;
   begin
      Res := Node_Class_Id_Sets.Empty_Set;

      if Include_Parent then
         Node_Class_Id_Sets.Insert (Res, Node_Class);
      end if;

      Get_Successors (Node_Class, Res);

      return Res;
   end Get_Successors;

   ---------------------------------------------------------------------------
   function Edge_Id_Image
     (Edge : in Edge_Id)
     return String
   is
   begin
      return Natural'Image (Edge.Internal_Id);
   end Edge_Id_Image;

   ---------------------------------------------------------------------------
   function Node_Id_Image
     (Node : in Node_Id)
     return String
   is
      IML_Node_Id : IML_Node_IDs.Node_Id;
   begin
      if Node = Invalid_Node_Id then
         return "n/a";
      else
         IML_Node_Id := Storables.Get_Node_Id (Node.IML_Node);
         return IML_Node_IDs.Image (IML_Node_Id);
      end if;
   end Node_Id_Image;

   ---------------------------------------------------------------------------
   --  Implemented with 'Image, since IML_Node_IDs does not offer "Value"
   --
   --  Precondition:
   --    IML_Node_IDs.Node_IDs are stored internally as strings
   function Node_Id_Value
     (Node : in String)
     return Node_Id
   is
      P           : Positive;
      IML_Node_ID : IML_Node_IDs.Node_ID;
      Value       : Node_Id;
   begin
      begin
         P := Positive'Value (Node);
      exception
         --  following could be done better using different exceptions for
         --  different errors, but I don't think, that such a differenciation
         --  makes sence
         when others => raise Node_Does_Not_Exist;
      end;

      IML_Node_ID := IML_Node_IDs.Make_Node_ID (P);

      --  Fixed by Martin: ensures that "Node_Does_Not_Exist" will be raised
      --  if the node id is not part of the hash map.
      if not IML_Node_ID_Hashed_Mappings.Is_Bound
        (IML_Node_ID_Mapping,
         IML_Node_ID) then

         raise Node_Does_Not_Exist;
      end if;

      Value :=
        IML_Node_ID_Hashed_Mappings.Fetch
        (IML_Node_ID_Mapping,
         IML_Node_ID);

      return Value;
   end Node_Id_Value;

   ---------------------------------------------------------------------------
   function Does_Node_Id_Exist
     (Node : in String)
     return Boolean
   is
   begin
      declare
         ID : Node_Id;
      begin
         ID := Node_Id_Value (Node);

         return True;
      exception
         when Node_Does_Not_Exist =>
            return False;
      end;
   end Does_Node_Id_Exist;

   ---------------------------------------------------------------------------
   function Get_All_Edges
     return Edge_Id_Set
   is
   begin
      return Edge_Id_Array_Routines.To_Set (All_Edges.all);
   end Get_All_Edges;

   ---------------------------------------------------------------------------
   function Get_All_Edge_Class_Ids_For_Node_Attribute
     (Node_Attribute_Name : in String)
      return Edge_Class_Id_Set
   is
      Set      : Edge_Class_Id_Set;

      IterData : Node_Class_Id_Hashed_Mappings.Values_Iter;
      CurData  : Node_Class_Id_Hash_Data_Access;

      --  used for the inner loop
      IterAttrib : Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
        Bindings_Iter;
      CurAttribute   : Node_Attribute_Id;
      Cur_Edge_Class  : Edge_Class_Id;

   begin
      Set      := Edge_Class_Id_Sets.Empty_Set;
      IterData := Node_Class_Id_Hashed_Mappings.Make_Values_Iter
        (Node_Class_Id_Mapping);

      while Node_Class_Id_Hashed_Mappings.More (IterData) loop
         Node_Class_Id_Hashed_Mappings.Next (IterData, CurData);

         IterAttrib := Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
           Make_Bindings_Iter
           (CurData.Node_Attribute_Id_Mapping);

         while Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings
           .More (IterAttrib) loop
            Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
              Next (IterAttrib, CurAttribute, Cur_Edge_Class);

            if CurAttribute.Name = Node_Attribute_Name then
               Edge_Class_Id_Sets.Insert (Set, Cur_Edge_Class);
            end if;
         end loop;
      end loop;

      return Set;
   end Get_All_Edge_Class_Ids_For_Node_Attribute;

   ---------------------------------------------------------------------------
   function Get_All_Edge_Class_Ids_For_Node_Class
     (Node_Class : in Node_Class_Id)
      return Edge_Class_Id_Set
   is
      Set       : Edge_Class_Id_Set;
      ClassData : Node_Class_Id_Hash_Data_Access;

      IterAttrib : Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
        Values_Iter;
      Cur_Edge_Class  : Edge_Class_Id;

   begin
      Set := Edge_Class_Id_Sets.Empty_Set;

      ClassData := Node_Class_Id_Hashed_Mappings.Fetch
        (Node_Class_Id_Mapping, Node_Class);

      IterAttrib := Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
        Make_Values_Iter (ClassData.Node_Attribute_Id_Mapping);

      while Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings
        .More (IterAttrib) loop
         Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
           Next (IterAttrib, Cur_Edge_Class);

         Edge_Class_Id_Sets.Insert (Set, Cur_Edge_Class);
      end loop;

      return Set;
   end Get_All_Edge_Class_Ids_For_Node_Class;

   ---------------------------------------------------------------------------
   function Get_All_Node_Class_Ids return Node_Class_Id_Set is
      Set      : Node_Class_Id_Set;
   begin
      Set  := Node_Class_Id_Sets.Empty_Set;

      for I in All_Node_Classes'Range loop
         Node_Class_Id_Sets.Insert (Set, All_Node_Classes (I));
      end loop;

      return Set;
   end Get_All_Node_Class_Ids;

   ----------------------------------------------------------------------------
   --  Iterates over the hashed-mappings containing the mapping of all
   --  IML-Nodes to the internal ones
   function Get_All_Nodes
      return Node_Id_Set
   is
      Set      : Node_Id_Set;
      Iter     : IML_Node_ID_Hashed_Mappings.Values_Iter;
      Cur_Node : Node_Id;
   begin
      Set := Node_Id_Sets.Empty_Set;

      Iter := IML_Node_ID_Hashed_Mappings.Make_Values_Iter
        (IML_Node_ID_Mapping);

      while IML_Node_ID_Hashed_Mappings.More (Iter) loop
         IML_Node_ID_Hashed_Mappings.Next (Iter, Cur_Node);

         Node_Id_Sets.Insert (Set, Cur_Node);
      end loop;

      return Set;
   end Get_All_Nodes;

   ---------------------------------------------------------------------------
   function Get_Edge_Count
     return Natural
   is
   begin
      return All_Edges'Length;
   end Get_Edge_Count;

   ---------------------------------------------------------------------------
   function Get_Node_Count
     return Natural
   is
   begin
      return IML_Node_ID_Hashed_Mappings.Size (IML_Node_ID_Mapping);
   end Get_Node_Count;

   ---------------------------------------------------------------------------
   function Get_Edge_Class_Id
     (Edge : in Edge_Id)
      return Edge_Class_Id
   is
   begin
      return
        Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
        (Get_Node_Class_Id (Edge.Source_Node),
         Edge.Attribute);
   end Get_Edge_Class_Id;

   ---------------------------------------------------------------------------
   function Get_Node_Class_Id
     (Node : in Node_Id)
      return Node_Class_Id
   is
   begin
      return IML_Roots.Get_Class_ID (IML_Roots.IML_Root (Node.IML_Node));
   end Get_Node_Class_id;

   ---------------------------------------------------------------------------
   function Get_Edge_Tag
     (Edge : Edge_Id)
      return String
   is
      Attribute_Name : String :=
        Convert_Node_Attribute_Id_To_Name (Edge.Attribute);
   begin
      if Edge.Attribute_Element_Number = 0 then
         return Attribute_Name;
      else
         return Attribute_Name & "." &
           Integer'Image (Edge.Attribute_Element_Number);
      end if;
   end Get_Edge_Tag;

   ---------------------------------------------------------------------------
   function Get_Edge_Class_Tag
     (Edge_Class : Edge_Class_Id)
      return String
   is
   begin
      return
        Get_Node_Class_Tag (Edge_Class.Source_Node_Class) & "." &
        Convert_Node_Attribute_Id_To_Name (Edge_Class.Source_Node_Attribute);
   end Get_Edge_Class_Tag;

   ----------------------------------------------------------------------------
   function Get_Incoming_Edges
     (Node : in Node_Id)
      return Edge_Id_Set
   is
   begin
      return Edge_Id_Array_Routines.To_Set (Node.Incoming_Edges);
   end Get_Incoming_Edges;

   ----------------------------------------------------------------------------
   function Get_Incoming_Edges
      (Node : in Node_Id)
      return Edge_Id_Array
   is
   begin
      return Node.Incoming_Edges;
   end Get_Incoming_Edges;

   ----------------------------------------------------------------------------
   function Get_Outgoing_Edges
     (Node : in Node_Id)
      return Edge_Id_Set
   is
   begin
      return Edge_Id_Array_Routines.To_Set (Node.Outgoing_Edges);
   end Get_Outgoing_Edges;

   ----------------------------------------------------------------------------
   function Get_Outgoing_Edges
      (Node : in Node_Id)
      return Edge_Id_Array
   is
   begin
      return Node.Outgoing_Edges;
   end Get_Outgoing_Edges;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_Node_Id_List_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Node_Id_List
   is
      List : Node_Id_List;
   begin
      pragma Assert
        (Get_Node_Attribute_Class_Id (Attribute) = Class_Node_Id_List);

      List := Node_Id_Lists.Create;

      declare
         IML_List: IML_Reflection.List_Field
           := IML_Reflection.List_Field (Attribute.all);
         Iter    : IML_Reflection.List_Iterator;
         Target  : Storables.Storable;
         Res_Node : Node_Id;
      begin
         Iter     := IML_List.Make_Iterator (Node.IML_Node);

         while IML_Reflection.More (Iter) loop
            IML_Reflection.Next (Iter, Target);

            if (Storables."/=" (Target, null)) and then
              Is_IML_Root (Target) then
               Res_Node := IML_Node_ID_Hashed_Mappings.Fetch
                 (IML_Node_ID_Mapping,
                  Storables.Get_Node_Id (Target) );

               Node_Id_Lists.Attach (List, Res_Node);
            else
               Logger.Debug ("No IML_Root @ List_Value");
            end if;

         end loop;
      end;

      return List;
   end Get_Node_Attribute_Node_Id_List_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_Boolean_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Boolean
   is
   begin
      pragma Assert
        (Get_Node_Attribute_Class_Id (Attribute) = Class_Boolean);

      return IML_Reflection.Boolean_Field (Attribute.all).Get_Value
        (Node.IML_Node);
   end Get_Node_Attribute_Boolean_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_Identifier_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
     return String
   is
      Target : Storables.Storable;
   begin
      pragma Assert
        (Get_Node_Attribute_Class_Id(Attribute) = Class_Identifier);

      declare
          IML_Edge : IML_Reflection.Edge_Field
            := IML_Reflection.Edge_Field (Attribute.all);
      begin
         --  Identifieres are realised as edges
         Target := IML_Edge.Get_Target (Node.IML_Node);

         if Storables."=" (Target, null) then
            --  if they do not point to somewhere, they are not available
            return "n/a";
         else
            begin
               return IML_Reflection.Identifier_Field (Attribute.all).Get_Name
              (Node.IML_Node);
            exception
               --  should never be reached
               --  N/A in uppercase to show difference to above used "n/a"
               when others =>
                  return "N/A";
            end;
         end if;
      end;
   end Get_Node_Attribute_Identifier_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_Class_Id
     (Node_Attribute : in Node_Attribute_Id)
      return Node_Attribute_Class_Id
   is
   begin
      --  special case Identifiers, subclass of edges, has to be
      --    tested before edges
      if Node_Attribute.all in IML_Reflection.Identifier_Field then
         return Class_Identifier;

         --  special case for types of literal_types.ads
      elsif Node_Attribute.all in Literal_Types.Char_Literal_Field or
        Node_Attribute.all in Literal_Types.Int_Literal_Field or
        Node_Attribute.all in Literal_Types.Real_Literal_Field then
         --  literals are completly handled as strings, because
         --  GSL can't handle Integers and Reals
         return Class_String;

         --  Edges to other nodes
      elsif Node_Attribute.all in IML_Reflection.Edge_Field'Class then
         return Class_Node_Id;
      elsif Node_Attribute.all in IML_Reflection.List_Field'Class then
         return Class_Node_Id_List;
      elsif Node_Attribute.all in IML_Reflection.Set_Field'Class then
         return Class_Node_Id_Set;

         -- Buildin-Fields
      elsif Node_Attribute.all in IML_Reflection.SLoc_Field'Class then
         return Class_SLoc;
      elsif Node_Attribute.all in IML_Reflection.Boolean_Field'Class then
         return Class_Boolean;
      elsif Node_Attribute.all in IML_Reflection.Natural_Field'Class then
         return Class_Natural;
      elsif Node_Attribute.all in IML_Reflection.Enumerator_Field'Class then
         return Class_String;
      else
         Logger.Error ("Class " &
                       Ada.Tags.External_Tag (Node_Attribute.all'Tag) &
                       " for Node_Attribute """ & Node_Attribute.Name &
                       """ could not be found");
         return Class_Invalid;
      end if;
   end Get_Node_Attribute_Class_Id;

   ---------------------------------------------------------------------------
   function Get_Node_Attribute_Natural_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Natural
   is
   begin
      pragma Assert
        (Get_Node_Attribute_Class_Id (Attribute) = Class_Natural);

      return IML_Reflection.Natural_Field (Attribute.all).Get_Value
        (Node.IML_Node);
   end Get_Node_Attribute_Natural_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_Node_Id_Set_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Node_Id_Set
   is
      Set : Node_Id_Set;
   begin
      pragma Assert
        (Get_Node_Attribute_Class_Id (Attribute) = Class_Node_Id_Set);

      Set := Node_Id_Sets.Empty_Set;

      declare
         IML_Set : IML_Reflection.Set_Field :=
           IML_Reflection.Set_Field (Attribute.all);
         Iter    : IML_Reflection.Set_Iterator;
         Target  : Storables.Storable;
         Res_Node : Node_Id;
      begin
         Iter := IML_Set.Make_Iterator (Node.IML_Node);

         while IML_Reflection.More (Iter) loop
            IML_Reflection.Next (Iter, Target);

            if (Storables."/=" (Target, null)) and then
              Is_IML_Root (Target) then
               Res_Node := IML_Node_ID_Hashed_Mappings.Fetch
                 (IML_Node_ID_Mapping,
                  Storables.Get_Node_Id (Target) );

               Node_Id_Sets.Insert (Set, Res_Node);
            else
               Logger.Debug ("No IML_Root @ Set_Value");
            end if;
         end loop;
      end;

      return Set;
   end Get_Node_Attribute_Node_Id_Set_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_Node_Id_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Node_Id
   is
      Res_Node : Node_Id;
   begin
      pragma Assert (Get_Node_Attribute_Class_Id (Attribute) = Class_Node_Id);

      declare
          IML_Edge : IML_Reflection.Edge_Field
            := IML_Reflection.Edge_Field (Attribute.all);
          Target   : Storables.Storable;
       begin
          Target   := IML_Edge.Get_Target (Node.IML_Node);
          if Storables."/=" (Target, null) then
             if Is_IML_Root (Target) then
                Res_Node := IML_Node_ID_Hashed_Mappings.Fetch
                  (IML_Node_ID_Mapping,
                   Storables.Get_Node_Id (Target) );
             else
                --  this is reached at identifiers, which should never happen
                --  Logger.Debug ("Edge_Field with non-IML_Root-target");
                return Invalid_Node_Id;
             end if;
          else
             --  this is reached at identifiers, which should never happen
             --  Logger.Debug ("Edge_Field with null target");
             return Invalid_Node_Id;
          end if;
      end;

      return Res_Node;
   end Get_Node_Attribute_Node_Id_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_SLoc_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
     return SLocs.Sloc
   is
   begin
      pragma Assert (Get_Node_Attribute_Class_Id (Attribute) = Class_SLoc);

      return IML_Reflection.SLoc_Field (Attribute.all).Get_Value
        (Node.IML_Node);
   end Get_Node_Attribute_SLoc_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_SLoc_Column_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Natural
   is
   begin
      pragma Assert (Get_Node_Attribute_Class_Id (Attribute) = Class_SLoc);

      return SLocs.Get_Column
        (Get_Node_Attribute_SLoc_Value (Node, Attribute));
   end Get_Node_Attribute_SLoc_Column_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_SLoc_Filename_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return String
   is
   begin
      pragma Assert (Get_Node_Attribute_Class_Id (Attribute) = Class_SLoc);

      return SLocs.Get_Filename
        (Get_Node_Attribute_SLoc_Value (Node, Attribute));
   end Get_Node_Attribute_SLoc_Filename_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_SLoc_Line_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Natural
   is
   begin
      pragma Assert (Get_Node_Attribute_Class_Id (Attribute) = Class_SLoc);

      return SLocs.Get_Line
        (Get_Node_Attribute_SLoc_Value (Node, Attribute));
   end Get_Node_Attribute_SLoc_Line_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_SLoc_Path_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return String
   is
   begin
      pragma Assert (Get_Node_Attribute_Class_Id (Attribute) = Class_SLoc);

      return SLocs.Get_Path
        (Get_Node_Attribute_SLoc_Value (Node, Attribute));
   end Get_Node_Attribute_SLoc_Path_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_String_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return String
   is
   begin
      pragma Assert (Get_Node_Attribute_Class_Id (Attribute) = Class_String);

      --  handling for Literal_Types
      if Attribute.all in Literal_Types.Char_Literal_Field then
         declare
            Char  : Literal_Types.Char_Literal_Field :=
              Literal_Types.Char_Literal_Field (Attribute.all);
            Value : Literal_Types.Char_Literal_Type :=
              Char.Get_Value (Node.IML_Node);
         begin
            return Ada.Strings.Fixed.Trim
              (Literal_Types.Char_Literal_Type'Image (Value),
               Ada.Strings.Both);
         end;
      elsif Attribute.all in Literal_Types.Int_Literal_Field then
         declare
            Int  : Literal_Types.Int_Literal_Field :=
              Literal_Types.Int_Literal_Field (Attribute.all);
            Value : Literal_Types.Int_Literal_Type :=
              Int.Get_Value (Node.IML_Node);
         begin
            return Ada.Strings.Fixed.Trim
              (Literal_Types.Int_Literal_Type'Image (Value),
               Ada.Strings.Both);
         end;
      elsif Attribute.all in Literal_Types.Real_Literal_Field then
         declare
            Real  : Literal_Types.Real_Literal_Field :=
              Literal_Types.Real_Literal_Field (Attribute.all);
            Value : Literal_Types.Real_Literal_Type :=
              Real.Get_Value (Node.IML_Node);
         begin
            return Ada.Strings.Fixed.Trim
              (Literal_Types.Real_Literal_Type'Image (Value),
               Ada.Strings.Both);
         end;
      else
         --  handling for enumerators
         declare
            Enum       : IML_Reflection.Enumerator_Field :=
              IML_Reflection.Enumerator_Field (Attribute.all);
            Enum_Value : Natural :=
              Enum.Get_Value (Node.IML_Node);
         begin
            return Enum.Type_Id.Enumerators (Enum_Value).all;
         end;
      end if;
   end Get_Node_Attribute_String_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_Value_As_String
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return String
   is

      ------------------------------------------------------------------------
      --  List is destroyed at the end of the function
      --
      --  Performance-Problem could be here
      --    if it is a problem, then do it with a hashmap!
      function Convert_Node_Id_List_To_String
        (The_List : in Node_Id_List)
        return String
      is
         List : Node_Id_List := The_List;
         Iter : Node_Id_Lists.ListIter;
         Node : Node_Id;

         Res  : Ada.Strings.Unbounded.Unbounded_String;
      begin
         if Node_Id_Lists.IsEmpty (List) then
            Res := Ada.Strings.Unbounded.To_Unbounded_String ("()");
         else
            Iter := Node_Id_Lists.MakeListIter (List);
            Res  := Ada.Strings.Unbounded.To_Unbounded_String ("(");
            Node_Id_Lists.Next (Iter, Node);
            Ada.Strings.Unbounded.Append (Res, Node_Id_Image (Node));

            while Node_Id_Lists.More (Iter) loop
               Node_Id_Lists.Next (Iter, Node);
               Ada.Strings.Unbounded.Append
                 (Res, ", ");
               Ada.Strings.Unbounded.Append
                 (Res, Node_Id_Image (Node));
            end loop;
            Ada.Strings.Unbounded.Append (Res, ")");
         end if;

         Node_Id_Lists.Destroy (List);

         return Ada.Strings.Unbounded.To_String (Res);
      end Convert_Node_Id_List_To_String;

      ------------------------------------------------------------------------
      --  Set is destroyed at the end of the function
      --
      --  This function is implemented the same way as
      --    Convert_Node_Id_List_To_String
      --
      --  Performance-Problem could be here
      --    if it is a problem, then do it with a hashmap!
      function Convert_Node_Id_Set_To_String
        (The_Set : in Node_Id_Set)
        return String
      is
         Set  : Node_Id_Set := The_Set;
         Iter : Node_Id_Sets.Iterator;
         Node : Node_Id;

         Res  : Ada.Strings.Unbounded.Unbounded_String;
      begin
         if Node_Id_Sets.Is_Empty (Set) then
            Res := Ada.Strings.Unbounded.To_Unbounded_String ("{}");
         else
            Iter := Node_Id_Sets.Make_Iterator (Set);
            Res  := Ada.Strings.Unbounded.To_Unbounded_String ("(");
            Node_Id_Sets.Next (Iter, Node);
            Ada.Strings.Unbounded.Append (Res, Node_Id_Image (Node));

            while Node_Id_Sets.More (Iter) loop
               Node_Id_Sets.Next (Iter, Node);
               Ada.Strings.Unbounded.Append
                 (Res, ", ");
               Ada.Strings.Unbounded.Append
                 (Res, Node_Id_Image (Node));
            end loop;
            Ada.Strings.Unbounded.Append (Res, "}");
         end if;

         Node_Id_Sets.Destroy (Set);

         return Ada.Strings.Unbounded.To_String (Res);
      end Convert_Node_Id_Set_To_String;

   begin
      case Get_Node_Attribute_Class_Id (Attribute) is
         when Class_SLoc =>
            declare
               SLoc_Stored_In_IML : IML_Reflection.SLoc_Field :=
                 IML_Reflection.SLoc_Field (Attribute.all);
               SLoc               : SLocs.SLoc;
            begin
               SLoc := SLoc_Stored_In_IML.Get_Value (Node.IML_Node);
               return SLocs.Plain_Image (SLoc);
            end;

         when Class_Boolean =>
            return Boolean'Image (Get_Node_Attribute_Boolean_Value
                                  (Node, Attribute));

         when Class_Natural =>
            return Natural'Image (Get_Node_Attribute_Natural_Value
                                  (Node, Attribute));

         when Class_Identifier =>
            return Get_Node_Attribute_Identifier_Value (Node, Attribute);

         when Class_Node_Id =>
            return Node_Id_Image (Get_Node_Attribute_Node_Id_Value
                                  (Node, Attribute));

         when Class_Node_Id_List =>
            return Convert_Node_Id_List_To_String
              (Get_Node_Attribute_Node_Id_List_Value (Node, Attribute));

         when Class_Node_Id_Set =>
            return Convert_Node_Id_Set_To_String
              (Get_Node_Attribute_Node_Id_Set_Value (Node, Attribute));

         when Class_String =>
            return Get_Node_Attribute_String_Value (Node, Attribute);

         when Class_Invalid =>
            return "* invalid attribute class *";

         when others =>
            Logger.Fatal ("Unknown Attribute-Class in " &
                          "Get_Node_Attribute_Value_As_String");
            return "* unknown *";
      end case;
   end Get_Node_Attribute_Value_As_String;

   ----------------------------------------------------------------------------
   function Get_Root_Node
      return Node_Id
   is
      Root_Node : IML_Roots.IML_Root;
      Res_Node   : Node_Id;
   begin
      Root_Node := IML_Graphs.Get_Raw_Graph (IML_Graph);

      Res_Node := IML_Node_ID_Hashed_Mappings.Fetch
        (IML_Node_ID_Mapping,
         Storables.Get_Node_Id (Root_Node) );

      return Res_Node;
   end Get_Root_Node;

   ----------------------------------------------------------------------------
   function Get_Source_Node
     (Edge : in Edge_Id)
      return Node_Id
   is
   begin
      return Edge.Source_Node;
   end Get_Source_Node;

   ----------------------------------------------------------------------------
   function Get_Target_Node
     (Edge : in Edge_Id)
      return Node_Id
   is
   begin
      return Edge.Target_Node;
   end Get_Target_Node;

   ---------------------------------------------------------------------------
   function Get_Graph_Hash return Integer is
   begin
      return IML_Graphs.Hash (IML_Graph);
   end Get_Graph_Hash;

   ---------------------------------------------------------------------------
   function Is_IML_Root
     (The_Storable : access Storables.Storable_Class'Class)
     return Boolean
   is
   begin
      return The_Storable.all in IML_Roots.IML_Root_Class'Class;
   end Is_IML_Root;

   ---------------------------------------------------------------------------
   function Hash_Edge_Class_Id (Key : in Edge_Class_Id) return Integer is

      Edge_Class_Id_Hash_Range_Size : constant := 29;

      package Edge_Class_Id_Hash_Functions is
         new Ptr_Normal_Hashs
        (T          => Edge_Class,
         T_Ptr      => Edge_Class_Id,
         Range_Size => Edge_Class_Id_Hash_Range_Size);

   begin
      return Edge_Class_Id_Hash_Functions.Integer_Hash (Key);
   end Hash_Edge_Class_Id;

   ---------------------------------------------------------------------------
   function Hash_Edge_Id (Key : in Edge_Id) return Integer is

      Edge_Id_Hash_Range_Size : constant := 1024047;

      package Edge_Id_Hash_Functions is
         new Ptr_Normal_Hashs
        (T          => Edge_Record,
         T_Ptr      => Edge_Id,
         Range_Size => Edge_Id_Hash_Range_Size);

   begin
      return Edge_Id_Hash_Functions.Integer_Hash (Key);
   end Hash_Edge_Id;

   ---------------------------------------------------------------------------
   function Hash_Node_Class_Id (Key : in Node_Class_Id) return Integer is
   begin
      return Node_Class_Id_Hash_Functions.Integer_Hash (Key);
   end Hash_Node_Class_Id;

   ---------------------------------------------------------------------------
   function Hash_Node_Id (Key : in Node_Id) return Integer is
   begin
      return IML_Node_IDs.Hash (Storables.Get_Node_ID (Key.IML_Node));
   end Hash_Node_Id;

   ---------------------------------------------------------------------------
   function Make_Attribute_Iterator
     (Node_Class : in     Node_Class_Id)
     return Node_Attribute_Iterator
   is
      Iterator : Node_Attribute_Iterator;
   begin
      Iterator.Class := Node_Class;
      Iterator.CurrentIndex := Iterator.Class.Fields'First;
      return Iterator;
   end Make_Attribute_Iterator;

   ---------------------------------------------------------------------------
   function Make_Attribute_Iterator
     (Node     : in     Node_Id)
     return Node_Attribute_Iterator
   is
   begin
      return Make_Attribute_Iterator
        (Get_Node_Class_Id (Node));
   end Make_Attribute_Iterator;

   ---------------------------------------------------------------------------
   function More
     (Iterator : in Node_Attribute_Iterator)
     return Boolean
   is
   begin
      return (Iterator.CurrentIndex <= Iterator.Class.Fields'Last);
   end More;

   ---------------------------------------------------------------------------
   procedure Next
     (Iterator : in out Node_Attribute_Iterator;
      Info     :    out Node_Attribute_Id)
   is
   begin
      if not More (Iterator) then
         raise Constraint_Error;
         --  TBD: s/Contraint_Error/own_/
      end if;

      Info := Iterator.Class.Fields (Iterator.CurrentIndex);

      Iterator.CurrentIndex := Iterator.CurrentIndex + 1;
   end Next;

   ---------------------------------------------------------------------------
   procedure Read_Edge_Id
     (Stream : in     Bauhaus_Io.In_Stream_Type;
      Edge   :    out Edge_Id)
   is
      Edge_Internal_Id         : Positive;
   begin
      Bauhaus_Io.Read_Natural (Stream, Edge_Internal_Id);
      Edge := All_Edges (Edge_Internal_Id);
   end Read_Edge_Id;

   ---------------------------------------------------------------------------
   procedure Read_Node_Id
     (Stream : in     Bauhaus_Io.In_Stream_Type;
      Node   :    out Node_Id)
   is
      Len : Natural;
   begin
      Bauhaus_Io.Read_Natural (Stream, Len);

      declare
         Image : String (1..Len);
      begin
         Bauhaus_Io.Read_String (Stream, Image);
         Node := Node_Id_Value (Image);

         --  TBD: an exception could be risen here
      end;
   end Read_Node_Id;

   ----------------------------------------------------------------------
   procedure Write_Edge_Id
     (Stream : in Bauhaus_Io.Out_Stream_Type;
      Edge   : in Edge_Id)
   is
   begin
      Bauhaus_Io.Write_Natural (Stream, Edge.Internal_Id);
   end Write_Edge_Id;

   ----------------------------------------------------------------------
   --  The handling is not as straight-forward as in Write_Edge_Id,
   --    since Node_Ids are connected to IML-Nodes and Edges are created
   --    by graph_lib itself
   --  This could change, if graph_lib introduced an own numbering of the
   --    nodes, like it exists with the edge_ids.
   --  But this is an enourmos impact to the whole lib, which means,
   --    a refactoring has to be done.
   procedure Write_Node_Id
     (Stream : in Bauhaus_Io.Out_Stream_Type;
      Node   : in Node_Id)
   is
      Image : String := Node_Id_Image (Node);
   begin
      Bauhaus_Io.Write_Natural (Stream, Image'Length);
      Bauhaus_Io.Write_String  (Stream, Image);
   end Write_Node_Id;

end Giant.Graph_Lib;
