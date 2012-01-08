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
--  $RCSfile: giant-graph_lib.ads,v $, $Revision: 1.49 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
--  TBD:
--    * Write into comment, when the routine may be used
--        after create / load, not after unload / destroy
--    * Notify, that there is no possibility to distinguish between
--        edge and node-attributes
--    * Check, why there are no inspectors for Edge_Class_Ids

with Giant.Basic_Evolutions;
with Giant.Constant_Ptr_Hashs;

--  Bauhaus / IML
with IML_Reflection;
with IML_Roots;
with Storables;
with SLocs; --  used at private part

--  Bauhaus / Reuse
with Bauhaus_Io;
with Ordered_Sets;
with Lists;

--  Bauhaus / Reuse (only used at private part)
with Hashed_Mappings;
with IML_Node_IDs;

pragma Elaborate_All (Lists);
pragma Elaborate_All (Hashed_Mappings);
pragma Elaborate_All (Ordered_Sets);
pragma Elaborate_All (Giant.Constant_Ptr_Hashs);

package Giant.Graph_Lib is

   ------------------------------------
   --  Basic-Types:                  --
   --    Nodes, Attributes of Nodes  --
   ------------------------------------

   ---------------------------------------------------------------------------
   --  to be considered private
   --  is here to simplify implementation
   type Node_Record (Number_Of_Incoming_Edges : Natural;
                     Number_Of_Outgoing_Edges : Natural) is limited private;

   ---------------------------------------------------------------------------
   --  unique Id of one single node in the duplicated IML-Graph
   --
   --  It is constant throughout one runtime
   --  "access constant" could not be used, since an unchecked_deallocation
   --  is impossible then
   --
   --  Node_Id_Image is invariant over multiple runs of GIANT
   type Node_Id is access Node_Record;

   ---------------------------------------------------------------------------
   --  unique Id of one class to which a node is belonging to
   --  ("type of the node")
   --
   --    'Node_Class_Id' is declared as a subtype only to simplify the
   --    implementation of the package. It is to be considered a private type.
   --    Used for casting complete arrays
   subtype Node_Class_Id is IML_Reflection.Class_ID;

   ---------------------------------------------------------------------------
   --  Unique id of one single attribute "type" which appears in
   --    one or more nodes ("type of the attribute")
   --
   --    'Node_Attribute_Id' is declared as a subtype only to simplify
   --    implementation of the package. It is to be considered a private type.
   --    Used for casting complete arrays
   subtype Node_Attribute_Id is IML_Reflection.Field_ID;

   ---------------------------------------------------------------------------
   --  Needed by Attribute_Filters
   type Node_Attribute_Id_Array is
     array (Positive range <>) of Node_Attribute_Id;

   ---------------------------------------------------------------------------
   --  Id of one class to which an attribute may belong to
   --    ("Type of the attribute")
   --  Mirror of IML-Fields
   type Node_Attribute_Class_Id is
      (Class_Node_Id,
       Class_Node_Id_List,
       Class_Node_Id_Set,

       --  used for edge-fields pointing to identifiers
       Class_Identifier,

       --  used for IML-Enumerators
       Class_String,

       Class_SLoc,
       Class_Boolean,
       Class_Natural,

       --  used to deal with unknown classes
       Class_Invalid);

   ---------------------------
   --  Basic-Types          --
   --    Edges              --
   --                       --
   --    Analogue to Nodes  --
   ---------------------------

   ---------------------------------------------------------------------------
   type Edge_Record is limited private;

   ---------------------------------------------------------------------------
   --  unique Id of one single edge in the duplicated IML-Graph
   type Edge_Id is access all Edge_Record;

   ---------------------------------------------------------------------------
   --  Needed for Node_Record, used to store incoming and outgoing edges
   --
   --  (Positive range <>) is used in the implementation, but
   --    Edge_Id_Array_Routines need (Integer range <>)
   type Edge_Id_Array is array (Integer range <>) of Edge_Id;

   ---------------------------------------------------------------------------
   type Edge_Id_Array_Access is access Edge_Id_Array;

   ---------------------------------------------------------------------------
   --  to be considered private
   --  is here to simplify implementation
   type Edge_Class is limited private;

   ---------------------------------------------------------------------------
   --  unique ID of one class to which an edge may belong to
   --
   --    This is to be considered private
   type Edge_Class_Id is access all Edge_Class;


   -----------------------------------------------------------------------
   --  Edge_Attribute is not implemented, since it is not supported by  --
   --  IML_Reflection                                                   --
   -----------------------------------------------------------------------

   -----------------
   --  Iterators  --
   -----------------

   ---------------------------------------------------------------------------
   --  Desc:
   --    An iterator for iterating on the attributes of a single node
   type Node_Attribute_Iterator is private;


   -----------------
   --  Constants  --
   -----------------

   ---------------------------------------------------------------------------
   Invalid_Node_Id                : constant Node_Id;
   Invalid_Attribute_Value_String : constant String;

   ------------------
   --  Exceptions  --
   ------------------

   Load_Error                    : exception;
   Node_Does_Not_Exist           : exception;
   Node_Class_Does_Not_Exist     : exception;
   Node_Attribute_Does_Not_Exist : exception;
   Edge_Does_Not_Exist           : exception;
   Edge_Class_Does_Not_Exist     : exception;

   -------------------
   --  Comparators  --
   -------------------

   ---------------------------------------------------------------------------
   function "<"
      (Left  : Node_Id;
       Right : Node_Id)
      return Boolean;

   ---------------------------------------------------------------------------
   function "<"
      (Left  : Node_Class_Id;
       Right : Node_Class_Id)
      return Boolean;

   ---------------------------------------------------------------------------
   function "="
      (Left  : Node_Class_Id;
       Right : Node_Class_Id)
      return Boolean;

   ---------------------------------------------------------------------------
   function "<"
      (Left  : Edge_Id;
       Right : Edge_Id)
      return Boolean;

   ---------------------------------------------------------------------------
   function "<"
      (Left  : Edge_Class_Id;
       Right : Edge_Class_Id)
      return Boolean;

   -------------
   --  Lists  --
   -------------
   package Node_Id_Lists is
      new Lists (ItemType => Node_Id);
   subtype Node_Id_List is Node_Id_Lists.List;

   ------------
   --  Sets  --
   ------------

   ------------------------------------------------------------------------
   --  This package provides set and iterator operations.                --
   --  Functionality is not reimplented but offered using Ordered_Sets.  --
   ------------------------------------------------------------------------

   --  Read and Write will not be used and therefore are not
   --  instantiated.
   package Node_Id_Sets is
      new Ordered_Sets (Item_Type => Node_Id,
                        "="       => "=",
                        "<"       => "<");
   subtype Node_Id_Set is Node_Id_Sets.Set;

   ---------------------------------------------------------------------------
   --  This package provides set and iterator operations. The
   --  functionality is not reimplented but offered using Ordered_Sets.
   --
   --  Read and Write will not be used and therefore are not
   --  instantiated.
   package Node_Attribute_Class_Id_Sets is
      new Ordered_Sets (Item_Type => Node_Attribute_Class_Id,
                        "="       => "=",
                        "<"       => "<");
   subtype Node_Attribute_Class_Id_Set is
      Node_Attribute_Class_Id_Sets.Set;

   ---------------------------------------------------------------------------
   --  This package provides set and iterator operations. The
   --  functionality is not reimplented but offered using Ordered_Sets.
   --
   --  Read and Write will not be used and therefore are not
   --  instantiated.
   package Node_Class_Id_Sets is
      new Ordered_Sets (Item_Type => Node_Class_Id,
                        "="       => "=",
                        "<"       => "<");
   subtype Node_Class_Id_Set is Node_Class_Id_Sets.Set;

   ---------------------------------------------------------------------------
   --  Set and iterator operations are offered by Ordered_Set
   --
   --  Read and Write are not instantiated, since they won't be used
   package Edge_Id_Sets is
      new Ordered_Sets (Item_Type => Edge_Id,
                        "="       => "=",
                        "<"       => "<");
   subtype Edge_Id_Set is Edge_Id_Sets.Set;

   ---------------------------------------------------------------------------
   --  Set and iterator operations are offered by Ordered_Set
   --
   --  Read and Write are not instantiated, since they won't be used
   package Edge_Class_Id_Sets is
      new Ordered_Sets (Item_Type => Edge_Class_Id,
                        "="       => "=",
                        "<"       => "<");
   subtype Edge_Class_Id_Set is Edge_Class_Id_Sets.Set;


   ------------------------
   --  Create & Destroy  --
   ------------------------

   ---------------------------------------------------------------------------
   --  Inits Node_Class_Ids and Edge_Class_Ids
   procedure Initialize;

   ---------------------------------------------------------------------------
   --  Loads the graph
   --
   --  Raises:
   --    Load_Error if something has gone wrong
   procedure Load
     (Path_To_IML_File : in String;
      Individual       : in Basic_Evolutions.Basic_Evolution_Access := null);

   ---------------------------------------------------------------------------
   --  Unloads the graph from the memory
   procedure Unload;

   ---------------------------------------------------------------------------
   --  Cleans up memory
   procedure Destroy;

   ----------------------
   -- Existance-Checks --
   ----------------------

   ---------------------------------------------------------------------------
   --  Checks if given Node_Class exists anywhere in the loaded IML-Graph
   function Does_Node_Class_Exist
      (Node_Class_Name : in String)
      return Boolean;

   ---------------------------------------------------------------------------
   --  Checks if given Node_Attribute exists at given class
   --
   --  Returns:
   --    True  - if given attribute exists
   --    False - if it does not exist or if given Class does not exist
   function Does_Node_Attribute_Exist
     (Node_Class          : in Node_Class_Id;
      Node_Attribute_Name : in String)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Checks if given Node_Attribute exists at given class
   --
   --  Returns:
   --    True  - if given attribute exists
   --    False - if it does not exist or if given Class does not exist
   function Does_Node_Attribute_Exist
     (Node_Class_Name     : in String;
      Node_Attribute_Name : in String)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Checks if given Node_Class has an
   --    attribute Attribute_Name representing an edge
   function Does_Edge_Class_Exist
      (Node_Class     : in Node_Class_Id;
       Node_Attribute : in Node_Attribute_Id)
      return Boolean;

   ----------------------
   -- Type conversions --
   ----------------------

   ---------------------------------------------------------------------------
   --  Returns:
   --    Edge_Class_Id of given Edge_Class_Tag
   --  Raises:
   --    Edge_Class_Does_Not_Exist if given Edge_Class_Tag could not be
   --    converted to an Edge_Class_Id
   --  Pre:
   --    Edge_Class_Name contains "." exactly once
   function Convert_Edge_Class_Tag_To_Id
     (Edge_Class_Tag : in String)
     return Edge_Class_Id;

   ---------------------------------------------------------------------------
   --  Returns:
   --    The name belonging to given Node_Attribute
   --  Raises:
   --    Node_Attribute_Does_Not_Exist
   function Convert_Node_Attribute_Id_To_Name
     (Node_Attribute : in Node_Attribute_Id)
     return String;

   ---------------------------------------------------------------------------
   --  Returns:
   --    The id belonging to given Node_Attribute
   --    Invalid_Attribute_Id if given Node_Class does not exist
   --
   --  Raises:
   --    Node_Class_Does_Not_Exist     if given Node_Class does not exist
   --    Node_Attribute_Does_Not_Exist if given Node_Attribute does not exist
   function Convert_Node_Attribute_Name_To_Id
     (Node_Class_Name     : in String;
      Node_Attribute_Name : in String)
     return Node_Attribute_Id;

   ---------------------------------------------------------------------------
   --  Returns:
   --    The id belonging to given Node_Attribute
   --    Invalid_Attribute_Id if given Node_Class does not exist
   --
   --  Raises:
   --    Node_Attribute_Does_Not_Exist if given Node_Attribute does not exist
   function Convert_Node_Attribute_Name_To_Id
     (Node_Class          : in Node_Class_Id;
      Node_Attribute_Name : in String)
     return Node_Attribute_Id;

   ---------------------------------------------------------------------------
   --  Returns:
   --    The name belonging to given Node_Attribute_Class
   function Convert_Node_Attribute_Class_Id_To_Name
     (Node_Attribute_Class : in Node_Attribute_Class_Id)
     return String;

   ---------------------------------------------------------------------------
   --  Returns:
   --    Node_Class_Id belonging to given Node_Class_Name
   --  Parameters:
   --    Node_Class_Name in ("Id", "Id_Set", ...)
   --    defined in IML_refelection - Abstract_Class.Name
   --  Raises:
   --    Node_Class_Does_Not_Exist, if Does_Node_Class_Exist() = False
   function Convert_Node_Class_Name_To_Id
      (Node_Class_Name : in String)
      return Node_Class_Id;

   ---------------------------------------------------------------------------
   --  Raises:
   --    Edge_Class_Does_Not_Exist, if Does_Edge_Class_Exist() = False
   function Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
      (Node_Class     : in Node_Class_Id;
       Node_Attribute : in Node_Attribute_Id)
      return Edge_Class_Id;

   -----------------------------------
   -- Inspectors on Class-Relations --
   -----------------------------------

   ---------------------------------------------------------------------------
   --  Gets the class belonging to given Attribute
   function Get_Node_Attribute_Class_Id
      (Node_Attribute : in Node_Attribute_Id)
      return Node_Attribute_Class_Id;

   ---------------------------------------------------------------------------
   --  Used to get all children of given Node_Class
   --
   --  If Include_Parent is true, the given Node_Class will be included, too
   --
   --  Returns:
   --    A set including all children of given Node_Class
   --    This set has to be destroyed - as usual - by the caller
   function Get_Successors
     (Node_Class     : in Node_Class_Id;
      Include_Parent : in Boolean)
     return Node_Class_Id_Set;

   ---------------------------------------------------------------------------
   --  Used to get all parents of given Node_Class
   --
   --  If Include_Child is true, the given Node_Class will be included, too
   --
   --  Returns:
   --    A set including all parents of given Node_Class
   --    This set has to be destroyed - as usual - by the caller
   function Get_Predecessors
     (Node_Class    : in Node_Class_Id;
      Include_Child : in Boolean)
     return Node_Class_Id_Set;

   -----------------------------------
   --  Inspectors                   --
   --    On the nodes of the graph  --
   -----------------------------------

   ---------------------------------------------------------------------------
   --  Returns:
   --    "Type of Node"
   --
   --  Refactoring: Should be named "Get_Node_Class" to be consistent with
   --               other routines
   function Get_Node_Class_Id
      (Node : in Node_Id)
      return Node_Class_Id;

   ---------------------------------------------------------------------------
   --  Returns:
   --    All incoming edges of given node
   function Get_Incoming_Edges
      (Node : in Node_Id)
      return Edge_Id_Set;

   ---------------------------------------------------------------------------
   --  The caller may do anything with the array, since a copy is made
   --
   --  Faster than Get_Incoming_Edges returning a set
   --
   --  Returns:
   --    All incoming edges of given node
   function Get_Incoming_Edges
      (Node : in Node_Id)
      return Edge_Id_Array;

   ---------------------------------------------------------------------------
   --  The caller may do anything with the array, since a copy is made
   --
   --  disabled, since not needed yet
   --
   --  Returns:
   --    All incoming edges of given node
   --  function Get_Incoming_Edges
   --   (Node : in Node_Id)
   --   return Edge_Id_Array;

   ---------------------------------------------------------------------------
   --  The caller has to destroy this set, since a copy is made
   --
   --  Returns:
   --    All outgoing edges of given node
   function Get_Outgoing_Edges
      (Node : in Node_Id)
      return Edge_Id_Set;

   ---------------------------------------------------------------------------
   --  The caller may do anything with the array, since a copy is made
   --
   --  Faster than Get_Outgoing_Edges returning a set
   --
   --  Returns:
   --    All outgoing edges of given node
   function Get_Outgoing_Edges
      (Node : in Node_Id)
      return Edge_Id_Array;

   --------------------------------
   -- Inspectors                 --
   --  On the edges of the graph --
   --------------------------------

   ---------------------------------------------------------------------------
   --  Returns:
   --    Number of edges contained in currently loaded graph
   --  Pre:
   --    A graph is loaded
   function Get_Edge_Count
     return Natural;

   ---------------------------------------------------------------------------
   --  Returns:
   --    Number of nodes contained in currently loaded graph
   --  Pre:
   --    A graph is loaded
   function Get_Node_Count
     return Natural;

   ---------------------------------------------------------------------------
   --  Returns:
   --    "Type of Edge"
   function Get_Edge_Class_Id
      (Edge : in Edge_Id)
      return Edge_Class_Id;

   ---------------------------------------------------------------------------
   function Get_Source_Node
      (Edge : in Edge_Id)
      return Node_Id;

   ---------------------------------------------------------------------------
   function Get_Target_Node
      (Edge : in Edge_Id)
      return Node_Id;

   ---------------------------------------------------------------------------
   --  Returns:
   --    if given Edge was created from an list/set
   --      <Attribute_Name>.<Number in list/set>
   --    else
   --      <Attribute_Name>
   function Get_Edge_Tag
      (Edge : Edge_Id)
      return String;

   ---------------------------------------------------------------------------
   --  Returns:
   --     <Source_Node_Class_Tag>.<Source_Node_Attribute_Tag>
   function Get_Edge_Class_Tag
      (Edge_Class : Edge_Class_Id)
      return String;

   ---------------------------------
   -- Inspectors                  --
   --  On the struct of the graph --
   ---------------------------------

   ---------------------------------------------------------------------------
   --  Returns:
   --    Root-Node of the IML-Graph
   function Get_Root_Node
      return Node_Id;

   ---------------------------------------------------------------------------
   --  Returns:
   --    All nodes in the IML-Graph
   function Get_All_Nodes
     return Node_Id_Set;

   ---------------------------------------------------------------------------
   --  This set needs to be constructed, therefore it may be slow
   --    See global var All_Node_Classes and think about a speed up
   --    returning an array
   --
   --  Returns:
   --    All known node_class Ids
   function Get_All_Node_Class_Ids return Node_Class_Id_Set;

   ---------------------------------------------------------------------------
   --  Returns:
   --    All eges in the IML-Graph
   function Get_All_Edges
     return Edge_Id_Set;

   ---------------------------------------------------------------------------
   --  Returns:
   --    All known edge_class Ids
   function Get_All_Edge_Class_Ids return Edge_Class_Id_Set;

   ---------------------------------------------------------------------------
   --  Returns:
   --    All Edge_Class_Ids having given Node_Class (as Source)
   --    Empty_Set if there is none.
   function Get_All_Edge_Class_Ids_For_Node_Class
      (Node_Class : in Node_Class_Id)
      return Edge_Class_Id_Set;

   ---------------------------------------------------------------------------
   --  Used to determine edges having a certain attribute
   --    It is not depended of a certain Source_Node_Class
   --
   --  Returns:
   --    All Edge_Class_Ids which contains given Attribute
   --    Empty_Set if there are none.
   --  Extendable:
   --    s/Node_Attribute/Edge_Attribute/
   function Get_All_Edge_Class_Ids_For_Node_Attribute
      (Node_Attribute_Name : in String)
      return Edge_Class_Id_Set;

   ----------------------------------------------------
   --  Routines for an immutable ids                 --
   --  Idea is like Integer'Value and Integer'Image  --
   ----------------------------------------------------

   ---------------------------------------------------------------------------
   --  The generated string is unique and invariant over multiple runs of
   --    GIANT
   --
   --  Returns:
   --    String-representation of given Node_Id
   --    "n/a" if Node = Invalid_Node_Id
   --
   --  This routine is only needed at dump_iml_data, which is independent
   --    from the main executable
   --  There is no Edge_Id_Value, since it is not needed
   function Edge_Id_Image
     (Edge : in Edge_Id)
     return String;

   ---------------------------------------------------------------------------
   --  The generated string is unique and invariant over multiple runs of
   --    GIANT
   --
   --  Maybe u have to use
   --    "for Node_Id'Image use Node_Id_Image"
   --
   --  Returns:
   --    String-representation of given Node_Id
   --    "n/a" if Node = Invalid_Node_Id
   --
   --  Raises:
   --    Storables.Unknown_Node if something is wrong in the IML-Graph
   function Node_Id_Image
     (Node : in Node_Id)
     return String;

   ---------------------------------------------------------------------------
   --  Converts given string containing a Node_Id to a Node_Id
   --
   --  Raises:
   --    Node_Does_Not_Exist if given node_id_string does not match an
   --    existing node
   function Node_Id_Value
     (Node : in String)
     return Node_Id;

   ---------------------------------------------------------------------------
   --  inconsitent to Integer'Value and 'Image, but consistent to other
   --  routines like Node_Class_Id
   --
   --  Returns
   --    True if given Node_Id exists (i.e. can be converted to a Node_Id)
   function Does_Node_Id_Exist
     (Node : in String)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Returns a tag identifying the current node id
   --    is unique for each Node_Class_Id
   --
   --  Could also be named Convert_Node_Class_Id_To_Name
   --    since it returns the name of given Node_Class
   function Get_Node_Class_Tag
     (Node_Class : in Node_Class_Id)
     return String;

   ---------------------------
   -- Inspectors            --
   --  On the data of nodes --
   ---------------------------

   ---------------------------------------------------------------------------
   --  Precondition:
   --    Get_Node_Attribute_Class_Id(Attribute) = Class_Boolean
   function Get_Node_Attribute_Boolean_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return Boolean;

   ---------------------------------------------------------------------------
   --  Precondition:
   --    Get_Node_Attribute_Class_Id(Attribute) = Class_Natural
   function Get_Node_Attribute_Natural_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return Natural;

   ---------------------------------------------------------------------------
   --  Returns:
   --    Invalid_Node_Id if Attribute doesn't contain a valid node-id
   --
   --  Precondition:
   --    Get_Node_Attribute_Class_Id(Attribute) = Class_Node_Id
   function Get_Node_Attribute_Node_Id_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return Node_Id;

   ---------------------------------------------------------------------------
   --  Returns:
   --    A newly created Node_Id_List
   --    The caller has to take care for the destroyage that list
   --    Non-IML-Roots (i.e. empty or upper classes) are ignored
   --      (i.e. not included)
   --
   --  Precondition:
   --    Get_Node_Attribute_Class_Id(Attribute) = Class_Node_Id_List
   function Get_Node_Attribute_Node_Id_List_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return Node_Id_List;

   ---------------------------------------------------------------------------
   --  Returns:
   --    A newly created Node_Id_Set
   --    The caller has to take care for the destroyage that set
   --    Non-IML-Roots (i.e. empty or upper classes) are ignored
   --      (i.e. not included)
   --
   --  Precondition:
   --    Get_Node_Attribute_Class_Id(Attribute) = Class_Node_Id_Set
   function Get_Node_Attribute_Node_Id_Set_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return Node_Id_Set;

   ---------------------------------------------------------------------------
   --  Precondition:
   --    Get_Node_Attribute_Class_Id(Attribute) = Class_String
   function Get_Node_Attribute_String_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
     return String;

   ---------------------------------------------------------------------------
   --  Pre-Condition:
   --    Get_Node_Attribute_Class_Id(Attribute) = Class_Identifier
   function Get_Node_Attribute_Identifier_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
     return String;

   ---------------------------------------------------------------------------
   --  SLoc is split up into seperate parts, since GSL doesn't know type
   --    Sloc
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Slocs.Get_Line()-Wrapper
   --
   --  Precondition:
   --    Get_Node_Attribute_Class_Id(Attribute) = Class_SLoc
   function Get_Node_Attribute_SLoc_Line_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return Natural;

   ---------------------------------------------------------------------------
   --  Slocs.Get_Column()-Wrapper
   --
   --  Precondition:
   --    Get_Node_Attribute_Class_Id(Attribute) = Class_SLoc
   function Get_Node_Attribute_SLoc_Column_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return Natural;

   ---------------------------------------------------------------------------
   --  Slocs.Get_Path()-Wrapper
   --
   --  Precondition:
   --    Get_Node_Attribute_Class_Id(Attribute) = Class_SLoc
   function Get_Node_Attribute_SLoc_Path_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return String;

   ---------------------------------------------------------------------------
   --  Slocs.Get_Filename()-Wrapper
   --
   --  Precondition:
   --    Get_Node_Attribute_Class_Id(Attribute) = Class_SLoc
   function Get_Node_Attribute_SLoc_Filename_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return String;


   ---------------------------------------------------------------------------
   --  Returns:
   --    Value of the attribute converted to a String
   --    Invalid_Attribute_Value_String if
   --      * Attribute contains an invalid value
   --      * Conversion to string is not implemented for given Attribute_Id
   function Get_Node_Attribute_Value_As_String
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return String;


   ---------------
   -- Iterators --
   ---------------

   ---------------------------------------------------------------------------
   function Make_Attribute_Iterator
      (Node_Class : in     Node_Class_Id)
      return Node_Attribute_Iterator;

   ---------------------------------------------------------------------------
   function Make_Attribute_Iterator
      (Node       : in     Node_Id)
      return Node_Attribute_Iterator;

   ---------------------------------------------------------------------------
   --  analogue to Bauhaus::Lists
   function More
     (Iterator : in Node_Attribute_Iterator)
     return Boolean;

   ---------------------------------------------------------------------------
   --  analogue to Bauhaus::Lists
   --  Raises:
   --    dont know yet - TBD!
   procedure Next
     (Iterator : in out Node_Attribute_Iterator;
      Info     :    out Node_Attribute_Id);

   -------------
   -- Hashing --
   -------------

   ---------------------------------------------------------------------------
   --  used more than once, therefore it is included here
   package Node_Attribute_Id_Hash_Functions is
      new Constant_Ptr_Hashs
     (T          => IML_Reflection.Field'Class,
      T_Ptr      => Node_Attribute_Id);

   ---------------------------------------------------------------------------
   --  Returns:
   --    Hash of the loaded IML-Graph
   function Get_Graph_Hash return Integer;

   ---------------------------------------------------------------------------
   function Hash_Node_Id (Key : in Node_Id) return Integer;

   ---------------------------------------------------------------------------
   function Hash_Edge_Id (Key : in Edge_Id) return Integer;

   ---------------------------------------------------------------------------
   function Hash_Node_Class_Id (Key : in Node_Class_Id) return Integer;

   ---------------------------------------------------------------------------
   function Hash_Edge_Class_Id (Key : in Edge_Class_Id) return Integer;

   ---------------
   --  Streams  --
   ---------------

   ---------------------------------------------------------------------------
   procedure Read_Edge_Id
     (Stream : in     Bauhaus_Io.In_Stream_Type;
      Edge   :    out Edge_Id);

   ---------------------------------------------------------------------------
   procedure Read_Node_Id
     (Stream : in     Bauhaus_Io.In_Stream_Type;
      Node   :    out Node_Id);

   ----------------------------------------------------------------------
   procedure Write_Edge_Id
     (Stream : in Bauhaus_Io.Out_Stream_Type;
      Edge   : in Edge_Id);

   ----------------------------------------------------------------------
   procedure Write_Node_Id
     (Stream : in Bauhaus_Io.Out_Stream_Type;
      Node   : in Node_Id);

private

   ---------------------------------------------------------------------------
   --  Stores all edges of the graph
   All_Edges        : Edge_Id_Array_Access;

   ---------------------------------------------------------------------------
   --  Stores all known Node_Classes
   All_Node_Classes : IML_Reflection.Classes;

   ---------------------------------------------------------------------------
   --  This constant is needed to allow a direct mapping from
   --    Edge_Id.Internal_Id into the array
   --  "direct mapping" means without needing to add or substract any offset
   All_Edges_First_Index : constant := 1;

   ---------------------------------------------------------------------------
   --  Constant for better readability of the code
   --    has to be 1, all others value will surely lead to a constraint-error
   Node_Edges_First_Index : constant := 1;

   ---------------------------------------------------------------------------
   --  Represents a single node of the IML_Graph
   type Node_Record
     (Number_Of_Incoming_Edges : Natural;
      Number_Of_Outgoing_Edges : Natural) is
     limited record
        --  changed in revision 1.30 (.adb: 1.44) from Storables.Storable
        --    to IML_Roots_IML_Root, since graph_lib is able to handle
        --    IML_Roots only
        IML_Node       : IML_Roots.IML_Root;

        Incoming_Edges : Edge_Id_Array
          (Node_Edges_First_Index .. Number_Of_Incoming_Edges);
        Outgoing_Edges : Edge_Id_Array
          (Node_Edges_First_Index .. Number_Of_Outgoing_Edges);
   end record;

   ---------------------------------------------------------------------------
   --  Represents an edge of the IML_Graph
   type Edge_Record is
     record
        --  Internal Id of an edge, used at loading and storing subgraphs and
        --    selections
        Internal_Id                   : Positive;

        Source_Node                   : Node_Id;
        Target_Node                   : Node_Id;

        --  Indicates from which attribute this edge was generated from
        Attribute                     : Node_Attribute_Id;

        --   =0 : n/a
        --  /=0 : element# of list/set where attribute was included
        Attribute_Element_Number     : Natural;
     end record;

   ---------------------------------------------------------------------------
   --  an unique edgeclass
   type Edge_Class is
     limited record
        Source_Node_Class     : Node_Class_Id;
        Source_Node_Attribute : Node_Attribute_Id;
     end record;

   ---------------------------------------------------------------------------
   Invalid_Node_Id                : constant Node_Id := null;
   Invalid_Attribute_Value_String : constant String  := "*INVALID*";

   ---------------------------------------------------------------------------
   type Node_Attribute_Iterator is record
      CurrentIndex : Integer;

      --  The class of the node on which the iterator iterates on
      Class        : IML_Reflection.Class_ID;
   end record;

   ---------------------------------------------------------------------------
   function "<"
      (Left  : Node_Attribute_Id;
       Right : Node_Attribute_Id)
      return Boolean;

   ----------------------------------------------------------------------------
   --  This function is private, since SLoc is split up in atomar elements
   --     for the public
   --
   --  Precondition:
   --     Get_Node_Attribute_Class_Id(Attribute) = Class_SLoc
   function Get_Node_Attribute_SLoc_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
     return SLocs.Sloc;

   ---------------------------------------------------------------------------
   --  null checking has to be done by caller
   --  this signature doesn't allow any null-checking
   --
   --  Returns:
   --    True  - if given storable belongs to IML_Root_Class
   --    False - otherwise
   function Is_IML_Root
     (The_Storable : access Storables.Storable_Class'Class)
     return Boolean;

   ---------------------------------------------------------------------------
   --  initialises public variable All_Node_Classes
   procedure Initialize_All_Node_Classes;

   ---------------------------------------------------------------------------
   --  Following could also be in .adb, but is listed here, since it is needed
   --    for testing
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   package IML_Node_ID_Hashed_Mappings is
      new Hashed_Mappings
     (Key_Type   => IML_Node_IDs.Node_ID,
      Value_Type => Node_Id,
      Hash       => IML_Node_IDs.Hash);

   --  Contains all IML-nodes
   IML_Node_ID_Mapping : IML_Node_ID_Hashed_Mappings.Mapping;

   --------------------------------------
   --  Hashing for Node_Attribute_Ids  --
   --------------------------------------

   ---------------------------------------------------------------------------
   package Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings is
      new Hashed_Mappings
     (Key_Type   => Node_Attribute_Id,
      Value_Type => Edge_Class_Id,
      Hash       => Node_Attribute_Id_Hash_Functions.Integer_Hash);

   ----------------------------------
   --  Hashing for Node_Class_Ids  --
   ----------------------------------

   ---------------------------------------------------------------------------
   package Node_Class_Id_Hash_Functions is
      new Constant_Ptr_Hashs
     (T          => IML_Reflection.Abstract_Class'Class,
      T_Ptr      => Node_Class_Id);

   ---------------------------------------------------------------------------
   type Node_Class_Id_Hash_Data is record
      --  Hashtable hasing Node_Attribute_Ids to Edge_Class_Ids
      Node_Attribute_Id_Mapping :
        Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.Mapping;

      --  further contents will surely come
   end record;

   ---------------------------------------------------------------------------
   type Node_Class_Id_Hash_Data_Access is access Node_Class_Id_Hash_Data;

   ---------------------------------------------------------------------------
   package Node_Class_Id_Hashed_Mappings is
      new Hashed_Mappings
     (Key_Type   => Node_Class_Id,
      Value_Type => Node_Class_Id_Hash_Data_Access,
      Hash       => Node_Class_Id_Hash_Functions.Integer_Hash);

   ---------------------------------------------------------------------------
   Node_Class_Id_Mapping : Node_Class_Id_Hashed_Mappings.Mapping;

end Giant.Graph_Lib;
