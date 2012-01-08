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
--  $RCSfile: giant-node_annotations.ads,v $, $Revision: 1.12 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
------------------------------------------------------------------------------
--  This package overs the functionality needed to handle node annotations.
--
--  Node Annotations belong to a project.
--  In order to avoid cyclic package dependencies the
--  functionality of this package is not part of project_management.
--
with Ada.Strings.Unbounded;

with Giant.Graph_Lib; -- from GIANT

with Hashed_Mappings; -- from Bauhaus IML "Reuse.src"
pragma Elaborate_All (Hashed_Mappings);

package Giant.Node_Annotations is

   ---------------------------------------------------------------------------
   --  This ADT represents the node Annotations read from the
   --  management file for node annoatations of a project.
   type Node_Annotation_Access is private;

   ---------------------------------------------------------------------------
   --  Raised if a passed file is not found
   Node_Annotations_File_Not_Found_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if a passed file is not correct
   Node_Annotations_File_Not_Correct_Exception : exception;

   ---------------------------------------------------------------------------
   --  Raised if a not initialized instance is used as parameter
   Node_Annotation_Access_Not_Initialized_Exception : exception;

   ---------------------------------------------------------------------------
   --  Raised if an annotation is demanded for a node that
   --  has no annotation
   Node_Is_Not_Annotated_Exception : exception;

   ---------------------------------------------------------------------------
   --  Raised if a node that already has an annotation should be annotated
   Node_Is_Already_Annoated_Exception : exception;

   ---------------------------------------------------------------------------
   --  Raised if a file where node annotations should be stored could
   --  not be accessed.
   Node_Annotations_File_Could_Not_Be_Written_Exception : exception;


   ---------------------------------------------------------------------------
   --  A
   --  Initialisation, persistence and deallocation.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Initializes the ADT
   --  Reads all node annotations stored in the management file for
   --  node annotations of a project. Annotations for nodes, that the
   --  currently loaded iml graph does not have are ignored.
   --
   --  Parameters:
   --    Node_Annotations_File - The xml file there the annotations are
   --      stored.
   --      It is recommended to pass an absolute path to a file.
   --  Returns:
   --    A new instance of the ADT holding all annotation read from the file.
   --  Raises:
   --    Node_Annotations_File_Not_Found_Exception - Raised if the
   --      file "Node_Annotations_File" is not found.
   --    Node_Annotations_File_Not_Correct_Exception - Raise if something goes
   --      while parsing the xml file (e.g. xml file is not valid).
   function Load_From_File
     (Node_Annotations_File : in String)
     return Node_Annotation_Access;

   ---------------------------------------------------------------------------
   --  Initializes the ADT
   --  Creates a new empty instance that holds no annotations.
   --
   --  Returns:
   --    A new Instance of the ADT
   function Create_Empty
     return Node_Annotation_Access;

   ---------------------------------------------------------------------------
   --  Creates an xml-File holding the node annotations, existing files
   --  are ovewritten.
   --
   --  A Document Type Definition File for the xml-File holding the
   --  node annoations will also be created in the directory where
   --  the xml file is located.
   --
   --  Parameters:
   --    Node_Annotations - The Instance holding the annotations that
   --      should be written into the file.
   --    Node_Annotations_File - The xml file into that the annotations
   --      should be writte. Only ABSOLUTE Paths should be passed.
   --  Raises:
   --    Node_Annotation_Access_Not_Initialized_Exception - Raised
   --      if the parameter "Node_Annotations" was not initialized.
   --    Node_Annotations_File_Could_Not_Be_Written_Exception - Raised
   --      if now write access is possible for the the passed
   --      file.
   procedure Write_To_File
     (Node_Annotations      : in Node_Annotation_Access;
      Node_Annotations_File : in String);

   ---------------------------------------------------------------------------
   --  Deallocates an Instance of the ADT
   --
   --  Parameters:
   --    The instance that should be deallocated.
   --  Raises:
   --    Node_Annotation_Access_Not_Initialized_Exception - Raised
   --      if the parameter "Node_Annotations" was not initialized.
   procedure Deallocate
     (Node_Annotations : in out Node_Annotation_Access);


   ---------------------------------------------------------------------------
   --  B
   --  Node Annotations - Read Access
   --  The following subprogramms offer read access to node annotations.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Determines whether the given node is annotated or not.
   --  This subprograms garantees high Performance - O(1).
   --
   --  Parameters:
   --    Node_Annotations - The instance of the ADT where the annoation
   --      should be searched.
   --    Node - The ID of the node whose annotation should be searched.
   --  Returns:
   --    True, if "Node" is annotated; False, otherwise.
   --  Raises:
   --    Node_Annotation_Access_Not_Initialized_Exception - Raised
   --      if the parameter "Node_Annotations" was not initialized.
   function Is_Annotated
     (Node_Annotations : in Node_Annotation_Access;
      Node             : in Graph_Lib.Node_Id)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Returns the annotation text of a Node.
   --  This subprograms garantees high Performance - O(1).
   --
   --  Parameters:
   --    Node_Annotations - The instance of the ADT where the annoation
   --      text should be searched.
   --    Node - The ID of the node whose annotation text should be
   --      returned.
   --  Returns:
   --    The annotation of "Node".
   --  Raises:
   --    Node_Annotation_Access_Not_Initialized_Exception - Raised
   --      if the parameter "Node_Annotations" was not initialized.
   --    Node_Is_Not_Annotated_Exception - Raised if the node "Node"
   --      has no annotation.
   function Get_Annotation_Text
     (Node_Annotations : in Node_Annotation_Access;
      Node             : in Graph_Lib.Node_Id)
     return String;

   ---------------------------------------------------------------------------
   --  Returns a list of all annotated nodes.
   --
   --  Needed to realize the Filter described in the Specification of GIANT
   --  (see "5.4. UC: Nicht referenzierte Knoten-Annotationen löschen").
   --
   --  Parameters:
   --    Node_Annotations - The Instance of the ADT whose annotated nodes
   --      should be returned.
   --  Returns:
   --    A list holding the ID's of all nodes that are annotated
   --    (a empty list may be returned if there are no annotated nodes).
   --  Raises:
   --    Node_Annotation_Access_Not_Initialized_Exception - Raised
   --      if the parameter "Node_Annotations" was not initialized.
   function Get_All_Annotated_Nodes
     (Node_Annotations : in Node_Annotation_Access)
     return Graph_Lib.Node_Id_Lists.List;


   ---------------------------------------------------------------------------
   --  C
   --  Node Annotations - Write Access
   --  The following subprograms offer the functionality to change
   --  node annotations.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Adds a new node together with its annotation to the instance of the ADT
   --
   --  Parameters:
   --    Node_Annotations - The instance of the ADT to that the annotation
   --      should be added.
   --    Node - The ID of the node.
   --    Annotation - The annotation text for the node "Node".
   --  Raises:
   --    Node_Annotation_Access_Not_Initialized_Exception - Raised
   --      if the parameter "Node_Annotations" was not initialized.
   --    Node_Is_Already_Annoated_Exception - Raised if the node "Node"
   --      has already an annotation.
   procedure Add_Node_Annotation
     (Node_Annotations : in Node_Annotation_Access;
      Node             : in Graph_Lib.Node_Id;
      Annotation       : in String);

   ---------------------------------------------------------------------------
   --  Changes the annotation of an existing node.
   --
   --  Parameters:
   --    Node_Annotations - The instance of the ADT where a annotation
   --      should be changed.
   --    Node - The ID of the node whose annotation should be changed.
   --    New_Annotation - The new annotation text for the node "Node".
   --  Raises:
   --    Node_Annotation_Access_Not_Initialized_Exception - Raised
   --      if the parameter "Node_Annotations" was not initialized.
   --    Node_Is_Not_Annotated_Exception - Raised if the node "Node"
   --      has no old annotation.
   procedure Change_Node_Annotation
     (Node_Annotations : in Node_Annotation_Access;
      Node             : in Graph_Lib.Node_Id;
      New_Annotation   : in String);

   ---------------------------------------------------------------------------
   --  Removes an annoation from the instance of the ADT
   --
   --  Parameters:
   --    Node_Annotations - The instance of the ADT where a annotation
   --      should be deleted.
   --    Node - The ID of the node whose annotation should be deleted.
   --  Raises:
   --    Node_Annotation_Access_Not_Initialized_Exception - Raised
   --      if the parameter "Node_Annotations" was not initialized.
   --    Node_Is_Not_Annotated_Exception - Raised if the node "Node"
   --      has no annotation.
   procedure Remove_Node_Annotation
     (Node_Annotations : in Node_Annotation_Access;
      Node             : in Graph_Lib.Node_Id);


   ---------------------------------------------------------------------------
   --  D
   --  Iterators
   --
   --  Note
   --    During an Iteration you may not change the ADT holding the
   --    node annotations.
   --
   --  Use the Iterator in the following way in order to "catch" all
   --  annotated nodes:
   --
   --    My_Iter := Make_Node_ID_Iter (My_Node_Annotations_ADT);
   --
   --    while More (My_Iter) loop
   --       Next (My_Iter, Node);
   --       Do samething funny or not funny with "Node";
   --   end loop;
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  An iterator over all annotated nodes.
   type Node_ID_Iter is private;

   ---------------------------------------------------------------------------
   --  Builds an Iterator over all annotated nodes. The iterator initially
   --  points to the first annotated node.
   --
   --  Parameters:
   --    Node_Annotations - The instance of the ADT for that the iterator
   --      should be build.
   --  Raises:
   --    Node_Annotation_Access_Not_Initialized_Exception - Raised
   --      if the passed ADT "Node_Annotations" was not initialized.
   function Make_Node_ID_Iter
     (Node_Annotations : in Node_Annotation_Access)
     return Node_ID_Iter;

   ---------------------------------------------------------------------------
   --  Determines whether the "end of the iterator" is reached or not
   --  (the "end of the iterator" is not an element - the end comes one step
   --  after the last element).
   --
   --  Parameters:
   --    Iter - The Iterator.
   --  Returns:
   --    True if the iterator has not been exhausted; False, otherwise.
   function More (Iter : in Node_ID_Iter) return Boolean;

   ---------------------------------------------------------------------------
   --  Returns the element the iterator currently points to and advances
   --  the iterator by one step.
   --
   --  Paramters:
   --    Iter - The iterator.
   --    Node - The Node the iterator pointed to before the execution
   --      of "Next".
   --  Raises:
   --    No_More (propagated from Hashed_Mappings) - Raised if "Next" is
   --      performed on an iterator that has already reached its end.
   procedure Next
     (Iter : in out Node_ID_Iter;
      Node :    out Graph_Lib.Node_Id);

------------------------------------------------------------------------------
private

   type Node_Annotation_Element;

   --  The ADT
   type Node_Annotation_Access is access Node_Annotation_Element;


   package Node_Annotation_Hashed_Mappings is new Hashed_Mappings
     (Key_Type   => Graph_Lib.Node_Id,
      Hash       => Graph_Lib.Hash_Node_Id,
      Value_Type => Ada.Strings.Unbounded.String_Access);

   --  Necessary to garantee that the pointer Node_Annotation_Access stays
   --  unchanged, because the pointer  Hashed_Mappings.Mapping may
   --  be changed by the procedure Hashed_Mappings.Bind(...).
   type Node_Annotation_Element is record

      Annotations : Node_Annotation_Hashed_Mappings.Mapping;
   end record;

   type Node_ID_Iter is new Node_Annotation_Hashed_Mappings.Keys_Iter;

end Giant.Node_Annotations;






