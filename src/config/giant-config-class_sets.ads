------------------------------------------------------------------------------
-- GIANT - Graphical IML Analysis and Navigation Tool
--
-- Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
-- Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
-- First Author: Martin Schwienbacher
--
-- $RCSfile: giant-config-class_sets.ads,v $, $Revision: 1.7 $
-- $Author: schwiemn $
-- $Date: 2003-07-08 13:41:31 $
--
-- ----------------
-- This package provides the functionality needed to handle
-- class sets. A class set describes a collection of edge classes
-- and node classes (see GIANT Specification "Begfriffslexikon").
--
-- Inside this package each class set is represented by a data object,
-- i.e. each class set could be regarded as an ADO.
--
with String_Lists;     -- from Bauhaus IML "Reuse.src"
with Hashed_Mappings;  -- from Bauhaus IML "Reuse.src"
with Lists;            -- from Bauhaus IML "Reuse.src"
pragma Elaborate_All (Hashed_Mappings);

with Giant.Graph_Lib; -- from GIANT

package Giant.Config.Class_Sets is

   ---------------------------------------------------------------------------
   -- describes a class set
   type Class_Set_Data is private;
   type Class_Set_Access is access Class_Set_Data;
        
   ---------------------------------------------------------------------------
   -- Thrown if an invalid directory is given as paramter.
   Invalid_Class_Set_Directory_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised on attempt to call any subprogram before the ADO is
   -- initialized (by calling "Initialize_Class_Sets").
   Class_Sets_ADO_Not_Initialized_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if a not initialized instance of Class_Set_Access is used
   -- as parameter.
   Class_Sets_Access_Not_Initialized_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if a demanded class set does not exist
   Class_Set_Does_Not_Exist_Exception : exception;


   ---------------------------------------------------------------------------
   -- A
   -- Initialisation and deallocation of the internal datastrcuture
   -- that holds all known class sets.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Initializes all ADO's describing the class sets.
   -- Each class set is read out of a xml file.
   -- All xml files describing class sets have must be
   -- located in one directory.
   -- The xml Files are only accessed by this procedure,
   -- the other methods all work on a internal data structure.
   -- This procedure must have been executed before
   -- any other procedure or function of this package may be executed.
   --
   -- The name of a class set is defined by the name of the corresponding
   -- xml File. Therefore the name of each class set is unique.
   -- ----------
   --
   -- Note
   --   All xml files in the directory
   --   "GIANT_Class_Sets_Directory" are regarded
   --   as xml files describing a class set.
   --   GIANT will try to ignore XML files that do not describe a
   --   class set - but there is no warantee.
   -- Parameters:
   --   GIANT_Class_Sets_Directory - The directory there the class
   --     sets should be read from.
   -- Raises:
   --   Invalid_Class_Set_Directory_Exception - raised if the
   --     passed directory "GIANT_Class_Sets_Directory" is not found.
   procedure Initialize_Class_Sets
     (GIANT_Class_Sets_Directory  : in String);

   ---------------------------------------------------------------------------
   -- Deallocation.
   -- Deallocates all memory needed for the management of
   -- the class sets.
   -- After calling this perocedure Initialize_Class_Sets
   -- may be called again without causing memory leaks.
   --
   -- Note
   --   The type Class_Set_Accesss is a pointer that points
   --   directly into the internal data structure of this ADO.
   --   After calling this procedure, all pointers of type
   --   Class_Set_Access (returned by the subprograms of this package)
   --   will be dangling pointers.
   --
   -- Raises:
   --   Class_Sets_ADO_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Class_Sets".
   procedure Clear_Class_Sets;


   ---------------------------------------------------------------------------
   -- B
   -- Access to class sets.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Returns a list holding the names of all known class sets.
   -- You are responsible for the deallocation of the result.
   --
   -- Returns:
   --   A list holding the names of all known class sets.
   --   If there are no known class sets an empty list will be returned.
   -- Raises:
   --   Class_Sets_ADO_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Class_Sets".
   function Get_All_Existing_Class_Sets
     return String_Lists.List;

   ---------------------------------------------------------------------------
   -- Checks whether a class set with the name "Class_Set_Name"
   -- exists or not.
   --
   -- Parameters:
   --   Class_Set_Name - The name of a class set.
   -- Returns:
   --   True, if there is a class set with the given name;
   --   False, otherwise.
   -- Raises:
   --   Class_Sets_ADO_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Class_Sets".
   function Does_Class_Set_Exist (Class_Set_Name : in String)
     return Boolean;

   ---------------------------------------------------------------------------
   -- Used to initialize "Class_Set_Access". Returns an existing class
   -- set identified by the passed name.
   --
   -- The returned type is a pointer into the internal
   -- data structure of the ADO after calling "Clear_Class_Sets"
   -- you will have a dangling pointer.
   --
   -- Parameters:
   --   Class_Set_Name - the name of a class set.
   -- Returns:
   --   The instance describing the corresponding class set.
   -- Raises:
   --   Class_Sets_ADO_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Class_Sets"
   --   Class_Set_Does_Not_Exist_Exception - Raised if there is
   --     no class set with the name "Class_Set_Name".
   function Get_Class_Set_Access (Class_Set_Name : in String)
     return Class_Set_Access;
     
     
   ---------------------------------------------------------------------------
   -- D
   -- Queries to class sets.
   -- Provides functionalty needed to read the data stored
   -- inside a class set.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Determines whether a node class is part of a class
   -- set.
   -- Parameters:
   --   Class_Set - A class set.
   --   Node_Clas - A node class.
   -- Returns:
   --   True, if the node class is part of the class set;
   --   False, otherwise.
   -- Raises:
   --   Class_Sets_ADO_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Class_Sets"
   --   Class_Sets_Access_Not_Initialized_Exception - Raised if
   --     "Class_Set" was not initialized.
   function Is_Node_Class_Element_Of_Class_Set
     (Class_Set  : in Class_Set_Access;
      Node_Class : in Graph_Lib.Node_Class_Id)
     return Boolean;

   ---------------------------------------------------------------------------
   -- Determines whether an edge class is part of a class set.
   --
   -- Parameters:
   --   Class_Set - A class set.
   --   Edge_Clas - A edge class.
   -- Returns:
   --   True, if the edge class is part of the class set;
   --   False, otherwise.
   -- Raises:
   --   Class_Sets_ADO_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Class_Sets"
   --   Class_Sets_Access_Not_Initialized_Exception - Raised if
   --     "Class_Set" was not initialized.
   function Is_Edge_Class_Element_Of_Class_Set
     (Class_Set  : in Class_Set_Access;
      Edge_Class : in Graph_Lib.Edge_Class_Id)
     return Boolean;
               
   ---------------------------------------------------------------------------
   -- F
   -- Meta Class Sets
   --
   -- Meta Class Sets describe a union of class sets if you have to
   -- consider several class sets for an algorithm you may use them
   -- in the following way:
   --
   -- 1. Create a Meta_Class_Set over all needed Class Sets
   -- 2. Instead of asking each single class set ask the meta class set
   -- 3. Destroy the Meta Class set when it is now longer needed   
   --
   -- A Meta Class Set provides much faster access to its elements as
   -- asking each single class set separately.
   ---------------------------------------------------------------------------   

   ---------------------------------------------------------------------------
   -- A class set holding the elements (node and edge classes of several
   -- class sets).
   subtype Meta_Class_Set_Access is Class_Set_Access;
   
   type Class_Set_Array is array (Integer range <>) of Class_Set_Access;
   
   package Class_Sets_Lists is new
     Lists (ItemType => Class_Set_Access);
          
   ---------------------------------------------------------------------------
   -- Checks whether a meta class set is empty or not.
   --
   -- Returns:
   --   True, if "Meta" is empty, False otherwise.
   function Is_Empty (Meta : Meta_Class_Set_Access) return Boolean;
            
   ---------------------------------------------------------------------------
   -- Builds a meta class set out of a collection of class sets.
   --
   -- Note:
   --   Generates a "big class set" holding all elementes of the sub sets.
   --   As therefore a deep copy is generated this will cost a certain
   --   ammount of memory. Deallocation is up to you.
   --
   -- Parameters:
   --   Elements - All class sets based on that the meta class set should be
   --     built.   
   -- Returns:
   --   A "Meta Class Set" holding all Node Classes and Edge Classes
   --   of the passed Class Sets. You are responsible for the deallocation
   --   of the result. Returns an empty meta class set if an empty array
   --   (length < 1) is passed.
   function Build (Elements : in Class_Set_Array) 
     return Meta_Class_Set_Access;
         
   ---------------------------------------------------------------------------
   -- Just a wrapper for "Build" that uses an List as in Parameter.
   function Build (Elements : in Class_Sets_Lists.List) 
     return Meta_Class_Set_Access;
   
   ---------------------------------------------------------------------------
   -- Deallocates all memory needed for a meta class set. Has no effect
   -- on the class sets the meta class set was build out of.             
   --
   -- Parameters:
   --   Target - The meta class set that should be destroyed.
   -- Raises:
   --   Class_Sets_Access_Not_Initialized_Exception - Raised if  
   --     Target is not initialized.
   procedure Destroy (Target : in out Meta_Class_Set_Access);
      
------------------------------------------------------------------------------
private

   package Node_Class_Look_Up_Hashed_Mappings is new Hashed_Mappings
     (Key_Type   => Graph_Lib.Node_Class_Id,
      Hash       => Graph_Lib.Hash_Node_Class_Id,
      Value_Type => Graph_Lib.Node_Class_Id);

   package Edge_Class_Look_Up_Hashed_Mappings is new Hashed_Mappings
     (Key_Type   => Graph_Lib.Edge_Class_Id,
      Hash       => Graph_Lib.Hash_Edge_Class_Id,
      Value_Type => Graph_Lib.Edge_Class_Id);

   -- describes one class set
   type Class_Set_Data is record
      Class_Set_Name : Ada.Strings.Unbounded.Unbounded_String;
      Node_Classes   : Node_Class_Look_Up_Hashed_Mappings.Mapping;
      Edge_Classes   : Edge_Class_Look_Up_Hashed_Mappings.Mapping;
   end record;


end Giant.Config.Class_Sets;
