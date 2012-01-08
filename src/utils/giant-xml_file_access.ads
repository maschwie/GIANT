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
-- $RCSfile: giant-xml_file_access.ads,v $, $Revision: 1.3 $
-- $Author: schwiemn $
-- $Date: 2003-06-25 16:53:18 $
-- --------
-- 
-- This package offers basic functionality needed to handle xml files
-- with xmlada.
-- Only subprograms needed to open (retrieve the top level document node)
-- and check xml files are offered. The processing of the xml nodes
-- hierarchy is done by the specific functions from other packages.
-- 
-- If you change the xml parser it is supposed that you will have
-- to change the subprograms inside this package.
--
-- As the other suprograms (except from the deallocation of an xml tree)
-- in other packages
-- only work on the top level document node by using the functionality
-- specified in  
-- ----
-- Document Object Model (DOM) Level 2 Core Specification Version 
-- 1.0 W3C Recommendation 13 November, 2000
-- ----
-- it is supposed that there you will possibly only have to change
-- the names of all subprogram calls to the packages offered by
-- xmlada (your new xmlparser should have a corresponding 
-- subprogram as all used subprograms are specified in 
-- (DOM) Level 2 Core Specification)
--
with Input_Sources.File; -- from xmlada
with Tree_Readers;       -- from xmlada
 with Sax.Readers;        -- from xmlada

with DOM.Core.Nodes;     -- from xmlada
with DOM.Core.Documents; -- from xmlada

package Giant.XML_File_Access is
   
   ---------------------------------------------------------------------------
   -- Raised if something goes wrong while trying to open a given file
   XML_File_Access_Error_Exception : exception;
   
   ---------------------------------------------------------------------------
   -- Raised if something goes wrong while trying to parse
   -- a xml file (illegal file type, file not valid ...).
   --
   -- As xmlada does not document in way that exceptions its subprograms
   -- raise it is not possible to raise more specific exception.
   XML_File_Parse_Fatal_Error_Exception : exception;
   
   ---------------------------------------------------------------------------
   -- Reads an xml document from a file and returns the top level
   -- document node for that file.
   --
   -- A relative path for the position of the DTD file inside the xml file
   -- (when using external DTD's) will be regarded as relative towards the
   -- directory where the xml file is located.
   --
   --
   -- Note
   --   After the call of this subprogram the xml tree will be completely 
   --   stored in memory. If you do not need it any more you will have to
   --   deallocate it by calling 
   --   "Tree_Readers.Free(Read : in out Tree_Reader)"
   --   therefore the Reader used to parse the file is returned 
   --   aus out parameter by this subprogram.
   -- 
   --   Exceptions thrown by xml ada are catched by this subprogram,
   --   already allocated storrage will be deallocated.
   --   
   --   The validation done by xmlada does not detect all
   --   possible faults. The author ("Martin Schwienbacher")
   --   supposes that the parser will possible also accept
   --   non valid xml files even though the "validation
   --   feature" is activated.
   --
   -- Paramters:
   --   File - The xml file that will be parsed and validated.
   --   Tree_Reader - An instance of the reader holding the memory allocated
   --     for the xml tree. It is needed for deallocation
   --     after processing the xml file.
   --   XML_Document - The top level node of a xml document. In order to
   --     ensure compatibility to the 
   --     "Document Object Model (DOM) Level 2 Core Specification"
   --     other subprograms should only work on this toplevel node.
   -- Raises:
   --   XML_File_Access_Error_Exception - Raised if the passed File
   --     could not be read for any reason.
   --   XML_File_Parse_Fatal_Error_Exception - Raised if something went
   --     wrong duiring the parsing of the xml file (most likely to occure
   --     if the parsed xml file is not valid).
   procedure Load_XML_File_Validated
     (File         : in String;
      Tree_Reader  : in out Tree_Readers.Tree_Reader;
      XML_Document : out Dom.Core.Document);

   ---------------------------------------------------------------------------
   -- Used to differ several types of xml files (different xml files have
   -- different doctype definitions and possibly different tag names 
   -- for the top level node). 
   --
   -- As xmlada seems not to be able to retrieve the DTD associated
   -- to a parsed document (type "Dom.Core.Document") GIANT differs
   -- separate types of xml files (e.g. for visualisation styles and
   -- global config data) by the tag name of the top level node
   -- (the "document element").
   --
   -- Parameters:
   --   Type_Name - The type that the xml document should have
   --   Parsed_XML_Document - A persed xml document represented 
   --     in the main memory as a DOM-Tree.
   -- Returns:
   --   True if "Type_Name" is equal to the tag name of the
   --   top level node of "Top_Level_Element" - the "document element".
   function Does_XML_Document_Belong_To_Type
     (Type_Name           : in String;
      Parsed_XML_Document : in Dom.Core.Document) return Boolean;

end Giant.XML_File_Access;
