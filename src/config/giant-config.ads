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
-- $RCSfile: giant-config.ads,v $, $Revision: 1.9 $
-- $Author: schwiemn $
-- $Date: 2003-06-24 13:08:05 $
--
-- -----
-- Top Level Package for processed config settings 
--
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package Giant.Config is

   ---------------------------------------------------------------------------
   -- A String Pointer used to describe a Color.
   -- This pointer structure is used due to
   -- performance requirements, as the
   -- functions returning this type are called
   -- for each window node that has to be visualized.
   --
   -- The string describing the color (the result of
   -- function Get_Color_Value) is not specified.
   -- The reason for that ist that the config files should
   -- be independent of a special color model.
   -- The packages using this type will have to check
   -- whether it describes a valid color according to their
   -- color model.
   type Color_Access is private;

   ---------------------------------------------------------------------------
   -- Raised if an uninitialized instance of the ADT Color_Access
   -- is passed.
   Color_Access_Not_Initialized_Exception : exception;


   ---------------------------------------------------------------------------
   -- A
   -- Color_Access
   -- The following functions provide read access to the abstract data type
   -- "Color_Access".
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- This function returns a string value for the data object describing
   -- a color.
   -- The content of the returned String is not specified, as
   -- this package is independent from color models, the
   -- package using this package has to check whether the String
   -- is correct according to its color model.
   --
   -- Parameter:
   --   Color_Ptr - The Pointer describing a color value.
   -- Return:
   --   The corresponding color value.
   -- Raises:
   --   Color_Access_Not_Initialized_Exception - raised if an instance
   --     of Color_Access that is not initialized is passed as paraneter.
   function Get_Color_Value (Color_Ptr : in Color_Access) return String;

   ---------------------------------------------------------------------------
   -- This function calculates a hash value for Color_Access.
   --
   -- Note:
   --   The Hash Value is calculated based on the pointer Color_Access.
   --   That means that the Hash Value could be different for two
   --   instances of Color_Access even if the result of Get_Color_Value
   --   is equal.
   -- Parameter:
   --   The Color_Access Instance for that the Hash-Value should be
   --   calculated.
   -- Return:
   --   A Integer hash value.
   -- Raises:
   --   Color_Access_Not_Initialized_Exception - raised if an instance
   --     of Color_Access that is not initialized is passed as paraneter.
   function Hash_Color_Access (Color_Ptr : in Color_Access) return Integer;

------------------------------------------------------------------------------
private

   -- A String representing a Color Value.
   -- The Content of this string is not checked by
   -- this package.
   --
   -- Needs to be a pointer to "Unbounded_String" as a pointer to
   -- "String" has 64 Bit size what makes hashing this pointer not quite
   -- easy.
   type Color_Access is access all Ada.Strings.Unbounded.Unbounded_String;

   ---------------------------------------------------------------------------
   -- Deallocates Color_Access
   -- - needed by this package and subpackages for deallocation.
   procedure Free_Color_Access is new Ada.Unchecked_Deallocation
     (Ada.Strings.Unbounded.Unbounded_String, Color_Access);

end Giant.Config;
