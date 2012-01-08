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
-- $RCSfile: giant-gsl_support.ads,v $, $Revision: 1.3 $
-- $Author: schwiemn $
-- $Date: 2003-07-02 15:45:18 $
--
-- -----
-- This Package offers support for GSL Include Files.
--
package Giant.GSL_Support is


   ---------------------------------------------------------------------------
   -- A
   -- Support for Include Files
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Raised if GSL Skript File could not be located in the include paths.
   GSL_Script_Not_Found_Exception : Exception;

   ---------------------------------------------------------------------------
   -- Searches the GSL Include Paths (specified in the config file) and
   -- returns an absolute path to the file.
   -- If the file does exist several times the first hit will be returned.
   --
   -- Note:
   --   Giant.Config_Settings must have been initialized before executing
   --   this subprogram.
   
   -- Parameters:
   --   GSL_File_Name - The name (file name and extension) of GSL Include
   --     File. 
   -- Returns:
   --   An absolute path to a file.
   -- Raises:
   --   GSL_Script_Not_Found_Exception - Raised if no file 
   --   "GSL_File_Name" could be located in any of the include paths for
   --   GSL Skript files.
   function Get_GSL_Include (GSL_File_Name : in String) return String;

end Giant.GSL_Support;
