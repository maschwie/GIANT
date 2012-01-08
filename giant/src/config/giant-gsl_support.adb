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
-- $RCSfile: giant-gsl_support.adb,v $, $Revision: 1.3 $
-- $Author: schulzgt $
-- $Date: 2003-07-03 13:16:07 $
--
with Ada.Strings.Unbounded;

with Giant.Config_Settings;  -- from GIANT
with Giant.File_Management; -- from GIANT

with String_Lists; -- from Bauhaus IML "Reuse.src"

package body Giant.GSL_Support is


   ---------------------------------------------------------------------------
   -- A
   -- Color_Access
   ---------------------------------------------------------------------------
 
   ---------------------------------------------------------------------------
   function Get_GSL_Include 
     (GSL_File_Name : in String) 
     return String is
     
     use Ada.Strings.Unbounded;
     
     Include_Path_List : String_Lists.List;
     Abs_File_Path     : Ada.Strings.Unbounded.Unbounded_String;
   begin
      
      
      Include_Path_List := Config_Settings.Get_Setting_As_Expanded_Path_List 
        ("GSL.Include_Path");  
        
      Abs_File_Path :=  Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Locate_File_In_Directories
          (Include_Path_List, GSL_File_Name));                    
   
      String_Lists.Destroy (Include_Path_List);
      
      if Abs_File_Path = Ada.Strings.Unbounded.Null_Unbounded_String then
         raise GSL_Script_Not_Found_Exception;
      else
         return Ada.Strings.Unbounded.To_String (Abs_File_Path);
      end if;   
   end Get_GSL_Include;            
   
end Giant.GSL_Support;
