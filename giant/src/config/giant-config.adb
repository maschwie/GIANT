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
-- $RCSfile: giant-config.adb,v $, $Revision: 1.8 $
-- $Author: schwiemn $
-- $Date: 2003-06-24 13:08:05 $
--
with Ada.Unchecked_Deallocation;

with Giant.File_Management;  -- from GIANT
with Giant.Config_Settings; -- from GIANT
with Giant.Ptr_Normal_Hashs; -- from GIANT
pragma Elaborate_All (Giant.Ptr_Normal_Hashs);

package body Giant.Config is


   ---------------------------------------------------------------------------
   -- A
   -- Color_Access
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_Color_Value (Color_Ptr : in Color_Access) return String is
   begin

      if (Color_Ptr = null) then
         raise Color_Access_Not_Initialized_Exception;
      end if;

      return Ada.Strings.Unbounded.To_String (Color_Ptr.all);
   end Get_Color_Value;

   ---------------------------------------------------------------------------
   package Hash_Color_Access_Hashs
   is new Ptr_Normal_Hashs
     (Ada.Strings.Unbounded.Unbounded_String,
      Color_Access);

   function Hash_Color_Access (Color_Ptr : in Color_Access) return Integer is
   begin

      if (Color_Ptr = null) then
         raise Color_Access_Not_Initialized_Exception;
      end if;

      return Hash_Color_Access_Hashs.Integer_Hash (Color_Ptr);
   end Hash_Color_Access;

end Giant.Config;
