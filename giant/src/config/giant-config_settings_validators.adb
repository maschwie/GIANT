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
-- First Author: Steffen Pingel
--
-- $RCSfile: giant-config_settings_validators.adb,v $, $Revision: 1.1 $
-- $Author: squig $
-- $Date: 2003-06-25 18:59:59 $
--

package body Giant.Config_Settings_Validators is

   function Validate 
	 (Value : in String) 
	 return Boolean 
   is
      Test_Value : Data_Type;
   begin
      begin
         Test_Value := Data_Type'Value (Value);
      exception
         when Constraint_Error =>
            return False;
      end;
      return True;
   end Validate;

   function Get
	 (Value : in String)
	 return Data_Type 
   is
   begin 
	  return Data_Type'Value (Value);
   end Get;

end Giant.Config_Settings_Validators;
