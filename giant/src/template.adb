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
--  First Author: <unkown>
--
--  $RCSfile: template.adb,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--

with Giant.Controller;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Template is

   package Logger is new Giant.Logger("giant.main");

   Window_Count : Integer;
   MAX_WINDOW   : constant Integer := 5;

   function Show_Window
     (W : in Coordinate)
      return Booolean is
   begin
      if Is_Visible (W) then
         raise Already_Visible_Exception;
      elsif (Window_Count == MAX_WINDOW) then
         raise Constraint_Exception;
      end if;

      for I in 1 .. MAX_WINDOW loop
         case I is
            when 1 =>
               Foo_Bar;
            when others =>
               Bar_Foo;
         end case;
      end loop;

      Put_Line (-"User visible string.");
   end Show_Window;

end Giant.Template;

