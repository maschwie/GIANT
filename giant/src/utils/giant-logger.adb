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
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-logger.adb,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--

with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Giant.Default_Logger;

package body Giant.Logger is

   procedure Debug (Message : in String)
   is
   begin
      Default_Logger.Debug (Message, Name);
   end Debug;

   procedure Error (Message : in String)
   is
   begin
      Default_Logger.Error (Message, Name);
   end Error;

   procedure Error
     (Error : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Default_Logger.Error (Error);
   end;

   procedure Fatal (Message : in String)
   is
   begin
      Default_Logger.fatal (Message, Name);
   end Fatal;

   procedure Info (Message : in String)
   is
   begin
      Default_Logger.Info (Message, Name);
   end Info;

   procedure Warn (Message : in String)
   is
   begin
      Default_Logger.Warn (Message, Name);
   end Warn;

end Giant.Logger;

