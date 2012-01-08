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
--  $RCSfile: giant-logger.ads,v $, $Revision: 1.9 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
--  Contains the generic logging package.
--
--  To use this package, first create an instance of this package:
--
--    package My_Logger is new Logger("giant.mypackage");
--
--  Then call one of the methods depending on the severity:
--
--    My_Logger.Debug ("debug message");
--
--  See:
--    Giant.Default_Logger

with Ada.Exceptions;

generic

   ---------------------------------------------------------------------------
   --  The name of the logger. Use a hierarchial name like
   --  giant.mypackage. Log messages can be filtered by this name.
   Name : String;

package Giant.Logger is

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Debug
   procedure Debug (Message : in String);

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Error
   procedure Error (Message : in String);

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Error
   procedure Error
     (Error : in Ada.Exceptions.Exception_Occurrence);

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Fatal
   procedure Fatal (Message : in String);

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Info
   procedure Info (Message : in String);

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Warn
   procedure Warn (Message : in String);

end Giant.Logger;

