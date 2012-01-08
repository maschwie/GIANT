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
--  First Author: Oliver Kopp
--
--  $RCSfile: giant-graph_lib-subgraphs-atomic_tests.ads,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------
--
--  Provides an aunit test for atomic subgraphs-tests
--

with Ada.Strings.Unbounded;

with AUnit.Test_Cases;

package Giant.Graph_Lib.Subgraphs.Atomic_Tests is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   --  Registers routines to be run.
   procedure Register_Tests (T : in out Test_Case);

   --  Provides name identifying the test case.
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access;

   --  Preparation performed before each routine.
   procedure Set_Up (T : in out Test_Case);

   --  Cleanup performed after each routine.
   procedure Tear_Down (T :  in out Test_Case);

end Giant.Graph_Lib.Subgraphs.Atomic_Tests;