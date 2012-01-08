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
--  First Author: Steffen Keul
--
--  $RCSfile: giant-merging_iterators.adb,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Ada.Unchecked_Deallocation;

package body Giant.Merging_Iterators is

   function Has_Higher_Priority
     (Left  : in Single_Iterator;
      Right : in Single_Iterator)
     return Boolean is
   begin
      return Left.Head < Right.Head;
   end Has_Higher_Priority;

   ------------------------------
   -- Access via access values --
   ------------------------------

   function Create
     (Iterators : in Iterator_Lists.List)
     return Merger_Access is

      Merger   : Merger_Access;
      Iterator : Iterator_Lists.ListIter := Iterator_Lists.MakeListIter
        (Iterators);
      Current  : Sets.Iterator;
   begin
      Merger := new Merger_Type (Iterator_Lists.Length (Iterators));
      while Iterator_Lists.More (Iterator) loop
         Iterator_Lists.Next (Iterator, Current);
         Add_Iterator (Merger.all, Current);
      end loop;
      Start_Iteration (Merger.all);
      return Merger;
   end Create;

   function Has_More
     (Merger    : access Merger_Type)
     return Boolean is
   begin
      return Has_More (Merger.all);
   end Has_More;

   function Get_Current
     (Merger    : access Merger_Type)
     return Item_Type is
   begin
      return Get_Current (Merger.all);
   end Get_Current;

   procedure Forward
     (Merger    : access Merger_Type) is
   begin
      Forward (Merger.all);
   end Forward;

   procedure Destroy
     (Merger    : in out Merger_Access) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Merger_Type,
         Name   => Merger_Access);

   begin
      if Merger /= null then
         Destroy (Merger.all);
         Free (Merger);
      end if;
   end Destroy;


   -------------------------------
   -- Local Merger_Type-objects --
   -------------------------------

   procedure Advance_Iterator
     (Merger : in out Merger_Type;
      Item   : in out Single_Iterator) is
   begin
      if Sets.More (Merger.Pool (Item.Pool_Index)) then
         Item.Head := Sets.Current (Merger.Pool (Item.Pool_Index));
         Sets.Next (Merger.Pool (Item.Pool_Index));
         Single_Iterator_Queues.Insert (Merger.Queue, Item);
      else
         Sets.Destroy (Merger.Pool (Item.Pool_Index));
      end if;
   end Advance_Iterator;

   procedure Update_Next
     (Merger : in out Merger_Type) is

      Head_Item : Single_Iterator;
   begin
      pragma Assert (Merger.Current_Is_Available);
      Merger.Next_Is_Available := False;
      while not (Merger.Next_Is_Available or else
                 Single_Iterator_Queues.Is_Empty (Merger.Queue)) loop

         Head_Item := Single_Iterator_Queues.Get_Head (Merger.Queue);
         Single_Iterator_Queues.Remove_Head (Merger.Queue);

         Merger.Next := Head_Item.Head;
         Merger.Next_Is_Available := Merger.Next /= Merger.Current;

         Advance_Iterator (Merger, Head_Item);
      end loop;
   end Update_Next;

   procedure Add_Iterator
     (Merger   : in out Merger_Type;
      Iterator : in     Sets.Iterator) is

      Item       : Single_Iterator;
   begin
      pragma Assert (not Merger.Current_Is_Available);
      Item.Pool_Index := Single_Iterator_Queues.Get_Size (Merger.Queue) + 1;
      --  raises Constraint_Error if too many Iterators are added.
      Merger.Pool (Item.Pool_Index) := Sets.Copy (Iterator);

      if Sets.More (Merger.Pool (Item.Pool_Index)) then
         Item.Head := Sets.Current (Merger.Pool (Item.Pool_Index));
         Sets.Next (Merger.Pool (Item.Pool_Index));
         Single_Iterator_Queues.Insert (Merger.Queue, Item);
      else
         Sets.Destroy (Merger.Pool (Item.Pool_Index));
      end if;
   end Add_Iterator;

   procedure Start_Iteration
     (Merger   : in out Merger_Type) is

      Head_Item : Single_Iterator;
   begin
      Merger.Current_Is_Available := not Single_Iterator_Queues.Is_Empty
        (Merger.Queue);
      if Merger.Current_Is_Available then
         Head_Item := Single_Iterator_Queues.Get_Head (Merger.Queue);
         Single_Iterator_Queues.Remove_Head (Merger.Queue);
         Merger.Current := Head_Item.Head;

         Advance_Iterator (Merger, Head_Item);
         Update_Next (Merger);
      end if;
   end Start_Iteration;

   function Has_More
     (Merger   : in     Merger_Type)
     return Boolean is
   begin
      return Merger.Current_Is_Available;
   end Has_More;

   function Get_Current
     (Merger   : in     Merger_Type)
     return Item_Type is
   begin
      pragma Assert (Has_More (Merger));
      return Merger.Current;
   end Get_Current;

   procedure Forward
     (Merger   : in out Merger_Type) is
   begin
      Merger.Current := Merger.Next;
      Merger.Current_Is_Available := Merger.Next_Is_Available;
      if Merger.Current_Is_Available then
         Update_Next (Merger);
      end if;
   end Forward;

   procedure Destroy
     (Merger   : in out Merger_Type) is

      Item : Single_Iterator;
   begin
      while not Single_Iterator_Queues.Is_Empty (Merger.Queue) loop
         Item := Single_Iterator_Queues.Get_Head (Merger.Queue);
         Single_Iterator_Queues.Remove_Head (Merger.Queue);
         Sets.Destroy (Merger.Pool (Item.Pool_Index));
      end loop;
      Merger.Current_Is_Available := False;
      Merger.Next_Is_Available := False;
   end Destroy;

end Giant.Merging_Iterators;
