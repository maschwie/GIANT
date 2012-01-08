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
--  $RCSfile: giant-fixed_priority_queues.adb,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


package body Giant.Fixed_Priority_Queues is


   ------------------------
   -- Index calculations --
   ------------------------

   function Get_First_Index
     (Queue : in     Queue_Type)
     return Array_Bounds is
   begin
      return Queue.Field'First;
   end Get_First_Index;
   pragma Inline (Get_First_Index);


   function Get_Last_Index
     (Queue : in     Queue_Type)
     return Array_Bounds is
   begin
      return Queue.Size;
   end Get_Last_Index;
   pragma Inline (Get_Last_Index);


   function Get_Maximum_Index
     (Queue : in     Queue_Type)
     return Array_Bounds is
   begin
      return Queue.Field'Last;
   end Get_Maximum_Index;
   pragma Inline (Get_Maximum_Index);


   function Get_Parent_Index
     (Child_Index : in Array_Bounds)
     return Array_Bounds is
   begin
      return Child_Index / 2;
   end Get_Parent_Index;
   pragma Inline (Get_Parent_Index);


   function Get_Left_Child_Index
     (Parent_Index : in Array_Bounds)
     return Array_Bounds is
   begin
      return Parent_Index * 2;
   end Get_Left_Child_Index;
   pragma Inline (Get_Left_Child_Index);


   function Get_Right_Child_Index
     (Parent_Index : in Array_Bounds)
     return Array_Bounds is
   begin
      return Parent_Index * 2 + 1;
   end Get_Right_Child_Index;
   pragma Inline (Get_Right_Child_Index);


   function Get_Greater_Child_Index
     (Queue        : in Queue_Type;
      Parent_Index : in Array_Bounds)
     return Array_Bounds is

      Left_Index  : Array_Bounds;
      Right_Index : Array_Bounds;
   begin
      Left_Index  := Get_Left_Child_Index (Parent_Index);
      if Left_Index < Get_Last_Index (Queue) then
         Right_Index := Left_Index + 1;
         if Has_Higher_Priority
           (Queue.Field (Left_Index), Queue.Field (Right_Index)) then

            return Left_Index;
         else
            return Right_Index;
         end if;
      else
         return Left_Index;
      end if;
   end Get_Greater_Child_Index;


   -------------------------
   -- Accessing the array --
   -------------------------

   function Get_Item
     (Queue    : in     Queue_Type;
      Position : in     Array_Bounds)
     return Item_Type is
   begin
      pragma Assert (Get_Position (Queue.Field (Position)) = Position);
      return Queue.Field (Position);
   end Get_Item;
   pragma Inline (Get_Item);

   function Get_Item_Index
     (Queue : in     Queue_Type;
      Item  : in     Item_Type)
     return Array_Bounds is

      Position : Natural := Get_Position (Item);
   begin
      if Queue.Size > 0 and then
        Position in Get_First_Index (Queue) .. Get_Last_Index (Queue) and then
        Queue.Field (Position) = Item
      then
         return Position;
      else
         raise Unknown_Item;
      end if;
   end Get_Item_Index;

   procedure Set_Item
     (Queue    : in out Queue_Type;
      Position : in     Array_Bounds;
      Item     : in     Item_Type) is
   begin
      Queue.Field (Position) := Item;
      Set_Position (Item, Position);
   end Set_Item;
   pragma Inline (Set_Item);


   -------------
   -- Helpers --
   -------------

   procedure Remove_At_Index
     (Queue : in out Queue_Type;
      Index : in     Array_Bounds) is

      Child_Index          : Array_Bounds;
      Parent_Index         : Array_Bounds;
      Parent_Item          : Item_Type;
      Greater_Child_Index  : Array_Bounds;
      Greater_Child_Item   : Item_Type;
      Largest_Parent_Index : Array_Bounds;
      Update_Item          : Item_Type;
   begin
      Update_Item := Get_Item (Queue, Get_Last_Index (Queue));

      --  mark the item as removed
      Set_Position (Get_Item (Queue, Index), 0);

      Queue.Size := Queue.Size - 1;
      if Get_Size (Queue) < 1 then
         null;
      elsif Get_Size (Queue) = 1 and then Index <= Get_Last_Index (Queue) then
         Set_Item (Queue, Get_First_Index (Queue), Update_Item);
      elsif Index <= Get_Last_Index (Queue) then
         --  Propagate upwards if possible
         Child_Index := Index;
         loop
            exit when Child_Index = Get_First_Index (Queue);
            Parent_Index := Get_Parent_Index (Child_Index);
            Parent_Item := Get_Item (Queue, Parent_Index);
            exit when not Has_Higher_Priority (Update_Item, Parent_Item);
            Set_Item (Queue, Child_Index, Parent_Item);
            Child_Index := Parent_Index;
         end loop;

         if Child_Index /= Index then
            --  Upwards successful
            Set_Item (Queue, Child_Index, Update_Item);
         else
            --  Upwards not successful, try downwards
            Parent_Index := Index;
            Largest_Parent_Index := Get_Parent_Index (Get_Last_Index (Queue));

            loop
               exit when Parent_Index > Largest_Parent_Index;
               Greater_Child_Index := Get_Greater_Child_Index
                 (Queue, Parent_Index);
               Greater_Child_Item := Get_Item (Queue, Greater_Child_Index);
               exit when not Has_Higher_Priority
                 (Greater_Child_Item, Update_Item);
               Set_Item
                 (Queue, Parent_Index, Greater_Child_Item);
               Parent_Index := Greater_Child_Index;
            end loop;
            Set_Item (Queue, Parent_Index, Update_Item);
         end if;
      end if;
   end Remove_At_Index;


   procedure Update_Downward
     (Queue : in out Queue_Type;
      Index : in     Array_Bounds) is

      Updated_Item         : constant Item_Type := Get_Item (Queue, Index);
      Largest_Parent_Index : Array_Bounds;
      Parent_Index         : Array_Bounds;
      Greater_Child_Index  : Array_Bounds;
      Greater_Child_Item   : Item_Type;
   begin
      if Get_Size (Queue) > 1 then
         Parent_Index := Index;
         Largest_Parent_Index := Get_Parent_Index (Get_Last_Index (Queue));
         loop
            exit when Parent_Index > Largest_Parent_Index;
            Greater_Child_Index := Get_Greater_Child_Index
              (Queue, Parent_Index);
            Greater_Child_Item := Get_Item (Queue, Greater_Child_Index);
            exit when not Has_Higher_Priority
              (Greater_Child_Item, Updated_Item);
            Set_Item (Queue, Parent_Index, Greater_Child_Item);
            Parent_Index := Greater_Child_Index;
         end loop;
         Set_Item (Queue, Parent_Index, Updated_Item);
      end if;
   end Update_Downward;


   procedure Update_Upward
     (Queue : in out Queue_Type;
      Index : in     Array_Bounds) is

      Updated_Item : constant Item_Type := Get_Item (Queue, Index);
      Child_Index  : Array_Bounds := Index;
      Parent_Index : Array_Bounds;
      Parent_Item  : Item_Type;
   begin
      loop
         exit when Child_Index = Get_First_Index (Queue);
         Parent_Index := Get_Parent_Index (Child_Index);
         Parent_Item := Get_Item (Queue, Parent_Index);
         exit when not Has_Higher_Priority
           (Updated_Item, Parent_Item);
         Set_Item (Queue, Child_Index, Parent_Item);
         Child_Index := Parent_Index;
      end loop;
      Set_Item (Queue, Child_Index, Updated_Item);
   end Update_Upward;


   -------------
   -- Methods --
   -------------

   procedure Clear
     (Queue : in out Queue_Type) is
   begin
      --  'Get_Last_Index' is illegal if size < 1
      if not Is_Empty (Queue) then
         for I in Get_First_Index (Queue) .. Get_Last_Index (Queue) loop
            Set_Position (Get_Item (Queue, I), 0);
         end loop;
         Queue.Size := 0;
      end if;
   end Clear;


   function Contains
     (Queue : in     Queue_Type;
      Item  : in     Item_Type)
     return Boolean is

      Position : Natural := Get_Position (Item);
   begin
      return not Is_Empty (Queue) and then
        Position in Get_First_Index (Queue) .. Get_Last_Index (Queue) and then
        Get_Item (Queue, Position) = Item;
   end Contains;


   function Get_Size
     (Queue : in     Queue_Type)
     return Natural is
   begin
      return Queue.Size;
   end Get_Size;


   function Get_Max_Size
     (Queue : in     Queue_Type)
     return Natural is
   begin
      return Queue.Field'Length;
   end Get_Max_Size;


   function Get_Head
     (Queue : in     Queue_Type)
      return Item_Type is
   begin
      if Is_Empty (Queue) then
         raise Not_Enough_Items;
      else
         return Get_Item (Queue, Get_First_Index (Queue));
      end if;
   end Get_Head;


   procedure Insert
     (Queue : in out Queue_Type;
      Item  : in     Item_Type) is

      Current_Index : Array_Bounds;
      Parent_Index  : Array_Bounds;
      Parent_Item   : Item_Type;
   begin
      if Get_Position (Item) /= 0 then
         raise Invalid_Item;
      elsif Queue.Size >= Queue.Field'Length then
         raise Too_Many_Items;
      else
         Queue.Size := Queue.Size + 1;
         Current_Index := Get_Last_Index (Queue);
         loop
            exit when Current_Index = Get_First_Index (Queue);
            Parent_Index := Get_Parent_Index (Current_Index);
            Parent_Item := Get_Item (Queue, Parent_Index);
            exit when not Has_Higher_Priority
              (Item, Parent_Item);
            Set_Item (Queue, Current_Index, Parent_Item);
            Current_Index := Parent_Index;
         end loop;
         Set_Item (Queue, Current_Index, Item);
      end if;
   end Insert;


   function Is_Empty
     (Queue : in     Queue_Type)
      return Boolean is
   begin
      return Queue.Size = 0;
   end Is_Empty;


   procedure Remove_Head
     (Queue : in out Queue_Type) is
   begin
      if Queue.Size <= 0 then
         raise Not_Enough_Items;
      end if;
      Remove_At_Index (Queue, Get_First_Index (Queue));
   end Remove_Head;


   procedure Remove_Item
     (Queue : in out Queue_Type;
      Item  : in     Item_Type) is
   begin
      Remove_At_Index (Queue, Get_Item_Index (Queue, Item));
   end Remove_Item;


   procedure Update_Head
     (Queue : in out Queue_Type) is
   begin
      if Queue.Size <= 0 then
         raise Not_Enough_Items;
      end if;
      Update_Downward (Queue, Get_First_Index (Queue));
   end Update_Head;


   procedure Update_Item
     (Queue : in out Queue_Type;
      Item  : in     Item_Type) is

      Index : Array_Bounds := Get_Item_Index (Queue, Item);
   begin
      if Index > Get_First_Index (Queue) and then
        Has_Higher_Priority (Item, Get_Item (Queue, Get_Parent_Index (Index)))
      then
         Update_Upward (Queue, Index);
      else
         Update_Downward (Queue, Index);
      end if;
   end Update_Item;

end Giant.Fixed_Priority_Queues;
