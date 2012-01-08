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
--  $RCSfile: giant-evolutions.adb,v $, $Revision: 1.23 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Tags;

with Gdk.Main;
with Gdk.Threads;
with Gtk.Main;

with Tagged_Ptr_Ops;

with Giant.Fixed_Priority_Queues;
pragma Elaborate_All (Giant.Fixed_Priority_Queues);
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);


package body Giant.Evolutions is


   package Evolution_Logger is new Logger
     (Name => "Giant.Evolutions");


   -------------------------------
   -- Driver_Controller         --
   -- for concurrent evolutions --
   -------------------------------

   --  Cancel handling
   package Driver_Dialog_Cb is new Gtk.Handlers.User_Callback
     (Widget_Type => Progress_Dialog.Progress_Dialog_Record,
      User_Type   => Driver_Id_Type);
   package Dialog_Cb is new Gtk.Handlers.Callback
     (Widget_Type => Progress_Dialog.Progress_Dialog_Record);
   procedure Cancel_Callback
     (Dialog    : access Progress_Dialog.Progress_Dialog_Record'Class;
      Driver_Id : in     Driver_Id_Type);


   --  Reflect priority ordering on driver states to ids of driver states
   function Has_Higher_Priority_Id
     (Left  : in Driver_Id_Type;
      Right : in Driver_Id_Type)
     return Boolean;

   procedure Set_Position
     (Driver_Id : in     Driver_Id_Type;
      Position  : in     Natural);

   function Get_Position
     (Driver_Id : in Driver_Id_Type)
     return Natural;

   --  Priority queues to handle updates of drivers
   package Driver_State_Queues is new Fixed_Priority_Queues
     (Item_Type           => Driver_Id_Type,
      Has_Higher_Priority => Has_Higher_Priority_Id,
      Set_Position        => Set_Position,
      Get_Position        => Get_Position);

   --  Stores all manageable Drivers
   type Driver_Array is array (Driver_Id_Type) of Driver_State_Type;


   --  Protected unit used for Driver control.
   protected Driver_Controller is

      -- must be called between Gdk.Threads.Enter and Gdk.Threads.Leave
      procedure Start_Driver
        (Individual       : access Concurrent_Evolution'Class;
         Started          :    out Boolean;
         Dialog           : in     Progress_Dialog.Progress_Dialog_Access);

      -- must be called between Gdk.Threads.Enter and Gdk.Threads.Leave
      procedure Perform_Update
        (Must_Synchronize :    out Concurrent_Driver_Access);

      entry Set_Individual
        (Driver_Id        : in     Driver_Id_Type;
         Individual       : access Concurrent_Evolution'Class);

      entry Schedule_Synchronization
        (Driver_Id        : in     Driver_Id_Type);

      entry Driver_Died
        (Driver_Id        : in     Driver_Id_Type);

      procedure Schedule_Cancel
        (Driver_Id        : in     Driver_Id_Type);

      function Is_Canceled
        (Driver_Id        : in     Driver_Id_Type)
        return Boolean;

   private

      --  "All managed drivers" is implemented as a global variable to
      --  simplify usage of Update_Queue.
      --  Drivers : Driver_Array

      --  Updates are done in order of 'Update_Queue'. All managed Drivers are
      --  in that queue at any point of time.
      Update_Queue : Driver_State_Queues.Queue_Type (Number_Of_Slots);

   end Driver_Controller;


   ------------------------------
   -- Controlling              --
   -- for iterative evolutions --
   ------------------------------

   procedure Start_Iterative_Driver
     (Individual : access Iterative_Evolution'Class;
      Started    :    out Boolean;
      Dialog     : in     Progress_Dialog.Progress_Dialog_Access);

   procedure Stop_Iterative_Driver;

   ----------------------------------------------------------------------------
   --  Originally implemented to be a callback to GtkAda's idle signal.
   --  Since GtkAda 2.2.0 this does not work anymore because one cannot
   --  start new event-loops inside an idle signal. (Thus an iterative
   --  evolution could not open modal dialogs)
   --
   --  Return:
   --    True if 'Step_Iterative_Driver' need be called again, False if
   --    if all work is done and the iterative driver was already stopped.
   function Step_Iterative_Driver
     return Boolean;


   ---------------
   -- Evolution --
   ---------------

   procedure Initialize
     (Individual : access Evolution;
      Complexity : in     Natural := 0) is
   begin
      Individual.Progress_Count := 0;
      Individual.Complexity := Complexity;
      Individual.Child_Progress := 0;
      Individual.Next_Action := Run;
      Individual.Parent := null;
      Individual.Child := null;
   end Initialize;

   procedure Synchronized_Step
     (Individual  : access Evolution;
      Next_Action :    out Evolution_Action) is
   begin
      Next_Action := Run;
   end Synchronized_Step;

   procedure Set_Complexity
     (Individual   : access Evolution'Class;
      Total_Number : in     Natural) is
   begin
      Individual.Complexity := Total_Number;
   end Set_Complexity;

   procedure Advance_Progress
     (Individual : access Evolution'Class;
      Progress   : in     Natural) is
   begin
      Individual.Progress_Count := Individual.Progress_Count + Progress;
   end Advance_Progress;

   function Get_Progress_Count
     (Individual   : access Evolution'Class)
      return Natural is
   begin
      return Individual.Progress_Count;
   end Get_Progress_Count;

   function Get_Complexity
     (Individual   : access Evolution'Class)
      return Natural is
   begin
      return Individual.Complexity;
   end Get_Complexity;

   function Logging_Name
     (Individual : access Evolution'Class)
     return String is

      package Ops is new Tagged_Ptr_Ops
        (T => Evolution, T_Ptr => Evolution_Class_Access);

   begin
      return "Individual"
        & Ops.Image (Evolution_Class_Access (Individual))
        & " of type " & Ada.Tags.External_Tag (Individual'Tag);
   end Logging_Name;

   function Validate
     (Individual : access Evolution'Class)
     return Boolean is
   begin
      if Get_Next_Action (Individual) = Cancel then
         Evolution_Logger.Error
           ("Validation failed (because Next_Action set to Cancel) for "
            & Logging_Name (Individual)
            & ". Possibly Initialize was not called.");
         return False;
      end if;
      return True;
   end Validate;

   procedure Init_Visuals
     (Individual : access Evolution'Class;
      Dialog     : in     Progress_Dialog.Progress_Dialog_Access := null) is
   begin
      if Progress_Dialog."/=" (Dialog, null) then
         Progress_Dialog.Set_Lower (Dialog, 0.0);
         --  Progress_Dialog.Set_Upper (Dialog, 10_000.0);
         Progress_Dialog.Set_Activity_Mode (Dialog, True);
         Progress_Dialog.Set_Progress_Text
           (Dialog, Get_Progress_Text_Unknown_Complexity (Individual));
         Progress_Dialog.Show_All (Dialog);
      end if;
   end Init_Visuals;

   procedure Update_Visuals
     (Individual : access Evolution'Class;
      Dialog     : in     Progress_Dialog.Progress_Dialog_Access := null) is

      Current          : Evolution_Class_Access :=
        Evolution_Class_Access (Individual);
      Complexity_Known : Boolean := True;
      Complexity       : Natural := 0;
      Step_Count       : Natural := 0;
   begin
      if Progress_Dialog."/=" (Dialog, null) then
         loop
            if Current.Complexity = 0 then
               Complexity_Known := False;
            end if;
            Complexity := Complexity + Current.Complexity
              + Current.Child_Progress;
            Step_Count := Step_Count + Current.Progress_Count
              + Current.Child_Progress;
            Current := Get_Parent (Current);
            exit when Current = null;
         end loop;

         if Complexity_Known then
            if Progress_Dialog.Get_Activity_Mode (Dialog) then
               Progress_Dialog.Set_Activity_Mode (Dialog, False);
               Progress_Dialog.Set_Progress_Text
                 (Dialog, Get_Progress_Text_Showing_Complexity (Individual));
            end if;
            Progress_Dialog.Set_Upper (Dialog, Glib.Gdouble (Complexity));
            Progress_Dialog.Set_Value (Dialog, Glib.Gdouble (Step_Count));
         else
            if not Progress_Dialog.Get_Activity_Mode (Dialog) then
               Progress_Dialog.Set_Activity_Mode (Dialog, True);
               Progress_Dialog.Set_Progress_Text
                 (Dialog, Get_Progress_Text_Unknown_Complexity (Individual));
            end if;
            Progress_Dialog.Set_Value (Dialog, Glib.Gdouble (Step_Count));
         end if;
      end if;
   end Update_Visuals;

   procedure Add_Child_Progress
     (Individual    : access Evolution'Class;
      Progress      : in     Natural) is
   begin
      Individual.Child_Progress := Individual.Child_Progress + Progress;
   end Add_Child_Progress;

   function Get_Progress_Text_Showing_Complexity
     (Individual : access Evolution)
     return String is
   begin
      return "%v " & (-"of") & " %u";
   end Get_Progress_Text_Showing_Complexity;

   function Get_Progress_Text_Unknown_Complexity
     (Individual : access Evolution)
     return String is
   begin
      return "%v";
   end Get_Progress_Text_Unknown_Complexity;


   procedure Done
     (Individual    : access Evolution'Class;
      Canceled      : in     Boolean) is

      Parent : Evolution_Class_Access := Get_Parent (Individual);
   begin
      if Parent /= null then
         Add_Child_Progress (Parent, Get_Progress_Count (Individual));
         Set_Child (Parent, null);
      end if;
      --  Force re-Initialize before next start of 'Individual'
      Set_Next_Action (Individual, Cancel);
      --  Dispatching call to user-defined subprogram
      Finish (Individual, Canceled);
   end Done;

   procedure Set_Next_Action
     (Individual  : access Evolution'Class;
      Next_Action : in     Evolution_Action) is
   begin
      --  'Next_Action' may be out of range if the out-Parameter was not
      --  assigned any value in the last call to 'Step' or
      --  'Synchronized_Step'. Since this error should be quite hard to
      --  find, we provide checking.
      if Next_Action in Evolution_Action then
         Individual.Next_Action := Next_Action;
      else
         Evolution_Logger.Error
           (Logging_Name (Individual)
            & " requested an illegal Next_Action (Pos ="
            & Integer'Image (Evolution_Action'Pos (Next_Action))
            & "). Use Cancel instead.");
         Individual.Next_Action := Cancel;
      end if;
   end Set_Next_Action;

   function Get_Next_Action
     (Individual  : access Evolution'Class)
     return Evolution_Action is
   begin
      return Individual.Next_Action;
   end Get_Next_Action;


   procedure Set_Parent
     (Individual : access Evolution'Class;
      Parent     : Evolution_Class_Access) is
   begin
      Individual.Parent := Parent;
   end Set_Parent;

   function Get_Parent
     (Individual : access Evolution'Class)
     return Evolution_Class_Access is
   begin
      return Individual.Parent;
   end Get_Parent;

   function Has_Parent
     (Individual : access Evolution'Class)
     return Boolean is
   begin
      return Individual.Parent /= null;
   end Has_Parent;


   procedure Set_Child
     (Individual : access Evolution'Class;
      Child      : in     Evolution_Class_Access) is
   begin
      Individual.Child := Child;
   end Set_Child;

   function Get_Child
     (Individual : access Evolution'Class)
     return Evolution_Class_Access is
   begin
      return Individual.Child;
   end Get_Child;

   function Has_Child
     (Individual : access Evolution'Class)
     return Boolean is
   begin
      return Individual.Child /= null;
   end Has_Child;


   --------------------------
   -- Concurrent_Evolution --
   --------------------------

   procedure Start_Calculation
     (Individual : access Concurrent_Evolution;
      Dialog     : in     Progress_Dialog.Progress_Dialog_Access := null;
      Started    :    out Boolean) is
   begin
      if Validate (Individual) then
         Driver_Controller.Start_Driver
           (Individual,
            Started,
            Dialog);
      end if;
   end Start_Calculation;

   procedure Start_Sub_Calculation
     (Master          : access Concurrent_Evolution'Class;
      Sub_Calculation : access Concurrent_Evolution'Class) is

      Sub_Insert_Point : Concurrent_Evolution_Class_Access :=
        Concurrent_Evolution_Class_Access (Sub_Calculation);
      Master_Child     : Concurrent_Evolution_Class_Access :=
        Get_Concurrent_Child (Master);
   begin
      if Validate (Sub_Calculation) then
         while Has_Child (Sub_Insert_Point) loop
            Sub_Insert_Point := Get_Concurrent_Child (Sub_Insert_Point);
         end loop;
         Set_Concurrent_Child (Sub_Insert_Point, Master_Child);
         if Master_Child /= null then
            Set_Concurrent_Parent (Master_Child, Sub_Insert_Point);
         end if;
         Set_Concurrent_Child
           (Master,
            Concurrent_Evolution_Class_Access (Sub_Calculation));
         Set_Concurrent_Parent
           (Sub_Calculation,
            Concurrent_Evolution_Class_Access (Master));
      end if;
   end Start_Sub_Calculation;

   procedure Set_Concurrent_Child
     (Individual : access Concurrent_Evolution'Class;
      Child      : in     Concurrent_Evolution_Class_Access) is
   begin
      Set_Child (Individual, Evolution_Class_Access (Child));
   end Set_Concurrent_Child;

   function Get_Concurrent_Child
     (Individual : access Concurrent_Evolution'Class)
     return Concurrent_Evolution_Class_Access is

      Child : Evolution_Class_Access := Get_Child (Individual);
   begin
      pragma Assert
        (Child = null or else
         Child.all in Concurrent_Evolution'Class);
      return Concurrent_Evolution_Class_Access (Child);
   end Get_Concurrent_Child;


   procedure Set_Concurrent_Parent
     (Individual : access Concurrent_Evolution'Class;
      Parent     : in     Concurrent_Evolution_Class_Access) is
   begin
      Set_Parent (Individual, Evolution_Class_Access (Parent));
   end Set_Concurrent_Parent;

   function Get_Concurrent_Parent
     (Individual : access Concurrent_Evolution'Class)
     return Concurrent_Evolution_Class_Access is

      Parent : Evolution_Class_Access := Get_Parent (Individual);
   begin
      pragma Assert
        (Parent = null or else
         Parent.all in Concurrent_Evolution'Class);
      return Concurrent_Evolution_Class_Access (Parent);
   end Get_Concurrent_Parent;


   -------------------------
   -- Iterative_Evolution --
   -------------------------

   procedure Start_Calculation
     (Individual : access Iterative_Evolution;
      Dialog     : in     Progress_Dialog.Progress_Dialog_Access := null;
      Started    :    out Boolean) is
   begin
      if Validate (Individual) then
         Start_Iterative_Driver
           (Individual,
            Started,
            Dialog);
      end if;
   end Start_Calculation;

   procedure Start_Sub_Calculation
     (Master          : access Iterative_Evolution'Class;
      Sub_Calculation : access Evolution'Class) is

      Sub_Insert_Point : Evolution_Class_Access :=
        Evolution_Class_Access (Sub_Calculation);
      Master_Child     : Evolution_Class_Access :=
        Get_Child (Master);
   begin
      if Validate (Sub_Calculation) then
         while Has_Child (Sub_Insert_Point) loop
            Sub_Insert_Point := Get_Child (Sub_Insert_Point);
         end loop;
         Set_Child (Sub_Insert_Point, Master_Child);
         if Master_Child /= null then
            Set_Parent (Master_Child, Sub_Insert_Point);
         end if;
         Set_Child (Master, Evolution_Class_Access (Sub_Calculation));
         Set_Parent (Sub_Calculation, Evolution_Class_Access (Master));
      end if;
   end Start_Sub_Calculation;


   -----------------------
   -- Concurrent_Driver --
   -----------------------

   task body Concurrent_Driver is
      Individual     : Concurrent_Evolution_Class_Access :=
        Concurrent_Evolution_Class_Access (Driven_Calculation);
      Parent         : Concurrent_Evolution_Class_Access;
      Deepest_Child  : Concurrent_Evolution_Class_Access;
      Perform_Action : Evolution_Action;
      Next_Action    : Evolution_Action;
   begin
      --  exit after evolution (including all subcalculations) has
      --  finished or was canceled
      while Individual /= null loop

         --  Individual might have produced sub-calculations. Continue with
         --  deepest child
         Deepest_Child := Individual;
         while Has_Child (Deepest_Child) loop
            Deepest_Child := Get_Concurrent_Child (Deepest_Child);
            pragma Assert (Get_Concurrent_Parent (Deepest_Child) /= null);
         end loop;
         if Deepest_Child /= Individual then
            Individual := Deepest_Child;
            Driver_Controller.Set_Individual (Id, Individual);
         end if;

         --  fetch next action to be performed
         if Driver_Controller.Is_Canceled (Id) then
            Perform_Action := Cancel;
         else
            Perform_Action := Get_Next_Action (Individual);
         end if;

         --  perform the action
         if Perform_Action = Run then
            Next_Action := Cancel;
            Step (Individual, Next_Action);
            --  memorize next action to be performed on 'Individual'
            Set_Next_Action (Individual, Next_Action);
         else
            --  need synchronization
            Driver_Controller.Schedule_Synchronization (Id);
            accept Synchronize do
               --  Cancel might have been requested while waiting for
               --  entry call --> poll again
               if Driver_Controller.Is_Canceled (Id) then
                  Perform_Action := Cancel;
               end if;

               pragma Assert (Perform_Action in Evolution_Action);
               case Perform_Action is
                  when Synchronize =>
                     Next_Action := Cancel;
                     Synchronized_Step (Individual, Next_Action);
                     Set_Next_Action (Individual, Next_Action);

                  when Finish =>
                     --  Fetch parent first, because 'Individual' must not
                     --  be accessed after the call to 'Done'
                     Parent := Get_Concurrent_Parent (Individual);
                     Done (Individual, False);
                     --  Finish 'Individual' and continue with parent
                     --  if no parent exists then task terminates
                     Individual := Parent;
                     if Individual /= null then
                        Driver_Controller.Set_Individual (Id, Individual);
                     else
                        Driver_Controller.Driver_Died (Id);
                     end if;

                  when Cancel =>
                     loop
                        --  Fetch parent first, because 'Individual' must not
                        --  be accessed after the call to 'Done'
                        Parent := Get_Concurrent_Parent (Individual);
                        Done (Individual, True);
                        --  Finish 'Individual' and continue with parent
                        --  if no parent exists then task terminates
                        Individual := Parent;
                        exit when Individual = null;
                     end loop;
                     Driver_Controller.Driver_Died (Id);

                  when others =>
                     --  cannot happen
                     pragma Assert (False);
                     null;

               end case;
            end Synchronize;
         end if;
      end loop;
   exception
      when Occurrence : others =>
         Evolution_Logger.Fatal
           ("Exception raised in concurrent driver. Currently evolving: " &
            Logging_Name (Individual) & ". Recovery not implemented, " &
            "shutting down driver immediately. Exception Name = " &
            Ada.Exceptions.Exception_Name
              (Ada.Exceptions.Exception_Identity (Occurrence)) &
            " Information: " & Ada.Exceptions.Exception_Information
              (Occurrence));
         Driver_Controller.Driver_Died (Id);
   end Concurrent_Driver;


   -----------------------
   -- Driver priorities --
   -----------------------

   function Has_Higher_Priority
     (Left  : in Driver_State_Type;
      Right : in Driver_State_Type)
     return Boolean is
   begin
      if Left.Current_State = Right.Current_State then
         return Ada.Real_Time."<" (Left.State_Change, Right.State_Change);
      else
         return Left.Current_State < Right.Current_State;
      end if;
   end Has_Higher_Priority;


   --------------------------------------
   -- Driver_Controller implementation --
   --------------------------------------

   --  All managed Drivers
   Drivers : Driver_Array :=
     (others => No_Driver_State);


   function Has_Higher_Priority_Id
     (Left  : in Driver_Id_Type;
      Right : in Driver_Id_Type)
     return Boolean is
   begin
      return Has_Higher_Priority (Drivers (Left), Drivers (Right));
   end Has_Higher_Priority_Id;

   procedure Set_Position
     (Driver_Id : in     Driver_Id_Type;
      Position  : in     Natural) is
   begin
      Drivers (Driver_Id).Position := Position;
   end Set_Position;

   function Get_Position
     (Driver_Id : in Driver_Id_Type)
     return Natural is
   begin
      return Drivers (Driver_Id).Position;
   end Get_Position;


   --  must be run between Gdk.Threads.Enter and Gdk.Threads.Leave
   procedure Begin_Concurrent_Updates;

   --  must be run between Gdk.Threads.Enter and Gdk.Threads.Leave
   procedure End_Concurrent_Updates;


   protected body Driver_Controller is

      procedure Set_State
        (Driver_Id : in     Driver_Id_Type;
         State     : in     Driver_Action_Type) is
      begin
         Drivers (Driver_Id).Current_State := State;
         Drivers (Driver_Id).State_Change := Ada.Real_Time.Clock;
         Driver_State_Queues.Update_Item (Update_Queue, Driver_Id);
      end Set_State;

      --  must be run between Gdk.Threads.Enter and Gdk.Thread.Leave.
      procedure Initialize_Driver
        (Driver_Id  : in     Driver_Id_Type;
         Individual : access Concurrent_Evolution'Class;
         Dialog     : in     Progress_Dialog.Progress_Dialog_Access) is
      begin
         Drivers (Driver_Id) :=
           (Driver          => new Concurrent_Driver (Driver_Id, Individual),
            Individual      => Concurrent_Evolution_Class_Access (Individual),
            Current_State   => Running,
            State_Change    => Ada.Real_Time.Clock,
            Cancel_Request  => False,
            Position        => 0,
            Cancel_Handler  => Null_Handler,
            Dialog          => Dialog);

         if Driver_State_Queues.Is_Empty (Update_Queue) then
            Begin_Concurrent_Updates;
         end if;
         Driver_State_Queues.Insert (Update_Queue, Driver_Id);

         if Progress_Dialog."/=" (Dialog, null) then
            Progress_Dialog.Ref (Dialog);
            Drivers (Driver_Id).Cancel_Handler :=
              Driver_Dialog_Cb.Connect
              (Widget    => Dialog,
               Name      => "cancelled",
               Marsh     => Driver_Dialog_Cb.To_Marshaller
                              (Cancel_Callback'Access),
               User_Data => Driver_Id);
            Init_Visuals (Drivers (Driver_Id).Individual, Dialog);
         end if;
      end Initialize_Driver;


      --  must be run between Gdk.Threads.Enter and Gdk.Threads.Leave
      procedure Shutdown_Driver
        (Driver_Id : in     Driver_Id_Type) is

         procedure Free is new Ada.Unchecked_Deallocation
           (Object => Concurrent_Driver,
            Name   => Concurrent_Driver_Access);

      begin
         Driver_State_Queues.Remove_Item (Update_Queue, Driver_Id);
         if Driver_State_Queues.Is_Empty (Update_Queue) then
            End_Concurrent_Updates;
         end if;

         Free (Drivers (Driver_Id).Driver);

         --  Drivers (Driver_Id).Individual must not be touched

         if Progress_Dialog."/=" (Drivers (Driver_Id).Dialog, null)
         then
            Gtk.Handlers.Disconnect
              (Drivers (Driver_Id).Dialog,
               Drivers (Driver_Id).Cancel_Handler);
            Progress_Dialog.Destroy (Drivers (Driver_Id).Dialog);
            Progress_Dialog.Unref (Drivers (Driver_Id).Dialog);
         end if;

         Drivers (Driver_Id) := No_Driver_State;
      end Shutdown_Driver;


      --  must be run between Gdk.Threads.Enter and Gdk.Threads.Leave
      procedure Update_Visuals
        (Driver_Id : in     Driver_Id_Type) is
      begin
         Update_Visuals
           (Individual => Drivers (Driver_Id).Individual,
            Dialog     => Drivers (Driver_Id).Dialog);
      end Update_Visuals;


      --  must be called between Gdk.Thread.Enter and Gdk.Threads.Leave
      procedure Start_Driver
        (Individual : access Concurrent_Evolution'Class;
         Started    :    out Boolean;
         Dialog     : in     Progress_Dialog.Progress_Dialog_Access)
      is
         Current : Driver_Id_Type := Drivers'First;
      begin
         Started := False;
         if Current <= Drivers'Last then
            loop
               Started := Drivers (Current) = No_Driver_State;
               exit when Started or else Current = Drivers'Last;
               Current := Current + 1;
            end loop;
            if Started then
               Initialize_Driver
                 (Current, Individual, Dialog);
            end if;
         end if;
      end Start_Driver;

      -- must be called between Gdk.Threads.Enter and Gdk.Threads.Leave
      procedure Perform_Update
        (Must_Synchronize :    out Concurrent_Driver_Access) is

         Driver_Id : Driver_Id_Type;
      begin
         Must_Synchronize := null;

         if not Driver_State_Queues.Is_Empty (Update_Queue) then
            Driver_Id := Driver_State_Queues.Get_Head (Update_Queue);

            --  Set_State must be called to assign a new time-stamp and
            --  thus set to correct position in 'Update_Queue'
            case Drivers (Driver_Id).Current_State is

               when Waiting_On_Sync =>
                  Update_Visuals (Driver_Id);
                  Must_Synchronize := Drivers (Driver_Id).Driver;
                  Set_State (Driver_Id, Running);

               when Dead =>
                  --  must not update visuals because 'Individual' must not
                  --  be accessed
                  if Drivers (Driver_Id).Driver'Terminated then
                     Shutdown_Driver (Driver_Id);
                  else
                     Set_State (Driver_Id, Dead);
                  end if;

               when Running =>
                  Update_Visuals (Driver_Id);
                  Set_State (Driver_Id, Running);
            end case;
         end if;
      end Perform_Update;

      entry Set_Individual
        (Driver_Id       : in     Driver_Id_Type;
         Individual      : access Concurrent_Evolution'Class) when True is
      begin
         Drivers (Driver_Id).Individual :=
           Concurrent_Evolution_Class_Access (Individual);
      end Set_Individual;

      entry Schedule_Synchronization
        (Driver_Id       : in     Driver_Id_Type) when True is
      begin
         Set_State (Driver_Id, Waiting_On_Sync);
      end Schedule_Synchronization;

      entry Driver_Died
        (Driver_Id       : in     Driver_Id_Type) when True is
      begin
         Drivers (Driver_Id).Individual := null;
         Set_State (Driver_Id, Dead);
      end Driver_Died;

      procedure Schedule_Cancel
        (Driver_Id       : in     Driver_Id_Type) is
      begin
         Drivers (Driver_Id).Cancel_Request := True;
         --  does not affect priority.
      end Schedule_Cancel;

      function Is_Canceled
        (Driver_Id       : in     Driver_Id_Type)
        return Boolean is
      begin
         return Drivers (Driver_Id).Cancel_Request;
      end Is_Canceled;

   end Driver_Controller;


   ---------------------------------
   -- Concurrent Driver Callbacks --
   ---------------------------------

   procedure Cancel_Callback
     (Dialog    : access Progress_Dialog.Progress_Dialog_Record'Class;
      Driver_Id : in     Driver_Id_Type) is
   begin
      Driver_Controller.Schedule_Cancel (Driver_Id);
   end Cancel_Callback;


   function Concurrent_Update_Callback
     return Boolean is

      Driver_To_Sync : Concurrent_Driver_Access;
   begin
      Gdk.Threads.Enter;

      Driver_Controller.Perform_Update (Driver_To_Sync);
      if Driver_To_Sync /= null then
         Driver_To_Sync.Synchronize;
      end if;

      Gdk.Main.Flush;
      Gdk.Threads.Leave;
      return True;
   end Concurrent_Update_Callback;


   --------------------------------
   -- Iterative Driver Callbacks --
   --------------------------------

   ----------------------------------------------------------------------------
   --  State of the iterative driver, used by 'Start_Iterative_Driver_State',
   --  'Step_Iterative_Driver', 'Stop_Iterative_Driver' and
   --  'Iterative_Cancel_Callback'
   Driver_State : Iterative_Driver_State_Type := No_Iterative_Driver_State;


   procedure Iterative_Cancel_Callback
     (Dialog : access Progress_Dialog.Progress_Dialog_Record'Class) is
   begin
      Driver_State.Cancel_Request := True;
   end Iterative_Cancel_Callback;


   -----------------------
   -- Callback-handling --
   -----------------------

   Concurrent_Update_Id : Gtk.Main.Timeout_Handler_Id;

   procedure Begin_Concurrent_Updates is
   begin
      Concurrent_Update_Id := Gtk.Main.Timeout_Add
        (Poll_Delay_Milli_Seconds, Concurrent_Update_Callback'Access);
   end Begin_Concurrent_Updates;

   procedure End_Concurrent_Updates is
   begin
      Gtk.Main.Timeout_Remove (Concurrent_Update_Id);
   end End_Concurrent_Updates;


   ----------------------
   -- Iterative_Driver --
   ----------------------

   procedure Start_Iterative_Driver
     (Individual : access Iterative_Evolution'Class;
      Started    :    out Boolean;
      Dialog     : in     Progress_Dialog.Progress_Dialog_Access) is

      Dead : Boolean := False;
   begin
      Started := Driver_State = No_Iterative_Driver_State;
      if Started then
         Driver_State :=
           (Individual      => Evolution_Class_Access (Individual),
            Update_Time     => Ada.Real_Time.Clock,
            Cancel_Request  => False,
            Cancel_Handler  => Null_Handler,
            Dialog          => Dialog);

         if Progress_Dialog."/=" (Driver_State.Dialog, null) then
            Progress_Dialog.Ref (Driver_State.Dialog);
            Driver_State.Cancel_Handler := Dialog_Cb.Connect
              (Widget    => Driver_State.Dialog,
               Name      => "cancelled",
               Marsh     => Dialog_Cb.To_Marshaller
                             (Iterative_Cancel_Callback'Access));
            Progress_Dialog.Set_Modal (Driver_State.Dialog);
            Init_Visuals (Driver_State.Individual, Driver_State.Dialog);
         end if;

         loop
            while not Dead and then Gtk.Main.Events_Pending loop
               Dead := Gtk.Main.Main_Iteration;
            end loop;

            exit when not Step_Iterative_Driver;
         end loop;
      end if;
   end Start_Iterative_Driver;

   procedure Stop_Iterative_Driver is
   begin
      if Progress_Dialog."/=" (Driver_State.Dialog, null) then
         Gtk.Handlers.Disconnect
           (Driver_State.Dialog,
            Driver_State.Cancel_Handler);
         Progress_Dialog.Unref (Driver_State.Dialog);
         Progress_Dialog.Destroy (Driver_State.Dialog);
      end if;
      Driver_State := No_Iterative_Driver_State;
   end Stop_Iterative_Driver;

   function Step_Iterative_Driver
     return Boolean is

      Parent         : Evolution_Class_Access;
      Child          : Evolution_Class_Access;
      Perform_Action : Evolution_Action;
      Next_Action    : Evolution_Action;
   begin
      pragma Assert (Driver_State.Individual /= null);

      --  Individual might have produced sub-calculations. Continue with
      --  deepest child
      loop
         Child := Get_Child (Driver_State.Individual);
         exit when Child = null;
         pragma Assert (Get_Parent (Child) = Driver_State.Individual);
         Driver_State.Individual := Child;
      end loop;

      --  Update visuals if time has come
      if Ada.Real_Time.">="
        (Left  => Ada.Real_Time.Clock,
         Right => Ada.Real_Time."+"
                    (Driver_State.Update_Time,
                     Ada.Real_Time.Milliseconds (Poll_Delay_Milli_Seconds)))
      then
         Update_Visuals
           (Driver_State.Individual,
            Driver_State.Dialog);
         Driver_State.Update_Time := Ada.Real_Time.Clock;
      end if;

      --  fetch next action to be performed
      if Driver_State.Cancel_Request then
         Perform_Action := Cancel;
      else
         Perform_Action := Get_Next_Action (Driver_State.Individual);
      end if;

      --  perform the action
      case Perform_Action is
         when Run =>
            Next_Action := Cancel;
            Step (Driver_State.Individual, Next_Action);
            --  memorize next action to be performed on 'Individual'
            Set_Next_Action (Driver_State.Individual, Next_Action);

         when Synchronize =>
            Next_Action := Cancel;
            Synchronized_Step (Driver_State.Individual, Next_Action);
            --  memorize next action to be performed on 'Individual'
            Set_Next_Action (Driver_State.Individual, Next_Action);

         when Finish =>
            --  Fetch parent first, because 'Individual' must not
            --  be accessed after the call to 'Done'
            Parent := Get_Parent (Driver_State.Individual);
            Done (Driver_State.Individual, False);
            --  Finish 'Individual' and continue with parent
            --  if no parent exists then terminate Driver
            Driver_State.Individual := Parent;

         when Cancel =>
            loop
               --  Fetch parent first, because 'Individual' must not
               --  be accessed after the call to 'Done'
               Parent := Get_Parent (Driver_State.Individual);
               Done (Driver_State.Individual, True);
               --  Finish 'Individual' and continue with parent
               --  if no parent exists then terminate Driver
               Driver_State.Individual := Parent;
               exit when Driver_State.Individual = null;
            end loop;
      end case;

      if Driver_State.Individual = null then
         --  terminate Driver
         Stop_Iterative_Driver;
         Driver_State := No_Iterative_Driver_State;
         return False;
      else
         --  continue with next step
         return True;
      end if;
   end Step_Iterative_Driver;


   procedure Start_Calculation_Blocked
     (Individual : access Evolution'Class)
   is
      Next_Action : Evolution_Action := Run;
   begin
      loop
         case Next_Action is
           when Run =>
              Next_Action := Cancel;
              Step (Individual, Next_Action);
           when Synchronize =>
              Next_Action := Cancel;
              Synchronized_Step (Individual, Next_Action);
           when Finish =>
              Finish (Individual, False);
              return;
           when Cancel =>
              Finish (Individual, True);
              return;
         end case;
      end loop;
   end Start_Calculation_Blocked;


end Giant.Evolutions;












































