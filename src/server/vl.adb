with Boards;
with Actions;
with Servers;
with Clients;

with Ada.Unchecked_Deallocation;

package body VL is
    procedure Free_VL_State is new Ada.Unchecked_Deallocation (
        VL_State, VL_State_Access);

    procedure Free_State (State : in out VL_State_Access) is begin
        Free_VL_State (State);
    end Free_State;

    protected body VL_Notifier is
        entry Query (Changed : out Boolean) when True is begin
            Changed := Has_Changed;
        end Query;

        entry Get_Dimensions (
            Width : out Positive;
            Height : out Positive;
            Num_Players : out Positive;
            Num_Actions : out Natural)
            when Has_Changed is
        begin
            Width := Last_State.Width;
            Height := Last_State.Height;
            Num_Players := Last_State.Num_Players;
            Num_Actions := Last_State.Num_Actions;
        end Get_Dimensions;

        entry Get_State (State : out VL_State)
            when Has_Changed is
        begin
            State := Last_State.all;
        end Get_State;

        procedure Notify (State : in VL_State) is begin
            if Last_State /= null then
                Free_State (Last_State);
            end if;

            Last_State := new VL_State (
                Width => State.Width,
                Height => State.Height,
                Num_Players => State.Num_Players,
                Num_Actions => State.Num_Actions);
            Last_State.all := State;
            Has_Changed := True;
        end Notify;
    end VL_Notifier;

    procedure Free_Action_List is new Ada.Unchecked_Deallocation (
        Actions.Action_Array, Action_List_Ptr);

    protected body VL_Game is
        procedure Notify_All;
        procedure Free_Listeners;
        procedure Free_Notifiers;
        procedure Free_Uncommitted_Actions;
        procedure Clear_Uncommitted_Actions;
        procedure Update_Turn;

        entry Attach_Listener (What : in VL_Callback)
            when Created and not Destroyed is
            New_Listener_Node : Listener_Node_Ptr := new Listener_Node;
        begin
            New_Listener_Node.Callback := What;
            New_Listener_Node.Next := Listeners;
            Listeners := New_Listener_Node;
            Notify_All;
        end Attach_Listener;

        entry Attach_Notifier (Notifier : in VL_Notifier_Access)
            when Created and not Destroyed is
                New_Notifier_Node : Notifier_Node_Ptr := new Notifier_Node;
        begin
            New_Notifier_Node.Notifier := Notifier;
            New_Notifier_Node.Next := Notifiers;
            Notifiers := New_Notifier_Node;
            Notify_All;
        end Attach_Notifier;

        procedure Clear_Uncommitted_Actions is begin
            for Index in Current_Actions'Range loop
                if Current_Actions (Index) /= null then
                    Free_Action_List (Current_Actions (Index));
                    Current_Actions (Index) := null;
                end if;
            end loop;
        end Clear_Uncommitted_Actions;

        entry Commit (Which : in Actions.Action_Array; Team : in Natural)
            when Created and not Destroyed is
            Any_Holdouts : Boolean := False;
        begin
            for Index in State.Players'Range loop
                if State.Players (Index).Team = Team then
                    State.Players (Index).Status := COMMITTED;
                    Current_Actions (Index) :=
                        new Actions.Action_Array (Which'Range);
                    Current_Actions (Index).all := Which;
                end if;
                if State.Players (Index).Status /= COMMITTED then
                    Any_Holdouts := True;
                end if;
            end loop;

            if not Any_Holdouts then
                Update_Turn;

                for Index in State.Players'Range loop
                    State.Players (Index).Status := ACTIVE;
                end loop;

                Notify_All;
            end if;
        end Commit;

        entry Create (Target : in Boards.Board)
            when not Created is
            Player_List : Boards.Player_List := Boards.Get_Players (Target);
        begin
            State := new VL_State (
                Width => Target.Width,
                Height => Target.Height,
                Num_Players => Player_List'Length,
                Num_Actions => 0);
            State.Board := Target;
            for Index in State.Players'Range loop
                State.Players (Index) := (EMPTY, Player_List (Index));
            end loop;
            Current_Actions := new Uncommitted_Actions (1 .. State.Num_Players);
            for Index in Current_Actions'Range loop
                Current_Actions (Index) := null;
            end loop;
            Created := True;
            Destroyed := False;
        end Create;

        entry Destroy when Created and not Destroyed is
        begin
            Notify_All;
            Free_State (State);
            Destroyed := True;
            Created := False;
            Free_Listeners;
            Free_Notifiers;
            Free_Uncommitted_Actions;
        end Destroy;

        procedure Free_Listeners is
            procedure Free_Listener_Node is new Ada.Unchecked_Deallocation (
                Listener_Node, Listener_Node_Ptr);
            Temp_Listener : Listener_Node_Ptr;
        begin
            while Listeners /= null loop
                Temp_Listener := Listeners;
                Listeners := Listeners.Next;
                Free_Listener_Node (Temp_Listener);
            end loop;
        end Free_Listeners;

        procedure Free_Notifiers is
            procedure Free_Notifier_Node is new Ada.Unchecked_Deallocation (
                Notifier_Node, Notifier_Node_Ptr);
            Temp_Notifier : Notifier_Node_Ptr;
        begin
            while Notifiers /= null loop
                Temp_Notifier := Notifiers;
                Notifiers := Notifiers.Next;
                Free_Notifier_Node (Temp_Notifier);
            end loop;
        end Free_Notifiers;

        procedure Free_Uncommitted_Actions is
            procedure Free_VL_Uncommitted_Actions is
                new Ada.Unchecked_Deallocation (
                    Uncommitted_Actions, Uncommitted_Actions_Ptr);
        begin
            Clear_Uncommitted_Actions;
            Free_VL_Uncommitted_Actions (Current_Actions);
            Current_Actions := null;
        end Free_Uncommitted_Actions;

        entry Join (Team : out Natural)
            when Created and not Destroyed is
        begin
            for Index in State.Players'Range loop
                if State.Players (Index).Status = EMPTY then
                    State.Players (Index).Status := ACTIVE;
                    Team := State.Players (Index).Team;
                    Notify_All;
                    exit;
                end if;
            end loop;
        end Join;

        entry Leave (Team : in Natural)
            when Created and not Destroyed is
        begin
            for Index in State.Players'Range loop
                if State.Players (Index).Team = Team then
                    State.Players (Index).Status := QUIT;
                    Notify_All;
                    exit;
                end if;
            end loop;
        end Leave;

        procedure Notify_All is
            Current_Listener : Listener_Node_Ptr;
            Current_Notifier : Notifier_Node_Ptr;
        begin
            Current_Listener := Listeners;
            while Current_Listener /= null loop
                Current_Listener.Callback.all (State.all);
                Current_Listener := Current_Listener.Next;
            end loop;

            Current_Notifier := Notifiers;
            while Current_Notifier /= null loop
                Current_Notifier.Notifier.Notify (State.all);
                Current_Notifier := Current_Notifier.Next;
            end loop;
        end Notify_All;

        entry Set (New_State : in VL_State)
            when Created and not Destroyed is
        begin
            if State /= null then
                Free_State (State);
            end if;
            State := new VL_State (
                New_State.Width,
                New_State.Height,
                New_State.Num_Players,
                New_State.Num_Actions);
            State.all := New_State;
            Notify_All;
        end Set;

        entry Uncommit (Team : in Natural)
            when Created and not Destroyed is
        begin
            for Index in State.Players'Range loop
                if State.Players (Index).Team = Team then
                    if Current_Actions (Index) /= null then
                        Free_Action_List (Current_Actions (Index));
                        Current_Actions (Index) := null;
                    end if;
                    State.Players (Index).Status := ACTIVE;
                    Notify_All;
                    exit;
                end if;
            end loop;
        end Uncommit;

        procedure Update_Turn is
            Total_Actions : Natural := 0;
        begin
            for Index in Current_Actions'Range loop
                for Act in Current_Actions (Index).all'Range loop
                    if Boards.Is_Valid (
                        State.Board,
                        Current_Actions (Index).all (Act))
                    then
                        Total_Actions := Total_Actions + 1;
                    end if;
                end loop;
            end loop;

            declare
                New_State : VL_State_Access := new VL_State (
                    Width => State.Width,
                    Height => State.Height,
                    Num_Players => State.Num_Players,
                    Num_Actions => Total_Actions);
                Next_Action : Positive := 1;
            begin
                for Index in Current_Actions'Range loop
                    for Act in Current_Actions (Index).all'Range loop
                        if Boards.Is_Valid (
                            State.Board,
                            Current_Actions (Index).all (Act))
                        then
                            New_State.Last_Actions (Next_Action) :=
                                Current_Actions (Index).all (Act);
                                Next_Action := Next_Action + 1;
                        end if;
                    end loop;
                end loop;

                New_State.Board := State.Board;
                New_State.Players := State.Players;
                Free_Uncommitted_Actions;
                Free_State (State);
                State := New_State;
                Boards.Apply_Actions (
                    State.Board,
                    State.Last_Actions);
            end;
        end Update_Turn;
    end VL_Game;

    task body VL_Manager is
        Current_Game : aliased VL_Game;
        Temp_Lobby : Lobby.Lobby_Element;
        Is_Server : Boolean;
        Is_Killed : Boolean := False;
    begin
        Life : loop
            select
                accept Host (Which : in Lobby.Lobby_Element) do
                    Current_Game.Create (Lobby.Read_Board (Which));
                    Temp_Lobby := Which;
                    Is_Server := True;
                end Host;
            or
                accept Join (Which : in Lobby.Lobby_Element) do
                    Temp_Lobby := Which;
                    Is_Server := False;
                end Join;
            or
                accept Kill do
                    Is_Killed := True;
                end Kill; -- SOMEHOW STUCK HERE AFTER HOST+BACK 2x THEN QUIT
            end select;

            if Is_Killed then
                exit Life;
            end if;

            if Is_Server then
                declare
                    The_Server : Servers.Server (
                        Temp_Lobby.Max_Players,
                        Current_Game'Access);
                    Quit_Game : Boolean := False;
                begin
                    The_Server.Initialize (Temp_Lobby);
                    while not Quit_Game loop
                        select
                            accept Commit (Which : in Actions.Action_Array) do
                                The_Server.Commit (Which);
                            end Commit;
                        or
                            accept Uncommit do
                                The_Server.Uncommit;
                            end Uncommit;
                        or
                            accept Set_Notifier (
                                Notifier : in VL_Notifier_Access) do
                                Current_Game.Attach_Notifier (Notifier);
                            end Set_Notifier;
                        or
                            accept Detach do
                                Quit_Game := True;
                            end Detach;
                        or
                            accept Kill do
                                Quit_Game := True;
                                Is_Killed := True;
                            end Kill;
                        end select;
                    end loop;
                    Current_Game.Destroy;
                    The_Server.Kill;
                end;
            else
                declare
                    The_Client : Clients.Client (Current_Game'Access);
                    Quit_Game : Boolean := False;
                begin
                    The_Client.Initialize (Temp_Lobby);
                    while not Quit_Game loop
                        select
                            accept Commit (Which : in Actions.Action_Array) do
                                The_Client.Commit (Which);
                            end Commit;
                        or
                            accept Uncommit do
                                The_Client.Uncommit;
                            end Uncommit;
                        or
                            accept Set_Notifier (
                                Notifier : in VL_Notifier_Access) do
                                Current_Game.Attach_Notifier (Notifier);
                            end Set_Notifier;
                        or
                            accept Detach do
                                Current_Game.Destroy;
                                The_Client.Kill;
                                Quit_Game := True;
                            end Detach;
                        or
                            accept Kill do
                                Current_Game.Destroy;
                                The_Client.Kill;
                                Quit_Game := True;
                                Is_Killed := True;
                            end Kill;
                        end select;
                    end loop;
                end;
            end if;
            if Is_Killed then
                exit Life;
            end if;
        end loop Life;
    end VL_Manager;

end VL;
