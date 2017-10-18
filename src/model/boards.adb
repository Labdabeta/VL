with Actions; use Actions;
with Tiles; use Tiles;
with Units; use Units;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded;

package body Boards is
    procedure Apply_Actions (
        This : in out Board;
        Action_List : in Actions.Action_Array) is
        Presences : array (1 .. This.Width, 1 .. This.Height) of Presence_Array
            := (others => (others => (others => (NONE, 0))));
        Creation : array (1 .. This.Width, 1 .. This.Height) of Boolean := (
            others => (others => False));
        Has_Acted : array (1 .. This.Width, 1 .. This.Height) of Boolean := (
            others => (others => False));
        Spawns : array (1 .. This.Width, 1 .. This.Height) of Tile_Occupant := (
            others => (others => (NONE, 0)));

        procedure Post_Process_Action (
            Which : in Action) is
            SX : Positive := Which.Source.X;
            SY : Positive := Which.Source.Y;
            TX : Positive := Which.Target.X;
            TY : Positive := Which.Target.Y;
            Who : Tile_Occupant := (Which.Unit, Which.Team);
        begin
            if (Which.Kind = INFECT or Which.Kind = SPAWN) and
                not Has_Acted (SX, SY)
            then
                Has_Acted (SX, SY) := True;
                if This.Contents (TX, TY).Occupant.Unit = NONE and
                    This.Contents (TX, TY).Kind = FLOOR
                then
                    if Spawns (TX, TY) = (NONE, 0) then
                        Spawns (TX, TY) := Who;
                    else
                        Spawns (TX, TY) := (NONE, 1);
                    end if;
                end if;
            end if;
        end Post_Process_Action;

        procedure Process_Action (
            Which : in Action) is
            SX : Positive := Which.Source.X;
            SY : Positive := Which.Source.Y;
            TX : Positive := Which.Target.X;
            TY : Positive := Which.Target.Y;
            Who : Tile_Occupant := (Which.Unit, Which.Team);

            procedure Push_Presence (
                List : in out Presence_Array;
                Item : in Tile_Occupant) is
                Index : Positive := 1;
            begin
                while List (Index) /= (NONE, 0) loop
                    Index := Index + 1;
                end loop;

                List (Index) := Item;
            end Push_Presence;
        begin
            if not Has_Acted (SX, SY) then
                Has_Acted (SX, SY) := True;
                case Which.Kind is
                    when MOVE =>
                        This.Contents (SX, SY).Occupant := (NONE, 0);
                        Push_Presence (Presences (TX, TY), Who);
                    when CREATE =>
                        Creation (TX, TY) := True;
                    when DESTROY =>
                        if This.Contents (TX, TY).Kind = FLOOR then
                            This.Contents (TX, TY).Kind := PIT;
                        end if;
                    when others =>
                        Has_Acted (SX, SY) := False;
                end case;
            end if;
        end Process_Action;
    begin
        for Index in Action_List'Range loop
            Process_Action (Action_List (Index));
        end loop;

        for X in This.Contents'Range (1) loop
            for Y in This.Contents'Range (2) loop
                Resolve (
                    This => This.Contents (X, Y),
                    Presence => Presences (X, Y),
                    Create => Creation (X, Y));
            end loop;
        end loop;

        for Index in Action_List'Range loop
            Post_Process_Action (Action_List (Index));
        end loop;

        for X in This.Contents'Range (1) loop
            for Y in This.Contents'Range (2) loop
                if Spawns (X, Y).Unit /= NONE and
                    This.Contents (X, Y).Kind = FLOOR
                then
                    This.Contents (X, Y).Occupant := Spawns (X, Y);
                end if;
            end loop;
        end loop;
    end Apply_Actions;

    function Create_Board (
        Width, Height : in Positive)
        return Board is
        New_Board : Board := (
            Width => Width,
            Height => Height,
            Contents => (others => (others => (
                (Units.UNKNOWN, 0), Tiles.UNKNOWN))));
    begin
        return New_Board;
    end Create_Board;

    function Get_Actions_From (
        This : in Board;
        From : in Coordinates.Coordinate)
        return Actions.Action_Array is
        Candidates : Actions.Action_Array := Actions.Get_Actions_From (
            What => This.Contents (From.X, From.Y),
            Where => From,
            Board_Size => (This.Width, This.Height));
        Results : Actions.Action_Array (1 .. Candidates'Length) := (
            others => ((1, 1), (1, 1), Actions.MOVE, Units.VAMPIRE, 1));
        Result_Count : Integer := 0;
    begin
        for Index in Candidates'Range loop
            if Is_Valid (This, Candidates (Index)) then
                Result_Count := Result_Count + 1;
                Results (Result_Count) := Candidates (Index);
            end if;
        end loop;

        return Results (1 .. Result_Count);
    end Get_Actions_From;

    function Get_Players (
        This : in Board)
        return Player_List is
        package Natural_Set is new Ada.Containers.Ordered_Sets (Natural);
        use Natural_Set;
        Players : Set := Empty_Set;

        function Listify (The_Set : in out Set) return Player_List is
            New_Player : Natural;
            Empty_Array : Player_List (1 .. 0) := (others => 0);
        begin
            if The_Set.Is_Empty then
                return Empty_Array;
            end if;

            New_Player := First_Element (The_Set);
            Delete_First (The_Set);

            if New_Player > 0 then
                return New_Player & Listify (The_Set);
            else
                return Listify (The_Set);
            end if;
        end Listify;
    begin
        for X in This.Contents'Range (1) loop
            for Y in This.Contents'Range (2) loop
                if This.Contents (X, Y).Kind = BASE then
                    Include (Players, This.Contents (X, Y).Occupant.Team);
                end if;
            end loop;
        end loop;

        return Listify (Players);
    end Get_Players;

    function Get_Tile (
        This : in Board;
        From : in Coordinates.Coordinate) return Tiles.Tile is
    begin
        return This.Contents (From.X, From.Y);
    end Get_Tile;

    function Get_Winner (
        This : in Board)
        return Natural is
        Players : Player_List := Get_Players (This);
    begin
        if Players'Length = 1 then
            return Players (1);
        end if;

        return 0;
    end Get_Winner;

    function Input (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class)
        return Board is
        Width, Height : Positive;
    begin
        Positive'Read (Stream, Width);
        Positive'Read (Stream, Height);

        declare
            Result : Board (Width, Height);
        begin
            for Y in Result.Contents'Range (2) loop
                for X in Result.Contents'Range (1) loop
                    Tile'Read (Stream, Result.Contents (X, Y));
                end loop;
            end loop;

            return Result;
        end;
    end Input;

    function Is_Valid (
        This : in Board;
        What : in Actions.Action)
        return Boolean is
    begin
        if What.Source.X > This.Width or What.Source.Y > This.Height then
            return False;
        end if;
        declare
            Source_Tile : Tile := This.Contents (What.Source.X, What.Source.Y);
        begin
            if What.Kind = SPAWN then
                --  Search surroundings of Source for any friendly base
                declare
                    Locals : Coordinates.Coordinate_List :=
                        Coordinates.Get_Adjacent_Within (
                            Source => What.Source,
                            Bounds => (This.Width, This.Height));
                    Query : Tile;
                    Query_Coord : Coordinates.Coordinate;
                begin
                    for Index in Locals'Range loop
                        Query_Coord := (Locals (Index).X, Locals (Index).Y);
                        Query := This.Contents (Query_Coord.X, Query_Coord.Y);
                        if Query.Kind = BASE and Query.Occupant.Team = What.Team
                        then
                            return True;
                        end if;
                    end loop;
                    return False;
                end;
            end if;
            if not Is_Valid (What) or
                not (Source_Tile.Occupant.Unit = What.Unit) or
                not (Source_Tile.Occupant.Team = What.Team) or
                What.Target.X > This.Width or
                What.Target.Y > This.Height or
                (What.Kind = MOVE and Source_Tile.Kind = PRODUCER)
            then
                return False;
            end if;
        end;

        return True;
    end Is_Valid;

    function Is_Visible (
        This : in Board;
        Team : in Positive;
        Where : in Coordinates.Coordinate)
        return Boolean is
        X, Y : Integer;
        X_Coord : Positive := Where.X;
        Y_Coord : Positive := Where.Y;
    begin
        for Delta_Y in Integer range -Max_Vision .. Max_Vision loop
            for Delta_X in Integer range -Max_Vision .. Max_Vision loop
                X := X_Coord + Delta_X;
                Y := Y_Coord + Delta_Y;
                if X > 0 and Y > 0 and X <= This.Width and Y <= This.Height then
                    if This.Contents (X, Y).Occupant.Team = Team and
                        Coordinates.Distance ((X, Y), Where) <
                        Vision (This.Contents (X, Y).Occupant.Unit)
                    then
                        return True;
                    end if;
                end if;
            end loop;
        end loop;
        return False;
    end Is_Visible;

    function Localize (
        This : in Board;
        Team : in Positive)
        return Board is
        Result : Board := This;

    begin
        for Y in This.Contents'Range (2) loop
            for X in This.Contents'Range (1) loop
                if not Is_Visible (This, Team, (X, Y)) then
                    Result.Contents (X, Y) := ((UNKNOWN, 0), UNKNOWN);
                end if;
            end loop;
        end loop;

        return Result;
    end Localize;

    function Localize_Actions (
        This : in Board;
        Team : in Positive;
        Action_List : in Action_Array)
        return Action_Array is
        Results : Action_Array (1 .. Action_List'Length) := (
            others => ((1, 1), (1, 1), Actions.MOVE, Units.VAMPIRE, 1));
        Result_Size : Integer := 0;
    begin
        for Index in Action_List'Range loop
            if Is_Visible (This, Team, Action_List (Index).Source) or
                Is_Visible (This, Team, Action_List (Index).Target)
            then
                Result_Size := Result_Size + 1;
                Results (Result_Size) := Action_List (Index);
            end if;
        end loop;

        return Results (1 .. Result_Size);
    end Localize_Actions;

    function Num_Actions (
        This : in Board;
        Team : in Positive)
        return Natural is
        Result : Natural := 0;
    begin
        for Y in This.Contents'Range (2) loop
            for X in This.Contents'Range (1) loop
                if This.Contents (X, Y).Occupant.Team = Team and
                    (This.Contents (X, Y).Kind = BASE or
                     This.Contents (X, Y).Kind = PRODUCER)
                then
                    Result := Result + 1;
                end if;
            end loop;
        end loop;

        return Result;
    end Num_Actions;

    procedure Output (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Board) is

    begin
        Positive'Write (Stream, Item.Width);
        Positive'Write (Stream, Item.Height);

        for Y in Item.Contents'Range (2) loop
            for X in Item.Contents'Range (1) loop
                Tile'Write (Stream, Item.Contents (X, Y));
            end loop;
        end loop;
    end Output;

    procedure Read (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : out Board) is
    begin
        for Y in Item.Contents'Range (2) loop
            for X in Item.Contents'Range (1) loop
                Tile'Read (Stream, Item.Contents (X, Y));
            end loop;
        end loop;
    end Read;

    procedure Set_Tile (
        This : in out Board;
        From : in Coordinates.Coordinate;
        To : in Tiles.Tile) is
    begin
        This.Contents (From.X, From.Y) := To;
    end Set_Tile;

    function To_String (
        This : in Board)
        return String is
        Buffer : Ada.Strings.Unbounded.Unbounded_String :=
            Ada.Strings.Unbounded.Null_Unbounded_String;
    begin
        for Y in This.Contents'Range (2) loop
            for X in This.Contents'Range (1) loop
                Ada.Strings.Unbounded.Append (
                    Buffer, To_String (This.Contents (X, Y)));
            end loop;
            Ada.Strings.Unbounded.Append (
                Buffer, ASCII.LF);
        end loop;

        return Ada.Strings.Unbounded.To_String (Buffer);
    end To_String;

    procedure Write (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Board) is
    begin
        for Y in Item.Contents'Range (2) loop
            for X in Item.Contents'Range (1) loop
                Tile'Write (Stream, Item.Contents (X, Y));
            end loop;
        end loop;
    end Write;

end Boards;
