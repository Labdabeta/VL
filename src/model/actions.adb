with Units; use Units;
with Tiles; use Tiles;
with Coordinates; use Coordinates;
with Ada.Streams; use Ada.Streams;

package body Actions is

    function Get_Actions_From (
        What : in Tiles.Tile;
        Where : in Coordinates.Coordinate;
        Board_Size : in Coordinates.Coordinate)
        return Action_Array is
        Empty_Array : Actions.Action_Array (1 .. 0) := (
            others => ((1, 1), (1, 1), Actions.MOVE, Units.VAMPIRE, 1));
        Surroundings : Coordinates.Coordinate_List := Get_Adjacent_Within (
            Source => Where,
            Bounds => Board_Size);

        function Base_Results return Action_Array is
            Results : Action_Array (1 .. Num_Unit_Types * Surroundings'Length);
        begin
            for Index in Surroundings'Range loop
                for Unit_Type in Unit'Range loop
                    Results (Units.Unit'Pos (Unit_Type) +
                        1 + ((Index - 1) * Num_Unit_Types)) := (
                            Source => Surroundings (Index),
                            Target => Surroundings (Index),
                            Kind => SPAWN,
                            Unit => Unit_Type,
                            Team => What.Occupant.Team);
                end loop;
            end loop;
            return Results;
        end Base_Results;

        function Fairy_Results return Action_Array is
            Results : Action_Array (1 .. Surroundings'Length);
        begin
            for Index in Surroundings'Range loop
                Results (Index) := (
                    Source => Where,
                    Target => Surroundings (Index),
                    Kind => MOVE,
                    Unit => FAIRY,
                    Team => What.Occupant.Team);
            end loop;

            return Results;
        end Fairy_Results;

        function Human_Results return Action_Array is
            Results : Action_Array (1 .. 2 * Surroundings'Length);
        begin
            for Index in Surroundings'Range loop
                Results (Index) := (
                    Source => Where,
                    Target => Surroundings (Index),
                    Kind => MOVE,
                    Unit => HUMAN,
                    Team => What.Occupant.Team);
                Results (Index + Surroundings'Length) := (
                    Source => Where,
                    Target => Surroundings (Index),
                    Kind => DESTROY,
                    Unit => HUMAN,
                    Team => What.Occupant.Team);
            end loop;

            return Results;
        end Human_Results;

        function Leprechaun_Results return Action_Array is
            Results : Action_Array (1 .. 2 * Surroundings'Length);
        begin
            for Index in Surroundings'Range loop
                Results (Index) := (
                    Source => Where,
                    Target => Surroundings (Index),
                    Kind => MOVE,
                    Unit => LEPRECHAUN,
                    Team => What.Occupant.Team);
                Results (Index + Surroundings'Length) := (
                    Source => Where,
                    Target => Surroundings (Index),
                    Kind => CREATE,
                    Unit => LEPRECHAUN,
                    Team => What.Occupant.Team);
            end loop;

            return Results;
        end Leprechaun_Results;

        function Vampire_Results return Action_Array is
            Results : Action_Array (1 .. Surroundings'Length + 4);
            Length : Integer := Surroundings'Length;
        begin
            for Index in Surroundings'Range loop
                Results (Index) := (
                    Source => Where,
                    Target => Surroundings (Index),
                    Kind => MOVE,
                    Unit => VAMPIRE,
                    Team => What.Occupant.Team);
            end loop;

            if Where.X > 2 then
                Length := Length + 1;
                Results (Length) := (
                    Source => Where,
                    Target => (Where.X - 2, Where.Y),
                    Kind => MOVE,
                    Unit => VAMPIRE,
                    Team => What.Occupant.Team);
            end if;

            if Where.Y > 2 then
                Length := Length + 1;
                Results (Length) := (
                    Source => Where,
                    Target => (Where.X, Where.Y - 2),
                    Kind => MOVE,
                    Unit => VAMPIRE,
                    Team => What.Occupant.Team);
            end if;

            if Where.X < Board_Size.X - 2 then
                Length := Length + 1;
                Results (Length) := (
                    Source => Where,
                    Target => (Where.X + 2, Where.Y),
                    Kind => MOVE,
                    Unit => VAMPIRE,
                    Team => What.Occupant.Team);
            end if;

            if Where.Y < Board_Size.Y - 2 then
                Length := Length + 1;
                Results (Length) := (
                    Source => Where,
                    Target => (Where.X, Where.Y + 2),
                    Kind => MOVE,
                    Unit => VAMPIRE,
                    Team => What.Occupant.Team);
            end if;

            return Results (1 .. Length);
        end Vampire_Results;

        function Zombie_Results return Action_Array is
            Results : Action_Array (1 .. 2 * Surroundings'Length);
        begin
            for Index in Surroundings'Range loop
                Results (Index) := (
                    Source => Where,
                    Target => Surroundings (Index),
                    Kind => MOVE,
                    Unit => ZOMBIE,
                    Team => What.Occupant.Team);
                Results (Index + Surroundings'Length) := (
                    Source => Where,
                    Target => Surroundings (Index),
                    Kind => INFECT,
                    Unit => ZOMBIE,
                    Team => What.Occupant.Team);
            end loop;

            return Results;
        end Zombie_Results;
    begin
        if What.Kind = BASE then
            return Base_Results;
        end if;

        case What.Occupant.Unit is
            when VAMPIRE => return Vampire_Results;
            when LEPRECHAUN => return Leprechaun_Results;
            when HUMAN => return Human_Results;
            when ZOMBIE => return Zombie_Results;
            when FAIRY => return Fairy_Results;
            when others => return Empty_Array;
        end case;
    end Get_Actions_From;

    function Input (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class)
        return Action is
        Result : Action;
        Buffer : Stream_Element_Array (1 .. 1);
        Last : Stream_Element_Offset;
    begin
        Result.Source.X := Positive'Input (Stream);
        Result.Source.Y := Positive'Input (Stream);
        Result.Target.X := Positive'Input (Stream);
        Result.Target.Y := Positive'Input (Stream);

        Stream.Read (Buffer, Last);
        Result.Kind := Action_Kind'Val (Buffer (1) / 16);
        Result.Unit := Unit'Val (Buffer (1) mod 16);

        Stream.Read (Buffer, Last);
        Result.Team := Natural (Buffer (1));

        return Result;
    end Input;

    function Is_Valid (
        This : in Action)
        return Boolean is

        Distance : Natural := Coordinates.Distance (
            Source => This.Source,
            Destination => This.Target);

    begin

        case This.Kind is
            when MOVE =>
                if Distance = 0 then
                    return False;
                elsif Distance > 1 then
                    if This.Unit /= VAMPIRE then
                        return False;
                    else
                        if (This.Source.X = This.Target.X or
                            This.Source.Y = This.Target.Y) and
                            Distance = 2
                        then
                            return True;
                        else
                            return False;
                        end if;
                    end if;
                else
                    return True;
                end if;
            when CREATE =>
                if This.Unit /= LEPRECHAUN or Distance /= 1 then
                    return False;
                else
                    return True;
                end if;
            when DESTROY =>
                if This.Unit /= HUMAN or Distance /= 1 then
                    return False;
                else
                    return True;
                end if;
            when INFECT =>
                if This.Unit /= ZOMBIE or Distance /= 1 then
                    return False;
                else
                    return True;
                end if;
            when SPAWN =>
                if Distance > 0 then
                    return False;
                else
                    return True;
                end if;
        end case;
    end Is_Valid;

    procedure Output (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Action) is
        Buffer : Stream_Element_Array (1 .. 1);
    begin
        Positive'Output (Stream, Item.Source.X);
        Positive'Output (Stream, Item.Source.Y);
        Positive'Output (Stream, Item.Target.X);
        Positive'Output (Stream, Item.Target.Y);

        Buffer (1) := Stream_Element (
            Action_Kind'Pos (Item.Kind) * 16 + Unit'Pos (Item.Unit));
        Stream.Write (Buffer);

        Buffer (1) := Stream_Element (Item.Team);
        Stream.Write (Buffer);
    end Output;

    procedure Read (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : out Action) is
    begin
        --  Actions don't have descriminants
        Item := Action'Input (Stream);
    end Read;

    function To_String (
        This : in Action)
        return String is
    begin
        if This.Kind = MOVE then
            return Positive'Image (This.Source.X) & "," &
                Positive'Image (This.Source.Y) & " -> " &
                Positive'Image (This.Target.X) & "," &
                Positive'Image (This.Target.Y) & " (" &
                To_String (This.Unit) & Positive'Image (This.Team) & ")";
        else
            return Positive'Image (This.Source.X) & "," &
                Positive'Image (This.Source.Y) & " ~> " &
                Positive'Image (This.Target.X) & "," &
                Positive'Image (This.Target.Y) & " (" &
                To_String (This.Unit) & Positive'Image (This.Team) & ")";
        end if;
    end To_String;

    procedure Write (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Action) is
    begin
        --  Actions don't have descriminants
        Action'Output (Stream, Item);
    end Write;
end Actions;
