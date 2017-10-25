with Actions;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics;
with Sprites;
with SDL;
with Tiles; use Tiles;
with Units; use Units;

package body Games is
    package Math is new Ada.Numerics.Generic_Elementary_Functions (Float);

    procedure Draw_Actions (
        Reference : in Boards.Board;
        Which : in Actions.Action_Array;
        Where : in SDL.Rectangle) is
        use SDL;
        use Actions;

        Delta_X : Natural := Where.Width / Reference.Width;
        Delta_Y : Natural := Where.Height / Reference.Height;
        Offset_X : Natural := Where.Left +
            (Where.Width mod Reference.Width) / 2;
        Offset_Y : Natural := Where.Top +
            (Where.Height mod Reference.Height) / 2;

        function Determine_Distance (
            Which : in Actions.Action) return Integer is
            DX : Integer := (Which.Target.X - Which.Source.X) * Delta_X;
            DY : Integer := (Which.Target.Y - Which.Source.Y) * Delta_Y;
        begin
            return Integer (Math.Sqrt (Float (DX * DX + DY * DY)));
        end Determine_Distance;

        function Determine_Rotation (
            Which : in Actions.Action) return SDL.Angle is
            Theta : SDL.Angle;
            DX, DY : Float;
        begin
            DX := Float (Which.Target.X - Which.Source.X) * Float (Delta_X);
            DY := Float (Which.Target.Y - Which.Source.Y) * Float (Delta_Y);
            if Which.Target.Y /= Which.Source.Y then
                Theta := SDL.Angle (Math.Arctan (
                    Y => DY / DX,
                    Cycle => 2.0 * Ada.Numerics.Pi));
            end if;
            if Which.Source.X < Which.Target.X then
                if Which.Source.Y < Which.Target.Y then
                    return Theta;
                elsif Which.Source.Y > Which.Target.Y then
                    return -Theta;
                else
                    return 0.0;
                end if;
            elsif Which.Source.X > Which.Target.X then
                if Which.Source.Y < Which.Target.Y then
                    return SDL.Angle'Last + Theta;
                elsif Which.Source.Y > Which.Target.Y then
                    return SDL.Angle'First - Theta;
                else
                    return SDL.Angle'Last;
                end if;
            else
                if Which.Source.Y < Which.Target.Y then
                    return SDL.Angle'Last / 2.0;
                elsif Which.Source.Y > Which.Target.Y then
                    return SDL.Angle'First / 2.0;
                else
                    return 0.0;
                end if;
            end if;
        end Determine_Rotation;

        procedure Render_Action (
            Which : in Actions.Action;
            Where : in SDL.Rectangle;
            Rotation : in SDL.Angle) is
        begin
            SDL.Draw_Image (
                Sprites.Arrow_Sprites,
                Where,
                Sprites.Arrow_Sprite_Clips (Which.Kind),
                Rotation,
                (0, Where.Height / 2));
        end Render_Action;
    begin
        for Index in Which'Range loop
            if Which (Index).Kind = Actions.SPAWN then
                SDL.Draw_Image (
                    Sprites.Spawn_Sprites,
                    (Left => Offset_X + Delta_X * (Which (Index).Source.X - 1),
                     Top => Offset_Y + Delta_Y * (Which (Index).Source.Y - 1),
                     Width => Delta_X, Height => Delta_Y),
                    Sprites.Spawn_Sprite_Clips (Which (Index).Unit));
            else
                Render_Action (
                    Which => Which (Index),
                    Where => (
                        Left => Which (Index).Source.X * Delta_X -
                            (Delta_X / 2) + Offset_X,
                        Top => Which (Index).Source.Y * Delta_Y +
                            Offset_Y - Delta_Y,
                        Width => Determine_Distance (Which (Index)),
                        Height => Delta_Y),
                    Rotation => Determine_Rotation (Which (Index)));
            end if;
        end loop;
    end Draw_Actions;

    procedure Draw_Board (
        Which : in Boards.Board;
        Where : in SDL.Rectangle) is
        Delta_X : Natural := Where.Width / Which.Width;
        Delta_Y : Natural := Where.Height / Which.Height;
        Offset_X : Natural := Where.Left + (Where.Width mod Which.Width) / 2;
        Offset_Y : Natural := Where.Top + (Where.Height mod Which.Height) / 2;
    begin
        for Y in Positive range 1 .. Which.Height loop
            for X in Positive range 1 .. Which.Width loop
                Draw_Tile (
                    Which => Boards.Get_Tile (
                        This => Which,
                        From => (X, Y)),
                    Where => (
                        Left => Offset_X + Delta_X * (X - 1),
                        Top => Offset_Y + Delta_Y * (Y - 1),
                        Width => Delta_X, Height => Delta_Y));
            end loop;
        end loop;
    end Draw_Board;

    procedure Draw_Tile (
        Which : in Tiles.Tile;
        Where : in SDL.Rectangle) is
    begin
        if Which.Kind = Tiles.BASE then
            SDL.Draw_Image (
                Sprites.Base_Sprites,
                Where,
                Sprites.Unit_Sprite_Clips (Which.Occupant.Team));
        else
            SDL.Draw_Image (
                Sprites.Tile_Sprites,
                Where,
                Sprites.Tile_Sprite_Clips (Which.Kind));
        end if;

        if Which.Occupant.Unit /= Units.NONE and
            Which.Occupant.Unit /= Units.UNKNOWN
        then
            SDL.Draw_Image (
                Sprites.Unit_Sprites (Which.Occupant.Unit),
                Where,
                Sprites.Unit_Sprite_Clips (Which.Occupant.Team));
        end if;
    end Draw_Tile;

    function Get_Mouse_Coord (
        Which : in Boards.Board;
        Where : in SDL.Rectangle) return Coordinates.Coordinate is
        Delta_X : Natural := Where.Width / Which.Width;
        Delta_Y : Natural := Where.Height / Which.Height;
        Offset_X : Natural := Where.Left + (Where.Width mod Which.Width) / 2;
        Offset_Y : Natural := Where.Top + (Where.Height mod Which.Height) / 2;
    begin
        if not SDL.Within (Where, SDL.State.Mouse.Where) then
            return (1, 1);
        end if;

        return ((SDL.State.Mouse.Where.X - Offset_X) / Delta_X + 1,
                (SDL.State.Mouse.Where.Y - Offset_Y) / Delta_Y + 1);
    end Get_Mouse_Coord;

    function Get_Rectangle (
        Which : in Boards.Board;
        Where : in SDL.Rectangle;
        Coord : in Coordinates.Coordinate) return SDL.Rectangle is
        Delta_X : Natural := Where.Width / Which.Width;
        Delta_Y : Natural := Where.Height / Which.Height;
        Offset_X : Natural := Where.Left + (Where.Width mod Which.Width) / 2;
        Offset_Y : Natural := Where.Top + (Where.Height mod Which.Height) / 2;
    begin
        return (
            Left => Offset_X + Delta_X * (Coord.X - 1),
            Top => Offset_Y + Delta_Y * (Coord.Y - 1),
            Width => Delta_X, Height => Delta_Y);
    end Get_Rectangle;
end Games;
