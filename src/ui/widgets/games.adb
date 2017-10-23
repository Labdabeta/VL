with Actions;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics;
with Paths;
with SDL;
with Tiles; use Tiles;
with Units; use Units;

package body Games is
    package Math is new Ada.Numerics.Generic_Elementary_Functions (Float);

    Sprite_Dimension : constant := 64;

    Unit_Sprite_Sheets : array (Units.Unit) of SDL.Image;

    Arrows_Sprite_Sheet : SDL.Image;
    Spawn_Sprite_Sheet : SDL.Image;
    Base_Sprite_Sheet : SDL.Image;
    Tile_Sprite_Sheet : SDL.Image;

    procedure Draw_Actions (
        Reference : in Boards.Board;
        Which : in Actions.Action_Array;
        Where : in SDL.Rectangle) is
        use SDL;
        use Actions;

        Delta_X : Natural := Where.Width / Reference.Width;
        Delta_Y : Natural := Where.Height / Reference.Height;
        Offset_X : Natural := (Where.Width mod Reference.Width) / 2;
        Offset_Y : Natural := (Where.Height mod Reference.Height) / 2;

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
                Arrows_Sprite_Sheet,
                Where,
                Paths.Arrow_Sprite_Clips (Which.Kind),
                Rotation,
                (0, Where.Height / 2));
        end Render_Action;
    begin
        for Index in Which'Range loop
            if Which (Index).Kind = Actions.SPAWN then
                SDL.Draw_Image (
                    Spawn_Sprite_Sheet,
                    (Left => Offset_X + Delta_X * (Which (Index).Source.X - 1),
                     Top => Offset_Y + Delta_Y * (Which (Index).Source.Y - 1),
                     Width => Delta_X, Height => Delta_Y),
                    Paths.Spawn_Sprite_Clips (Which (Index).Unit));
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
        Offset_X : Natural := (Where.Width mod Which.Width) / 2;
        Offset_Y : Natural := (Where.Height mod Which.Height) / 2;
        procedure Render_Tile (
            Which : in Tiles.Tile;
            Where : in SDL.Rectangle) is
        begin
            if Which.Kind = Tiles.BASE then
                SDL.Draw_Image (
                    Base_Sprite_Sheet,
                    Where,
                    Paths.Base_Sprite_Clips (Which.Occupant.Team));
            else
                SDL.Draw_Image (
                    Tile_Sprite_Sheet,
                    Where,
                    Paths.Tile_Sprite_Clips (Which.Kind));
            end if;

            if Which.Occupant.Unit /= Units.NONE and
                Which.Occupant.Unit /= Units.UNKNOWN
            then
                SDL.Draw_Image (
                    Unit_Sprite_Sheets (Which.Occupant.Unit),
                    Where,
                    Paths.Unit_Sprite_Clips (Which.Occupant.Team));
            end if;
        end Render_Tile;
    begin
        for Y in Positive range 1 .. Which.Height loop
            for X in Positive range 1 .. Which.Width loop
                Render_Tile (
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

    procedure Finalize is begin
        for U in Unit_Sprite_Sheets'Range loop
            SDL.Free_Image (Unit_Sprite_Sheets (U));
        end loop;

        SDL.Free_Image (Arrows_Sprite_Sheet);
        SDL.Free_Image (Spawn_Sprite_Sheet);
        SDL.Free_Image (Tile_Sprite_Sheet);
        SDL.Free_Image (Base_Sprite_Sheet);
    end Finalize;

    procedure Initialize is begin
        for U in Unit_Sprite_Sheets'Range loop
            Unit_Sprite_Sheets (U) := SDL.Load_Image (Paths.Unit_Sprites (U));
        end loop;

        Arrows_Sprite_Sheet := SDL.Load_Image (Paths.Arrow_Sprites);
        Spawn_Sprite_Sheet := SDL.Load_Image (Paths.Spawn_Sprites);
        Tile_Sprite_Sheet := SDL.Load_Image (Paths.Tile_Sprites);
        Base_Sprite_Sheet := SDL.Load_Image (Paths.Base_Sprites);
    end Initialize;
end Games;
