with SDL;
with Paths;

package body Sprites is
    function Arrow_Sprite_Centers (Which : in Actions.Action_Kind)
    return SDL.Coordinate is begin
        return (Actions.Action_Kind'Pos (Which) * 64 + 32, 32);
    end Arrow_Sprite_Centers;

    function Arrow_Sprite_Clips (Which : in Actions.Action_Kind)
    return SDL.Rectangle is begin
        return (Actions.Action_Kind'Pos (Which) * 64, 0, 64, 64);
    end Arrow_Sprite_Clips;

    procedure Finalize is begin
        for U in Unit_Sprites'Range loop
            SDL.Free_Image (Unit_Sprites (U));
        end loop;
        SDL.Free_Image (Base_Sprites);
        SDL.Free_Image (Tile_Sprites);
        SDL.Free_Image (Arrow_Sprites);
        SDL.Free_Image (Spawn_Sprites);
        SDL.Free_Image (Button_Sprites);
        SDL.Free_Image (Text_Box_Sprites);
        SDL.Free_Image (Background_Sprite);
    end Finalize;

    procedure Initialize is begin
        for U in Unit_Sprites'Range loop
            Unit_Sprites (U) := SDL.Load_Image (Paths.Unit_Sprites (U));
        end loop;
        Base_Sprites := SDL.Load_Image (Paths.Base_Sprites);
        Tile_Sprites := SDL.Load_Image (Paths.Tile_Sprites);
        Arrow_Sprites := SDL.Load_Image (Paths.Arrow_Sprites);
        Spawn_Sprites := SDL.Load_Image (Paths.Spawn_Sprites);
        Button_Sprites := SDL.Load_Image (Paths.Button_Sprites);
        Text_Box_Sprites := SDL.Load_Image (Paths.Text_Box_Sprites);
        Background_Sprite := SDL.Load_Image (Paths.Background_Sprite);

        Button_Normal_Clip := (0, 0, 256, 128);
        Button_Hover_Clip := (0, 128, 256, 128);
        Button_Pressed_Clip := (0, 256, 256, 128);
        Text_Box_Enabled_Clip := (0, 0, 512, 64);
        Text_Box_Disabled_Clip := (0, 64, 512, 64);
    end Initialize;

    function Spawn_Sprite_Clips (Which : in Units.Unit) return SDL.Rectangle
    is begin
        return (Units.Unit'Pos (Which) * 64, 0, 64, 64);
    end Spawn_Sprite_Clips;

    function Tile_Sprite_Clips (Which : in Tiles.Tile_Kind) return SDL.Rectangle
    is begin
        return ((Tiles.Tile_Kind'Pos (Which) - 1) * 64, 0, 64, 64);
    end Tile_Sprite_Clips;

    function Unit_Sprite_Clips (Team : in Natural) return SDL.Rectangle is begin
        return (((Team - 1) mod 4) * 64, ((Team - 1) / 4) * 64, 64, 64);
    end Unit_Sprite_Clips;
end Sprites;
