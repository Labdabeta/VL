with SDL;

package body Paths is
    function Arrow_Sprite_Centers (Which : in Actions.Action_Kind)
    return SDL.Coordinate is begin
        return (Actions.Action_Kind'Pos (Which) * 64 + 32, 32);
    end Arrow_Sprite_Centers;

    function Arrow_Sprite_Clips (Which : in Actions.Action_Kind)
    return SDL.Rectangle is begin
        return (Actions.Action_Kind'Pos (Which) * 64, 0, 64, 64);
    end Arrow_Sprite_Clips;

    function Base_Sprite_Clips (Team : in Natural) return SDL.Rectangle is begin
        return ((Team mod 4) * 64, (Team / 4) * 64, 64, 64);
    end Base_Sprite_Clips;

    function Spawn_Sprite_Clips (Which : in Units.Unit) return SDL.Rectangle
    is begin
        return (Units.Unit'Pos (Which) * 64, 0, 64, 64);
    end Spawn_Sprite_Clips;

    function Tile_Sprite_Clips (Which : in Tiles.Tile_Kind) return SDL.Rectangle
    is begin
        return ((Tiles.Tile_Kind'Pos (Which) - 1) * 64, 0, 64, 64);
    end Tile_Sprite_Clips;

    function Unit_Sprite_Clips (Team : in Natural) return SDL.Rectangle is begin
        return ((Team mod 4) * 64, (Team / 4) * 64, 64, 64);
    end Unit_Sprite_Clips;
end Paths;
