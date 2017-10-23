with Actions;
with Units;
with Tiles;
with SDL;

package Paths is
    Unit_Sprites : constant array (Units.Unit) of String (1 .. 12) := (
        Units.VAMPIRE => "../res/v.png",
        Units.LEPRECHAUN => "../res/l.png",
        Units.HUMAN => "../res/h.png",
        Units.ZOMBIE => "../res/z.png",
        Units.FAIRY => "../res/f.png");
    Glow_Sprite : constant String := "../res/glow.png";
    Base_Sprites : constant String := "../res/bases.png";
    function Unit_Sprite_Clips (Team : in Natural) return SDL.Rectangle;
    function Base_Sprite_Clips (Team : in Natural) return SDL.Rectangle;

    Tile_Sprites : constant String := "../res/tiles.png";
    function Tile_Sprite_Clips (Which : in Tiles.Tile_Kind)
        return SDL.Rectangle;

    Arrow_Sprites : constant String := "../res/arrows.png";
    function Arrow_Sprite_Clips (Which : in Actions.Action_Kind)
        return SDL.Rectangle;
    function Arrow_Sprite_Centers (Which : in Actions.Action_Kind)
        return SDL.Coordinate;

    Spawn_Sprites : constant String := "../res/spawn.png";
    function Spawn_Sprite_Clips (Which : in Units.Unit) return SDL.Rectangle;
end Paths;
