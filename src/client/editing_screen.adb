with Buttons;
with SDL;
with Screens;
with Sprites;
with Strings;
with Fonts;
with Maps;
with Boards;
with Tiles;
with Units;
with Games;
with Coordinates;

package body Editing_Screen is
    Save, Done, Vampire, Leprechaun, Human, Zombie, Fairy, None, Base, Pit,
    Floor, Producer, Impassable, Add_Team, Sub_Team : Buttons.Button;
    Board_Area, Sample_Area : SDL.Rectangle;
    Current : Maps.Map;
    Tile : Tiles.Tile;

    procedure Update_Layout;

    procedure Draw is
    begin
        Update_Layout;
        Buttons.Draw (Save);
        Buttons.Draw (Done);
        Buttons.Draw (Add_Team);
        Buttons.Draw (Sub_Team);
        Buttons.Draw (None);
        Buttons.Draw_Area (
            Vampire, Sprites.Unit_Sprite_Clips (Tile.Occupant.Team), 10);
        Buttons.Draw_Area (
            Leprechaun, Sprites.Unit_Sprite_Clips (Tile.Occupant.Team), 10);
        Buttons.Draw_Area (
            Human, Sprites.Unit_Sprite_Clips (Tile.Occupant.Team), 10);
        Buttons.Draw_Area (
            Zombie, Sprites.Unit_Sprite_Clips (Tile.Occupant.Team), 10);
        Buttons.Draw_Area (
            Fairy, Sprites.Unit_Sprite_Clips (Tile.Occupant.Team), 10);
        Buttons.Draw_Area (
            Base, Sprites.Unit_Sprite_Clips (Tile.Occupant.Team), 10);
        Buttons.Draw_Area (
            Pit, Sprites.Tile_Sprite_Clips (Tiles.PIT), 10);
        Buttons.Draw_Area (
            Floor, Sprites.Tile_Sprite_Clips (Tiles.FLOOR), 10);
        Buttons.Draw_Area (
            Producer, Sprites.Tile_Sprite_Clips (Tiles.PRODUCER), 10);
        Buttons.Draw_Area (
            Impassable, Sprites.Tile_Sprite_Clips (Tiles.IMPASSABLE), 10);
        Games.Draw_Board (Current.Contents.all, Board_Area);
        Games.Draw_Tile (Tile, Sample_Area);
    end Draw;

    procedure Finalize is
    begin
        Buttons.Free_Overlay (Save);
        Buttons.Free_Overlay (Done);
        Buttons.Free_Overlay (Add_Team);
        Buttons.Free_Overlay (Sub_Team);
    end Finalize;

    procedure Initialize is
    begin
        Save := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Save, True));
        Done := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, Strings.Done, True));
        Vampire := Buttons.Create (Sprites.Unit_Sprites (Units.VAMPIRE));
        Leprechaun := Buttons.Create (Sprites.Unit_Sprites (Units.LEPRECHAUN));
        Human := Buttons.Create (Sprites.Unit_Sprites (Units.HUMAN));
        Zombie := Buttons.Create (Sprites.Unit_Sprites (Units.ZOMBIE));
        Fairy := Buttons.Create (Sprites.Unit_Sprites (Units.FAIRY));
        None := Buttons.Create (SDL.Null_Image);
        Base := Buttons.Create (Sprites.Base_Sprites);
        Pit := Buttons.Create (Sprites.Tile_Sprites);
        Floor := Buttons.Create (Sprites.Tile_Sprites);
        Producer := Buttons.Create (Sprites.Tile_Sprites);
        Impassable := Buttons.Create (Sprites.Tile_Sprites);
        Add_Team := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, "+", True));
        Sub_Team := Buttons.Create (
            SDL.Render_Text (Fonts.Main_Font, "-", True));
        Tile := ((Units.NONE, 1), Tiles.FLOOR);
    end Initialize;

    function Process_Event (What : in SDL.Event) return Screens.Transition is
    begin
        Update_Layout;
        if Buttons.Process_Event (Save, What) then
            Maps.Save (Current);
        elsif Buttons.Process_Event (Done, What) then
            return (To => Screens.MAIN_MENU);
        elsif Buttons.Process_Event (Vampire, What) then
            Tile.Occupant.Unit := Units.VAMPIRE;
        elsif Buttons.Process_Event (Leprechaun, What) then
            Tile.Occupant.Unit := Units.LEPRECHAUN;
        elsif Buttons.Process_Event (Human, What) then
            Tile.Occupant.Unit := Units.HUMAN;
        elsif Buttons.Process_Event (Zombie, What) then
            Tile.Occupant.Unit := Units.ZOMBIE;
        elsif Buttons.Process_Event (Fairy, What) then
            Tile.Occupant.Unit := Units.FAIRY;
        elsif Buttons.Process_Event (None, What) then
            Tile.Occupant.Unit := Units.NONE;
        elsif Buttons.Process_Event (Base, What) then
            Tile.Kind := Tiles.BASE;
        elsif Buttons.Process_Event (Pit, What) then
            Tile.Kind := Tiles.PIT;
        elsif Buttons.Process_Event (Floor, What) then
            Tile.Kind := Tiles.FLOOR;
        elsif Buttons.Process_Event (Producer, What) then
            Tile.Kind := Tiles.PRODUCER;
        elsif Buttons.Process_Event (Impassable, What) then
            Tile.Kind := Tiles.IMPASSABLE;
        elsif Buttons.Process_Event (Add_Team, What) then
            Tile.Occupant.Team := Tile.Occupant.Team + 1;
        elsif Buttons.Process_Event (Sub_Team, What) then
            if Tile.Occupant.Team > 1 then
                Tile.Occupant.Team := Tile.Occupant.Team - 1;
            end if;
        else
            if SDL.State.Mouse.Buttons (SDL.LEFT) and
                SDL.Within (Board_Area, SDL.State.Mouse.Where) and
                Games.Get_Mouse_Coord (Current.Contents.all, Board_Area).X <=
                Current.Contents.all.Width and
                Games.Get_Mouse_Coord (Current.Contents.all, Board_Area).Y <=
                Current.Contents.all.Height
            then
                Boards.Set_Tile (
                    Current.Contents.all,
                    Games.Get_Mouse_Coord (
                        Current.Contents.all,
                        Board_Area),
                    Tile);
            end if;
        end if;
        return (To => Screens.NONE);
    end Process_Event;

    procedure Update (Document : Maps.Map) is
    begin
        Current := Document;
        Tile := ((Units.NONE, 1), Tiles.FLOOR);
    end Update;

    procedure Update_Layout is
        W32 : Integer := SDL.State.Window.Width / 32;
        H24 : Integer := SDL.State.Window.Height / 24;
    begin
        Buttons.Set_Area (Save, (
            Left => W32 * 24,
            Top => H24 * 15,
            Width => W32 * 8,
            Height => H24 * 4));
        Buttons.Set_Area (Done, (
            Left => W32 * 24,
            Top => H24 * 20,
            Width => W32 * 8,
            Height => H24 * 4));
        Buttons.Set_Area (Vampire, (
            Left => W32 * 25,
            Top => H24,
            Width => W32 * 2,
            Height => H24 * 2));
        Buttons.Set_Area (Leprechaun, (
            Left => W32 * 29,
            Top => H24,
            Width => W32 * 2,
            Height => H24 * 2));
        Buttons.Set_Area (Human, (
            Left => W32 * 27,
            Top => H24,
            Width => W32 * 2,
            Height => H24 * 2));
        Buttons.Set_Area (Zombie, (
            Left => W32 * 25,
            Top => H24 * 3,
            Width => W32 * 2,
            Height => H24 * 2));
        Buttons.Set_Area (Fairy, (
            Left => W32 * 29,
            Top => H24 * 3,
            Width => W32 * 2,
            Height => H24 * 2));
        Buttons.Set_Area (None, (
            Left => W32 * 27,
            Top => H24 * 3,
            Width => W32 * 2,
            Height => H24 * 2));
        Buttons.Set_Area (Base, (
            Left => W32 * 25,
            Top => H24 * 10,
            Width => W32 * 2,
            Height => H24 * 2));
        Buttons.Set_Area (Pit, (
            Left => W32 * 29,
            Top => H24 * 10,
            Width => W32 * 2,
            Height => H24 * 2));
        Buttons.Set_Area (Floor, (
            Left => W32 * 27,
            Top => H24 * 12,
            Width => W32 * 2,
            Height => H24 * 2));
        Buttons.Set_Area (Producer, (
            Left => W32 * 25,
            Top => H24 * 12,
            Width => W32 * 2,
            Height => H24 * 2));
        Buttons.Set_Area (Impassable, (
            Left => W32 * 29,
            Top => H24 * 12,
            Width => W32 * 2,
            Height => H24 * 2));
        Buttons.Set_Area (Add_Team, (
            Left => W32 * 28,
            Top => H24 * 6,
            Width => W32 * 3,
            Height => H24 * 3));
        Buttons.Set_Area (Sub_Team, (
            Left => W32 * 25,
            Top => H24 * 6,
            Width => W32 * 3,
            Height => H24 * 3));
        Board_Area := (
            Left => 0,
            Top => 0,
            Width => W32 * 24,
            Height => H24 * 24);
        Sample_Area := (
            Left => W32 * 27,
            Top => H24 * 10,
            Width => W32 * 2,
            Height => H24 * 2);
    end Update_Layout;

end Editing_Screen;
