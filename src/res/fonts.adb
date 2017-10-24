with SDL;
with Paths;

package body Fonts is
    procedure Finalize is
    begin
        SDL.Free_Font (Main_Font);
    end Finalize;

    procedure Initialize is
    begin
        Main_Font := SDL.Load_Font (Paths.Font_TTF, 48);
    end Initialize;
end Fonts;
