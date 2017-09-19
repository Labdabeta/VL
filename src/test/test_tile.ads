with Test_Suites;

package Test_Tile is
    Suite : aliased constant Test_Suites.Test_Suite;

    procedure Test_Basic;
    Test_Basic_Name : aliased String := "Basic Test";

    procedure Test_Singles;
    Test_Singles_Name : aliased String := "Singles Test";

    procedure Test_Doubles;
    Test_Doubles_Name : aliased String := "Doubles Test";

    procedure Test_Triples;
    Test_Triples_Name : aliased String := "Triples Test";

    procedure Test_Quads;
    Test_Quads_Name : aliased String := "Quad Test";

    procedure Test_All;
    Test_All_Name : aliased String := "All Units Test";

    procedure Test_Create;
    Test_Create_Name : aliased String := "Creation Test";

    procedure Test_Unusual_Tiles;
    Test_Unusual_Tiles_Name : aliased String := "Unusual Tiles Test";

    procedure Test_Stress;
    Test_Stress_Name : aliased String := "Stress Test";

private
    Suite : aliased constant Test_Suites.Test_Suite := (
        1 => (
            Callback => Test_Basic'Access,
            Name => Test_Basic_Name'Access),
        2 => (
            Callback => Test_Singles'Access,
            Name => Test_Singles_Name'Access),
        3 => (
            Callback => Test_Doubles'Access,
            Name => Test_Doubles_Name'Access),
        4 => (
            Callback => Test_Triples'Access,
            Name => Test_Triples_Name'Access),
        5 => (
            Callback => Test_Quads'Access,
            Name => Test_Quads_Name'Access),
        6 => (
            Callback => Test_All'Access,
            Name => Test_All_Name'Access),
        7 => (
            Callback => Test_Create'Access,
            Name => Test_Create_Name'Access),
        8 => (
            Callback => Test_Unusual_Tiles'Access,
            Name => Test_Unusual_Tiles_Name'Access),
        9 => (
            Callback => Test_Stress'Access,
            Name => Test_Stress_Name'Access));
end Test_Tile;
