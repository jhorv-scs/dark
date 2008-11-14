#light

namespace dark_tests 
    open FsUnit
    open dark
    
    module tile_tests =
        let tests ()= 
            let tile = {x = 1; y = 1; Terrain = Floor; Flags = Flags.None}
            
            [
                spec "get_x should be 1" (Tile.get_x tile |> should equal 1)
                spec "get_y should be 1" (Tile.get_y tile |> should equal 1)               
                spec "is_seeable for floor should be true" (Tile.is_seeable tile |> should be True)
                spec "is_walkable for floor should be true" (Tile.is_walkable tile |> should be True)
                spec "is_seeable for wall should be false" (Tile.is_seeable {tile with Terrain = Wall} |> should be False)
                spec "is_walkable for wall should be false" (Tile.is_walkable {tile with Terrain = Wall} |> should be False)
                spec "is_seeable for rock wall should be false" (Tile.is_seeable {tile with Terrain = RockWall} |> should be False)
                spec "is_walkable for rock wall should be false" (Tile.is_walkable {tile with Terrain = RockWall} |> should be False)
                spec "is_seeable for door should be false" (Tile.is_seeable {tile with Terrain = Door} |> should be False)
                spec "is_walkable for door should be false" (Tile.is_walkable {tile with Terrain = Door} |> should be False)
                spec "is_seeable for secret door should be false" (Tile.is_seeable {tile with Terrain = SecretDoor} |> should be False)
                spec "is_walkable for secret door should be false" (Tile.is_walkable {tile with Terrain = SecretDoor} |> should be False)
                spec "get_display_char for floor should be '.'" ( Tile.get_display_char tile |> should equal '.')                                
                spec "get_display_char for rock wall should be ' '" (Tile.get_display_char {tile with Terrain = RockWall} |> should equal ' ')
                spec "get_display_char for wall should be '#'" (Tile.get_display_char {tile with Terrain = Wall} |> should equal '#')
                spec "get_display_char for door should be '+'" (Tile.get_display_char {tile with Terrain = Door} |> should equal '+')
                spec "get_display_char for secret door should be ' '" (Tile.get_display_char {tile with Terrain = SecretDoor} |> should equal ' ')
            ]
