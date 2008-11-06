#light

namespace dark
    open System
    //open System.Drawing
    open libtcodWrapper
    open dark
    
    type MapObject = {
        Tiles : Tile list;
        Height : int;
        Width : int;}

    type Map =
        | Empty
        | Map of MapObject
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Map =            
        type private Room = {
            x : int;
            y : int;
            Height : int;
            Width : int;}
        
        type private MapBuild = {
            Width : int;
            Height : int;
            Tiles : Tile list;
            Rooms : Rectangle list;
            Corridors : Rectangle list;}
                        
        type private Direction = 
            | Up
            | Down
            | Left
            | Right
            
        let private r = new Random()                
        
        let private get_tile_helper x y tiles = 
            List.first (fun t -> if (Tile.get_x t) = x && (Tile.get_y t) = y then Some(t) else None) tiles                
            
        let private map_test emptyValueF mapValueF map =
            match map with
            | Empty -> emptyValueF()
            | Map(x) -> (mapValueF x)     
            
        let private convert_tile test_f convert_f tile = 
            match test_f tile with
            | true -> convert_f tile
            | false -> tile
            
        let private tile_in_room (room:Rectangle) tile = 
            Rectangle.contains room (Tile.get_x tile) (Tile.get_y tile)
            
(*
        let private draw_map (console:RootConsole) tiles =
            List.iter (fun t -> console.PutChar(Tile.get_x t,Tile.get_y t,Tile.get_display_char t, new Background(BackgroundFlag.Set))) tiles                         
        
        let private get_direction () =
            match r.Next(0,3) with
            | 0 -> Direction.Up
            | 1 -> Direction.Right
            | 2 -> Direction.Down
            | 3 -> Direction.Left
            | _ -> failwith "Not valid direction"
        
        let private get_corridor_length () = 
            r.Next(3,20)
            
                    
        let private get_corridor x y direction = 
            let length = get_corridor_length()
            match direction with
            | Up -> new Rectangle(x, y - length - 1, 1, length)
            | Right -> new Rectangle(x + 1, y, length, 1)
            | Down -> new Rectangle(x, y + 1, 1, length)
            | Left -> new Rectangle(x - length - 1, y, length, 1)
        
        let private get_corridor_end mapBuild = 
            let corridor = mapBuild.Corridors.[r.Next(0, List.length mapBuild.Corridors)]
            //match r.Next(0,4) with
            match corridor.Width, corridor.Height, get_direction(), r.Next(0,2) with
            | 1,_,Up,_    -> corridor.Left, corridor.Top, Up
            | 1,_,Right,0 -> corridor.Right, corridor.Top, Right
            | 1,_,Right,1 -> corridor.Right, corridor.Bottom - 1, Right
            | 1,_,Down,_  -> corridor.Left, corridor.Bottom, Down
            | 1,_,Left,0   -> corridor.Left, corridor.Top, Left
            | 1,_,Left,1   -> corridor.Left, corridor.Bottom - 1, Left
            | _,1,Up,0    -> corridor.Left, corridor.Top, Up
            | _,1,Up,1    -> corridor.Right - 1, corridor.Top, Up
            | _,1,Right,_ -> corridor.Right, corridor.Top, Right
            | _,1,Down,0  -> corridor.Left, corridor.Bottom, Down
            | _,1,Down,1  -> corridor.Right - 1, corridor.Bottom, Down
            | _,1,Left,_  -> corridor.Left, corridor.Top, Left
            
            //| 0 -> (corridor.Left, corridor.Top, get_direction())
            //| 1 -> (corridor.Right, corridor.Top, get_direction())
            //| 2 -> (corridor.Left, corridor.Bottom, get_direction())
            //| _ -> (corridor.Right, corridor.Bottom, get_direction())
        
        let private get_corridor_point mapBuild = 
            let corridor = mapBuild.Corridors.[r.Next(0, List.length mapBuild.Corridors)]
            (r.Next(corridor.Left, corridor.Right), r.Next(corridor.Top, corridor.Bottom), get_direction())
        
        let private get_room x y direction = 
            let width = r.Next(3,6)
            let height = r.Next(3,6)
            
            match direction with
            | Up -> (new Rectangle(x - r.Next(1,width), y - 2 - height, width, height), x, y - 1)
            | Right -> (new Rectangle(x + 2, y - r.Next(1, height), width, height), x + 1, y)
            | Down -> (new Rectangle(x - r.Next(1, width), y + 2, width, height), x, y + 1)
            | Left -> (new Rectangle(x - 2 - width, y - r.Next(1, height), width, height), x - 1, y)
        
        let private get_room_edge mapBuild =
            let room = mapBuild.Rooms.[r.Next(0, List.length mapBuild.Rooms)]
            let direction = get_direction()
            //room.Inflate(1,1)
            match direction with
            | Up -> (r.Next(room.Left + 1, room.Right), room.Top, direction)
            | Right -> (room.Right, r.Next(room.Top + 1, room.Bottom), direction)            
            | Down -> (r.Next(room.Left + 1, room.Right), room.Bottom, direction)
            | Left -> (room.Left, r.Next(room.Top + 1, room.Bottom), direction)            

        let private test_room mapBuild (room:Rectangle) = 
            let mutable valid = true
            let testRoom = new Rectangle(room.X,room.Y,room.Width,room.Height)
            
            System.Console.Clear()
            
            //0printfn "%O" testRoom
            
            match testRoom.Width, testRoom.Height with
            | 0,_ -> testRoom.Inflate(1,0)
            | _,0 -> testRoom.Inflate(0,1)
            | _   -> testRoom.Inflate(1,1)
            
            //printfn "%O" testRoom
            
            for r in mapBuild.Rooms do
                //printfn "%O" r
                valid <- valid && not(testRoom.IntersectsWith(r))
            for r in mapBuild.Corridors do
                //printfn "%O" r
                valid <- valid && not(testRoom.IntersectsWith(r))

            valid

        let private inflate (room:Rectangle) direction =
            room

        let private make_connector type_f x y tiles =
            let process_fun = convert_tile (fun t -> (Tile.get_x t = x) && (Tile.get_y t = y)) (fun t -> type_f (Tile.get_x t) (Tile.get_y t))
            List.map process_fun tiles

        let private make_corridor_on_corridor mapBuild = 
            let x,y,direction = get_corridor_point mapBuild
            let corridor = get_corridor x y direction
            let process_fun = convert_tile (tile_in_room corridor) (fun t -> Tile.get_floor (Tile.get_x t) (Tile.get_y t))
            if (test_room mapBuild (inflate corridor direction)) then
                {mapBuild with Tiles = (List.map process_fun mapBuild.Tiles) |> make_connector Tile.get_floor x y; Corridors = ( corridor :: mapBuild.Corridors)}
            else
                mapBuild
            
        let private make_corridor_on_corridor_end mapBuild = 
            let x,y,direction = get_corridor_end mapBuild
            let corridor = get_corridor x y direction
            let process_fun = convert_tile (tile_in_room corridor) (fun t -> Tile.get_floor (Tile.get_x t) (Tile.get_y t))
            if (test_room mapBuild (inflate corridor direction)) then
                {mapBuild with Tiles = (List.map process_fun mapBuild.Tiles) |> make_connector Tile.get_floor x y; Corridors = ( corridor :: mapBuild.Corridors)}
            else
                mapBuild
                
        let private make_corridor_on_room_edge mapBuild =
            let floor_or_door () =
                match r.Next(0,100) with
                | Common.Between 0 45 n -> Tile.get_floor
                | Common.Between 46 90 n -> Tile.get_door 
                | _ -> Tile.get_door_secret
            let x,y,direction = get_room_edge mapBuild
            let corridor = get_corridor x y direction
            
            let process_fun = convert_tile (tile_in_room corridor) (fun t -> Tile.get_floor (Tile.get_x t) (Tile.get_y t))
            if (test_room mapBuild (inflate corridor direction)) then
                {mapBuild with Tiles = (List.map process_fun mapBuild.Tiles) |> make_connector (floor_or_door()) x y; Corridors = ( corridor :: mapBuild.Corridors)}
            else
                mapBuild
        
        let private make_room_on_corridor mapBuild = 
            let floor_or_door () =
                match r.Next(0,100) with
                | Common.Between 0 45 n -> Tile.get_floor
                | Common.Between 46 90 n -> Tile.get_door 
                | _ -> Tile.get_door_secret
            let x,y,direction = get_corridor_point mapBuild
            let room, doorX, doorY = get_room x y direction
            let process_fun = convert_tile (tile_in_room room) (fun t -> Tile.get_floor (Tile.get_x t) (Tile.get_y t))
            if (test_room mapBuild (inflate room direction)) then
                {mapBuild with Tiles = (List.map process_fun mapBuild.Tiles) |> make_connector (floor_or_door()) doorX doorY; Rooms = ( room :: mapBuild.Rooms)}
            else
                mapBuild
          
        let private make_room_on_corridor_end mapBuild = 
            let floor_or_door () =
                match r.Next(0,100) with
                | Common.Between 0 45 n -> Tile.get_floor
                | _ -> Tile.get_door
            let x,y,direction = get_corridor_end mapBuild
            let room, doorX, doorY = get_room x y direction
            let process_fun = convert_tile (tile_in_room room) (fun t -> Tile.get_floor (Tile.get_x t) (Tile.get_y t))
            if (test_room mapBuild (inflate room direction)) then
                {mapBuild with Tiles = (List.map process_fun mapBuild.Tiles) |> make_connector (floor_or_door()) doorX doorY; Rooms = ( room :: mapBuild.Rooms)}
            else
                mapBuild
*)

        let private make_next getPointFun getRoomFun testRoomFun tileTestFun newTileFun connectorFun incrementorFun mapBuild =
            let x,y,doorX,doorY,direction = getPointFun mapBuild
            let room = getRoomFun x y direction
            if (testRoomFun mapBuild room) then
                {mapBuild with Tiles = (List.map (convert_tile (tileTestFun room) (fun t -> newTileFun (Tile.get_x t) (Tile.get_y t)) mapBuild.Tiles) |> (make_connector connectorFun doorX doorY));} |> incrementorFun
            else
                mapBuild

(*                    
        let rec private generate_loop mapBuild = 
            match (List.length mapBuild.Rooms) with
            | 10 -> mapBuild
            | _ -> match r.Next(0,100) with
                   | x when x < 20 -> generate_loop (make_corridor_on_corridor mapBuild)
                   | x when x < 40 -> generate_loop (make_corridor_on_corridor_end mapBuild)
                   | x when x < 60 -> generate_loop (make_corridor_on_room_edge mapBuild)
                   | x when x < 80 -> generate_loop (make_room_on_corridor mapBuild)
                   | x when x < 100-> generate_loop (make_room_on_corridor_end mapBuild)
                   | _ -> failwith "Shouldn't be here"
*)
            
        let rec private generate_loop_debug (console:RootConsole) mapBuild = 
            console.Clear()
            draw_map console mapBuild.Tiles
            console.Flush()
            
            Keyboard.WaitForKeyPress(true) |> ignore            
            
            match (List.length mapBuild.Rooms) with
            | 15 -> mapBuild
            | _ -> match r.Next(0,100) with
                   | x when x < 20 -> generate_loop_debug console (make_corridor_on_corridor mapBuild)
                   | x when x < 40 -> generate_loop_debug console (make_corridor_on_corridor_end mapBuild)
                   | x when x < 60 -> generate_loop_debug console (make_corridor_on_room_edge mapBuild)
                   | x when x < 80 -> generate_loop_debug console (make_room_on_corridor mapBuild)
                   | x when x < 100-> generate_loop_debug console (make_room_on_corridor_end mapBuild)
                   | _ -> failwith "Shouldn't be here"    

            
        let private buildFirstRoom mapBuild =
            let room = {X = (mapBuild.Width / 2); Y = (mapBuild.Height / 2); Width = r.Next(3,6); Height = r.Next(3,6)}
            let process_fun = convert_tile (tile_in_room room) (fun t -> Tile.get_floor (Tile.get_x t) (Tile.get_y t))
            {mapBuild with Tiles = (List.map process_fun mapBuild.Tiles); Rooms = (room :: mapBuild.Rooms)}
            
        let private generate_blank width height =                
            { Width = width;
              Height = height;
              Tiles = [for row in 0 .. (height - 1) do
                        for column in 0 .. (width - 1) do
                            yield (Tile.get_rock column row) ];
              Rooms = [];
              Corridors = [];}
          
            
            
        let generate width height =                           
            //let mb = generate_blank width height |> buildFirstRoom |> make_corridor_on_room_edge |> generate_loop 
            let mb = generate_blank width height |> buildFirstRoom
            Map({Tiles = mb.Tiles; 
             Height = height;
             Width = width;})        
        
        let generate_debug width height console = 
            //let mb = generate_blank width height |> buildFirstRoom |> make_corridor_on_room_edge |> generate_loop_debug console
            let mb = generate_blank width height |> buildFirstRoom |> make_next get_room_edge get_corridor test_room tile_in_room Tile.get_floor Tile.get_floor, (fun mb r -> {mb with Corridors = (r :: mb.Corridors)})
            Map({Tiles = mb.Tiles; 
             Height = height;
             Width = width;})        
             
        let get_tiles map = 
            map_test (fun () -> []) (fun x -> x.Tiles) map

        let get_tile x y map = 
            get_tile_helper x y (get_tiles map)
            
        let get_empty_tile map = 
            List.first (fun t -> if (Tile.is_walkable t) then Some(t) else None) (get_tiles map)
                        
        let is_empty map = 
            map_test (fun () -> false) (fun x -> true) map
            
        
