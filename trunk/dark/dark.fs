#light

open System
open libtcodWrapper
open dark
   
type Character = {
    x : int;
    y : int;}

let draw_map (console:RootConsole) map =
    List.iter (fun t -> console.PutChar(Tile.get_x t,Tile.get_y t,Tile.get_display_char t, new Background(BackgroundFlag.Set))) (Map.get_tiles map)

let draw_character (console:RootConsole) character = 
    console.PutChar(Character.get_x character, Character.get_y character, Character.get_display_char character, new Background(BackgroundFlag.Set))

let key_press () = 
    Keyboard.WaitForKeyPress(true)
    
let play session =   
    let actions = Action.get_predefined_actions()
    let root = Session.get_root_console session
    let map = Session.get_current_map session
    let player = Session.get_player session
    
    root.Clear()        
    root.ForegroundColor <- ColorPresets.Gray
    root.BackgroundColor <- ColorPresets.Black

    draw_map root map
    draw_character root player
    root.Flush()
    
    let key = key_press()
    match (Microsoft.FSharp.Collections.Map.tryfind (key.KeyCode, key.Character, key.Alt, key.Control, key.Shift) actions) with
    | Some(action) -> action key_press session
    | None         -> session

let rec game_loop session = 
    let x = play session
    match Session.get_status x with
    | Terminate -> "" |> ignore
    | _         -> game_loop x         

RootConsole.Width <- 80
RootConsole.Height <- 50
RootConsole.Fullscreen <- false
RootConsole.WindowTitle <- "Test"

game_loop (Session.generate 80 50)
