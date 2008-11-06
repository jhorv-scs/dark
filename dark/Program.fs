(*    
#light

open System
//open System.Drawing
open libtcodWrapper

[<Literal>]
let screen_width = 46
[<Literal>]
let screen_height = 20

type point = {
    x : int;
    y : int; }
    
type mapRecord = {
    mapObject: TCODFov ;
    stringMap : string list;
    console : Console; }

let loop_and_apply maxX maxY f =
    let rec helper a b g =
        match a,b with
        | x,y when x < maxX && y < maxY -> g x y
                                           helper x (y+1) g
        | x,y when x < maxX && y >= maxY -> helper (x+1) 0 g
        | x -> x |> ignore
    helper 0 0 f

let set_map (map:mapRecord) =
    let setter x y =
        match map.stringMap.[y].[x] with
        | _   -> map.mapObject.SetCell(x,y,false,false)
        | ' ' -> map.mapObject.SetCell(x,y,true,true)
        //| ' ' -> map.mapObject.SetCell(x,y,true,true)
        | '=' -> map.mapObject.SetCell(x,y,true,false)
        //| _   -> map.mapObject.SetCell(x,y,false,false)
    loop_and_apply screen_width screen_height setter
    
    map.console.Clear()    

let draw_map (map:mapRecord) =
    let darkWall = Color.FromRGB(byte 0, byte 0, byte 100)
    let darkGround = Color.FromRGB(byte 50, byte 50, byte 150)
    let lightWall = Color.FromRGB(byte 130, byte 110, byte 50)
    let lightGround = Color.FromRGB(byte 200, byte 180, byte 50)
    let drawer x y =
        let isWall = (map.stringMap.[y].[x] = '#')
        match map.mapObject.CheckTileFOV(x,y) with
        | b when b = false && isWall -> map.console.SetCharBackground(x,y,darkWall, new Background(BackgroundFlag.Set))
        | b when b = false && not(isWall) -> map.console.SetCharBackground(x,y,darkGround, new Background(BackgroundFlag.Set))
        | b when b = true && isWall -> map.console.SetCharBackground(x,y,lightWall, new Background(BackgroundFlag.Set))
        | b when b = true && not(isWall) -> map.console.SetCharBackground(x,y,lightGround, new Background(BackgroundFlag.Set))
        | b -> b |> ignore
                
    loop_and_apply screen_width screen_height drawer

let draw_char (map:mapRecord) (x,y) = 
    map.console.PutChar(x,y,'@')
    map.mapObject.CalculateFOV(x,y, 0)   

let read_key () =
    let x = ref 20
    let y = ref 10
    let f () =
        let key = Keyboard.CheckForKeypress(KeyPressType.Pressed)
        match key with
        | k when (char k.Character) = 'i' -> y := !y - 1
        | k when (char k.Character) = 'j' -> x := !x - 1
        | k when (char k.Character) = 'k' -> y := !y + 1
        | k when (char k.Character) = 'l' -> x := !x + 1
        | k -> k |> ignore
        (!x,!y)
    f
    
let smap = ["##############################################";
            "#######################      #################";
            "#####################    #     ###############";
            "######################  ###        ###########";
            "##################      #####             ####";
            "################       ########    ###### ####";
            "###############      #################### ####";
            "################    ######                  ##";
            "########   #######  ######   #     #     #  ##";
            "########   ######      ###                  ##";
            "########                                    ##";
            "####       ######      ###   #     #     #  ##";
            "#### ###   ########## ####                  ##";
            "#### ###   ##########   ###########=##########";
            "#### ##################   #####          #####";
            "#### ###             #### #####          #####";
            "####           #     ####                #####";
            "########       #     #### #####          #####";
            "########       #####      ####################";
            "##############################################"]

RootConsole.Width <- 80
RootConsole.Height <- 50
RootConsole.Fullscreen <- false
RootConsole.WindowTitle <- "Test"
RootConsole.Font <- new CustomFontRequest("terminal.bmp", 8,8,16,16,false, ColorPresets.Black)

let rootConsole = RootConsole.GetInstance()
let map = {
            mapObject = new TCODFov(screen_width, screen_height) ; 
            stringMap = smap ; 
            console = RootConsole.GetNewConsole(screen_width, screen_height);}
let getkey = read_key()

let play () =
    rootConsole.Clear()    
    rootConsole.ForegroundColor <- ColorPresets.Gray
    rootConsole.BackgroundColor <- ColorPresets.Black

    set_map map
    let key = (getkey())
    draw_char map key
    draw_map map    

    map.console.Blit(0,0,screen_width, screen_height, rootConsole, 20,10,255)
    rootConsole.Flush()
    
    key

let rec play_loop () =
    match play() with
    | 
    | _   -> play_loop()
    *)