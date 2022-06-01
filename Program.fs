// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Text.RegularExpressions

//generate a 10 x 10 grid - is this global?
let mutable grid = Array2D.init 10 10 (fun x y -> ("0"))

let gameGrid = grid


type Destroyer(i: int) = 
    let identify = i
    let lenght = 4

type Battleship(i: int) = 
    let identify = i
    let lenght = 5


let AddShip(x:int)(y:int)(identifyer:int)(length:int)(orientation:String) =
    //from cordinates given add the ship alon a given axis(orientation : "x"/"y")
    if orientation = "y"
    then
        for i in 0 .. length-1 do
            gameGrid.[x, (y + i)] <- identifyer.ToString()
    else
        for i in 0 .. length-1 do
            gameGrid.[(x + i),y] <- identifyer.ToString()
    true
    

//How could I aproach this from a functional perspective?
let CheckPath(x:int)(y:int)(length:int)(direction:String) =
    let mutable counter = 0
    if direction = "x"
    then
        //check x axis
        for i in 0 .. (length-1) do
            counter <- counter + if gameGrid.[(x + i),y] = "0" 
                                    then 0
                                    else 1

    else
        //check y axis
        for i in 0 .. (length-1) do
            counter <- counter + if gameGrid.[x,(y + i)] = "0"
                                    then 0
                                    else 1
    if counter > 0
    then System.Console.WriteLine("Path Obstructed {0}",direction); false
    else System.Console.WriteLine("Adding Ship"); true


//returns true if a ship is added and false if not
let TryToAddShip (length:int) (identifyer:int) = 


    //pick some cordinates
    let rand = new System.Random()
    let x = rand.Next(0,10)
    let y = rand.Next(0,10)
    //pick an orientation
    let orientation = rand.Next(0,2)    //assume 0 is vertical and 1 is horizontal
    
    

    //if orientation + lenght > gridlenght-1 then try again otherwise place ship
    if orientation = 0 
    //vertical
    then 
        if (y + (length - 1)) < 10
        then 
            if CheckPath x y length "y"
                then AddShip  x y identifyer length "y";
                else false
        else 
            System.Console.WriteLine("Clipping Ship on Y");
            if CheckPath x (10-length) length "y"
                then AddShip  x (10-length) identifyer length "y";
                else System.Console.WriteLine("Path blocked y"); false


    //horizontal
    else 
        if (x + (length - 1)) < 10
        then 
            if CheckPath x y length "x"
                then AddShip x y identifyer length "x";
                else false
        else 
            System.Console.WriteLine("Clipping Ship on X");
            if CheckPath (10-length) y length "x"
                then AddShip (10-length) y identifyer length "x";
                else System.Console.WriteLine("Path blocked x"); false


    
//Get User Input and return their chosen cordinates or false if invalid
let GetUsersInput() = 
    Console.WriteLine("Please input your guess: ");
    let userInput = Console.ReadLine();
    let rawX = userInput.Substring(0,1);    //X part should be between a-j
    let rawY = userInput.Substring(1);      //Y part should be an int between 1 and 10

    let regex = Regex.Match(rawX,@"^[A-Ja-j]{1}$");
    let finalX = 
        if(regex.Success)
        then 
            //give the corresponding x cordinate back as an int
            let map = Map [("a",0);("b",1);("c",2);("d",3);("e",4);("f",5);("g",6);("h",7);("i",8);("j",9);]
            let lower = rawX.ToLower();
            //let finalX = map.TryFind lower
            //true
            map.TryFind lower;
        else
            //is invalid X cordinate
            Console.WriteLine("X invalid regex");
            None;

    //check Y Cordinate
    let regex = Regex.Match(rawY,@"^\d{1,2}$"); //checks that it is no more than two numbers
    let finalY = 
        if(regex.Success)
        then
            let finalY = (rawY |> int) - 1;
            if(finalY >= 0 && finalY < 10)
            then
                //y is valid
                option.Some finalY;
            else
                //y is not valid
                Console.WriteLine("Y invalid");
                None;
        
        else
            Console.WriteLine("Y invalid regex");
            None;

    if(finalY = None || finalX = None)
    then
        (false,finalX,finalY);
    else
        (true,finalX,finalY);
    






//check how many of a given identifyer exist
let countIdentifyer(i) = 
    let mutable count = 0;
    for x = 0 to Array2D.length1 gameGrid - 1 do
        for y = 0 to Array2D.length1 gameGrid - 1 do
            count <- count + if gameGrid.[x, y] = i
                            then
                                1;
                            else
                                0;
    count;


let handleHit(x,y) = 
    //Get ship hit,
    let hitShip = gameGrid.[x,y]
    //if it's the only one then Announce that they have sunk it
    if countIdentifyer(hitShip) = 1
    then
        //hit and sink
        //TODO: Depending on type of ship depends on message
        if (gameGrid.[x,y] = "1")
        then 
            gameGrid.[x,y] <- "X";
            "You have sunk a destroyer...";
        elif (gameGrid.[x,y] = "2")
        then
            gameGrid.[x,y] <- "X";
            "You have sunk a destroyer...";
        else 
            gameGrid.[x,y] <- "X";
            "You have sunk a battleship...";
        
        
    else
        //hit
        gameGrid.[x,y] <- "X";
        "HIT!"


let checkShot(x,y) = 
    if(gameGrid.[x,y] = "0")
    then
        //miss
        gameGrid.[x,y] <- "x"; 
        "MISS!"
    elif (gameGrid.[x,y] = "x" || gameGrid.[x,y] = "X")
    then 
        //already done
        "You have already taken this shot..."
    else
        //seperate function to handle hit/check status of all ships
        handleHit(x,y);


//print the array
let printboard() = 
    for x = 0 to Array2D.length1 gameGrid - 1 do
        for y = 0 to Array2D.length1 gameGrid - 1 do
            printf "%A " gameGrid.[x, y]
        printfn""
    

let CheckShips() = 
    let mutable counter = 0;
    for i = 1 to 3 do
        for x = 0 to Array2D.length1 gameGrid - 1 do
            for y = 0 to Array2D.length1 gameGrid - 1 do
                counter <- counter + if gameGrid.[x,y] = i.ToString()
                                        then 
                                            1;
                                        else
                                            0;
    if (counter = 0)
    then false;
    else true;

[<EntryPoint>]
let main argv =  

    //Generate Ships
    let Destroyer1 = Destroyer(1);
    let Destroyer2 = Destroyer(2);
    let Battleship = Battleship(3);

    let mutable ship1 = false;
    let mutable ship2 = false;
    let mutable ship3 = false;

    while not ship1 do
        ship1 <- TryToAddShip 4 1;
    
    while not ship2 do
        ship2 <- TryToAddShip 4 2;
    
    while not ship3 do
        ship3 <- TryToAddShip 5 3;

    printboard();
    Console.WriteLine("\n\n\n");
    Console.WriteLine("Your guesses should be in the following format: \"B3\" with ranges from A-J and 1-10");

    //Start Game Loop
    while CheckShips() do
        let valid, x, y = GetUsersInput();
        if(valid)
        then
            //Handle Valid Shot
            Console.WriteLine(checkShot(x.Value,y.Value));
        else
            Console.WriteLine("Invalid Input");
    //When all ships dead end loop...
    
    
    //print array
    Console.WriteLine("Congratulations, You have sunk all ships...");
    Console.WriteLine("A Large X indicates a hit on a ship whereas a small x indicates a miss");
    printboard();

    0 // return an integer exit code



