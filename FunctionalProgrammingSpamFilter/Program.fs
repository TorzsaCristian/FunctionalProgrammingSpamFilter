// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.IO
open System.Text.RegularExpressions

let hamFolder = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "enron/ham")
let spamFolder = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "enron/spam")

let hamTestFolder = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "testData/ham")
let spamTestFolder = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "testData/spam")


// Folder and Directory example for the laborator PDF
type Folder(path: string) =
     let filenames: string array = 
        Directory.GetFiles(path)
     member this.Files = 
         Array.map (fun el -> new File(el, this)) filenames
     member this.Path = path
 and File(filename: string, owningFolder: Folder) =
     member this.Name = filename
     member this.OwningFolder = owningFolder



// Custom labels in order to label the emails
type Label = Spam | Ham


// return the text from the file
let getEmailFromFile file = 
    let textEmail = System.IO.File.ReadAllText(file)
    textEmail



//get features (all words >= size 3, no special characters) and convert them to lower case
// find and return the matching words, convert to a seq, then apply map to every match to convert it to lower case
let features (msg:string) : seq<string> =
    // Basic splitting... not working but letting it here to prove we tried
    // msg.Split([|' '|]) |> Array.toSeq  
    Regex.Matches(msg, "(?:[A-z]{3,})+") |> Seq.cast |> Seq.map (fun (x:Match) -> x.Value.ToLower())


// printing the map (for DEBUGGING purposes)
let printMap map =
    map|> Map.map(fun k v -> printfn "%A %A \n" k v ) |> ignore


// get the count of the word, if not, return a low number that wont affect the score
let getCount map word = 
    match (Map.tryFind word map) with
    | Some(x) -> (float x)
    | _ -> 0.000000000000001 // words that does not exist in the table, have a very low impact in the overall score



let pHam hamCount totalCount : float = 
    (float hamCount) / (float totalCount)

let pSpam spamCount totalCount : float = 
    (float spamCount) / (float totalCount)


let pWordGivenSpam word spamMapFrequency spamCount : float =
    (getCount spamMapFrequency word) / (float spamCount)

let pWordGivenHam word hamMapFrequency hamCount : float =
    (getCount hamMapFrequency word) / (float hamCount)


let pWord word hamCount spamCount totalCount hamMapFrequency spamMapFrequency: float = 
    (pWordGivenHam word hamMapFrequency hamCount) * (pHam hamCount totalCount) + (pWordGivenSpam word hamMapFrequency hamCount) * (pSpam spamCount totalCount)


let pHamGivenWords words hamCount spamCount totalCount hamMapFrequency spamMapFrequency = 
    let product = 
        words
        |> Seq.map (fun w -> (pWordGivenHam w hamMapFrequency hamCount) / (pWord w hamCount spamCount totalCount hamMapFrequency spamMapFrequency))
        |> Seq.reduce (*)
    product * (pHam hamCount totalCount)

let pSpamGivenWords words hamCount spamCount totalCount hamMapFrequency spamMapFrequency = 
    let product = 
        words
        |> Seq.map (fun w -> (pWordGivenSpam w spamMapFrequency spamCount) / (pWord w hamCount spamCount totalCount hamMapFrequency spamMapFrequency))
        |> Seq.reduce (*)
    product * (pSpam spamCount hamCount)


// The actual classifier
let classifier email hamWordsCount spamWordsCount totalCount hamMapFrequency spamMapFrequency : Label =
        let words = features email
        let hamScore = pHamGivenWords words hamWordsCount spamWordsCount totalCount hamMapFrequency spamMapFrequency
        let spamScore = pSpamGivenWords words hamWordsCount spamWordsCount totalCount hamMapFrequency spamMapFrequency

        if hamScore > spamScore then Ham else Spam


// calculate the accuracy of the classifier
let accuracyRate labeledData hamWordsCount spamWordsCount totalCount hamMapFrequency spamMapFrequency = 
    let numberCorrectlyClassified = 
        labeledData
        |> Seq.sumBy (fun (label, msg) -> if (classifier msg hamWordsCount spamWordsCount totalCount hamMapFrequency spamMapFrequency) = label then 1.0 else 0.0)
    numberCorrectlyClassified / float(Seq.length labeledData)


[<EntryPoint>]
let main argv = 

    printfn "Starting.."

    let spamFolder = new Folder(spamFolder)
    let hamFolder = new Folder(hamFolder)


    printfn "Creating the Spam Map Frequency"
    let spamMapFrequency = 
        spamFolder.Files // all files from folder
        |> Seq.map(fun file -> getEmailFromFile file.Name) // get the text content from file
        |> Seq.map(fun msg -> features msg) // get all words from the text
        |> Seq.collect(fun words ->  words) // create one single sequence with all the words
        |> Seq.groupBy(fun w -> w) // create a (word, seq[word, word, word...])format in order to count them
        |> Seq.map(fun (w, ws) -> (w, Seq.length ws)) // create a (word, lentgh) seq
        |> Map.ofSeq // cast seq to map (we are going to refer to the map as a "table"


    
    printfn "Creating the Ham Map Frequency"
    let hamMapFrequency = 
        hamFolder.Files // all files from folder
        |> Seq.map(fun file -> getEmailFromFile file.Name) // get the text content from file
        |> Seq.map(fun msg -> features msg) // get all words from the text
        |> Seq.collect(fun words ->  words) // create one single sequence with all the words
        |> Seq.groupBy(fun w -> w) // create a (word, seq[word, word,word...])format in order to count them
        |> Seq.map(fun (w, ws) -> (w, Seq.length ws)) // create a seq (word, lentgh)
        |> Map.ofSeq // cast seq to map (we are going to refer to the map as a "table"


    let spamWordsCount = Map.count spamMapFrequency // the number of words in the spam table
    let hamWordsCount = Map.count hamMapFrequency // the numbers of words in the ham table
    let totalCount = spamWordsCount + hamWordsCount // the total number of words


    let spamTestFolder = new Folder(spamTestFolder)
    let hamTestFolder = new Folder(hamTestFolder)

    printfn "Creating the test data"
    let testData = 
        Seq.concat[
            hamTestFolder.Files
            |> Seq.map(fun file -> getEmailFromFile file.Name) 
            |> Seq.map( fun msg -> (Ham, msg));
            spamTestFolder.Files
            |> Seq.map(fun file -> getEmailFromFile file.Name) 
            |> Seq.map( fun msg -> (Spam, msg))]


    printfn "Calculating the accuracy..."
    printfn "The accuracy is %A" (accuracyRate testData hamWordsCount spamWordsCount totalCount hamMapFrequency spamMapFrequency)
    printfn "Total= %A, Ham= %A, Spam= %A" totalCount hamWordsCount spamWordsCount

    let testEmail = """
        Hi Cristian,
        Silent Partner Investment Opportunity that we recently discovered.


        Web Development team needs funds for expansion and will pay great returns!

        CLICK HERE NOW

        Invest $300 get back $2610 paid $10 daily 5 days per week for one year by Paypal

        Invest $900 get back $7830 paid $30 daily 5 days per week for one year by Paypal

        Invest $1250 get back $13,050 paid $50 daily 5 days per week for one year by Paypal

        Larger Investments available (Payment proof for us above)

        CLICK HERE NOW
        Serious inquiries only.

        This is an opportunity to get DAILY returns to your #PayPal WITHOUT ever recruiting anyone...yea, that's right...I don't NEED you to join to make money in this, but it would be unfair of me to keep all of this money to myself.;)

        I introduced this program last week to my team and EVERY single person that got involved with me has gotten paid.

        (One Year Contracts...Pay once, get paid daily (weekdays) for 1 year.)

        CLICK HERE NOW

        NOTE: Only click if you ARE serious about starting and have at least $300-$2,500 to get started...yes they take Credit Cards/Paypal


        **Disclaimer: My results ARE typical of other people who have invested at my same level. Your returns will be equal to the level you join at. I do not own/control this program and can't predict the future or how long the returns will keep coming. Past performance is not a prediction of future performance.This is NOT financial advice, this is only a recommendation of something that is currently working and please do your own research before investing in anything.

        Good Luck!
        """

    let words = features testEmail
    let pHam = pHamGivenWords words hamWordsCount spamWordsCount totalCount hamMapFrequency spamMapFrequency
    let pSpam = pSpamGivenWords words hamWordsCount spamWordsCount totalCount hamMapFrequency spamMapFrequency

    let label = if pHam > pSpam then Ham else Spam

    printfn "The email is: %A \n INFO: ham=%A, spam=%A" label pHam pSpam


                
     
    0 // return an integer exit code
