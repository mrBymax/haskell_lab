import GHC.RTS.Flags (ProfFlags (typeSelector))
import System.Directory (Permissions (readable))
import Text.XHtml (title)

-- BookInfo is the name of the new type (a type constructor)
-- The Book is the value constructor, used to create a Book type (made of its id (Int),
-- title (String), and list of its authors ([String]))
-- It is a tuple <Int, String, [String]> but with a distinct type.

data BookInfo = Book Int String [String] deriving (Show)

-- Same as above
data MagazineInfo = Magazine Int String [String] deriving (Show)

-- We can create new objects as:
myInfo = Book 9780135072455 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

-- Constructor's name is not necessarly different from the object name
-- We can use also type synonims for any existent type or to reduce the verbosity of a long type
type CustomerID = Int

type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview) -- Notice how a synonim can refer only to an existing type

-- We can also use Algebraic Data Types (actually, all the data type defined by the `data` keyword are algebraic)

type CardHolder = String

type CardNumber = String

type Address = [String]

data BillingInfo
  = CreditCard CardNumber CardHolder Address
  | CashOnDelivery
  | Invoice CustomerID
  deriving (Show)

-- We can also use pattern matching and wildcards when defining variables
nicerID (Book id _ _) = id

nicerTitle (Book _ title _) = title

nicerAuthors (Book _ _ authors) = authors

-- While defining records, we can use some "boilerplate" to make things more readable
-- while defining data type

data Customer = Customer
  { customerID :: CustomerID,
    customerName :: String,
    customerAddress :: Address
  }
  deriving (Show)

-- Which is equivalent to:
-- data Customer = Customer Int String [String]
--                 deriving (Show)

-- customerID :: Customer -> Int
-- customerID (Customer id _ _) = id

-- customerName :: Customer -> String
-- customerName (Customer _ name _) = name

-- customerAddress :: Customer -> [String]
-- customerAddress (Customer _ _ address) = address

customer1 =
  Customer
    271828
    "J.R. Hacker"
    [ "255 Syntax Ct",
      "Milpitas, CA 95134",
      "USA"
    ]

-- Which is definitely less verbose than:
customer2 =
  Customer
    { customerID = 271828,
      customerAddress =
        [ "1048576 Disk Drive",
          "Milpitas, CA 95134",
          "USA"
        ],
      customerName = "Jane Q. Citizen"
    }
