-- module Example

-- basic sum type
type Roast = Dark | Medium | Light

-- sum type with record
type Candy = KitKat | ICS | Special { name : String }

-- associated types of varying associativity
type Drinks = Tea (Maybe String) | Coffee Roast

-- type alias
type alias Calories = List Int

-- record alias
type alias Model =
    { count : Int
    , favourite_candy : Candy
    , friends : List String
    , caloryHistory : Calories
    }