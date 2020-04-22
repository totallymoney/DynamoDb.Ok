# How To write an item

## Write a simple item

```fsharp
open System
open Amazon.DynamoDBv2.Model
open DynamoDb.Ok

type SimpleObject =
    { Id: Guid
      Email: String
      IsVerified: Boolean
      DateOfBirth: DateTime
      Balance: Decimal }

let client = new AmazonDynamoDBClient()

let write (o: SimpleObject): Async<Result<unit, DynamoDbError list>> =
    [ Attr("Id", ScalarGuid o.Id)
      Attr("Email", ScalarString o.Email)
      Attr("Verified", ScalarBool o.IsVerified)
      Attr("DateOfBirth", ScalarDate o.DateOfBirth)
      Attr("Balance", ScalarDecimal o.Balance) ]
    |> Write.putItem client "CustomersTable"

```

## Write a complex item

```fsharp
type ComplexObject =
    { Name: String
      Created: DateTime
      MiddleName: String option
      Address1: Address
      Address2: Address option
      Days: String Set
      Measurements: Decimal list
      Awards: Award list }

and Address =
    { Number: String
      Postcode: String }

and Award =
    { Id: Guid
      Level: Int32 }

let buildAddressAttr (a: Address) =
    DocMap
        [ Attr("Number", ScalarString a.Number)
          Attr("Postcode", ScalarString a.Postcode) ]

let buildAwardAttr (a: Award) =
    DocMap
        [ Attr("Id", ScalarGuid a.Id)
          Attr("Level", ScalarInt32 a.Level) ]

open DynamoDb.Ok.Write

let write (o: ComplexObject) =
    [ Attr("Name", ScalarString o.Name)
      Attr("Created", ScalarDate o.Created)
      Attr("Address1", buildAddressAttr o.Address1) ]
    @ BuildAttr.optional "Address2" buildAddressAttr o.Address2
    @ BuildAttr.optional "MiddleName" ScalarString o.MiddleName
    @ BuildAttr.setString "Days" (Set.toList o.Days)
    @ BuildAttr.docList "Measurements" (List.map ScalarDecimal o.Measurements)
    @ BuildAttr.docList "Awards" (List.map buildAwardAttr o.Awards)
    |> Write.putItem null "ComplexObjectsTable"

```
