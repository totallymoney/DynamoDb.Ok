module Tests

open System
open Expecto
open Expecto.Flip
open DynamoDb.Ok
open DynamoDb.Ok.Read.Query
open KeyConditionExpression

[<Tests>]

let tests =
    testList
        "Key condition expression"
        [ testCase "key condition expression is correct"
          <| fun () ->
              let exp, attrs =
                  buildKeyConditionExpression
                      (KeyConditionExpression(
                          StringEquals("customerId", "123"),
                          [ And,
                            KeyConditionExpression(
                                StringBeginsWith("created", "20"),
                                [ Or, KeyConditionExpression(StringBeginsWith("created", "SHALLOW"), []) ]
                            ) ]
                      ))
                      []

              Expect.equal "" "customerId = :a AND (begins_with(created, :b) OR (begins_with(created, :c)))" exp

              Expect.equal
                  ""
                  [ ":a", ScalarString "123"
                    ":b", ScalarString "20"
                    ":c", ScalarString "SHALLOW" ]
                  (List.rev attrs)


          testCase "key condition expression is correct with sibling conditions"
          <| fun () ->
              let exp, attrs =
                  buildKeyConditionExpression
                      (KeyConditionExpression(
                          StringEquals("customerId", "123"),
                          [ And,
                            KeyConditionExpression(
                                StringBeginsWith("created", "20"),
                                [ Or, KeyConditionExpression(StringBeginsWith("created", "SHALLOW"), []) ]
                            )
                            Or, KeyConditionExpression(StringEquals("created", "foo"), []) ]
                      ))
                      []

              Expect.equal
                  ""
                  "customerId = :a AND (begins_with(created, :b) OR (begins_with(created, :c))) OR (created = :d)"
                  exp

              Expect.equal
                  ""
                  [ ":a", ScalarString "123"
                    ":b", ScalarString "20"
                    ":c", ScalarString "SHALLOW"
                    ":d", ScalarString "foo" ]
                  (List.rev attrs)

          testCase "key condition expression is correct with betwixt expression"
          <| fun () ->
              let exp, attrs =
                  buildKeyConditionExpression
                      (KeyConditionExpression(
                          StringEquals("customerId", "123"),
                          [ And, KeyConditionExpression(NumberBetwixt("score", 20m, 50m), [])
                            And, KeyConditionExpression(NumberBetwixt("rbp", 0m, 3m), []) ]
                      ))
                      []

              Expect.equal "" "customerId = :a AND (score between :b and :c) AND (rbp between :d and :e)" exp

              Expect.equal
                  ""
                  [ ":a", ScalarString "123"
                    ":b", ScalarDecimal 20m
                    ":c", ScalarDecimal 50m
                    ":d", ScalarDecimal 0m
                    ":e", ScalarDecimal 3m ]
                  (List.rev attrs)

          testCase "AttrMapping with IsLSet enabled"
          <| fun () ->
              let x = AttrMapping.mapAttrValue ((DocList([])))
              let y = AttrMapping.mapAttrValue ((DocMap([])))

              Expect.equal "" x.IsLSet true
              Expect.equal "" y.IsLSet false

          testCase "UpdateExpression aliases reserved names in SET"
              <| fun () ->
                  let exp, valueAttrs, nameAliases =
                      Write.UpdateExpression.buildUpdateExpression
                          [ Write.UpdateExpression.Set("Hash", ScalarString "ok") ]
                          []
                          []

                  Expect.equal "" "SET #a = :a" exp
                  Expect.equal "" [ ":a", ScalarString "ok" ] (List.rev valueAttrs)
                  Expect.equal "" [ "#a", "Hash" ] (List.rev nameAliases)

          testCase "UpdateExpression aliases names in INCREMENT and REMOVE, reuses alias"
              <| fun () ->
                  let exp, valueAttrs, nameAliases =
                      Write.UpdateExpression.buildUpdateExpression
                          [ Write.UpdateExpression.Increment("Hash", 1)
                            Write.UpdateExpression.Remove "Order" ]
                          []
                          []

                  Expect.equal "" "SET #a = :a + if_not_exists(#a, :b) REMOVE #b" exp
                  Expect.equal "" [ ":a", ScalarInt32 1; ":b", ScalarInt32 0 ] (List.rev valueAttrs)
                  Expect.equal "" [ "#a", "Hash"; "#b", "Order" ] (List.rev nameAliases) ]
