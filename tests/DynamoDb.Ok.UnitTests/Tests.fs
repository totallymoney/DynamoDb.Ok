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
              Expect.equal "" [ "#a", "Hash"; "#b", "Order" ] (List.rev nameAliases)

          testCase "ConditionExpression aliases reserved names"
          <| fun () ->
              let exp, valueAttrs, nameAliases =
                  Write.ConditionExpression.buildConditionExpression
                      (Write.ConditionExpression.ConditionExpression(
                          Write.ConditionExpression.AttributeExists "Hash",
                          [ Write.ConditionExpression.And,
                            Write.ConditionExpression.ConditionExpression(
                                Write.ConditionExpression.StringEquals("Order", "test"),
                                []
                            ) ]
                      ))
                      []
                      []

              Expect.equal "" "attribute_exists(#a) AND (#b = :a)" exp
              Expect.equal "" [ ":a", ScalarString "test" ] (List.rev valueAttrs)
              Expect.equal "" [ "#a", "Hash"; "#b", "Order" ] (List.rev nameAliases)

          testCase "ConditionExpression aliases names in NumberBetwixt"
          <| fun () ->
              let exp, valueAttrs, nameAliases =
                  Write.ConditionExpression.buildConditionExpression
                      (Write.ConditionExpression.ConditionExpression(
                          Write.ConditionExpression.NumberBetwixt("Hash", 10m, 20m),
                          []
                      ))
                      []
                      []

              Expect.equal "" "#a between :a and :b" exp
              Expect.equal "" [ ":a", ScalarDecimal 10m; ":b", ScalarDecimal 20m ] (List.rev valueAttrs)
              Expect.equal "" [ "#a", "Hash" ] (List.rev nameAliases)

          testCase "UpdateExpression reuses alias across multiple operations on same field"
              <| fun () ->
                  let exp, valueAttrs, nameAliases =
                      Write.UpdateExpression.buildUpdateExpression
                          [ Write.UpdateExpression.Set("Hash", ScalarString "ok")
                            Write.UpdateExpression.Increment("Hash", 2)
                            Write.UpdateExpression.Remove "Hash" ]
                          []
                          []

                  // Should use the same #a alias for all three occurrences of "Hash"
                  Expect.equal
                      ""
                      "SET #a = :a,#a = :b + if_not_exists(#a, :c) REMOVE #a"
                      exp

                  // Values should be created for :a (set), :b (inc), :c (default 0 for inc)
                  Expect.equal
                      ""
                      [ ":a", ScalarString "ok"; ":b", ScalarInt32 2; ":c", ScalarInt32 0 ]
                      (List.rev valueAttrs)

                  Expect.equal "" [ "#a", "Hash" ] (List.rev nameAliases) ]
