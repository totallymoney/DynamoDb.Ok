module Tests

open System
open Expecto
open Expecto.Flip
open DynamoDb.Ok
open DynamoDb.Ok.Read.Query

[<Tests>]

  let tests =
    testList "Key condition expression" [

      testCase "key condition expression is correct" <| fun () ->
        let exp, attrs =
          buildKeyConditionExpression (
            KeyConditionExpression (
              StringEquals ("customerId", "123"),
              [ And,
                KeyConditionExpression (
                  StringBeginsWith ("created", "20"),
                  [ Or,
                    KeyConditionExpression (
                      StringBeginsWith ("created", "SHALLOW"), [])
                  ])
              ])) []

        Expect.equal ""
          "customerId = :a AND (begins_with(created, :b) OR (begins_with(created, :c)))" exp

        Expect.equal ""
          [ ":a", ScalarString "123"
            ":b", ScalarString "20"
            ":c", ScalarString "SHALLOW" ] (List.rev attrs)


      testCase "key condition expression is correct with sibling conditions" <| fun () ->
        let exp, attrs =
          buildKeyConditionExpression (
            KeyConditionExpression (
              StringEquals ("customerId", "123"),
              [ And,
                KeyConditionExpression (
                  StringBeginsWith ("created", "20"),
                  [ Or,
                    KeyConditionExpression (
                      StringBeginsWith ("created", "SHALLOW"), [])
                  ])
                Or,
                KeyConditionExpression (
                  StringEquals ("created", "foo"), [])
              ])) []

        Expect.equal ""
          "customerId = :a AND (begins_with(created, :b) OR (begins_with(created, :c))) OR (created = :d)" exp

        Expect.equal ""
          [ ":a", ScalarString "123"
            ":b", ScalarString "20"
            ":c", ScalarString "SHALLOW"
            ":d", ScalarString "foo" ] (List.rev attrs)

      testCase "key condition expression is correct with betwixt expression" <| fun () ->
        let exp, attrs =
          buildKeyConditionExpression (
            KeyConditionExpression (
              StringEquals ("customerId", "123"),
              [ And,
                KeyConditionExpression (
                  NumberBetwixt ("score", 20m, 50m), [])
                And,
                KeyConditionExpression (
                  NumberBetwixt ("rbp", 0m, 3m), [])
              ])) []

        Expect.equal ""
          "customerId = :a AND (score between :b and :c) AND (rbp between :d and :e)" exp

        Expect.equal ""
          [ ":a", ScalarString "123"
            ":b", ScalarDecimal 20m
            ":c", ScalarDecimal 50m
            ":d", ScalarDecimal 0m
            ":e", ScalarDecimal 3m ] (List.rev attrs)
    ]
