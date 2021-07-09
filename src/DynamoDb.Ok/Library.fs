namespace DynamoDb.Ok

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open Amazon.DynamoDBv2
open Amazon.DynamoDBv2.Model


type Attr = Attr of name: string * value: AttrValue

and AttrValue =
    | ScalarString of String
    | ScalarInt32 of Int32
    | ScalarDecimal of Decimal
    | ScalarBinary of String
    | ScalarBool of Boolean
    | ScalarGuid of Guid
    | ScalarDate of DateTime
    | ScalarNull
    | SetString of NonEmptyList<String>
    | SetDecimal of NonEmptyList<Decimal>
    | SetInt32 of NonEmptyList<Int32>
    | SetBinary of NonEmptyList<String>
    | DocList of AttrValue list
    | DocMap of Attr list

and NonEmptyList<'a when 'a: comparison> = NonEmptyList of head: 'a * tail: 'a list

type A = AttributeValue

module AttrMapping =

    let toSet (NonEmptyList (head, tail)) = Set.ofList tail |> Set.add head

    let rec mapAttrValue =
        function
        | ScalarString s -> A(S = s)
        | ScalarGuid g -> A(S = string g)
        | ScalarDate d -> A(S = d.ToString("s"))
        | ScalarInt32 i -> A(N = string i)
        | ScalarDecimal d -> A(N = string d)
        | ScalarBinary b -> A(B = toGzipMemoryStream b)
        | ScalarBool b -> A(BOOL = b)
        | ScalarNull -> A(NULL = true)
        | SetString ss -> A(SS = ResizeArray(toSet ss))
        | SetDecimal sd -> A(NS = ResizeArray(Seq.map string (toSet sd)))
        | SetInt32 si -> A(NS = ResizeArray(Seq.map string (toSet si)))
        | SetBinary bs -> A(BS = ResizeArray(Seq.map toGzipMemoryStream (toSet bs)))
        | DocList l -> A(L = ResizeArray(List.map mapAttrValue l))
        | DocMap m -> A(M = mapAttrsToDictionary m)

    and mapAttr (Attr (name, value)) = name, mapAttrValue value

    and mapAttrsToDictionary =
        List.map mapAttr >> dict >> Dictionary<string, A>

    and toGzipMemoryStream (s: string) =
        let output = new MemoryStream()

        use zipStream =
            new GZipStream(output, CompressionMode.Compress, true)

        use writer = new StreamWriter(zipStream)
        writer.Write s
        output

    let buildAttrDictionary =
        List.map (fun (attr, value) -> attr, mapAttrValue value)
        >> dict
        >> Dictionary<string, A>


module Async =

    let retn x = async { return x }

    let map f m =
        async {
            let! x = m
            return f x
        }

    let bind f m =
        async {
            let! x = m
            return! f x
        }

module AsyncResult =

    let retn x = async { return Ok x }

    let map f m =
        async {
            let! r = m

            match r with
            | Ok a -> return Ok(f a)
            | Error e -> return Error e
        }

    let bind f m =
        async {
            let! r = m

            match r with
            | Ok a -> return! f a
            | Error e -> return Error e
        }

type DynamoDbError =
    | ParseError of attributeName: string
    | MissingAttributeError of attributeName: string
    | OperationError of exn
    | UnprocessedItemsError

module DynamoDbError =

    let private toString =
        function
        | ParseError e
        | MissingAttributeError e -> e
        | OperationError e -> string e
        | UnprocessedItemsError -> "Unprocessed items in batch operation"

    let flatten =
        List.fold (fun acc e -> sprintf "%s\n%s" acc (toString e)) String.Empty

    let handleAsyncError =
        function
        | Choice1Of2 x -> Ok x
        | Choice2Of2 (e: exn) when (e :? ConditionalCheckFailedException) -> Error [ OperationError e.InnerException ]
        | Choice2Of2 (e: exn) when (e :? AggregateException) -> Error [ OperationError e.InnerException ]
        | Choice2Of2 (e: exn) -> Error [ OperationError e ]

module Expiry =

    let private epochTime (d: DateTime) =
        Convert.ToInt32((d - DateTime.UnixEpoch).TotalSeconds)

    let attribute name d = Attr(name, epochTime d |> ScalarInt32)


module ExpressionAttributeName =

    let getExpAttrName attributes value =
        let getAlphabetLetter =
            (+) 64
            >> char
            >> string
            >> fun s -> s.ToLower()

        let attributeName =
            List.length attributes
            + 1
            |> getAlphabetLetter
            |> sprintf ":%s"

        attributeName, (attributeName, value) :: attributes

module Write =

    module ConditionExpression =

        type Condition =
            | AttributeExists of key: String
            | AttributeDoesNotExist of key: String
            | AttributeIsType of key: String * Type
            | BeginsWith of key: String * value: String
            | Contains of key: String * value: String
            | StringEquals of key: String * value: String
            | StringIn of key: String * String list
            | NumberEquals of key: String * value: Decimal
            | NumberLessThan of key: String * value: Decimal
            | NumberLessThanOrEqualTo of key: String * value: Decimal
            | NumberGreaterThan of key: String * value: Decimal
            | NumberGreaterThanOrEqualTo of key: String * value: Decimal
            | NumberBetwixt of key: String * Decimal * Decimal
            | NumberIn of key: String * Decimal list
            | Not of Condition

        and Type =
            | String
            | StringSet
            | Number
            | NumberSet
            | Binary
            | BinarySet
            | Boolean
            | Null
            | List
            | Map

        and ConditionExpression = ConditionExpression of Condition * (BoolOperator * ConditionExpression) list

        and BoolOperator =
            | And
            | Or

        let private boolOperatorToExpression =
            function
            | And -> "AND"
            | Or -> "OR"

        let private getAttributeName = ExpressionAttributeName.getExpAttrName

        let private getAttribute attributes key value format =
            let attributeName, attributes = getAttributeName attributes value
            sprintf format key attributeName, attributes

        let csvAttributes attributes key values valueF format =
            let folder (names, attrs) value =
                let name, attrs = getAttributeName attrs (valueF value)
                name :: names, attrs

            let names, attributes = List.fold folder ([], attributes) values
            sprintf format key (String.Join(",", names)), attributes

        let rec private conditionToString attributes =
            function
            | AttributeExists key -> sprintf "attribute_exists(%s)" key, attributes

            | AttributeDoesNotExist key -> sprintf "attribute_not_exists(%s)" key, attributes

            | AttributeIsType (key, type_) ->
                let typeToString =
                    function
                    | Type.String -> "S"
                    | Type.StringSet -> "SS"
                    | Type.Number -> "N"
                    | Type.NumberSet -> "NS"
                    | Type.Binary -> "B"
                    | Type.BinarySet -> "BS"
                    | Type.Boolean -> "BOOL"
                    | Type.Null -> "NULL"
                    | Type.List -> "L"
                    | Type.Map -> "M"

                getAttribute attributes key (ScalarString <| typeToString type_) "attribute_type(%s, %s)"

            | BeginsWith (key, value) -> getAttribute attributes key (ScalarString value) "begins_with(%s, %s)"

            | Contains (key, value) -> getAttribute attributes key (ScalarString value) "contains(%s, %s)"

            | StringEquals (key, value) -> getAttribute attributes key (ScalarString value) "%s = %s"
            | NumberEquals (key, value) -> getAttribute attributes key (ScalarDecimal value) "%s = %s"
            | NumberLessThan (key, value) -> getAttribute attributes key (ScalarDecimal value) "%s < %s"
            | NumberLessThanOrEqualTo (key, value) -> getAttribute attributes key (ScalarDecimal value) "%s <= %s"
            | NumberGreaterThan (key, value) -> getAttribute attributes key (ScalarDecimal value) "%s > %s"
            | NumberGreaterThanOrEqualTo (key, value) -> getAttribute attributes key (ScalarDecimal value) "%s >= %s"
            | NumberBetwixt (key, start, end_) ->
                let startAttributeName, attributes =
                    getAttributeName attributes (ScalarDecimal start)

                let endAttributeName, attributes =
                    getAttributeName attributes (ScalarDecimal end_)

                sprintf "%s between %s and %s" key startAttributeName endAttributeName, attributes
            | StringIn (key, values) -> csvAttributes attributes key values ScalarString "%s IN (%s)"
            | NumberIn (key, values) -> csvAttributes attributes key values ScalarDecimal "%s IN (%s)"
            | Not c ->
                let s, attributes = conditionToString attributes c
                sprintf "NOT %s" s, attributes

        let rec buildConditionExpression (ConditionExpression (kc, additionalConditions)) attributes =
            let init = conditionToString attributes kc

            let folder (acc, attributes) (operator, kce) =
                let exp, attributes = buildConditionExpression kce attributes

                let bool = boolOperatorToExpression operator
                sprintf "%s %s (%s)" acc bool exp, attributes

            List.fold folder init additionalConditions

        let catch =
            function
            | Choice1Of2 _ -> Ok()
            | Choice2Of2 (e: exn) when (e :? AggregateException
                                        && e.InnerException :? ConditionalCheckFailedException) -> Ok()
            | Choice2Of2 (e: exn) when (e :? AggregateException) -> Error [ OperationError e.InnerException ]
            | Choice2Of2 (e: exn) -> Error [ OperationError e ]


    module UpdateExpression =

        type UpdateExpression =
            | Set of key: String * value: AttrValue
            | Increment of key: String * qty: Int32
            | Remove of key: String
        
        let private getAttributeName = ExpressionAttributeName.getExpAttrName

        let private joinSpace (l: String list) = String.Join(" ", l)

        let private joinComma (l: String list) = String.Join(",", l)

        let buildUpdateExpression updateExpressions attributes =
            let folder (sets, removes, attributes) =
                function
                | Increment (key, qty) ->
                    let attributeName, attributes = getAttributeName attributes (ScalarInt32 qty)
                    let attributeNameDefault, attributes = getAttributeName attributes (ScalarInt32 0)

                    let exp =
                        match sets with
                        | [] -> sprintf "SET %s = %s + if_not_exists(%s, %s)" key attributeName key attributeNameDefault
                        | _ -> sprintf "     %s = %s + if_not_exists(%s, %s)" key attributeName key attributeNameDefault

                    exp :: sets, removes, attributes
                | Set (key, value) ->
                    let attributeName, attributes = getAttributeName attributes value

                    let exp =
                        match sets with
                        | [] -> sprintf "SET %s = %s" key attributeName
                        | _ -> sprintf "%s = %s" key attributeName

                    exp :: sets, removes, attributes
                | Remove key ->
                    let exp =
                        match removes with
                        | [] -> sprintf "REMOVE %s" key
                        | _ -> sprintf "%s" key

                    sets, exp :: removes, attributes

            let sets, removes, attributes =
                List.fold folder ([], [], attributes) updateExpressions

            let join l =
                if not <| List.isEmpty l then List.rev l |> joinComma |> Some else None

            let exp =
                [ sets; removes ]
                |> List.choose join
                |> joinSpace

            exp, attributes

    let putItems (client: AmazonDynamoDBClient) tableName =

        let (|Success|Retry|GiveUp|) =
            function
            | u: Dictionary<_, _>, a when u.Count > 0 && a > 3 -> GiveUp
            | u, a when u.Count > 0 -> Retry(u, a * 100)
            | _ -> Success

        let rec write attempt items: Async<Result<Unit, DynamoDbError list>> =
            new BatchWriteItemRequest(RequestItems = items)
            |> client.BatchWriteItemAsync
            |> Async.AwaitTask
            |> Async.Catch
            |> Async.map DynamoDbError.handleAsyncError
            |> AsyncResult.bind (fun r ->
                match r.UnprocessedItems, attempt with
                | Success -> AsyncResult.retn ()
                | GiveUp -> Async.retn (Error [ UnprocessedItemsError ])
                | Retry (unprocessedItems, sleep) ->
                    Async.Sleep sleep
                    |> Async.map Ok
                    |> AsyncResult.bind (fun _ -> write (attempt + 1) unprocessedItems))

        List.map
            (AttrMapping.mapAttrsToDictionary
             >> fun a -> new PutRequest(Item = a)
             >> fun r -> new WriteRequest(PutRequest = r))
        >> fun reqs -> [ tableName, ResizeArray reqs ]
        >> dict
        >> Dictionary<string, ResizeArray<WriteRequest>>
        >> write 0

    let deleteItem (client: AmazonDynamoDBClient) tableName fields =
        new DeleteItemRequest(tableName, AttrMapping.mapAttrsToDictionary fields)
        |> client.DeleteItemAsync
        |> Async.AwaitTask
        |> Async.Catch
        |> Async.map
            (DynamoDbError.handleAsyncError
             >> Result.map ignore)

    module BuildAttr =

        let optional name attr =
            function
            | Some s -> [ Attr(name, attr s) ]
            | None -> []

        let setString name =
            function
            | h :: t -> [ Attr(name, NonEmptyList(h, t) |> SetString) ]
            | _ -> []

        let setDecimal name =
            function
            | h :: t -> [ Attr(name, NonEmptyList(h, t) |> SetDecimal) ]
            | _ -> []

        let string name s =
            if String.IsNullOrEmpty s then [] else [ Attr(name, ScalarString s) ]

        let docList name =
            function
            | h :: t -> [ Attr(name, DocList(h :: t)) ]
            | _ -> []

    module BuildAttrValue =

        let optionToNull attr =
            function
            | Some s -> attr s
            | None -> ScalarNull

[<AbstractClass>]
type Write private () =

    static member PutItem(client: AmazonDynamoDBClient, tableName, fields, ?conditionExpression) =

        match conditionExpression
              |> Option.map (fun ce -> Write.ConditionExpression.buildConditionExpression ce []) with
        | Some (exp, attrs) ->
            new PutItemRequest(tableName,
                               AttrMapping.mapAttrsToDictionary fields,
                               ConditionExpression = exp,
                               ExpressionAttributeValues = AttrMapping.buildAttrDictionary attrs)
        | None -> new PutItemRequest(tableName, AttrMapping.mapAttrsToDictionary fields)

        |> client.PutItemAsync
        |> Async.AwaitTask
        |> Async.Catch
        |> Async.map Write.ConditionExpression.catch

    static member UpdateItem(client: AmazonDynamoDBClient, tableName, key, updateExpression, ?conditionExpression) =

        let updateExp, attributes =
            Write.UpdateExpression.buildUpdateExpression updateExpression []

        match conditionExpression
              |> Option.map (fun ce -> Write.ConditionExpression.buildConditionExpression ce attributes) with
        | Some (exp, attributes) ->
            new UpdateItemRequest(tableName,
                                  AttrMapping.mapAttrsToDictionary key,
                                  null,
                                  UpdateExpression = updateExp,
                                  ExpressionAttributeValues = AttrMapping.buildAttrDictionary attributes,
                                  ConditionExpression = exp)
        | None ->
            new UpdateItemRequest(tableName,
                                  AttrMapping.mapAttrsToDictionary key,
                                  null,
                                  UpdateExpression = updateExp,
                                  ExpressionAttributeValues = AttrMapping.buildAttrDictionary attributes)

        |> client.UpdateItemAsync
        |> Async.AwaitTask
        |> Async.Catch
        |> Async.map Write.ConditionExpression.catch


module Read =


    type AttrReader<'a> = AttrReader of (Map<string, A> -> 'a)

    module AttrReader =

        let run (AttrReader f) a = f a

        let retn a = AttrReader(fun _ -> a)

        let bind f ra =
            AttrReader(fun m ->
                run ra m
                |> f
                |> fun rb -> run rb m)

        let map f r = AttrReader(run r >> f)

    // let apply f r =
    //   AttrReader (fun a -> run r a |> run f a)


    module private AttrReaderResult =

        let retn a = Ok a |> AttrReader.retn

        let map f =
            Result.map f
            |> fun f r -> AttrReader(AttrReader.run r >> f)

        let bind f r =
            AttrReader(fun b ->
                AttrReader.run r b
                |> Result.bind (fun a -> AttrReader.run (f a) b))

        // let apply f r =
        //   AttrReader <| fun a ->
        //     let fa = AttrReader.run f a
        //     let fb = AttrReader.run r a
        //     match fa, fb with
        //     | Ok a, Ok b -> Ok (a b)
        //     | Error e, _ -> Error e
        //     | _, Error e -> Error e

        let apply f readerResultA =
            let newReader m =
                let resultF = AttrReader.run f m
                let resultA = AttrReader.run readerResultA m

                let resultB =
                    match resultF, resultA with
                    | Ok f, Ok a -> Ok(f a)
                    | Error e1, Error e2 -> Error(e1 @ e2)
                    | Error e1, _ -> Error e1
                    | _, Error e2 -> Error e2

                resultB

            AttrReader newReader

        let mapError e f =
            AttrReader(AttrReader.run f >> Result.mapError e)


    type AttrReaderResultBuilder() =
        member __.Return(x) = AttrReaderResult.retn x
        member __.ReturnFrom(m: AttrReader<Result<'a, 'b>>) = m
        member __.Bind(f, r) = AttrReaderResult.bind r f
        member __.Zero() = __.Return()

    let attrReaderResult = new AttrReaderResultBuilder()


    let private toMap d = Seq.map (|KeyValue|) d |> Map.ofSeq

    let private traverseResult f list =
        let folder head tail =
            f head
            |> Result.bind (fun h -> tail |> Result.bind (fun t -> Ok(h :: t)))

        List.foldBack folder list (Ok [])

    let private ifSome f =
        function
        | Some x -> f x |> Result.map Some
        | None -> Ok None

    let private required key =
        function
        | Some x -> Ok x
        | None -> Error [ MissingAttributeError(sprintf "could not find attr %s" key) ]

    let req key typ =
        AttrReader(Map.tryFind key >> required key >> Result.map typ)


    let opt key typ =
        AttrReader(Map.tryFind key >> Option.map typ >> Ok)

    let (<!>) = AttrReaderResult.map
    let (<*>) = AttrReaderResult.apply
    let (>>=) r f = AttrReaderResult.bind f r

    /// pass ARR into map
    let (>-) r f = AttrReaderResult.map f r

    /// pass ARR into Result returing f (e.g. Parse.*)
    let (>->) r f = AttrReader.map (Result.bind f) r

    /// pass ARR option into map
    let (?>-) r f = r >- Option.map f

    /// pass ARR option into Result returing f (e.g. Parse.*)
    let (?>->) r f = r >-> ifSome f

    /// pass ARR list into map
    let (@>-) r typ = r >- List.map typ

    /// pass ARR list into Result returing f (e.g. Parse.*)
    let (@>->) r (typ, f) = r >-> (List.map typ >> traverseResult f)

    /// pass ARR option list into map
    let (?@>-) r f = r >- Option.map (List.map f)

    /// pass ARR option list into Result returing f (e.g. Parse.*)
    let (?@>->) r (typ, f) =
        r >-> ifSome (List.map typ >> traverseResult f)

    /// pass ARR list option into map
    let (@?>-) r f = r >- List.map (Option.map f)

    /// pass ARR list option into Result returing f (e.g. Parse.*)
    let (@?>->) r (typ, f) =
        r
        >-> (List.map (Option.map typ)
             >> traverseResult (ifSome f))


    module Query =

        module KeyConditionExpression =

            type KeyCondition =
                | StringBeginsWith of key: String * value: String
                | StringEquals of key: String * value: String
                | NumberEquals of key: String * value: Decimal
                | NumberLessThan of key: String * value: Decimal
                | NumberLessThanOrEqualTo of key: String * value: Decimal
                | NumberGreaterThan of key: String * value: Decimal
                | NumberGreaterThanOrEqualTo of key: String * value: Decimal
                | NumberBetwixt of key: String * value: Decimal * Decimal

            and KeyConditionExpression = KeyConditionExpression of KeyCondition * (BoolOperator * KeyConditionExpression) list

            and BoolOperator =
                | And
                | Or

            let private boolOperatorToExpression =
                function
                | And -> "AND"
                | Or -> "OR"

            let getAttributeName = ExpressionAttributeName.getExpAttrName

            let private getAttribute attributes key value format =
                let attributeName, attributes = getAttributeName attributes value
                sprintf format key attributeName, attributes

            let private keyConditionToString attributes =
                function
                | StringBeginsWith (key, value) ->
                    getAttribute attributes key (ScalarString value) "begins_with(%s, %s)"
                | StringEquals (key, value) -> getAttribute attributes key (ScalarString value) "%s = %s"
                | NumberEquals (key, value) -> getAttribute attributes key (ScalarDecimal value) "%s = %s"
                | NumberLessThan (key, value) -> getAttribute attributes key (ScalarDecimal value) "%s < %s"
                | NumberLessThanOrEqualTo (key, value) -> getAttribute attributes key (ScalarDecimal value) "%s <= %s"
                | NumberGreaterThan (key, value) -> getAttribute attributes key (ScalarDecimal value) "%s > %s"
                | NumberGreaterThanOrEqualTo (key, value) ->
                    getAttribute attributes key (ScalarDecimal value) "%s >= %s"
                | NumberBetwixt (key, start, end_) ->
                    let startAttributeName, attributes =
                        getAttributeName attributes (ScalarDecimal start)

                    let endAttributeName, attributes =
                        getAttributeName attributes (ScalarDecimal end_)

                    sprintf "%s between %s and %s" key startAttributeName endAttributeName, attributes

            let rec buildKeyConditionExpression (KeyConditionExpression (kc, additionalConditions)) attributes =
                let init = keyConditionToString attributes kc

                let folder (acc, attributes) (operator, kce) =
                    let exp, attributes =
                        buildKeyConditionExpression kce attributes

                    let bool = boolOperatorToExpression operator
                    sprintf "%s %s (%s)" acc bool exp, attributes

                List.fold folder init additionalConditions


    [<AbstractClass>]
    type Read private () =
        static member Query(client: AmazonDynamoDBClient,
                            tableName,
                            reader,
                            kce,
                            ?indexName,
                            ?limit,
                            ?scanIndexForward,
                            ?exclusiveStartKey,
                            ?consistentRead) =
            // not yet supported:
            // ProjectionExpression
            // FilterExpression

            let expression, attrs =
                Query.KeyConditionExpression.buildKeyConditionExpression kce []

            let setOptionalProperty obj prop setter =
                if Option.isSome prop then setter obj prop.Value

            let queryRequest =
                QueryRequest
                    (tableName,
                     KeyConditionExpression = expression,
                     ExpressionAttributeValues = AttrMapping.buildAttrDictionary attrs,
                     ScanIndexForward = defaultArg scanIndexForward true,
                     IndexName = defaultArg indexName null,
                     ExclusiveStartKey =
                         (defaultArg exclusiveStartKey []
                          |> AttrMapping.mapAttrsToDictionary))

            setOptionalProperty queryRequest limit (fun qr v -> qr.Limit <- v)
            setOptionalProperty queryRequest consistentRead (fun qr v -> qr.ConsistentRead <- v)

            queryRequest
            |> client.QueryAsync
            |> Async.AwaitTask
            |> Async.Catch
            |> Async.map
                (DynamoDbError.handleAsyncError
                 >> Result.map (fun r -> Seq.map toMap r.Items |> List.ofSeq)
                 >> Result.bind (traverseResult (AttrReader.run reader)))

    let getItem (client: AmazonDynamoDBClient) tableName reader fields =
        new GetItemRequest(tableName, AttrMapping.mapAttrsToDictionary fields)
        |> client.GetItemAsync
        |> Async.AwaitTask
        |> Async.Catch
        |> Async.map
            (DynamoDbError.handleAsyncError
             >> Result.map (fun r -> toMap r.Item)
             >> Result.bind (AttrReader.run reader))

    let tryGetItem (client: AmazonDynamoDBClient) tableName (reader: AttrReader<Result<'a, list<DynamoDbError>>>) fields =
        new GetItemRequest(tableName, AttrMapping.mapAttrsToDictionary fields)
        |> client.GetItemAsync
        |> Async.AwaitTask
        |> Async.Catch
        |> Async.map
            (DynamoDbError.handleAsyncError
             >> Result.bind (fun r ->
                 toMap r.Item
                 |> fun m ->
                     if Map.isEmpty m
                     then Ok None
                     else AttrReader.run reader m |> Result.map Some))

    let doesItemExist (client: AmazonDynamoDBClient) tableName fields =
        new GetItemRequest(tableName, AttrMapping.mapAttrsToDictionary fields)
        |> client.GetItemAsync
        |> Async.AwaitTask
        |> Async.Catch
        |> Async.map
            (DynamoDbError.handleAsyncError
             >> Result.map (fun r -> toMap r.Item |> Map.isEmpty |> not))



    module Attribute =

        let string (a: A) = a.S

        let bool (a: A) = a.BOOL

        let number (a: A) = a.N

        let docMap (a: A) = toMap a.M

        let docList (a: A) = List.ofSeq a.L

        let setString (a: A) = Set.ofSeq a.SS

        let isNull (a: A) = a.NULL

        let nullOr f (a: A) = if a.NULL then None else Some(f a)


    module Parse =

        let private fromByRef e =
            function
            | true, x -> Ok x
            | _ -> Error [ ParseError e ]

        let guid (s: String) =
            Guid.TryParse s
            |> fromByRef (sprintf "could not parse %s as guid" s)

        let dateTime (s: String) =
            DateTime.TryParse s
            |> fromByRef (sprintf "could not parse %s as date" s)

        let decimal (s: String) =
            Decimal.TryParse s
            |> fromByRef (sprintf "could not parse %s as decimal" s)

        let int (s: String) =
            Int32.TryParse s
            |> fromByRef (sprintf "could not parse %s as integer" s)



/// todo:
/// parameterize error type (not hardcoded string)
/// update
/// page results
/// filter expressions
/// global/local secondary indexes
//√ key condition expression with 'or' logic


module Example =

    open Read

    module A = Attribute
    module P = Parse
    module R = Read.AttrReader

    type Inny = { X: String; Y: Decimal }

    let buildInny x y = { X = x; Y = y }

    let innyReader =
        buildInny
        <!> (req "x" A.string)
        <*> (req "y" A.string >-> P.decimal)

    type Outty =
        { F: String
          G: DateTime
          I: Inny
          H: Inny list
          J: Decimal list
          K: String list
          L: DateTime list option }

    let buildOutty f g i h j k l =
        { F = f
          G = g
          I = i
          H = h
          J = j
          K = k
          L = l }


    let outtyReader =
        buildOutty
        <!> (req "f" A.string)
        <*> (req "g" A.string >-> P.dateTime)
        <*> (req "i" A.docMap >-> R.run innyReader)
        <*> (req "h" A.docList
             @>-> (A.docMap, R.run innyReader))
        <*> (req "j" A.docList @>-> (A.number, P.decimal))
        <*> (req "k" A.docList @>- A.string)
        <*> (opt "l" A.docList ?@>-> (A.string, P.dateTime))
