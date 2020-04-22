# DynamoDb.Ok

---

## What is DynamoDb.Ok?

DynamoDb.Ok is a library that wraps the AWS DynamoDB .net client in F#.

## Why use DynamoDb.Ok?

DynamoDb.Ok aims to provide client interface that is more functionally idiomatic.
It makes use of the `Result` type, hence the suffix `Ok`.

The AWS client library is not broken but it's trivial to make mistakes.
DynamoDb.Ok wraps AWS client with a type system that makes illegal states unreprestable,
thereby catching _write_ mistakes at compile time.

For _reads_, DynamoDb.Ok provides a mechanism based on the `Reader monad` to fluently hydrate
domain types from query results. By providing parsers that return a
`Result` you can deal with errors using the `ROP`.

---

<div class="row row-cols-1 row-cols-md-2">
  <div class="col mb-4">
    <div class="card h-100">
      <div class="card-body">
        <h5 class="card-title">Tutorials</h5>
        <p class="card-text">Takes you by the hand through a series of steps to create your first thing. </p>
      </div>
      <div class="card-footer text-right   border-top-0">
        <a href="{{siteBaseUrl}}/Tutorials/Getting_Started.html" class="btn btn-primary">Get started</a>
      </div>
    </div>
  </div>
  <div class="col mb-4">
    <div class="card h-100">
      <div class="card-body">
        <h5 class="card-title">How-To Guides</h5>
        <p class="card-text">Guides you through the steps involved in addressing key problems and use-cases. </p>
      </div>
      <div class="card-footer text-right   border-top-0">
        <a href="{{siteBaseUrl}}/How_Tos/Doing_A_Thing.html" class="btn btn-primary">Learn Usecases</a>
      </div>
    </div>
  </div>
  <div class="col mb-4 mb-md-0">
    <div class="card h-100">
      <div class="card-body">
        <h5 class="card-title">Explanations</h5>
        <p class="card-text">Discusses key topics and concepts at a fairly high level and provide useful background information and explanation..</p>
      </div>
      <div class="card-footer text-right   border-top-0">
        <a href="{{siteBaseUrl}}/Explanations/Background.html" class="btn btn-primary">Dive Deeper</a>
      </div>
    </div>
  </div>
  <div class="col">
    <div class="card h-100">
      <div class="card-body">
        <h5 class="card-title">Api Reference</h5>
        <p class="card-text">Contain technical reference for APIs.</p>
      </div>
      <div class="card-footer text-right   border-top-0">
        <a href="{{siteBaseUrl}}/Api_Reference/DynamoDb.Ok/DynamoDb.Ok.html" class="btn btn-primary">Read Api Docs</a>
      </div>
    </div>
  </div>
</div>
