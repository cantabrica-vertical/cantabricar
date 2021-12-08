app <- ShinyDriver$new("../../")
app$snapshotInit("test-calendar")

app$snapshot()
app$setInputs(calendario_next = "click")
app$snapshot()
app$setInputs(calendario_prev = "click")
app$snapshot()
