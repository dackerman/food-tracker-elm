# Elm Food Tracker

_Warning: Major WIP at the moment_

This is a calorie counting / food tracking application written in the
[Elm programming language](https://elm-lang.org). I plan to also write
a [React redux]() version as well and compare the two in a blog post,
since redux takes a lot of ideas from Elm.

## Status

Right now, this isn't really working as anything real - I have a fake
JSON backend using `json-server`, and am hooking it up to a simple
domain model that I've created.

## Main Idea

A food can be of two types: an "ingredient", or a "compound food".

An ingredient has calories and macros directly defined with a quantity
(i.e. 100 grams = 52 calories, 3g protein, 10g carbs, 1g fat).

A compound food is a grouping of ingredients and potentially other
compound foods. The calories and macros are implicitly the sum of the
parts. This covers the case where you might have a delicious Chipotle
burrito bowl, and you choose all the ingredients separately, but you
want it to really count as one "food".

This strategy also works really well when you cook for yourself,
because you can enter in ingredients you use separately, and then
combine them to create new foods. It works because if you're like me,
you reuse ingredients all the time (like butter, 93% lean ground beef,
chicken breast, etc). If you then weigh each ingredient, you can get a
really accurate nutritional count of the whole batch.
