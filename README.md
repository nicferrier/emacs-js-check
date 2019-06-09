More and more it seems I want JS to be typed.

I should probably switch to Typescript.

But I still need a tool to run the compile constantly and identify
issues and mark them in Emacs.

So that's what this is.

Yes, it does not use flymake. What's the point? It's pretty easy to do
this. So this is no depends, except the js depends to do the checking.


## What are the depends?

eslint is the *compiler* I'm currently using - yep, just a linter
really. That's what I actually want. Variable names wrong, commas and
colons and stuff.

You can install eslint like this:

```
npm install -g eslint
```

that's all you need.

## Starting in a source buffer

Just do:

```
M-x js-check-init
```

and it will start.

You can of course, put that in your js buffer hook.


## Other stuff

I couldn't resist throwing in something that calcs the indent in the
file and sets it appropriately.

Apparently there is a lot of debate about what the correct indentation
is for js.

