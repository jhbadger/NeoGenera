# Flavors

Flavors is the object system of the Lisp Machine and of Symbolics Genera: objects are **instances of flavors**, and
you act on them by **sending messages**. NeoGenera supports it as a layer over CLOS, so the two live side by side.
A flavor is a CLOS class, and a flavor instance is an ordinary instance: `typep`, `make-instance`, `describe`,
the Inspector, `print-object` and generic functions specialized on the flavor all work. Flavors does not replace CLOS.

## A first flavor

```lisp
(defflavor ship ((x-position 0.0) (y-position 0.0) (x-velocity 2.0) (y-velocity 3.0) mass)
  ()
  :gettable-instance-variables :settable-instance-variables :inittable-instance-variables)

(defmethod (ship :speed) ()
  (sqrt (+ (* x-velocity x-velocity) (* y-velocity y-velocity))))

(defmethod (ship :advance) (dt)
  (incf x-position (* x-velocity dt))
  (incf y-position (* y-velocity dt)))

(setq s (make-instance 'ship :x-position 3.4))
(send s :speed)            ; => 3.605551275463989
(send s :set-mass 10)      ; settable variables get a :set-NAME message
(send s :x-position)       ; gettable variables get a message named for them
```

[defflavor](fn:defflavor) takes a name, a list of instance variables (a symbol, or `(symbol default-form)`), the
component flavors it is built from, and options. A method is written with [defmethod](fn:defmethod), whose name is
a list, `(flavor message)` or `(flavor type message)`; an ordinary `defmethod` name (a symbol, or `(setf name)`)
still defines a CLOS method. Inside a method the instance variables are ordinary variables, so `setq`, `setf` and
`incf` change them, and a parameter with the same name hides the variable. `self` is the instance.
The `defflavor` must be evaluated before the methods that use it, because the methods are compiled against its
instance variables.

[send](fn:send) sends a message; [lexpr-send](fn:lexpr-send) takes a list as the last argument.
[send-if-handles](fn:send-if-handles) and [lexpr-send-if-handles](fn:lexpr-send-if-handles) return `nil` for a message
the object does not understand, [operation-handled-p](fn:operation-handled-p) asks, and
[get-handler-for](fn:get-handler-for) returns a function that sends the message. A message nobody handles goes to the
flavor's `:default-handler` (a function of the message and its arguments, with the special variable
[self](fn:self) bound), or signals [unclaimed-message](fn:unclaimed-message), whose object and arguments are
[unclaimed-message-object](fn:unclaimed-message-object) and
[unclaimed-message-arguments](fn:unclaimed-message-arguments). Sending to a function calls it with the message and
the arguments. Unlike Genera, a flavor instance cannot itself be called with `funcall`: use `send`.

Outside a method `self` is the special variable that `send` binds, so a function called from a method can use it, and
[declare-flavor-instance-variables](fn:declare-flavor-instance-variables) makes the instance variables of a flavor
visible in its body: `(defun speed-of-self () (declare-flavor-instance-variables (ship) (sqrt ...)))`. Inside a
method `self` is a lexical variable, so closures keep it.

## Making instances

`(make-instance 'ship :mass 5)` and [instantiate-flavor](fn:instantiate-flavor) take an *init plist*. Its keys must be
init keywords: the names of the flavor's inittable instance variables, the keywords of `:init-keywords` options, and
the keys of `:default-init-plist` options (whose values are forms, evaluated at each instantiation). Any other key is
an error unless `:allow-other-keys t` is passed. [flavor-allowed-init-keywords](fn:flavor-allowed-init-keywords) and
[flavor-allows-init-keyword-p](fn:flavor-allows-init-keyword-p) report them. After the variables are set from the plist
or their default forms, the `:init` message is sent with the plist, so `(defmethod (ship :after :init) (plist) ...)`
finishes the job. A variable with no default form and no init keyword is unbound until it is set.

## Defflavor options

`:gettable-instance-variables`, `:settable-instance-variables` and `:inittable-instance-variables` (or
`:initable-instance-variables`) give the messages and keywords described above, each for all the flavor's own
variables if written alone or for the variables listed after it; a settable variable is gettable and inittable too.
`:init-keywords` and `:default-init-plist` are as above, `:required-init-keywords` lists keywords every instantiation
must supply, and `:default-handler` names the function for unhandled messages. `:documentation` is a string.
`:required-instance-variables`, `:required-methods` and `:required-flavors` say what a flavor expects of the flavors it
is mixed into; they are checked when an instance is made. `:included-flavors` names flavors that should be components
of anything that uses this flavor, without a flavor having to list them. `:no-vanilla-flavor` leaves out
[vanilla-flavor](fn:vanilla-flavor). `:method-combination` is described below. `:abstract-flavor` makes instantiating the flavor an error.
`:special-instance-variables`, `:ordered-instance-variables` and `:outside-accessible-instance-variables` are accepted
and do nothing. Redefining a flavor keeps its methods, updates its instances, and changes the flavors built on it.

## Mixing flavors

The components of a flavor are visited depth first, left to right, each flavor before the flavors it is made of; a
flavor that is reached again is ignored, so it stays at its **first** place. If `flavor-1` is made of `flavor-2` and
`flavor-3`, `flavor-2` of `flavor-4` and `flavor-5`, and `flavor-3` of `flavor-4`, the order is `flavor-1`, `flavor-2`,
`flavor-4`, `flavor-5`, `flavor-3`. A flavor from `:included-flavors` that is not a component anywhere is inserted
right after the last flavor that includes it; `vanilla-flavor` is last. [flavor-components](fn:flavor-components) returns the order, which is also the class
precedence list of the flavor's class. [describe-flavor](fn:describe-flavor) prints a flavor's components, variables,
init keywords and messages, and [flavor-instance-p](fn:flavor-instance-p) tests for a flavor instance.

`vanilla-flavor` gives every flavor these messages: `:init`, `:print-self` (stream, depth and slashify; override it to
change how instances print), `:describe` (what `describe` prints), `:which-operations`, `:operation-handled-p`,
`:send-if-handles`, `:get-handler-for`, `:eval-inside-yourself` and `:funcall-inside-yourself`.

## Daemons and method combination

Unlike CLOS, **only the most specific primary method runs**: a method does not call the one it overrides, and there is
no `call-next-method`. The combination called `:daemon` is the default. A `:before` method runs before the primary
one and an `:after` method runs after it; `:before` methods run most specific flavor first and `:after` methods
least specific first, and the message's value is the primary method's. A `:default` method runs only if there is no
primary method.

An `:around` method wraps everything inside it. Its lambda list starts with three special arguments, the continuation,
the mapping table and the original argument list (the message and its arguments), followed by the message's
arguments. It runs the rest of the message with
`(funcall-with-mapping-table continuation mapping-table message arguments...)`, or
[lexpr-funcall-with-mapping-table](fn:lexpr-funcall-with-mapping-table) with the original argument list (the table
itself is not used here). `:inverse-around` methods work the same way but are outside everything, the most basic
flavor outermost. [apply-with-mapping-table](fn:apply-with-mapping-table) is also provided.
[defwhopper](fn:defwhopper) is the cleaner way: the body calls `(continue-whopper args...)` or
`(lexpr-continue-whopper args... list)`, with a lambda list that is just the message's arguments.
[defwrapper](fn:defwrapper) is the older form: `(defwrapper (flavor message) ((arg1 arg2) . body) `(code ,@body))`
returns code that is wrapped around the other methods, with the message's arguments as variables (the pattern's first
element may be a single symbol for all of them). Whoppers and wrappers enclose `:around` methods, most specific flavor
outermost. [undefmethod](fn:undefmethod) removes a method.

Other combinations come from the `:method-combination` option, `(:method-combination (type order message...))`,
where `type` is one of `:progn`, `:or`, `:and`, `:append`, `:nconc`, `:list`, `:max`, `:min`, `:+`, `:case`,
`:daemon-with-or`, `:daemon-with-and` or `:daemon-with-override`, and `order` is `:base-flavor-last` (the most
specific flavor's method first) or `:base-flavor-first`. Without message names it applies to every message. The
methods are then defined with the type in the name, `(defmethod (flavor :or message) ...)`, and are combined across
all the components, the typed methods before the untyped ones: `:or` stops at the first non-`nil` value, `:and` at
the first `nil`, `:progn` returns the last value and `:list` collects all of them. `:daemon-with-or` and
`:daemon-with-and` keep the daemons and run `:or` or `:and` methods before the primary method, which is skipped when
one of them decides; `:daemon-with-override` runs `:override` methods before everything, and the daemons and primary
method only if none returns non-`nil`. With `:case` the first argument is a suboperation:
`(defmethod (flavor :case message suboperation) ...)` handles one suboperation and the untyped method, which receives
the suboperation, handles the rest. The `:set` message of `vanilla-flavor` sends `:set-NAME`. A typed method on a message with
no matching combination is an error when the message is sent.

## Other Lisp Machine functions

`(defmethod (flavor message) function-name)` makes a named function the method (it receives the message first, with
`self` bound). [undefflavor](fn:undefflavor) removes a flavor, [instancep](fn:instancep) tests for an instance,
[symeval-in-instance](fn:symeval-in-instance) and [set-in-instance](fn:set-in-instance) read and write an instance
variable by name, `*all-flavor-names*` lists the flavors, and [funcall-self](fn:funcall-self) and
[lexpr-funcall-self](fn:lexpr-funcall-self) are the obsolete ways to send to `self`.
[property-list-mixin](fn:property-list-mixin) gives a flavor a property list with the messages `:get`, `:getl`,
`:putprop`, `:remprop`, `:push-property`, `:property-list` and `:set-property-list`, and the init keyword
`:property-list`.

## What is not supported

Instances are not funcallable. The `:inverse-list` and `:pass-on` combinations, `:alias-flavor`,
`:run-time-alternatives`, `:instantiation-flavor-function`, `with-self-variables-bound`, `(declare (:self-flavor ..))`,
`si:print-readably-mixin` and the `:break` message are not implemented, and
[compile-flavor-methods](fn:compile-flavor-methods) and [recompile-flavor](fn:recompile-flavor) are accepted but do
nothing, since methods are combined when first used. Instance variables are visible in methods and under
`declare-flavor-instance-variables`, but not as special variables in arbitrary functions, and `:funcall-inside-yourself`
only binds `self`.
