1. Use the React DOM diffing algorithm, producing a queue of updates. If a property value is a DOMDependency always
   schedule an update.
2. Now we have the complete VDOM tree since the diffing also rendered components when necessary. We also have the queue
   of updates. The child updates are fully resolved (because the VDOM tree is already complete). Some of the property
   updates might not be.
3. Loop
    1. Try to resolve the property values in property updates.
        * If the value can be resolved, assign it to self and return it to caller.
        * If a cycle is detected, throw an exception in disgust.
        * If the value (transitively) depends on an actual DOM node that hasn't been sufficiently updated in this render
          cycle (i.e. a vertical layout property of a "Rich Text" subtree whose offsetWidth hasn't been updated), return
          some sentinel value to signal "not yet".
    2. Flush all resolved updates to the DOM. If there are none, break.
4. If updates remain, they must be unresolvable. Throw an exception in disgust.
