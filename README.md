# tplyr2
Currently just an idea repo to POC what a new Tplyr framework might be

Below is an idea list of significant changes from Tplyr's current architecture:

- Framework is rewritten in data.table to focus on performance
- table object is uptimately a spec - and data itself is provided to `build()` and not `tplyr_table()`.
   - All the validators end up moving to the build process, so we could realistically build up a check queue based on the table settings provided. So basically:
      - Construct a table and add layers
      - For each layer, any compliance check (i.e. variable exists, variable type is accurate, etc.) that can't be evaluated without the input dataframe is added to a queue of checks
      - Before build starts executing, the queue of checks runs
   - If the table object isn't holding data, then the use of environments isn't as significant. Furthermore, data.table allows modification by reference when necessary so memory can be optimized. This would reduce some unexpected sideeffects. 
- Remove the concept of treat_var and take only use the cols parameter
- Recursive implementation of nested count layers
  - Allow more than inner and outer layer, and there's an opportunity for efficiency increases here as well
- Better support for analysis results data
  - Pull from concepts of ardis, but try to build in better support for the actual CDISC standard
  - Allow for output of ARD, and then build from intake of ARD and a table spec
  - There are additionally parts of our numeric data that the structure can just be improved. For example, in the descriptive statistics layers, I probably could have avoided some complex transpositions and assembly of summary variables by just not transposing in the first place.
- Redesign concept of sorting variables
  - This is the single slowest part of Tplyr and much of the structure could be rethought
  - Sorting might better be handled during numeric summaries, trying to avoid as much transposition as possible
- Change the way execution of build works
  - We eval in the layer environments and this introduces a lot of complexity. Clean this up to have a more straightforward functional approach
- Error handling generally has to be improved. All traces of {assertthat} need to be ripped out, and messages can get distorted through the `tryCatch()` that happens during build.
- Try to design a framework for an open ended analyze function
  - Great concept that's been requested, but some of the rigidity in Tplyr makes this hard to implement.
- Unify string formatting into 1 application function (i.e. `Tplyr::apply_formats()`)
- By not building on environments, avoid weird side effects of namespace issues like [this issue](https://github.com/atorus-research/Tplyr/issues/154)
