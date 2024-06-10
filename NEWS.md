---

# disprofas 0.2.0

- The deprecated ggplot2 function aes_string() has been replaced by aes().
  Variables containing column names as a character vector (var_name) are
  now called via .data[[var_name]]. In addition, 'size' has been replaced by
  'linewidth' when used in the geom_line() function.
- Variables have been renamed to make them snake_case_like. Visible binding
  for global variables was added.
- Since providing a numeric vector to theme(legend.position) has been
  deprecated it has been replaced by theme(legend.position = "inside",
  legend.position.inside = c(...)).
- Examples have been slimmed down and the execution of some of the example
  code is prevented to reduce the execution time of examples.
- Tests were complemented to reach 100% coverage. On the other hand, some
  redundant tests were removed.
- Cyclomatic complexity was reduced by the creation of the new function
  check_point_location().
- The new function get_T2_one() for the one-sample T2 test was added. The 
  function get_hotellings() performing the two-sample T2 test was renamed
  to get_T2_two(). Both functions calculate corresponding confidence intervals,
  i.e. the get_T2_two() function was complemented accordingly.
- An additional parameter for the option to remove NA cases was added to
  the functions mztia(), get_T2_one() and get_T2_two(). The treatment of
  NA/NaN cases by the get_profile_portion() function was amended. The
  bootstrap_f2() and the mimcr() functions now check for the presence of
  NA/NaN values in the provided data frames and halt execution if any are
  found with the recommendation to impute the missing data. The functions
  get_f1() and get_f2(), and thus also the functions f1() and f2(), return a
  message if the data frame handed in contains any NAs/NaNs.
- The parameter 'nsf' (number of significant figures) was added to the 
  functions with the 'bounds' parameter, i.e. bootstrap_f2(), mimcr(), f1(), 
  f2() and get_profile_portion(). The 'nsf' parameter allows to control how 
  the test values are rounded before they are compared with the limits set by
  the 'bounds' parameter. For example, by setting the 'nsf' value for the
  comparison with the upper bound of 85% to 2, an observed test value of
  85.025% is rounded to two significant figures before the comparison takes
  place. Thus, the test value is evaluated as not being greater than the limit.
- The complete documentation was reviewed and harmonised, e.g. by the sharing
  of specific text elements between the various function descriptions.

---

# disprofas 0.1.3

- Made test parameters of function gep_by_nera more restrictive in order to
  throw a warning on testing.

---

# disprofas 0.1.2

- Example from unexported function was removed.
- Examples that were wrapped by \\dontrun{} although they are executable in
  < 5 sec are now unwrapped.
- Errors from examples that deliberately produce errors are caught by tryCatch()
  and are no longer wrapped by \\dontrun{}.
- The package T2EQ that is used in three examples is listed under 'Suggests'.
  Furthermore, the examples using a function or data sets from this package are
  now wrapped by if(requireNamespace("T2EQ")){} rather than by \\dontrun{}.

---

# disprofas 0.1.1

- CRAN submission
- Exclusively use secure links
- Correct spelling errors

---

# disprofas 0.1.0

- Add further tests
- Add further and more illustrative examples
- Review documentation, correct spelling errors and assess all links
- Tidy UP the DESCRIPTION
- Replace for loops with apply family functions
- Harmonise parameters between functions

---

# disprofas 0.1.0.900x
