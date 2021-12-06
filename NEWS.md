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
