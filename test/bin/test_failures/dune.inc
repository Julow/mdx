
(rule
  (targets both_prelude.md.actual)
  (action
    (with-outputs-to %{targets}
      (system "%{bin:ocaml-mdx} test --prelude=a --prelude-str=b %{dep:both_prelude.md} || echo"))))

(alias
  (name runtest)
  (action
    (diff both_prelude.md.actual both_prelude.md.expected)))

(rule
  (targets part_not_found.md.actual)
  (action
    (with-outputs-to %{targets}
      (system "%{bin:ocaml-mdx} test --direction=to-md %{dep:part_not_found.md} || echo"))))

(alias
  (name runtest)
  (action
    (diff part_not_found.md.actual part_not_found.md.expected)))
