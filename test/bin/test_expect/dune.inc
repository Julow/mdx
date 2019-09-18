
(alias
  (name runtest)
  (deps (:x dir1.md) (:y dir1.md.expected) (package mdx))
  (action
    (progn
      (run ocaml-mdx test --force-output --root "../../../bin" dir1.md)
      (diff? %{y} %{x}.corrected))
  )
)

(alias
  (name runtest)
  (deps (:x ellipsis_updates.md) (:y ellipsis_updates.md.expected) (package mdx))
  (action
    (progn
      (run ocaml-mdx test --force-output  ellipsis_updates.md)
      (diff? %{y} %{x}.corrected))
  )
)

(alias
  (name runtest)
  (deps (:x empty_lines.md) (:y empty_lines.md.expected) (package mdx))
  (action
    (progn
      (run ocaml-mdx test --force-output  empty_lines.md)
      (diff? %{y} %{x}.corrected))
  )
)

(alias
  (name runtest)
  (deps (:x labels.md) (:y labels.md.expected) (package mdx))
  (action
    (progn
      (run ocaml-mdx test --force-output  labels.md)
      (diff? %{y} %{x}.corrected))
  )
)

(alias
  (name runtest)
  (deps (:x section.md) (:y section.md.expected) (package mdx))
  (action
    (progn
      (run ocaml-mdx test --force-output -s Testing section.md)
      (diff? %{y} %{x}.corrected))
  )
)

(alias
  (name runtest)
  (deps (:x sync_to_md.md) (:y sync_to_md.md.expected) (package mdx))
  (action
    (progn
      (run ocaml-mdx test --force-output --direction=to-md sync_to_md.md)
      (diff? %{y} %{x}.corrected))
  )
)

(alias
  (name runtest)
  (deps (:x trailing.md) (:y trailing.md.expected) (package mdx))
  (action
    (progn
      (run ocaml-mdx test --force-output  trailing.md)
      (diff? %{y} %{x}.corrected))
  )
)
