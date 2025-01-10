# NEXT STEP

At the bottom of `MondrianSheets.R`, create an `Excelable` using an `ItemSet`, and create functions so you can add an item as a terminal node and a row/column node.

# Items

Items are defined structures. They can have 

# Planning

## Adding Layout

When the `ItemSet` is combined with a layout plan, then everything required is present to be printed.

Suppose we have a Layout called MondrianSheets, where each sheet can be characterised as a Mondrian pattern. These can usually be encoded as a tree where each node is either a row-pair or column-pair. Therefore, if each item is represented somewhere as an end-node, and there are no nasty circular dependencies, then we can:

  1. Determine the non-Formula dimensions
  2. Determine the Formula dimensions. This needs to be done progressively based on the Reference dependencies.
  3. Determine within-parent item location and extent.
  4. With this, determine absolute item location and extent within excel sheets.
  5. Convert Formulas to excel formulas, including conditional formatting formulas.
  6. Print.


A MondrianSheets layout can be characterised by a table with:

  - sheet
  - id
  - parent_id: Parent node's ID
  - child_id: Within-parent ID
  - type: parent or terminal?
  - item: if terminal, the name of the item.
  - axis: row or column pair, if parent
  - row_extent: how big is this node in rows?
  - column_extent: how big is this node in columns?
  - within_parent_row_location
  - within_parent_col_location
  - absolute_row_location
  - absolute_col_location