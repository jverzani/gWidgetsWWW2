\name{GGroup}
\alias{GGroup}
\title{base class for ggroup}
\description{
  Boxes are different. For horizontal boxes they need to
  have a width set, for vertical boxes they need to have a
  height set. We use a default of 200.
}
\details{
  The standard expand, fill, anchor arugments are not
  implemented as they are in RGtk2, say.

  The expand maps to flex which is a weight for stretching
  the object in the packing direction. That is for a hbox,
  it will strecth in horizontal direction

  The fill corresponds to the align argument for the
  container -- not the component. There is no way to fill
  just one. The fill value of "stretch" will stretch the
  component in the orthogonal directin to filling. Passing
  the value "stretchmax" should stretch to the largest
  child size, but not the max.

  So expand=TRUE, fill=TRUE will will stretch in both
  directions.

  But wait, the fill value -- since it appears to the
  container, but is specified by the children can be
  specified more than once. The last one wins.

  When expand and fill are not used, anchoring should be
  possible. The CSS class needs to be set up properly
  though.
}

