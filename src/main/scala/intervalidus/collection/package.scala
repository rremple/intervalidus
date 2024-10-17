package intervalidus

/**
  * Intervalidus's custom data structures for optimizing 1d, 2d, and 3d interval retrieval. Built for internal use, but
  * use them if they are useful. There are both immutable and mutable versions of each data structure, but only the
  * mutable versions are used (for performance reasons).
  *   - Box trees manage boxed data structures in multidimensional double space (B-trees, quadtrees, and octrees,
  *     depending on the dimension). They hold boxes rather than individual points, where boxes are split to fit into
  *     the subtrees of the data structure. Intervalidus uses the ordered hashes defined on the discrete domain
  *     components of intervals to approximate all intervals as boxes in double space, and manages valid data associated
  *     with these boxes in a box trees, resulting in dramatically faster intersection retrieval (in most cases).
  *   - Multi-dictionaries (a.k.a., multimaps) allow multiple values to be associated with the same key. Intervalidus
  *     uses multi-dictionaries to store the association of each valid value with all the intervals where it is valid.
  *     This allows value-driven functions like compression to operate on a smaller subset of valid values and be
  *     substantially faster.
  */
package object collection
