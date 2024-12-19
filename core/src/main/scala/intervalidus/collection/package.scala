package intervalidus

/**
  * Intervalidus's custom data structures for optimizing one-, two-, and three-dimensional interval retrieval. Built for
  * internal use, but use them if they are useful. There are both immutable and mutable versions of each data structure,
  * but only the mutable versions are used internally (for performance reasons).
  *   - Box search trees manage boxed data structures in multidimensional double space (B-trees, quadtrees, and octrees,
  *     depending on the dimension). They hold boxes rather than individual points, where boxes are split to fit into
  *     the subtrees of the data structure. Intervalidus uses the ordered hashes defined on the discrete domain
  *     components of intervals to approximate all intervals as boxes in double space, and manages valid data associated
  *     with these boxes in box search trees, resulting in dramatically faster intersection retrieval (in most cases).
  *   - Multimaps (a.k.a., multi-dictionaries) allow multiple values to be associated with the same key. Intervalidus
  *     uses multimaps to store the association of each valid value with all the intervals where it is valid. This
  *     allows value-driven functions like compression to operate on a smaller subset of valid values and to be
  *     substantially faster.
  */
package object collection
