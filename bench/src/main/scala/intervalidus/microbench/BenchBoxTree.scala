package intervalidus.microbench

import intervalidus.DiscreteValue.given
import intervalidus.*
import intervalidus.collection.*
import intervalidus.collection.mutable.BoxTree
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * Not using the "features" aspect of BenchBase. Only using it to derive test boxed payloads from dimensional data so
  * that there aren't any overlapping boxes. Box trees support overlaps, but having data non-overlapping is a more
  * realistic test based on how box trees are used within intervalidus.
  */
object BenchBoxTree extends BenchBase(baselineFeature = None, featuredFeature = None):
  // Sample results, where "_p" indicates scaling _plus orders of magnitude and "_m" indicates scaling _minus orders of
  // magnitude. "_000" is where the capacity fits the data the tightest. There are variations for 1, 2, and 3-dimensional
  // data, as well as intervals in 1k, 10k, and 100k randomized sizes.
  //
  // Benchmark results indicate reasonable performance when the capacity is tighter than the actual data, without much
  // variation until the capacity exactly matches that of the data ("_000"). Performance is optimal at this point (up to
  // 50% faster), but it falls off quickly as the capacity gets larger. All the variations showed similar performance
  // patterns when scaling magnitude, though performance degradation was greater in higher dimensions.
  //
  // The key take-away is that it is better to be overly tight than overly loose when defining the initial capacity.
  // It seems that resizing the boundary capacity of a tree is relatively inexpensive and doesn't affect amortized
  // performance much. (This benchmark was constrained to Int sizes, so things would likely get even crazier for Longs
  // and Doubles!)
  //
  // [info] Benchmark                          Mode  Cnt       Score        Error  Units
  // [info] BenchBoxTree.Bench1d100k.add_000  thrpt    3  376494.870 ± 853209.922  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_m02  thrpt    3  181133.255 ±  53935.070  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_m04  thrpt    3  195699.960 ± 507095.168  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_m06  thrpt    3  171502.037 ±  25550.072  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_m08  thrpt    3  170911.299 ± 288948.779  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_m10  thrpt    3  204953.035 ± 203991.295  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_m12  thrpt    3  190278.969 ± 212434.932  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_m14  thrpt    3  182175.088 ± 213571.591  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_m16  thrpt    3  197728.832 ±  88862.925  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_m18  thrpt    3  200003.282 ±  45361.121  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_m20  thrpt    3  188356.445 ± 209042.900  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_m22  thrpt    3  205810.444 ±  55195.310  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_m24  thrpt    3  213389.646 ±  52633.888  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_p02  thrpt    3  287248.049 ± 521770.021  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_p04  thrpt    3  255892.304 ± 260952.747  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_p06  thrpt    3  224488.891 ±  45346.588  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_p08  thrpt    3  203931.347 ±  70911.412  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_p10  thrpt    3  183741.458 ±  76147.233  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_p12  thrpt    3  164501.171 ±  27968.042  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_p14  thrpt    3  134657.965 ±  96169.847  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_p16  thrpt    3  138479.175 ±  27700.390  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_p18  thrpt    3  125960.853 ±  19464.668  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_p20  thrpt    3  121841.185 ±  74779.430  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_p22  thrpt    3  111984.217 ±  68167.551  ops/s
  // [info] BenchBoxTree.Bench1d100k.add_p24  thrpt    3   97053.203 ± 216974.014  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_000   thrpt    3  349625.378 ±  55014.828  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_m02   thrpt    3  166334.447 ±  25931.880  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_m04   thrpt    3  174349.303 ±  79392.922  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_m06   thrpt    3  159623.925 ± 186422.144  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_m08   thrpt    3  172250.554 ±  67369.492  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_m10   thrpt    3  169550.810 ± 254241.885  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_m12   thrpt    3  179675.711 ±  53665.386  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_m14   thrpt    3  176644.898 ±  91116.674  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_m16   thrpt    3  179278.476 ±  56100.450  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_m18   thrpt    3  199263.922 ±   8053.760  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_m20   thrpt    3  173550.384 ± 131613.039  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_m22   thrpt    3  173376.392 ± 204131.598  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_m24   thrpt    3  180461.155 ±  76899.336  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_p02   thrpt    3  274743.340 ± 138714.298  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_p04   thrpt    3  233166.773 ±  25551.501  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_p06   thrpt    3  221985.139 ±  54581.362  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_p08   thrpt    3  182958.044 ± 317246.729  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_p10   thrpt    3  166601.853 ±  68922.589  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_p12   thrpt    3  120386.468 ±  52292.996  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_p14   thrpt    3  148323.989 ±  51366.491  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_p16   thrpt    3  106414.887 ±  26173.408  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_p18   thrpt    3  126968.577 ±  64741.094  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_p20   thrpt    3  122859.952 ±  17913.257  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_p22   thrpt    3  114043.977 ±  38695.527  ops/s
  // [info] BenchBoxTree.Bench1d10k.add_p24   thrpt    3   99905.437 ± 181732.749  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_000    thrpt    3  345150.797 ±  47887.969  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_m02    thrpt    3  156401.890 ± 127966.685  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_m04    thrpt    3  169072.293 ± 186038.113  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_m06    thrpt    3  182706.453 ±  51816.384  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_m08    thrpt    3  169756.610 ±  98745.382  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_m10    thrpt    3  179653.563 ±  25910.421  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_m12    thrpt    3  172204.669 ± 155659.714  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_m14    thrpt    3  181537.045 ±  60105.878  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_m16    thrpt    3  183739.139 ±  77970.211  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_m18    thrpt    3  183232.745 ±  75954.195  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_m20    thrpt    3  182892.390 ±  65238.374  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_m22    thrpt    3  175913.544 ±  41416.968  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_m24    thrpt    3  176838.123 ±  41197.589  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_p02    thrpt    3  280629.571 ± 246850.630  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_p04    thrpt    3  237923.381 ± 155782.438  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_p06    thrpt    3  208918.269 ±  79376.159  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_p08    thrpt    3  190849.118 ±  82133.451  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_p10    thrpt    3  167296.857 ± 177531.908  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_p12    thrpt    3  158773.695 ±  80251.024  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_p14    thrpt    3  135928.324 ± 275151.484  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_p16    thrpt    3  133967.550 ±  70544.153  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_p18    thrpt    3   92400.566 ±  43138.660  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_p20    thrpt    3  121006.480 ±  31821.872  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_p22    thrpt    3  103041.982 ± 145780.469  ops/s
  // [info] BenchBoxTree.Bench1d1k.add_p24    thrpt    3  103419.749 ±  48108.344  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_000  thrpt    3  344631.155 ± 379168.431  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_m02  thrpt    3  275965.131 ±  36854.472  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_m04  thrpt    3  210941.747 ±  46248.171  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_m06  thrpt    3  232575.526 ±  56231.386  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_m08  thrpt    3  221335.905 ± 137236.676  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_m10  thrpt    3  229414.392 ±  38267.204  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_m12  thrpt    3  296512.204 ± 109416.659  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_m14  thrpt    3  297306.235 ±  33741.836  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_m16  thrpt    3  288041.208 ± 127342.621  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_m18  thrpt    3  242365.984 ±  20177.560  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_m20  thrpt    3  290262.555 ±  73227.500  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_m22  thrpt    3  220427.748 ± 138060.943  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_m24  thrpt    3  216732.597 ±  50263.049  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_p02  thrpt    3  235067.465 ±  59291.368  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_p04  thrpt    3  192623.064 ±  77170.348  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_p06  thrpt    3  164709.141 ±  13348.546  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_p08  thrpt    3  131396.644 ±  56766.262  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_p10  thrpt    3  121620.027 ±  51113.772  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_p12  thrpt    3  104877.175 ±  26962.334  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_p14  thrpt    3   91831.394 ±  25299.767  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_p16  thrpt    3   93710.825 ±   4440.585  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_p18  thrpt    3   78547.409 ±  20921.569  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_p20  thrpt    3   66101.013 ± 116474.951  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_p22  thrpt    3   63599.969 ±  16490.427  ops/s
  // [info] BenchBoxTree.Bench2d100k.add_p24  thrpt    3   59131.785 ±  21627.084  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_000   thrpt    3  361621.890 ±  87623.182  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_m02   thrpt    3  268986.829 ±  95135.205  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_m04   thrpt    3  240445.495 ±  87617.825  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_m06   thrpt    3  302670.389 ± 112031.775  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_m08   thrpt    3  301745.991 ±  18079.257  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_m10   thrpt    3  296988.656 ±  16366.512  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_m12   thrpt    3  281055.274 ± 630621.352  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_m14   thrpt    3  234387.036 ±  49596.680  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_m16   thrpt    3  218565.099 ± 282062.753  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_m18   thrpt    3  293687.065 ±  45873.989  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_m20   thrpt    3  244606.524 ±  19382.069  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_m22   thrpt    3  293042.948 ± 111812.919  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_m24   thrpt    3  217239.679 ± 193965.273  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_p02   thrpt    3  254342.962 ±  61081.117  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_p04   thrpt    3  195667.295 ± 105650.041  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_p06   thrpt    3  161775.486 ±  46192.804  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_p08   thrpt    3  129630.811 ± 239978.848  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_p10   thrpt    3  120860.086 ±  14982.038  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_p12   thrpt    3  102650.922 ±  43119.305  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_p14   thrpt    3   78599.914 ± 302825.050  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_p16   thrpt    3   84116.471 ±  66364.129  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_p18   thrpt    3   75120.461 ±  14975.150  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_p20   thrpt    3   72769.328 ±   7996.388  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_p22   thrpt    3   66720.621 ±  36055.528  ops/s
  // [info] BenchBoxTree.Bench2d10k.add_p24   thrpt    3   60487.669 ±  29503.939  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_000    thrpt    3  351585.840 ±  67811.092  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_m02    thrpt    3  274586.160 ± 114441.813  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_m04    thrpt    3  266732.814 ± 274544.314  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_m06    thrpt    3  217336.625 ±  66134.244  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_m08    thrpt    3  287098.444 ± 129106.275  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_m10    thrpt    3  240056.561 ± 378783.706  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_m12    thrpt    3  303193.691 ±  49198.769  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_m14    thrpt    3  298257.237 ±  49977.512  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_m16    thrpt    3  236695.533 ±  88821.965  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_m18    thrpt    3  286072.687 ±  36244.439  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_m20    thrpt    3  266448.643 ± 106253.910  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_m22    thrpt    3  210454.693 ±  72291.818  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_m24    thrpt    3  298324.155 ±  80734.590  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_p02    thrpt    3  249019.747 ±  85831.608  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_p04    thrpt    3  197067.506 ±  34653.073  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_p06    thrpt    3  161177.189 ±  54431.360  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_p08    thrpt    3  130900.647 ± 242862.221  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_p10    thrpt    3  117491.311 ±  27023.523  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_p12    thrpt    3  106043.178 ±  16717.570  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_p14    thrpt    3   89625.693 ±  56400.860  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_p16    thrpt    3   83891.572 ±  38401.584  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_p18    thrpt    3   77930.474 ±  43610.583  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_p20    thrpt    3   69599.084 ±  12755.460  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_p22    thrpt    3   67606.928 ±  30569.373  ops/s
  // [info] BenchBoxTree.Bench2d1k.add_p24    thrpt    3   62717.248 ±   9946.982  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_000  thrpt    3  299618.237 ±  64101.751  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_m02  thrpt    3  213753.021 ± 441054.765  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_m04  thrpt    3  188314.767 ± 279368.348  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_m06  thrpt    3  198826.882 ±  46112.958  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_m08  thrpt    3  216603.764 ± 121194.542  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_m10  thrpt    3  197646.801 ±  35445.284  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_m12  thrpt    3  198875.388 ±  22411.625  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_m14  thrpt    3  241465.427 ±  73505.124  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_m16  thrpt    3  180142.459 ±  43604.656  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_m18  thrpt    3  203765.065 ±  13288.177  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_m20  thrpt    3  194471.983 ± 375075.236  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_m22  thrpt    3  195652.909 ±  36297.854  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_m24  thrpt    3  221485.289 ± 207152.564  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_p02  thrpt    3  183858.741 ±  22332.088  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_p04  thrpt    3  135929.934 ±  33510.529  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_p06  thrpt    3  108536.853 ±  17016.077  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_p08  thrpt    3   90980.421 ±   8371.329  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_p10  thrpt    3   74801.901 ±  18928.958  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_p12  thrpt    3   67895.878 ±   6504.053  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_p14  thrpt    3   53681.356 ±  11603.646  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_p16  thrpt    3   48038.478 ±  46379.872  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_p18  thrpt    3   46579.441 ±   6802.747  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_p20  thrpt    3   39745.742 ±  66420.125  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_p22  thrpt    3   33082.052 ±  10867.157  ops/s
  // [info] BenchBoxTree.Bench3d100k.add_p24  thrpt    3   35541.596 ±   6029.890  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_000   thrpt    3  293970.436 ± 397947.608  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_m02   thrpt    3  235220.959 ± 104816.324  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_m04   thrpt    3  234243.970 ±  57904.224  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_m06   thrpt    3  185748.122 ±  38331.880  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_m08   thrpt    3  239483.399 ±  21297.823  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_m10   thrpt    3  240088.946 ±  18536.076  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_m12   thrpt    3  234473.947 ±  64959.940  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_m14   thrpt    3  235089.056 ±  84752.672  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_m16   thrpt    3  191724.543 ± 303857.016  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_m18   thrpt    3  191783.975 ±  99792.680  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_m20   thrpt    3  221844.274 ± 128320.597  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_m22   thrpt    3  186450.710 ±  67720.539  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_m24   thrpt    3  193014.817 ± 228773.259  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_p02   thrpt    3  181302.204 ± 192286.567  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_p04   thrpt    3  131791.289 ±  81098.632  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_p06   thrpt    3  107681.750 ±  31278.858  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_p08   thrpt    3   84466.909 ± 179438.768  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_p10   thrpt    3   74896.496 ±  23785.973  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_p12   thrpt    3   56475.727 ±  19410.765  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_p14   thrpt    3   54331.636 ±  88892.974  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_p16   thrpt    3   52071.930 ±   6425.108  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_p18   thrpt    3   45555.439 ±  26863.974  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_p20   thrpt    3   42105.316 ±  14195.826  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_p22   thrpt    3   36408.826 ±   5338.320  ops/s
  // [info] BenchBoxTree.Bench3d10k.add_p24   thrpt    3   33323.611 ±  42523.634  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_000    thrpt    3  289802.990 ± 474902.520  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_m02    thrpt    3  215778.258 ±   9176.201  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_m04    thrpt    3  177970.060 ± 212540.315  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_m06    thrpt    3  194960.129 ±  51477.504  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_m08    thrpt    3  175421.415 ± 535845.994  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_m10    thrpt    3  197362.683 ±  51698.431  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_m12    thrpt    3  215605.741 ±  43237.282  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_m14    thrpt    3  189024.561 ±  56296.559  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_m16    thrpt    3  234930.115 ±  36216.398  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_m18    thrpt    3  183767.333 ±  30482.436  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_m20    thrpt    3  198163.563 ±  14365.699  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_m22    thrpt    3  252799.851 ±  26986.328  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_m24    thrpt    3  171584.816 ±  50409.406  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_p02    thrpt    3  175207.005 ± 183244.021  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_p04    thrpt    3  137779.718 ±  43525.312  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_p06    thrpt    3  104085.814 ±  41337.296  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_p08    thrpt    3   86450.323 ±  24137.827  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_p10    thrpt    3   73075.310 ±  24990.006  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_p12    thrpt    3   65143.382 ±   8784.692  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_p14    thrpt    3   49543.316 ±  87043.512  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_p16    thrpt    3   50042.875 ±   5173.708  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_p18    thrpt    3   45032.428 ±  16658.421  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_p20    thrpt    3   41035.658 ±   6471.956  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_p22    thrpt    3   38549.144 ±   6611.624  ops/s
  // [info] BenchBoxTree.Bench3d1k.add_p24    thrpt    3   29962.538 ±   7127.675  ops/s

  val rangeNum = math.pow(2, 26).toInt
  override val (fullRangeMin, fullRangeMax) = (-rangeNum, rangeNum)

  def boxAt(dims: Int, start: Option[Double], end: Option[Double]) = Box(
    Coordinate(Vector.fill(dims)(start)),
    Coordinate(Vector.fill(dims)(end))
  )

  def testBoxedPayloads[D <: NonEmptyTuple: DomainLike](
    dims: Int,
    data: DimensionalBase[String, D]
  ): IndexedSeq[BoxedPayload[String]] =
    val fromBottom = BoxedPayload(boxAt(dims, None, Some(fullRangeMin.toDouble)), "fromBottom")
    val toTop = BoxedPayload(boxAt(dims, Some(fullRangeMax.toDouble), None), "toTop")
    IndexedSeq(fromBottom, toTop) ++ data.getAll.toIndexedSeq.map: d =>
      BoxedPayload(d.interval.asBox, d.value)

  val cap = 10_000

  lazy val testBoxedPayloadsIn1d100k = testBoxedPayloads(1, buildMutable1d(validDataIn1d(100_000, cap)))
  lazy val testBoxedPayloadsIn1d10k = testBoxedPayloads(1, buildMutable1d(validDataIn1d(10_000, cap)))
  lazy val testBoxedPayloadsIn1d1k = testBoxedPayloads(1, buildMutable1d(validDataIn1d(1_000, cap)))

  lazy val testBoxedPayloadsIn2d100k = testBoxedPayloads(2, buildMutable2d(validDataIn2d(100_000, cap)))
  lazy val testBoxedPayloadsIn2d10k = testBoxedPayloads(2, buildMutable2d(validDataIn2d(10_000, cap)))
  lazy val testBoxedPayloadsIn2d1k = testBoxedPayloads(2, buildMutable2d(validDataIn2d(1_000, cap)))

  lazy val testBoxedPayloadsIn3d100k = testBoxedPayloads(3, buildMutable3d(validDataIn3d(100_000, cap)))
  lazy val testBoxedPayloadsIn3d10k = testBoxedPayloads(3, buildMutable3d(validDataIn3d(10_000, cap)))
  lazy val testBoxedPayloadsIn3d1k = testBoxedPayloads(3, buildMutable3d(validDataIn3d(1_000, cap)))

  def fullRange(dims: Int) = Capacity(
    CoordinateFixed(Vector.fill(dims)(fullRangeMin.toDouble)),
    CoordinateFixed(Vector.fill(dims)(fullRangeMax.toDouble))
  )

  @State(Scope.Benchmark)
  abstract class GenericBenchmarkState(
    dims: Int,
    testDataSeq: IndexedSeq[BoxedPayload[String]]
  ):
    var testDataIndex: Int = 0
    val recycleAt = testDataSeq.size
    println(s"Initializing ${dims}d state, recycling trees at data size $recycleAt...")

    val treesByScale: mutable.Map[Int, BoxTree[String]] = mutable.Map[Int, BoxTree[String]]()

    // range: 2^(26-24) = 2^2 = 4, 2^(26+24) = 2^50 = 1.1e15
    private def newTrees(): Unit = (-24 to 24 by 2).foreach: scale =>
      val boundary = Boundary(fullRange(dims).rescale(scale))
      // println(s"creating $dims dimension tree for scale $scale - boundary: $boundary")
      treesByScale.addOne(scale -> BoxTree(boundary))

    def testData(): BoxedPayload[String] = testDataSeq(testDataIndex)

    @Setup(Level.Iteration)
    def setUpIteration(): Unit =
      newTrees()
      testDataIndex = -1 // incremented to 0 by setUpInvocation

    @Setup(Level.Invocation)
    def setUpInvocation(): Unit =
      testDataIndex += 1
      if testDataIndex == recycleAt then
        // println(s"recycling trees with test case #$testDataIndex...")
        newTrees()
        testDataIndex = 0

  //// --- Concrete Benchmark State Classes ---

  class BenchmarkState1d100k extends GenericBenchmarkState(1, testBoxedPayloadsIn1d100k)
  class BenchmarkState1d10k extends GenericBenchmarkState(1, testBoxedPayloadsIn1d10k)
  class BenchmarkState1d1k extends GenericBenchmarkState(1, testBoxedPayloadsIn1d1k)

  class BenchmarkState2d100k extends GenericBenchmarkState(2, testBoxedPayloadsIn2d100k)
  class BenchmarkState2d10k extends GenericBenchmarkState(2, testBoxedPayloadsIn2d10k)
  class BenchmarkState2d1k extends GenericBenchmarkState(2, testBoxedPayloadsIn2d1k)

  class BenchmarkState3d100k extends GenericBenchmarkState(3, testBoxedPayloadsIn3d100k)
  class BenchmarkState3d10k extends GenericBenchmarkState(3, testBoxedPayloadsIn3d10k)
  class BenchmarkState3d1k extends GenericBenchmarkState(3, testBoxedPayloadsIn3d1k)

  //// --- Concrete Benchmark Classes ---

  // 23,982,712.406 ± 4,919,246.066 ops/s
  // @Benchmark def add_nul(blackhole: Blackhole, state: BenchmarkState2d100k_nocap): Unit =
  //  blackhole.consume(state.testData())

  class Bench1d100k extends GenericBench:
    private type S = BenchmarkState1d100k

    @Benchmark def add_m24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-24).addOne(s.testData()))
    @Benchmark def add_m22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-22).addOne(s.testData()))
    @Benchmark def add_m20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-20).addOne(s.testData()))
    @Benchmark def add_m18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-18).addOne(s.testData()))
    @Benchmark def add_m16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-16).addOne(s.testData()))
    @Benchmark def add_m14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-14).addOne(s.testData()))
    @Benchmark def add_m12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-12).addOne(s.testData()))
    @Benchmark def add_m10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-10).addOne(s.testData()))
    @Benchmark def add_m08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-8).addOne(s.testData()))
    @Benchmark def add_m06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-6).addOne(s.testData()))
    @Benchmark def add_m04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-4).addOne(s.testData()))
    @Benchmark def add_m02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-2).addOne(s.testData()))
    @Benchmark def add_000(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(0).addOne(s.testData()))
    @Benchmark def add_p02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(2).addOne(s.testData()))
    @Benchmark def add_p04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(4).addOne(s.testData()))
    @Benchmark def add_p06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(6).addOne(s.testData()))
    @Benchmark def add_p08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(8).addOne(s.testData()))
    @Benchmark def add_p10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(10).addOne(s.testData()))
    @Benchmark def add_p12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(12).addOne(s.testData()))
    @Benchmark def add_p14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(14).addOne(s.testData()))
    @Benchmark def add_p16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(16).addOne(s.testData()))
    @Benchmark def add_p18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(18).addOne(s.testData()))
    @Benchmark def add_p20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(20).addOne(s.testData()))
    @Benchmark def add_p22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(22).addOne(s.testData()))
    @Benchmark def add_p24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(24).addOne(s.testData()))

  class Bench1d10k extends GenericBench:
    private type S = BenchmarkState1d10k

    @Benchmark def add_m24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-24).addOne(s.testData()))
    @Benchmark def add_m22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-22).addOne(s.testData()))
    @Benchmark def add_m20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-20).addOne(s.testData()))
    @Benchmark def add_m18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-18).addOne(s.testData()))
    @Benchmark def add_m16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-16).addOne(s.testData()))
    @Benchmark def add_m14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-14).addOne(s.testData()))
    @Benchmark def add_m12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-12).addOne(s.testData()))
    @Benchmark def add_m10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-10).addOne(s.testData()))
    @Benchmark def add_m08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-8).addOne(s.testData()))
    @Benchmark def add_m06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-6).addOne(s.testData()))
    @Benchmark def add_m04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-4).addOne(s.testData()))
    @Benchmark def add_m02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-2).addOne(s.testData()))
    @Benchmark def add_000(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(0).addOne(s.testData()))
    @Benchmark def add_p02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(2).addOne(s.testData()))
    @Benchmark def add_p04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(4).addOne(s.testData()))
    @Benchmark def add_p06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(6).addOne(s.testData()))
    @Benchmark def add_p08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(8).addOne(s.testData()))
    @Benchmark def add_p10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(10).addOne(s.testData()))
    @Benchmark def add_p12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(12).addOne(s.testData()))
    @Benchmark def add_p14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(14).addOne(s.testData()))
    @Benchmark def add_p16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(16).addOne(s.testData()))
    @Benchmark def add_p18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(18).addOne(s.testData()))
    @Benchmark def add_p20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(20).addOne(s.testData()))
    @Benchmark def add_p22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(22).addOne(s.testData()))
    @Benchmark def add_p24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(24).addOne(s.testData()))

  class Bench1d1k extends GenericBench:
    private type S = BenchmarkState1d1k

    @Benchmark def add_m24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-24).addOne(s.testData()))
    @Benchmark def add_m22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-22).addOne(s.testData()))
    @Benchmark def add_m20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-20).addOne(s.testData()))
    @Benchmark def add_m18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-18).addOne(s.testData()))
    @Benchmark def add_m16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-16).addOne(s.testData()))
    @Benchmark def add_m14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-14).addOne(s.testData()))
    @Benchmark def add_m12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-12).addOne(s.testData()))
    @Benchmark def add_m10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-10).addOne(s.testData()))
    @Benchmark def add_m08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-8).addOne(s.testData()))
    @Benchmark def add_m06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-6).addOne(s.testData()))
    @Benchmark def add_m04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-4).addOne(s.testData()))
    @Benchmark def add_m02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-2).addOne(s.testData()))
    @Benchmark def add_000(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(0).addOne(s.testData()))
    @Benchmark def add_p02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(2).addOne(s.testData()))
    @Benchmark def add_p04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(4).addOne(s.testData()))
    @Benchmark def add_p06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(6).addOne(s.testData()))
    @Benchmark def add_p08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(8).addOne(s.testData()))
    @Benchmark def add_p10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(10).addOne(s.testData()))
    @Benchmark def add_p12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(12).addOne(s.testData()))
    @Benchmark def add_p14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(14).addOne(s.testData()))
    @Benchmark def add_p16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(16).addOne(s.testData()))
    @Benchmark def add_p18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(18).addOne(s.testData()))
    @Benchmark def add_p20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(20).addOne(s.testData()))
    @Benchmark def add_p22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(22).addOne(s.testData()))
    @Benchmark def add_p24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(24).addOne(s.testData()))

  class Bench2d100k extends GenericBench:
    private type S = BenchmarkState2d100k

    @Benchmark def add_m24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-24).addOne(s.testData()))
    @Benchmark def add_m22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-22).addOne(s.testData()))
    @Benchmark def add_m20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-20).addOne(s.testData()))
    @Benchmark def add_m18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-18).addOne(s.testData()))
    @Benchmark def add_m16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-16).addOne(s.testData()))
    @Benchmark def add_m14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-14).addOne(s.testData()))
    @Benchmark def add_m12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-12).addOne(s.testData()))
    @Benchmark def add_m10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-10).addOne(s.testData()))
    @Benchmark def add_m08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-8).addOne(s.testData()))
    @Benchmark def add_m06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-6).addOne(s.testData()))
    @Benchmark def add_m04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-4).addOne(s.testData()))
    @Benchmark def add_m02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-2).addOne(s.testData()))
    @Benchmark def add_000(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(0).addOne(s.testData()))
    @Benchmark def add_p02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(2).addOne(s.testData()))
    @Benchmark def add_p04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(4).addOne(s.testData()))
    @Benchmark def add_p06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(6).addOne(s.testData()))
    @Benchmark def add_p08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(8).addOne(s.testData()))
    @Benchmark def add_p10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(10).addOne(s.testData()))
    @Benchmark def add_p12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(12).addOne(s.testData()))
    @Benchmark def add_p14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(14).addOne(s.testData()))
    @Benchmark def add_p16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(16).addOne(s.testData()))
    @Benchmark def add_p18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(18).addOne(s.testData()))
    @Benchmark def add_p20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(20).addOne(s.testData()))
    @Benchmark def add_p22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(22).addOne(s.testData()))
    @Benchmark def add_p24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(24).addOne(s.testData()))

  class Bench2d10k extends GenericBench:
    private type S = BenchmarkState2d10k

    @Benchmark def add_m24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-24).addOne(s.testData()))
    @Benchmark def add_m22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-22).addOne(s.testData()))
    @Benchmark def add_m20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-20).addOne(s.testData()))
    @Benchmark def add_m18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-18).addOne(s.testData()))
    @Benchmark def add_m16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-16).addOne(s.testData()))
    @Benchmark def add_m14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-14).addOne(s.testData()))
    @Benchmark def add_m12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-12).addOne(s.testData()))
    @Benchmark def add_m10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-10).addOne(s.testData()))
    @Benchmark def add_m08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-8).addOne(s.testData()))
    @Benchmark def add_m06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-6).addOne(s.testData()))
    @Benchmark def add_m04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-4).addOne(s.testData()))
    @Benchmark def add_m02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-2).addOne(s.testData()))
    @Benchmark def add_000(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(0).addOne(s.testData()))
    @Benchmark def add_p02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(2).addOne(s.testData()))
    @Benchmark def add_p04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(4).addOne(s.testData()))
    @Benchmark def add_p06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(6).addOne(s.testData()))
    @Benchmark def add_p08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(8).addOne(s.testData()))
    @Benchmark def add_p10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(10).addOne(s.testData()))
    @Benchmark def add_p12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(12).addOne(s.testData()))
    @Benchmark def add_p14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(14).addOne(s.testData()))
    @Benchmark def add_p16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(16).addOne(s.testData()))
    @Benchmark def add_p18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(18).addOne(s.testData()))
    @Benchmark def add_p20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(20).addOne(s.testData()))
    @Benchmark def add_p22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(22).addOne(s.testData()))
    @Benchmark def add_p24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(24).addOne(s.testData()))

  class Bench2d1k extends GenericBench:
    private type S = BenchmarkState2d1k

    @Benchmark def add_m24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-24).addOne(s.testData()))
    @Benchmark def add_m22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-22).addOne(s.testData()))
    @Benchmark def add_m20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-20).addOne(s.testData()))
    @Benchmark def add_m18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-18).addOne(s.testData()))
    @Benchmark def add_m16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-16).addOne(s.testData()))
    @Benchmark def add_m14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-14).addOne(s.testData()))
    @Benchmark def add_m12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-12).addOne(s.testData()))
    @Benchmark def add_m10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-10).addOne(s.testData()))
    @Benchmark def add_m08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-8).addOne(s.testData()))
    @Benchmark def add_m06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-6).addOne(s.testData()))
    @Benchmark def add_m04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-4).addOne(s.testData()))
    @Benchmark def add_m02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-2).addOne(s.testData()))
    @Benchmark def add_000(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(0).addOne(s.testData()))
    @Benchmark def add_p02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(2).addOne(s.testData()))
    @Benchmark def add_p04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(4).addOne(s.testData()))
    @Benchmark def add_p06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(6).addOne(s.testData()))
    @Benchmark def add_p08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(8).addOne(s.testData()))
    @Benchmark def add_p10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(10).addOne(s.testData()))
    @Benchmark def add_p12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(12).addOne(s.testData()))
    @Benchmark def add_p14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(14).addOne(s.testData()))
    @Benchmark def add_p16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(16).addOne(s.testData()))
    @Benchmark def add_p18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(18).addOne(s.testData()))
    @Benchmark def add_p20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(20).addOne(s.testData()))
    @Benchmark def add_p22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(22).addOne(s.testData()))
    @Benchmark def add_p24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(24).addOne(s.testData()))

  class Bench3d100k extends GenericBench:
    private type S = BenchmarkState3d100k

    @Benchmark def add_m24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-24).addOne(s.testData()))
    @Benchmark def add_m22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-22).addOne(s.testData()))
    @Benchmark def add_m20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-20).addOne(s.testData()))
    @Benchmark def add_m18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-18).addOne(s.testData()))
    @Benchmark def add_m16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-16).addOne(s.testData()))
    @Benchmark def add_m14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-14).addOne(s.testData()))
    @Benchmark def add_m12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-12).addOne(s.testData()))
    @Benchmark def add_m10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-10).addOne(s.testData()))
    @Benchmark def add_m08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-8).addOne(s.testData()))
    @Benchmark def add_m06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-6).addOne(s.testData()))
    @Benchmark def add_m04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-4).addOne(s.testData()))
    @Benchmark def add_m02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-2).addOne(s.testData()))
    @Benchmark def add_000(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(0).addOne(s.testData()))
    @Benchmark def add_p02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(2).addOne(s.testData()))
    @Benchmark def add_p04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(4).addOne(s.testData()))
    @Benchmark def add_p06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(6).addOne(s.testData()))
    @Benchmark def add_p08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(8).addOne(s.testData()))
    @Benchmark def add_p10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(10).addOne(s.testData()))
    @Benchmark def add_p12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(12).addOne(s.testData()))
    @Benchmark def add_p14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(14).addOne(s.testData()))
    @Benchmark def add_p16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(16).addOne(s.testData()))
    @Benchmark def add_p18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(18).addOne(s.testData()))
    @Benchmark def add_p20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(20).addOne(s.testData()))
    @Benchmark def add_p22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(22).addOne(s.testData()))
    @Benchmark def add_p24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(24).addOne(s.testData()))

  class Bench3d10k extends GenericBench:
    private type S = BenchmarkState3d10k

    @Benchmark def add_m24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-24).addOne(s.testData()))
    @Benchmark def add_m22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-22).addOne(s.testData()))
    @Benchmark def add_m20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-20).addOne(s.testData()))
    @Benchmark def add_m18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-18).addOne(s.testData()))
    @Benchmark def add_m16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-16).addOne(s.testData()))
    @Benchmark def add_m14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-14).addOne(s.testData()))
    @Benchmark def add_m12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-12).addOne(s.testData()))
    @Benchmark def add_m10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-10).addOne(s.testData()))
    @Benchmark def add_m08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-8).addOne(s.testData()))
    @Benchmark def add_m06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-6).addOne(s.testData()))
    @Benchmark def add_m04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-4).addOne(s.testData()))
    @Benchmark def add_m02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-2).addOne(s.testData()))
    @Benchmark def add_000(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(0).addOne(s.testData()))
    @Benchmark def add_p02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(2).addOne(s.testData()))
    @Benchmark def add_p04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(4).addOne(s.testData()))
    @Benchmark def add_p06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(6).addOne(s.testData()))
    @Benchmark def add_p08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(8).addOne(s.testData()))
    @Benchmark def add_p10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(10).addOne(s.testData()))
    @Benchmark def add_p12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(12).addOne(s.testData()))
    @Benchmark def add_p14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(14).addOne(s.testData()))
    @Benchmark def add_p16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(16).addOne(s.testData()))
    @Benchmark def add_p18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(18).addOne(s.testData()))
    @Benchmark def add_p20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(20).addOne(s.testData()))
    @Benchmark def add_p22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(22).addOne(s.testData()))
    @Benchmark def add_p24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(24).addOne(s.testData()))

  class Bench3d1k extends GenericBench:
    private type S = BenchmarkState3d1k

    @Benchmark def add_m24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-24).addOne(s.testData()))
    @Benchmark def add_m22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-22).addOne(s.testData()))
    @Benchmark def add_m20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-20).addOne(s.testData()))
    @Benchmark def add_m18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-18).addOne(s.testData()))
    @Benchmark def add_m16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-16).addOne(s.testData()))
    @Benchmark def add_m14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-14).addOne(s.testData()))
    @Benchmark def add_m12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-12).addOne(s.testData()))
    @Benchmark def add_m10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-10).addOne(s.testData()))
    @Benchmark def add_m08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-8).addOne(s.testData()))
    @Benchmark def add_m06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-6).addOne(s.testData()))
    @Benchmark def add_m04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-4).addOne(s.testData()))
    @Benchmark def add_m02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(-2).addOne(s.testData()))
    @Benchmark def add_000(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(0).addOne(s.testData()))
    @Benchmark def add_p02(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(2).addOne(s.testData()))
    @Benchmark def add_p04(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(4).addOne(s.testData()))
    @Benchmark def add_p06(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(6).addOne(s.testData()))
    @Benchmark def add_p08(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(8).addOne(s.testData()))
    @Benchmark def add_p10(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(10).addOne(s.testData()))
    @Benchmark def add_p12(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(12).addOne(s.testData()))
    @Benchmark def add_p14(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(14).addOne(s.testData()))
    @Benchmark def add_p16(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(16).addOne(s.testData()))
    @Benchmark def add_p18(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(18).addOne(s.testData()))
    @Benchmark def add_p20(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(20).addOne(s.testData()))
    @Benchmark def add_p22(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(22).addOne(s.testData()))
    @Benchmark def add_p24(b: Blackhole, s: S): Unit = b.consume(s.treesByScale(24).addOne(s.testData()))
