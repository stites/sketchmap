---------------------------------------------------------------------------------
-- |
-- A Sketch Map is a generalized version of the Count-Min Sketch that is an
-- approximation of (Map k v) that stores reference to top heavy hitters. The
-- Sketch Map can approximate the sums of any summable value that has a monoid.
---------------------------------------------------------------------------------
module Data.SketchMap (mkSketchMap, SketchMap, SketchMapParams(..)) where

import Crypto.Number.ModArithmetic
import System.Random

data SketchMapParams = SketchMapParams
  { seed :: Int
  , width :: Nat -- width must be greater than 0
  , depth :: Int -- depth must be greater than 0
  , heavyHittersCount :: Int -- heavyHittersCount must be greater than 0
  }

data SketchMap seed width depth hhCount = SketchMap
  { maps :: HashMap HashingFunction (Array Integer) -- can be word8s later

  , seed  :: Int
  , width :: Int -- width must be greater than 0
  , depth :: Int -- depth must be greater than 0
  , heavyHittersCount :: Int -- heavyHittersCount must be greater than 0
  }

mkSketchMap :: Params -> SketchMap

update

estimate

instance Monoid SketchMap where
  mempty =
   =














eps :: Int -> Double
eps width = exp 1 / width

delta :: Int -> Double
delta depth = 1 / (exp depth)

width :: Double -> Int
width exp = ceil ( exp 1 / eps )

depth :: Double -> Int
depth delta = ceil ( log 1 / delta )

makeHash :: Int -> Integer
makeHash n = do

estimate :: Hashable k => SketchMap -> k -> Integer
estimate = undefined

update :: Hashable k => SketchMap -> k -> Integer -> SketchMap
update = undefined


-- hashes :: SketchMapParams -> Seq (k, Int)
-- hashes sm = do
--   let r = randomIO(seed)
--   let numHashes = depth sm
--   let numCounters = width sm
--
--   replicate numHashes
--   where
--     smhash :: SketchMapHash k
--     smhash = SketchMapHash(CMSHash[Long](r.nextInt, 0, numCounters), seed)(serialization)
--       new (K => Int) { override def apply(k: K) = smhash(k) }


-- Calculates the frequency for a key given a values table.
frequency :: Ord v => k -> AdaptiveMatrix v -> v
frequency key table =

  hashes
    .zipWithIndex
    .map { case (hash, row) => table.getValue((row, hash(key))) }
    .min


-- Returns a new set of sorted and concatenated heavy hitters given an arbitrary
-- list of keys.
-- updatedHeavyHitters :: Ord v => Seq k -> AdaptiveMatrix v -> [k]
-- updatedHeavyHitters hitters table =
--   hitters.sorted(specificOrdering).take(heavyHittersCount).toList
--   where
--     mapping :: Map k v
--     mapping = map (\item => (item, frequency (item, table) ) )(breakOut) hitters
--     specificOrdering = Ordering.by[K, V] { mapping(_) }.reverse


-- Responsible for creating instances of SketchMap.
data SketchMap k v = SketchMap {
  params :: SketchMapParams k
  ordering :: Ordering v

class SketchMapMonoid[K, V](val params: SketchMapParams[K])(implicit valueOrdering: Ordering[V], monoid: Monoid[V])
  extends Monoid[SketchMap[K, V]] {

  /**
   * A zero Sketch Map is one with zero elements.
   */
  val zero: SketchMap[K, V] = SketchMap(AdaptiveMatrix.fill(params.depth, params.width)(monoid.zero), Nil, monoid.zero)

  override def plus(left: SketchMap[K, V], right: SketchMap[K, V]): SketchMap[K, V] = {
    val newValuesTable = Monoid.plus(left.valuesTable, right.valuesTable)
    val newHeavyHitters = left.heavyHitterKeys.toSet ++ right.heavyHitterKeys

    SketchMap(
      newValuesTable,
      params.updatedHeavyHitters(newHeavyHitters.toSeq, newValuesTable),
      Monoid.plus(left.totalValue, right.totalValue))
  }

  override def sumOption(items: TraversableOnce[SketchMap[K, V]]): Option[SketchMap[K, V]] =
    if (items.isEmpty) None
    else {
      val buffer = scala.collection.mutable.Buffer[SketchMap[K, V]]()
      val maxBuffer = 1000
      def sumBuffer(): Unit = {
        val newValuesTable = Monoid.sum(buffer.iterator.map(_.valuesTable))
        val heavyHittersSet = Monoid.sum(buffer.iterator.map(_.heavyHitterKeys.toSet))
        val newtotalValue = Monoid.sum(buffer.iterator.map(_.totalValue))
        buffer.clear()
        buffer += SketchMap(
          newValuesTable,
          params.updatedHeavyHitters(heavyHittersSet.toSeq, newValuesTable),
          newtotalValue)
      }

      items.foreach { sm =>
        if (buffer.size > maxBuffer) sumBuffer()
        buffer += sm
      }
      if (buffer.size > 1) sumBuffer() //don't bother to sum if there is only one item.
      Some(buffer(0))
    }

  /**
   * Create a Sketch Map sketch out of a single key/value pair.
   */
  def create(pair: (K, V)): SketchMap[K, V] = create(Seq(pair))

  /**
   * Create a Sketch Map sketch from a sequence of pairs.
   */
  def create(data: Seq[(K, V)]): SketchMap[K, V] = {
    val heavyHitters = data.map { _._1 }
    val totalValue = Monoid.sum(data.map { _._2 })
    val initTable = AdaptiveMatrix.fill[V](params.depth, params.width)(monoid.zero)
    /* For each row, update the table for each K,V pair */
    val newTable = (0 to (params.depth - 1)).foldLeft(initTable) {
      case (table, row) =>
        data.foldLeft(table) {
          case (innerTable, (key, value)) =>
            val pos = (row, params.hashes(row)(key))
            val currValue: V = innerTable.getValue(pos)
            innerTable.updated(pos, Monoid.plus(currValue, value))
        }
    }

    SketchMap(newTable, params.updatedHeavyHitters(heavyHitters, newTable), totalValue)
  }

  /**
   * Calculates the approximate frequency for any key.
   */
  def frequency(sm: SketchMap[K, V], key: K): V =
    params.frequency(key, sm.valuesTable)

  def frequencyWithHHCache(sm: SketchMap[K, V]): K => V = {
    val hhMap: Map[K, V] = heavyHitters(sm).toMap
    (k: K) => hhMap.getOrElse(k, frequency(sm, k))
  }

  /**
   * Returns a sorted list of heavy hitter key/value tuples.
   */
  def heavyHitters(sm: SketchMap[K, V]): List[(K, V)] =
    sm.heavyHitterKeys.map { item => (item, frequency(sm, item)) }
}

