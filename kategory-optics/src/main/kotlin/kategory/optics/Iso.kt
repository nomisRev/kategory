package kategory.optics

import kategory.Either
import kategory.Functor
import kategory.HK
import kategory.Option
import kategory.Tuple2
import kategory.compose
import kategory.functor
import kategory.identity
import kategory.none
import kategory.right
import kategory.some
import kategory.toT

/**
 * [Iso] is a type alias for [PIso] which fixes the type arguments
 * and restricts the [PIso] to monomorphic updates.
 */
typealias Iso<S, T> = PIso<S, S, T, T>

/**
 * An [Iso] is an lossless invertible optic that defines an isomorphism between a type `S` and `A`.
 * i.e. a data class and it's data represented by TupleN
 *
 * A (polymorphic) [PIso] is useful when setting or modifying a value for a constructed type
 * i.e. PIso<Option<Int>, Option<String>, Int?, String?>
 *
 * An [PIso] is also a valid [Lens], [Prism]
 *
 * @param S the source of a [PIso]
 * @param T the modified source of a [PIso]
 * @param A the target of a [PIso]
 * @param B the modified target of a [PIso]
 */
abstract class PIso<S, T, A, B> {

    /**
     * Get the target of a [PIso]
     */
    abstract fun get(s: S): A

    /**
     * Get the modified source of a [PIso]
     */
    abstract fun reverseGet(b: B): T

    companion object {

        /**
         * create an [PIso] between any type and itself. id is the zero element of optics composition, for all optics o of type O (e.g. Lens, Iso, Prism, ...):
         * o      compose Iso.id == o
         * Iso.id composeO   o        == o (replace composeO by compose, compose, compose, ...)
         */
        fun <A> id(): Iso<A, A> = Iso(::identity, ::identity)

        /**
         * Invoke operator overload to create a [PIso] of type `S` with target `A`.
         * Can also be used to construct [Iso]
         */
        operator fun <S, T, A, B> invoke(get: (S) -> (A), reverseGet: (B) -> T) = object : PIso<S, T, A, B>() {

            override fun get(s: S): A = get(s)

            override fun reverseGet(b: B): T = reverseGet(b)
        }
    }

    /**
     * Reverse a [PIso]: the source becomes the target and the target becomes the source
     */
    fun reverse(): PIso<B, A, T, S> = PIso(this::reverseGet, this::get)

    /**
     * Lift a [PIso] to a Functor level
     */
    inline fun <reified F> mapping(FF: Functor<F> = functor()): PIso<HK<F, S>, HK<F, T>, HK<F, A>, HK<F, B>> = PIso(
            { fa -> FF.map(fa, this::get) },
            { fb -> FF.map(fb, this::reverseGet) }
    )

    /**
     * Find if the target satisfies the predicate
     */
    fun find(s: S, p: (A) -> Boolean): Option<A> = get(s).let { aa ->
        if (p(aa)) aa.some() else none()
    }

    /**
     * Check if the target satisfies the predicate
     */
    inline fun exist(s: S, crossinline p: (A) -> Boolean): Boolean = p(get(s))

    /**
     * Modify polymorphically the target of a [PIso] with a function
     */
    inline fun modify(s: S, crossinline f: (A) -> B): T = reverseGet(f(get(s)))

    /**
     * Modify polymorphically the target of a [PIso] with a Functor function
     */
    inline fun <reified F> modifyF(FF: Functor<F> = functor(), f: (A) -> HK<F, B>, s: S): HK<F, T> =
            FF.map(f(get(s)), this::reverseGet)

    /**
     * Set polymorphically the target of a [PIso] with a value
     */
    fun set(b: B): T = reverseGet(b)

    /**
     * Pair two disjoint [PIso]
     */
    infix fun <S1, T1, A1, B1> split(other: PIso<S1, T1, A1, B1>): PIso<Tuple2<S, S1>, Tuple2<T, T1>, Tuple2<A, A1>, Tuple2<B, B1>> = PIso(
            { (a, c) -> get(a) toT other.get(c) },
            { (b, d) -> reverseGet(b) toT other.reverseGet(d) }
    )

    /**
     * Create a pair of the target and a type C
     */
    fun <C> first(): PIso<Tuple2<S, C>, Tuple2<T, C>, Tuple2<A, C>, Tuple2<B, C>> = Iso(
            { (a, c) -> get(a) toT c },
            { (b, c) -> reverseGet(b) toT c }
    )

    /**
     * Create a pair of a type C and the target
     */
    fun <C> second(): PIso<Tuple2<C, S>, Tuple2<C, T>, Tuple2<C, A>, Tuple2<C, B>> = PIso(
            { (c, a) -> c toT get(a) },
            { (c, b) -> c toT reverseGet(b) }
    )

    /**
     * Create a sum of the target and a type C
     */
    fun <C> left(): PIso<Either<S, C>, Either<T, C>, Either<A, C>, Either<B, C>> = PIso(
            { it.bimap(this::get, ::identity) },
            { it.bimap(this::reverseGet, ::identity) }
    )

    /**
     * Create a sum of a type C and the target
     */
    fun <C> right(): PIso<Either<C, S>, Either<C, T>, Either<C, A>, Either<C, B>> = PIso(
            { it.bimap(::identity, this::get) },
            { it.bimap(::identity, this::reverseGet) }
    )

    /**
     * Compose a [PIso] with a [PIso]
     */
    infix fun <C, D> compose(other: PIso<A, B, C, D>): PIso<S, T, C, D> = PIso(
            other::get compose this::get,
            this::reverseGet compose other::reverseGet
    )

    /**
     * Compose a [PIso] with a [PLens]
     */
    infix fun <C, D> compose(other: PLens<A, B, C, D>): PLens<S, T, C, D> = asLens() compose other

    /**
     * Compose a [PIso] with a [Getter]
     */
    infix fun <C> compose(other: Getter<A, C>): Getter<S, C> = asGetter() composeGetter other

    /**
     * Compose a [PIso] with a [PSetter]
     */
    infix fun <C, D> compose(other: PSetter<A, B, C, D>): PSetter<S, T, C, D> = asSetter() compose other

    /**
     * Compose a [PIso] with a [POptional]
     */
    infix fun <C, D> compose(other: POptional<A, B, C, D>): POptional<S, T, C, D> = asOptional() compose other

    /**
     * Plus operator overload to compose lenses
     */
    operator fun <C, D> plus(other: PIso<A, B, C, D>): PIso<S, T, C, D> = compose(other)

    operator fun <C, D> plus(other: PLens<A, B, C, D>): PLens<S, T, C, D> = compose(other)

    operator fun <C> plus(other: Getter<A, C>): Getter<S, C> = compose(other)

    operator fun <C, D> plus(other: PSetter<A, B, C, D>): PSetter<S, T, C, D> = compose(other)

    operator fun <C, D> plus(other: POptional<A, B, C, D>): POptional<S, T, C, D> = compose(other)

    /**
     * View a [PIso] as a [PPrism]
     */
    fun asPrism(): PPrism<S, T, A, B> = PPrism(
            { a -> Either.Right(get(a)) },
            this::reverseGet
    )

    /**
     * View a [PIso] as a [PLens]
     */
    fun asLens(): PLens<S, T, A, B> = PLens(this::get, { b -> { _ -> set(b) } })

    /**
     * View a [PIso] as a [Getter]
     */
    fun asGetter(): Getter<S, A> = Getter(this::get)

    /**
     * View a [PIso] as a [POptional]
     */
    fun asOptional(): POptional<S, T, A, B> = POptional(
            { s -> get(s).right() },
            { b -> { _ -> set(b) } }
    )

    /**
     * View a [PIso] as a [PSetter]
     */
    fun asSetter(): PSetter<S, T, A, B> = PSetter { f -> { s -> modify(s, f) } }
}