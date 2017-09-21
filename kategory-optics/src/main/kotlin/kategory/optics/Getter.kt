package kategory.optics

import kategory.Either
import kategory.Option
import kategory.Tuple2
import kategory.compose
import kategory.identity
import kategory.none
import kategory.some
import kategory.toT

/**
 * A [Getter] is an optic that allows to see into a structure and getting a target.
 *
 * A [Getter] can be seen as a get function:
 * - `get: (S) -> A` meaning we can look into an `S` and get an `A`
 *
 * @param S the source of a [Getter]
 * @param A the target of a [Getter]
 */
abstract class Getter<S, A> {
    /**
     * Get the target of a [Getter]
     */
    abstract fun get(a: S): A

    companion object {

        fun <S> id() = Iso.id<S>().asGetter()

        /**
         * [Getter] that takes either A or A and strips the choice of A.
         */
        fun <S> codiagonal(): Getter<Either<S, S>, S> = Getter { aa -> aa.fold(::identity, ::identity) }

        /**
         * Invoke operator overload to create a [Getter] of type `S` with target `A`.
         */
        operator fun <S, A> invoke(get: (S) -> A) = object : Getter<S, A>() {
            override fun get(a: S): A = get(a)
        }
    }

    /**
     * Find if the target satisfies the predicate.
     */
    inline fun find(a: S, crossinline p: (A) -> Boolean): Option<A> = get(a).let { b ->
            if (p(b)) b.some() else none()
        }

    /**
     * Check if the target satisfies the predicate
     */
    inline fun exist(a: S, crossinline p: (A) -> Boolean): Boolean = p(get(a))

    /**
     * join two [Getter] with the same target
     */
    fun <C> choice(other: Getter<C, A>): Getter<Either<S, C>, A> = Getter { a ->
        a.fold(this::get, other::get)
    }

    /**
     * Pair two disjoint [Getter]
     */
    fun <C, D> split(other: Getter<C, D>): Getter<Tuple2<S, C>, Tuple2<A, D>> = Getter { (a, c) ->
        get(a) toT other.get(c)
    }

    fun <C> zip(other: Getter<S, C>): Getter<S, Tuple2<A, C>> = Getter { a ->
        get(a) toT other.get(a)
    }

    fun <C> first(): Getter<Tuple2<S, C>, Tuple2<A, C>> = Getter { (a, c) ->
        get(a) toT c
    }

    fun <C> second(): Getter<Tuple2<C, S>, Tuple2<C, A>> = Getter { (c, a) ->
        c toT get(a)
    }

    fun <C> left(): Getter<Either<S, C>, Either<A, C>> = Getter { ac ->
        ac.bimap(this::get, ::identity)
    }

    fun <C> right(): Getter<Either<C, S>, Either<C, A>> = Getter { ca ->
        ca.map(this::get)
    }

    /**
     * Compose a [Getter] with a [Getter]
     */
    infix fun <C> composeGetter(other: Getter<A, C>): Getter<S, C> = Getter(other::get compose this::get)

    /**
     * Compose a [Getter] with a [Lens]
     */
    infix fun <C> composeLens(other: Lens<A,C>): Getter<S,C> = Getter(other::get compose this::get)

    /**
     * Compose a [Getter] with a [Iso]
     */
    infix fun <C> composeIso(other: Iso<A,C>): Getter<S,C> = Getter(other::get compose this::get)

    /**
     * Plus operator overload to compose optionals
     */
    operator fun <C> plus(other: Getter<A, C>): Getter<S, C> = composeGetter(other)

    operator fun <C> plus(other: Lens<A,C>): Getter<S, C> = composeLens(other)

    operator fun <C> plus(other: Iso<A,C>): Getter<S, C> = composeIso(other)

}