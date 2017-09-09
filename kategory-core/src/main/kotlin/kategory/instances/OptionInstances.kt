package kategory

import kategory.Option.None
import kategory.Option.Some

interface OptionMonoid<A> : Monoid<Option<A>> {

    fun SG(): Semigroup<A>

    override fun empty(): Option<A> = None

    override fun combine(a: Option<A>, b: Option<A>): Option<A> =
            when (a) {
                is Some<A> -> when (b) {
                    is Some<A> -> Some(SG().combine(a.value, b.value))
                    is None -> b
                }
                is None -> a
            }

}

data class OptionMonadError<E>(val error: E) : MonadError<OptionHK, E> , OptionMonadInstance {

    override fun <A> raiseError(e: E): Option<A> = None

    override fun <A> handleErrorWith(fa: OptionKind<A>, f: (E) -> OptionKind<A>): Option<A> = fa.ev().orElse({ f(error).ev() })

}

