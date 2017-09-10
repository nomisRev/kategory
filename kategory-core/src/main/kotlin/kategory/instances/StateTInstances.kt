package kategory

interface StateTApplicative<F, S> : Applicative<StateTKindPartial<F, S>> {

    fun F(): Monad<F>

    override fun <A> pure(a: A): HK<StateTKindPartial<F, S>, A> = StateT(F(), F().pure({ s: S -> F().pure(Tuple2(s, a)) }))

    override fun <A, B> ap(fa: HK<StateTKindPartial<F, S>, A>, ff: HK<StateTKindPartial<F, S>, (A) -> B>): StateT<F, S, B> =
            ff.ev().map2(fa.ev()) { f, a -> f(a) }

    override fun <A, B, Z> map2(fa: HK<StateTKindPartial<F, S>, A>, fb: HK<StateTKindPartial<F, S>, B>, f: (Tuple2<A, B>) -> Z): StateT<F, S, Z> =
            fa.ev().map2(fb.ev(), { a, b -> f(Tuple2(a, b)) })

    @Suppress("UNCHECKED_CAST")
    override fun <A, B, Z> map2Eval(fa: HK<StateTKindPartial<F, S>, A>, fb: Eval<HK<StateTKindPartial<F, S>, B>>, f: (Tuple2<A, B>) -> Z):
            Eval<StateT<F, S, Z>> = fa.ev().map2Eval(fb as Eval<StateT<F, S, B>>) { a, b -> f(Tuple2(a, b)) }

    override fun <A, B> product(fa: HK<StateTKindPartial<F, S>, A>, fb: HK<StateTKindPartial<F, S>, B>): HK<StateTKindPartial<F, S>, Tuple2<A, B>> =
            fa.ev().product(fb.ev())
}

interface StateTFunctor<F, S> : Functor<StateTKindPartial<F, S>> {
    fun FF(): Functor<F>

    override fun <A, B> map(fa: HK<StateTKindPartial<F, S>, A>, f: (A) -> B): StateT<F, S, B> = fa.ev().map(f)
}

interface StateTMonad<F, S> : Monad<StateTKindPartial<F, S>> {
    fun F(): Monad<F>

    override fun <A> pure(a: A): HK<StateTKindPartial<F, S>, A> = StateT(F(), F().pure({ s: S -> F().pure(Tuple2(s, a)) }))

    override fun <A, B> flatMap(fa: HK<StateTKindPartial<F, S>, A>, f: (A) -> HK<StateTKindPartial<F, S>, B>): StateT<F, S, B> = fa.ev().flatMap(f)

    override fun <A, B> map(fa: HK<StateTKindPartial<F, S>, A>, f: (A) -> B): StateT<F, S, B> = fa.ev().map(f)

    override fun <A, B> tailRecM(a: A, f: (A) -> HK<StateTKindPartial<F, S>, Either<A, B>>): StateT<F, S, B> =
            StateT(F(), F().pure({ s: S ->
                F().tailRecM(Tuple2(s, a), { (s, a0) ->
                    F().map(f(a0).runM(s)) { (s, ab) ->
                        ab.bimap({ a1 -> Tuple2(s, a1) }, { b -> Tuple2(s, b) })
                    }
                })
            }))
}

interface StateTMonadState<F, S> : MonadState<StateTKindPartial<F, S>, S>, StateTMonad<F, S> {

    override fun get(): StateT<F, S, S> = StateT(F(), F().pure({ s: S -> F().pure(Tuple2(s, s)) }))

    override fun set(s: S): StateT<F, S, Unit> = StateT(F(), F().pure({ _: S -> F().pure(Tuple2(s, Unit)) }))
}

interface StateTSemigroupK<F, S> : SemigroupK<StateTKindPartial<F, S>> {
    fun F(): Monad<F>
    fun G(): SemigroupK<F>

    override fun <A> combineK(x: HK<HK2<StateTHK, F, S>, A>, y: HK<HK2<StateTHK, F, S>, A>): StateT<F, S, A> =
            StateT(F(), F().pure({ s -> G().combineK(x.ev().run(s), y.ev().run(s)) }))
}

interface StateTMonadCombine<F, S> : MonadCombine<StateTKindPartial<F, S>>, StateTMonad<F, S>, StateTSemigroupK<F, S> {
    override fun F(): MonadCombine<F>
    override fun G(): MonadCombine<F> = F()

    override fun <A> empty(): StateT<F, S, A> = liftT(F().empty())

    fun <A> liftT(ma: HK<F, A>): StateT<F, S, A> = StateT(F(), F().pure({ s: S -> F().map(ma, { a: A -> s toT a }) }))
}

interface StateTMonadError<F, S, E> : StateTMonad<F, S>, MonadError<StateTKindPartial<F, S>, E> {
    override fun F(): MonadError<F, E>

    override fun <A> raiseError(e: E): StateT<F, S, A> = StateT.lift(F().raiseError(e), F())

    override fun <A> handleErrorWith(fa: HK<StateTKindPartial<F, S>, A>, f: (E) -> HK<StateTKindPartial<F, S>, A>): StateT<F, S, A> =
            StateT(F(), F().pure({ s -> F().handleErrorWith(fa.runM(s), { e -> f(e).runM(s) }) }))
}
