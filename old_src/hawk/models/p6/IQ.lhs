<H3>IQ.lhs for P6 Case Study</H3>

\begin{code}
module IQ where
\end{code}

\begin{code}
import ST
\end{code}

<!-- import Hawk -->

\begin{code}
type Addr = Int
\end{code}

\begin{code}
type InstrQ q s a r = Rec ( new     :: Int ->  ST s (q s a)
			, enQueue :: q s a -> a -> ST s Addr
			, deQueue :: q s a ->  ST s (a,Addr)
			, getSize :: q s a ->  ST s Int
			, getMax  :: q s a ->  ST s Int
			, update  :: q s a -> Addr -> (a -> a) -> ST s ()
			, clear   :: q s a ->  ST s ()
			, space   :: q s a -> ST s Int
			| r
			)
\end{code}

\begin{code}
type InstrQ2 q s a = InstrQ q s a (reQueue::q s a -> a -> ST s Addr)
\end{code}

<!--
type InstrQ q s a = 
                Rec ( new     :: Int ->  ST s (q s a)
                    , enQueue :: q s a -> a -> ST s Addr
                    , deQueue :: q s a ->  ST s (a,Addr)
                    , getSize :: q s a ->  ST s Int
                    , getMax  :: q s a ->  ST s Int
                    , update  :: q s a -> Addr -> (a -> a) -> ST s ()
                    , clear   :: q s a ->  ST s ()
                    , space   :: q s a -> ST s Int
		    )

type InstrQ2 q s a = Rec (InstrQ q s a | (reQueue :: q s a -> a -> ST s Addr))
-->
