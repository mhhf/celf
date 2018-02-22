(*  Celf
 *  Copyright (C) 2008 Anders Schack-Nielsen and Carsten Sch�rmann
 *
 *  This file is part of Celf.
 *
 *  Celf is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  Celf is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Celf.  If not, see <http://www.gnu.org/licenses/>.
 *)

signature TLU_OpSem = TOP_LEVEL_UTIL
structure OpSem :> OPSEM =
struct

open Syntax
open Context
open PatternBind
open SignaturTable

val traceSolve = ref 0
val allowConstr = ref false
val debugForwardChaining = ref false

val fcLimit = ref NONE : int option ref

(* The type 'context' represents input and output contexts and the type
 * 'lcontext' represents the part of the input context that has to occur
 * at that specific point, i.e. it is not allowed to be passed to the
 * output context. *)
type context = (asyncType * (lr list * headType) list) context
type lcontext = int list (* must-occur context: list of indices *)



(* Printing out contexts that arise during forward chaining *)

(* Prepare the context for printing in a very loose sense. *)
(* Invariant: The head of "context" is always the i+1th element of the
 * original context. We're walking through both lists in tandem, noticing
 * when lcontext is telling us that we're at a mandatory item.
 *
 * It may not actually be the case that we need the lcontext at all - if
 * we look at the overlap, it appears that all the information we need
 * comes from whether the context-thing is persistent, affine, linear, or
 * gone. -rjs 2012-03-29 *)
fun prepCtx (lcontext, i, context) =
let
   exception Invariant of string
   fun modalityStr Context.INT = "pers"
     | modalityStr Context.AFF = "aff"
     | modalityStr Context.LIN = "lin"

   (* What is this "stuff"? The rest of the file indicates they're
   some sort of oracle-y thing? -rjs 2012-03-29 *)
   fun dataStr (asyncType: Syntax.asyncType,
                stuff: (SignaturTable.lr list * SignaturTable.headType) list) =
   let
      (* XXX PERF we do this over and over, quadratic *)
      val context' = map #1 (tl context)
   in
      PrettyPrint.printTypeInCtx context' asyncType
   end

   fun optionalItem (varname, data, NONE) =
          [] (* Item removed from ctx *)
     | optionalItem (varname, data, SOME Context.LIN) =
          (* If we are just reporting the intermediate contexts from
          forward chaining, it should be the case that everything in
          the context is required to be in the output context: the
          more interesting case for "maybe used" contexts should only
          arise during backward chaining proof search, I think. -rjs
          2012-03-29 *)
          [ "surprised that this happened!!!" ]
     | optionalItem (varname, data, SOME Context.AFF) =
          [ dataStr data^" aff" ]
     | optionalItem (varname, data, SOME Context.INT) =
          (* This seemed to work on the simple examples - is it really
          as simple as saying that the variable-stuff has names and
          the resource-stuff has a varname of emptystring? It's
          certainly a reasonable approximation - rjs 2012-03-29 *)
          if varname = "" then [ dataStr data ^ " pers" ]
          else [ varname^":"^dataStr data ]

   fun mandatoryItem (varname, data, NONE) =
          raise Invariant "mandatory item cannot also be removed from context!"
     | mandatoryItem (varname, data, SOME Context.LIN) =
          [ dataStr data^" lin" ]
     | mandatoryItem (varname, data, SOME modality) =
          (* Linear things are the only ones required to be in the context *)
          raise Invariant "Only linear things should be required!"
in
   case (lcontext, context) of
      ([], []) => []
    | ([], x :: xs) => optionalItem x @ prepCtx (lcontext, i+1, xs)
    | (j :: js, []) => raise Invariant "lcontext doesn't match context"
    | (j :: js, x :: xs) =>
         if j < i then raise Invariant "j < i should be impossible"
         else if j = i then mandatoryItem x @ prepCtx (js, i+1, xs)
         else optionalItem x @ prepCtx (lcontext, i+1, xs)
end

(* ctxToString : context -> (judgment list * name list) *)
and ctxToString (lcontext, context) =
let
   (* XXX PERF - Pretty terrible (quadratic at least) -rjs 2012-03-29 *)
   fun rename [] = []
     | rename ((item as (x, jdg, modality)) :: context) =
       let
          val context' = rename context
          fun inlist z = List.exists (fn (y,_,_) => z = y) context'
          fun loop x i =
             if inlist (x^"_"^Int.toString i)
             then loop x (i+1) else (x^"_"^Int.toString i)
          val x' = if x = "" then "x" else x
       in
          if inlist x' then (loop x' 1, jdg, modality) :: context'
          else (x', jdg, modality) :: context'
       end
   val renamedContext = rename (Context.ctx2list context)
   val printableCtx = rev (prepCtx (lcontext, 1, renamedContext))
in
  (printableCtx, map (#1) renamedContext)
end


(* XXX *)
   fun layout n [] = print "(nothing)\n"
     | layout n [ x ] =
          if n + size x > 80 then print ("\n   "^x^"\n") else print (x^"\n")
     | layout n (x :: xs) =
          if n + size x + 2 > 80
          then (print ("\n   "^x^", "); layout (size x + 5) xs)
          else (print (x^", "); layout (n + size x + 2) xs)




val pBindCtx = depPatBind {dep = fn A => (A, []), nodep = fn A => (A, heads A)}

fun pBindLCtx p l =
	let fun bind (n, p, l) = case Pattern.prj p of
			  PDepTensor (p1, p2) => bind (n, p2, bind (n + nbinds p2, p1, l))
			| PDown _ => n::l
			| _ => l (* POne, PAffi, PBang *)
	in bind (1, p, map (fn k => k + nbinds p) l) end

fun pBind (p, sty) (l, ctx) = (pBindLCtx p l, pBindCtx (p, sty) ctx)

fun linDiff ctxs =
	let fun f (SOME INT, SOME INT) = SOME INT
		  | f (SOME AFF, _) = SOME AFF
		  | f (NONE, NONE) = NONE
		  | f (SOME LIN, SOME LIN) = NONE
		  | f (SOME LIN, NONE) = SOME LIN
		  | f _ = raise Fail "Internal error: linDiff"
		fun g ((x, A, m1), (_, _, m2)) = (x, A, f (m1, m2))
		val diffctx = listPairMapEq g $ map12 ctx2list ctx2list ctxs
		fun allLin (_, []) = []
		  | allLin (n, (_, _, SOME LIN)::ctx) = n :: allLin (n+1, ctx)
		  | allLin (n, _::ctx) = allLin (n+1, ctx)
	in (allLin (1, diffctx), list2ctx diffctx) end

(* removeHyp : (lcontext * context) * int -> lcontext * context *)
(* Removes a variable from both lcontext and context. *)
fun removeHyp ((l, ctx), k) = (List.filter (fn n => n<>k) l, #1 $ ctxLookupNum (ctx, k))

(* Given a list of linear indices (an lcontext), remove those indices that no
 * longer occur in the context. *)
(* linIntersect' : int * lcontext * (string * 'a * cmodality) list -> lcontext *)
(* linIntersect : lcontext * context -> lcontext * context *)
(* FIXME: improve complexity: use a multilookup in Context based on drop *)
fun linIntersect' (n, k::l, (x, A, m)::G) =
		if n=k then
			if m=SOME LIN then k :: linIntersect' (n+1, l, G)
			else linIntersect' (n+1, l, G)
		else linIntersect' (n+1, k::l, G)
  | linIntersect' (_, [], _) = []
  | linIntersect' (_, _::_, []) = raise Fail "Internal error: linIntersect: malformed lctx"
fun linIntersect (l, ctx) = (linIntersect' (1, l, ctx2list ctx), ctx)

(* cannotConsumeLin : syncType -> bool *)
(* Checks whether an object of the given type can consume linear resources. *)
fun cannotConsumeLin sty = case SyncType.prj sty of
	  LExists (_, S1, S2) => cannotConsumeLin S1 andalso cannotConsumeLin S2
	| TDown _ => false
	| _ => true (* TOne, TAffi, TBang *)

(* multSplit : syncType ->
	{fst : lcontext * context -> lcontext * context,
	 snd : lcontext * context -> lcontext * context} *)
(* For a multiplicative context split involving the search for two objects
 * of type A and B, beginning with the search for A; multSplit B returns two
 * functions, fst and snd, which determine the lcontext for the individual
 * searches based on the lcontext for the combined object. *)
fun multSplit sty2 =
	if cannotConsumeLin sty2 then
		{ fst = fn (l, ctx) => (l, ctx),
		  snd = fn (_, ctxm) => ([], ctxm) }
	else
		{ fst = fn (_, ctx) => ([], ctx),
		  snd = linIntersect }


fun genMon (ctx : context, p, sty) =
	let val intCtx = ref NONE
		fun getIntCtx () = case !intCtx of
			  SOME G => SOME G
			| NONE => ( intCtx := (SOME $ ctxIntPart $ ctxMap #1 ctx) ; getIntCtx () )
		fun gen (p, sty) = case (Pattern.prj p, SyncType.prj sty) of
			  (PDepTensor (p1, p2), LExists (p1', S1, S2)) =>
					DepPair' (gen (Util.patternAddDep (p1, p1'), S1), gen (p2, S2))
			| (POne, TOne) => One'
			| (PDown (), TDown _) => MonUndef'
			| (PAffi (), TAffi _) => MonUndef'
			| (PBang NONE, TBang _) => MonUndef'
			| (PBang (SOME x), TBang A) =>
				let val X = newLVarCtx (getIntCtx ()) A
					val () = case Obj.prj X of Atomic (h, _) => Unify.pruneLVar $ normalizeHead h
							| _ => raise Fail "Internal error: lvar expected"
				in Bang' X end
			| _ => raise Fail "Internal error: genMon"
		fun gen' sty = case SyncType.prj sty of
			  LExists (p, S1, S2) => DepPair' (gen (p, S1), gen' S2)
			| TOne => One'
			| _ => MonUndef'
	in case p of NONE => gen' sty | SOME p => gen (p, sty) end

fun traceLeftFocus (h, ty) =
	if !traceSolve >= 2 then
		print ("Trying "^PrettyPrint.printPreObj (Atomic' (h, Nil'))^
				" : "^PrettyPrint.printType ty^"\n")
	else ()

val totalCtxLength = ref 0
val totalAvailLength = ref 0

fun syncType2pat sty =
   case SyncType.prj sty of
      LExists (p, _, S2) => PDepTensor' (p, syncType2pat S2)
    | TOne => POne'
    | TDown A => PDown' ()
    | TAffi A => PAffi' ()
    | TBang A => PBang' NONE

(* solve : (lcontext * context) * asyncType * (obj * context -> unit) -> unit *)
(* Right Inversion : Gamma;Delta => A *)
fun solve (ctx, ty, sc) =
	( if !traceSolve >= 3 then
		print ("Right Invert ("^PrettyPrint.printType ty^")\n")
	  else ()
	; solve' (ctx, ty, sc) )
and solve' (ctx, ty, sc) = case Util.typePrjAbbrev ty of
	  TLPi (p, S, A) => solve (pBind (p, S) ctx, A,
			(* The pattern p is a type pattern, which means that
			   it only names dependent variables. We need to create
			   p', the term pattern, which binds all the
			   variables; Util.patternT2O does this. *)
			fn (N, ctxo) => let val p' = Util.patternT2O p in
				sc (LLam' (p', N), patUnbind (p', ctxo)) end)
	(*| AddProd (A, B) => solve (ctx, A,
			fn (N1, ctxo1) => solve (ctx, B,
			fn (N2, ctxo2) => Option.app (fn ctxo => sc (AddPair' (N1, N2), ctxo))
				(ctxAddJoinOpt (ctxo1, ctxo2))))*)
	| AddProd (A, B) => solve (ctx, A,
			fn (N1, ctxo1) => solve (linDiff (#2 ctx, ctxo1), B,
			fn (N2, ctxo2) => sc (AddPair' (N1, N2),
					ctxAddJoin (ctxo1, ctxJoinAffLin (ctxo2, ctxo1)))))
	| TMonad S => forwardChain (!fcLimit, ctx, S, fn (E, ctxo) => sc (Monad' E, ctxo))
	| P as TAtomic _ => matchAtom (ctx, P, sc)
	| TAbbrev _ => raise Fail "Internal error: solve: TAbbrev"

(* matchAtom : (lcontext * context) * asyncType asyncTypeF * (obj * context -> unit) -> unit *)
(* Choice point: choose hypothesis and switch from Right Inversion to Left Focusing *)
and matchAtom (ctx, P, sc) =
	( if !traceSolve >= 2 then
		print ("Subgoal: MatchAtom ("^PrettyPrint.printType (AsyncType.inj P)^")\n")
	  else ()
	; matchAtom' (ctx, P, sc) )
and matchAtom' (ctx, P, sc) =
	let val aP = (case P of TAtomic (a, _) => a
					| _ => raise Fail "Internal error: wrong argument to matchAtom!")
	    val P' = AsyncType.inj P
	    fun lFocus (ctx', lr, A, h) = fn () =>
					     ( traceLeftFocus (h, A)
					     ; leftFocus (lr, ctx', P', A, fn (S, ctxo) =>
									      sc (Atomic' (h, S), ctxo)) )
	    fun matchSig (c, lr, A) = fn () => BackTrack.backtrack (lFocus (ctx, lr, A, Const c))
	    fun matchCtx ([], _) = []
	      | matchCtx ((_, _, NONE)::G, k) = matchCtx (G, k+1)
              | matchCtx ((_, (_, nil), _)::G, k) = matchCtx (G, k+1)
	      | matchCtx ((x, (A, hds), SOME modality)::G, k) =
		if #2 (List.hd hds) <> HdAtom(aP) andalso List.tl hds = nil
		then matchCtx (G, k+1)
		else
		let (* val ctx' = if modality=INT then ctx else removeHyp (ctx, k) *)
		    val A' = TClos (A, Subst.shift k)
		    val h = Var (modality, k)
		    val validHds = List.filter (fn (_, HdAtom a) => a=aP | _ => false) hds
		    val candidates = List.map (fn (lr, _) => (fn () => BackTrack.backtrack (lFocus (if modality=INT then ctx else removeHyp (ctx, k), lr, A', h)))) validHds
		in candidates @ matchCtx (G, k+1) end
	    val ctxlist = ctx2list $ #2 ctx
	    val available = matchCtx (ctxlist, 1)
(*
	    val ctxLength = List.length ctxlist
	    val () = totalCtxLength := !totalCtxLength+ctxLength
	    val availLength = List.length available
	    val () = totalAvailLength := !totalAvailLength+availLength
	    val () = TextIO.print (Int.toString (!totalCtxLength) ^ " -> " ^ Int.toString (!totalAvailLength)
				   ^ "\n")
*)
	in
(*	  Timers.time Timers.fairness (fn () => PermuteList.forAll (fn f => f ())
	  (PermuteList.fromList (matchCtx (ctx2list $ #2 ctx, 1) @ map matchSig (getCandAtomic aP)))) ()
*)
	  PermuteList.forAll (fn f => f ())
	     (PermuteList.fromList (available @ map matchSig (getCandAtomic aP)))
	end

(* forwardStep : (lcontext * context)
      -> ((lcontext * context) *
          whatever it is that nbinds returns *
          (pattern * pattern's type * atomicterm)) option *)
and forwardStep (l, ctx) =
let
   fun mlFocus (ctx', lr, A, h) =
      fn commitExn =>
       ( traceLeftFocus (h, A)
       ; monLeftFocus (lr, ctx', A,
                       fn (S, sty, ctxo) =>
                          if !allowConstr orelse
                             Unify.constrsSolvable (Atomic' (h, S))
                          then raise commitExn ((h, S), sty, ctxo)
                          else () ) )

   (* matchCtx and matchSig are going to give us the different ways of
   trying to foward chain one step in the current context as functions
   unit -> a term that can be derived, the synchronous type that
   left-focusing on this term will dump into the context, and
   presumably the new, modified context.

   Each of these candidate functions implement "if I try this rule,
   what progress do I make in the current context?" The PermuteList
   stuff allows us to then try all the possibilities in some
   (unspecified) order. *)

   fun matchSig (c, lr, A) =
      fn () => BackTrack.backtrackC (mlFocus (ctx, lr, A, Const c))

   fun matchCtx ([], _) = []
     | matchCtx ((_, _, NONE)::G, k) = matchCtx (G, k+1)
     | matchCtx ((x, (A, hds), SOME modality)::G, k) =
       let
          val ctx' = if modality=INT then ctx else #2 $ removeHyp (([], ctx), k)
          val A' = TClos (A, Subst.shift k)
       in List.mapPartial
             (fn (_, HdAtom _) => NONE
               | (lr, HdMonad) =>
                    SOME (fn () =>
                             BackTrack.backtrackC
                                (mlFocus (ctx', lr, A', Var (modality, k)))))
             hds
          @ matchCtx (G, k+1)
       end

   val candidates1 = matchCtx (ctx2list $ ctx, 1)
   val candidates2 = map matchSig (getCandMonad ())
   val candidates = candidates1 @ candidates2

   val () =
      if !debugForwardChaining
      then
       (* using #trace this context-printing is maybe no longer needed? *)
       (*print "Context: "
       ; printCtx (l, ctx) *)
       ( print (Int.toString (length candidates) ^ " candidates")
       ; if (length candidates1 > 0)
         then print (", " ^ Int.toString (length candidates2)
                     ^ " from the context. <press ENTER to continue>")
         else print (". <press ENTER to continue>")
       ; TextIO.flushOut TextIO.stdOut
       ; ignore (TextIO.inputLine TextIO.stdIn))
      else ()

in case PermuteList.findSome (fn f => f ())
           (PermuteList.fromList candidates) of
      NONE => NONE
    | SOME (N, sty, ctxm) =>
      let
         val p = syncType2pat sty
         val p' = Util.patternT2O p
      in
         SOME ((pBind (p, sty) $ linIntersect (l, ctxm)),
               nbinds p,
               (p', sty, N))
      end
end


(* forwardChain : int * (lcontext * context) * syncType * (expObj * context -> unit) -> unit *)
and forwardChain (fcLim, ctx, S, sc) =
 ( if !traceSolve >= 2
   then print ("ForwardChain ("^PrettyPrint.printType (TMonad' S)^")\n")
   else ()
 ; forwardChain' (fcLim, ctx, S, sc) )

and forwardChain' (fcLim, (l, ctx), S, sc) =
let in
   if fcLim = SOME 0
   then rightFocus ((l, ctx), genMon (ctx, NONE, S), S,
                    fn (M, ctxo) => sc (Mon' M, ctxo))
   else
     (case forwardStep (l, ctx) of
         NONE => rightFocus ((l, ctx), genMon (ctx, NONE, S), S,
                             fn (M, ctxo) => sc (Mon' M, ctxo))
       | SOME (newctx, newbinds, (p, sty, N)) =>
         let
            val () = if !traceSolve >= 1
                     then print ("Committing:\n   let {_} = "
                                 ^PrettyPrint.printObj (Atomic' N)
                                 ^" : {"^PrettyPrint.printSyncType sty^"}\n")
                     else ()
         in forwardChain' (Option.map (fn x => x - 1) fcLim,
                           newctx,
                           STClos (S, Subst.shift $ newbinds),
                           fn (E, ctxo) =>
                              sc (Let' (p, N, E), patUnbind (p, ctxo)))
         end)
end

(* rightFocus : (lcontext * context) * monadObj * syncType * (monadObj * context -> unit) -> unit *)
and rightFocus (ctx, m, sty, sc) =
	( if !traceSolve >= 3 then
		print ("RightFocus ("^PrettyPrint.printType (TMonad' sty)^")\n")
	  else ()
	; rightFocus' (ctx, m, sty, sc) )
and rightFocus' ((l, ctx), m, sty, sc) = case (MonadObj.prj m, SyncType.prj sty) of
	  (DepPair (m1, m2), LExists (p, S1, S2)) =>
		let val {fst, snd} = multSplit S2
		in rightFocus (fst (l, ctx), m1, S1, fn (M1, ctxm) =>
			rightFocus (snd (l, ctxm), m2,
				STClos (S2, Subst.subM $ normalizeMonadObj M1), (* M1=m1 on free vars in S2 *)
				fn (M2, ctxo) => sc (DepPair' (M1, M2), ctxo)))
		end
	| (One, TOne) => if l <> [] then () else sc (One', ctx)
	| (MonUndef, TDown A) => solve ((l, ctx), A, fn (N, ctxo) => sc (Down' N, ctxo))
	| (MonUndef, TAffi A) => if l <> [] then () else
			solve (([], ctxAffPart ctx), A, fn (N, ctxo) => sc (Affi' N, ctxJoinAffLin (ctxo, ctx)))
	| (MonUndef, TBang A) => if l <> [] then () else
			solve (([], ctxIntPart ctx), A, fn (N, _) => sc (Bang' N, ctx))
	| (Bang N, TBang A) => if l <> [] then () else sc (Bang' N, ctx)
	| _ => raise Fail "Internal error: rightFocus: partial monadObj mismatch"

(* leftFocus : lr list * (lcontext * context) * asyncType * asyncType * (spine * context -> unit) -> unit *)
(* Left Focusing : Gamma;Delta;A >> P  ~~  leftFocus (LR-Oracle, Gamma;Delta, P, A, SuccCont)
 * Construct the spine corresponding to the chosen hypothesis. *)
and leftFocus (lr, ctx, P, ty, sc) =
	( if !traceSolve >= 3 then
		print ("LeftFocus ("^PrettyPrint.printType ty^")\n")
	  else ()
	; leftFocus' (lr, ctx, P, ty, sc) )
and leftFocus' (lr, (l, ctx), P, ty, sc) = case Util.typePrjAbbrev ty of
	  TLPi (p, A, B) =>
		let val m = genMon (ctx, SOME p, A)
			val {fst, snd} = multSplit A
		in leftFocus (lr, fst (l, ctx), P, TClos (B, Subst.subM $ normalizeMonadObj m),
			fn (S, ctxm) => rightFocus (snd (l, ctxm), m, A,
			fn (M, ctxo) => sc (LApp' (M, S), ctxo)))
		end
	| AddProd (A, B) => (case lr of
			  [] => raise Fail "Internal error: LR-oracle is out of answers"
			| L::lrs => leftFocus (lrs, (l, ctx), P, A, fn (S, ctxo) => sc (ProjLeft' S, ctxo))
			| R::lrs => leftFocus (lrs, (l, ctx), P, B, fn (S, ctxo) => sc (ProjRight' S, ctxo)))
	| TMonad S => raise Fail "Internal error: leftFocus applied to monadic hypothesis!"
	| P' as TAtomic _ =>
		if l=[] then Unify.unifyAndBranch (AsyncType.inj P', P, fn () => sc (Nil', ctx))
		else ()
	| TAbbrev _ => raise Fail "Internal error: leftFocus: TAbbrev"

(* monLeftFocus : lr list * context * asyncType * (spine * syncType * context -> unit) -> unit *)
and monLeftFocus (lr, ctx, ty, sc) =
	( if !traceSolve >= 3 then
		print ("monLeftFocus ("^PrettyPrint.printType ty^")\n")
	  else ()
	; monLeftFocus' (lr, ctx, ty, sc) )
and monLeftFocus' (lr, ctx, ty, sc) = case Util.typePrjAbbrev ty of
	  TLPi (p, A, B) => let val m = genMon (ctx, SOME p, A)
			in rightFocus (([], ctx), m, A, fn (M, ctxm) =>
				monLeftFocus (lr, ctxm,
					TClos (B, Subst.subM $ normalizeMonadObj m),
					fn (S, sty, ctxo) => sc (LApp' (M, S), sty, ctxo)))
			end
	| AddProd (A, B) => (case lr of
			  [] => raise Fail "Internal error: LR-oracle is out of answers!"
			| L::lrs => monLeftFocus (lrs, ctx, A,
				fn (S, sty, ctxo) => sc (ProjLeft' S, sty, ctxo))
			| R::lrs => monLeftFocus (lrs, ctx, B,
				fn (S, sty, ctxo) => sc (ProjRight' S, sty, ctxo)))
	| TMonad sty => sc (Nil', sty, ctx)
	| TAtomic _ => raise Fail "Internal error: monLeftFocus applied to wrong hypothesis!"
	| TAbbrev _ => raise Fail "Internal error: monLeftFocus: TAbbrev"


(* solveEC : asyncType * (obj -> unit) -> unit *)
fun solveEC (ty, sc) = solve (([], emptyCtx), ty, sc o #1)


fun exec limit sty =
let
  fun loop context count =
    if limit = SOME count then (count, context) else
      case forwardStep context of
           NONE => (count, context)
         | SOME (context', _, _) => loop context' (count+1)
  val (count, context') =
   loop (pBind (syncType2pat sty, sty) ([], Context.emptyCtx)) 0
  (* Print the context *)
  val (ctxStrings, ctxNames) = ctxToString context'
  val () = print "\n-- "
  val () = layout 3 ctxStrings
  val () = print "\n--------------\n"
in
  (count, context')
end

(* TODO print initial context, here or in TypeRecon *)
fun trace limit sty =
let
   fun loop context count =
     if limit = SOME count then (count, context) else
          case forwardStep context of
              NONE => (count, context)
            | SOME (context', _, (pat, pat_type, atomic_term)) =>
                (* Print out the epsilon *)
                let
                  val (ctxStrings, ctxNames) = ctxToString context'
                  (* Chris & Rob printing out extra information
                   * trying to get the actual epsilon instead of the types
                   * (Added in 9ccfb8c8143b4f0f429322f913cb1cdfffd1eab5,
                   * removed July 25, 2013. -rjs)
                  val () = print "\nPATTERN TYPE:\n"
                  val () = print (PrettyPrint.printSyncType pat_type)
                  val () = print "\nAtomic term:\n"
                  val () = print
                    (PrettyPrint.printPreObjInCtx ctxNames
                       (Syntax.Atomic' atomic_term))
                  val () = print "\n"
                  *)
                  val () = print "-- "
                  val () = layout 3 ctxStrings
                  (* Complementary to printing out extra infomration
                   * trying to get the actual epsilon instead of the types -rjs
                  val () = print "\n--------------\n"
                  *)
                in (* Continue *)
                  loop context' (count+1)
                end
in
   loop (pBind (syncType2pat sty, sty) ([], Context.emptyCtx)) 0
end

end
