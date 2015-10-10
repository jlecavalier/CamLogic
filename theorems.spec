# Absorption
~((P=>Q)=>(P=>(P&Q)));

# Biconditional elimination
~((P<=>Q)=>(P=>Q));
~((P<=>Q)=>(Q=>P));

# Biconditional introduction
~(((P=>Q)&(Q=>P))=>(P<=>Q));

# Case analysis
~((((P=>Q)&(R=>Q))&(P|R))=>Q);

# Commutativity of conjunction
~((P&Q)<=>(Q&P));

# Conjunction elimination
~(((P&Q)=>P)&((P&Q)=>Q));

# Consequentia mirabilis
~((~A=>A)=>A);
~((~~A|A)=>A);

# Constructive dilemma
~((((P=>Q)&(R=>S))&(P|R))=>(Q|S));

# Contraposition
~((P=>Q)<=>(~Q=>~P));

# DeMorgan's laws
~(~(P&Q)<=>(~P|~Q));
~(~(P|Q)<=>(~P&~Q));

# Destructive dilemma
~((((P=>Q)&(R=>S))&(~Q|~S))=>(~P|~R));

# Disjunction introduction
~(P=>(P|Q));

# Disjunctive syllogism
~(((P|Q)&~P)=>Q);

# Distributive properties
~((P&(Q|R))<=>((P&Q)|(P&R)));
~((P|(Q&R))<=>((P|Q)&(P|R)));

# Double negation
~(~~A<=>A);

# Exportation
~(((P&Q)=>R)<=>(P=>(Q=>R)));

# Frege's theorem
~((P=>(Q=>R))=>((P=>Q)=>(P=>R)));

# Hypothetical syllogism
~(((P=>Q)&(Q=>R))=>(P=>R));

# Idempotency of entailment
~(((C&C)=>B)=>(C=>B));

# Law of excluded middle
~(P|~P);

# Law of noncontradiction
P&~P;

# Modus ponendo tollens
~((~(A&B)&A)=>~B);

# Modus ponens
~(((P=>Q)&P)=>Q);

# Modus tolens
~(((P=>Q)&~Q)=>~P);

# Monotonicity of entailment
~(Q=>(P=>Q));

# Peirce's law
~(((P=>Q)=>P)=>P);

# Principle of explosion
~(FALSE=>P);

# Reductio ad absurdum
~((P=>FALSE)=>~P);