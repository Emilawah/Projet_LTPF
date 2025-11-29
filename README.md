# Projet_LTPF

Auteur : Emilio STIEN & Kelig LE MARREC <br>
Date : Novembre 2025 <br>

--- 

## 1 - Préliminaires théoriques

### 1.1.2 Définition et analyse d’un langage de programmation simple

### WHILEb⁻⁻

Une première Grammaire (récursive à gauche indirecte) : 

**Grammaire :**

* **Var** ::= 'a' | 'b' | 'c' | 'd'<br>
* **Cst** ::= 0 | 1<br>
* **Expr** ::= Cst | Var<br>
* **Instr** ::= Assign | Seq | If | While | epsilon<br>
* **Assign** ::= Var ':' '=' Expr<br>
* **Seq** ::= Instr ';' Instr <br>
* **If** ::= 'i' '(' Var ')' '{' Prog '}' '{' Prog '}'<br>
* **While** ::= 'w' '(' Var ')' '{' Prog '}'<br>
* **Prog** ::= Prog ';' Instr | Instr<br>


->  En effet, lorsque qu'on appelle une instruction, cela peut
    être une séquence qui elle même est constituée de suites d'instruction etc. <br>
==> On s'embarque dans une boucle infinie.

### 1.1.3 Grammaire non récursive à gauche 

**Grammaire :**

* **Var** ::= 'a' | 'b' | 'c' | 'd'<br>
* **Cst** ::= 0 | 1<br>
* **Expr** ::= Cst | Var<br>
* **Instr** ::=  Assign | If | While<br>
* **InstrSuite** ::= ';' Instr InstrSuite | ε<br>
* **Assign** ::= Var ':' '=' Expr<br>
* **If** ::= 'i' '(' Var ')' '{' Prog '}' '{' Prog '}'<br>
* **While** ::= 'w' '(' Var ')' '{' Prog '}'<br>
* **Prog** ::= Instr InstrSuite <br>

### WHILEb

### 1.1.4 Grammaire pour le langage WHILEb

* **C** ::= '0' | '1' <br>
* **V** ::= 'a' |'b' | 'c' | 'd'<br>
* **A** ::= C | V<br>
* **F** ::= '!' F | A | '(' E ')'<br>
* **E** ::= T SE<br>
* **SE** ::= '+' T SE | epsilon<br>
* **T** ::= F ST<br>
* **ST** ::= '.' F ST | epsilon <br>

### 1.2 Sémantique naturelle (SN)

#### 1.2.1 Règles pour l'instruction conditionnelle (If)

**Notations :**
* `s` : état initial
* `s'` : état final
* `[[expr]]_s` : valeur de l'expression 'expr' évaluée dans l'état 's'
* `s -P-> s'` : l'exécution du programme P à partir de s conduit à s'

**Règle 1 : Cas VRAI (Condition = 1)**

```text
      [[expr]]_s = 1       s --P--> s'
    ---------------------------------------
      s --if expr then P else Q--> s'
```

**Règle 2 : Cas FAUX (Condition = 0)**

```text
[[expr]]_s = 0       s --Q--> s'
    ---------------------------------------
      s --if expr then P else Q--> s'
```