# Projet_LTPF

## 1 - Préliminiaires théoriques

### 1.1.2 Définition et analyse d’un langage de programmation simple



## WHILEb⁻⁻

Une première Grammaire (récursive à gauche indirecte) : 

  Grammaire :

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
    être une séquence qui elle même est constituée de suites d'instruction ect. <br>
==> On s'embarque dans une boucle infinie.

---

### 1.1.3 Grammaire non récursive à gauche 


  Grammaire :

  * **Var** ::= 'a' | 'b' | 'c' | 'd'<br>
  * **Cst** ::= 0 | 1<br>
  * **Expr** ::= Cst | Var<br>
  * **Instr** ::=  Assign | If | While<br>
  * **InstrSuite** ::= ';' Instr InstrSuite | epsilon<br>
  * **Assign** ::= Var ':' '=' Expr<br>
  * **If** ::= 'i' '(' Var ')' '{' Prog '}' '{' Prog '}'<br>
  * **While** ::= 'w' '(' Var ')' '{' Prog '}'<br>
  * **Prog** ::= Instr InstrSuite <br>


## WHILEb

### 1.1.4 Grammaire pour le langage WHILEb


* **C** ::= '0' | '1' <br>
* **V** ::= 'a' |'b' | 'c' | 'd'<br>
* **A** ::= C | V<br>
* **F** ::= '!' F | A | '(' E ')'<br>

* **E** ::= T SE<br>
* **SE** ::= '+' T SE | epsilon<br>

* **T** ::= F ST<br>
* **ST** ::= '.' F ST | epsilon <br>

--- 
## 1.2 Sémantique Naturelle (SN)

### 1.2.1 Règles SN pour If 

| If true  | If false  |
|----------|-----------|
| ![alt text](SN_If_true.png) | ![alt text](SN_If_false.png) |

-> Cette sémantique naturelle du If évalue l'expression depuis l'état initial s1. 
- **true** : à partir de l'état s1, on aboutit à l'état final s2 en exécutant P.
- **false** : à partir de l'état s1, on aboutit à l'état final s2 en exécutant Q'.
