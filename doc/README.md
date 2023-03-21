# FLP -- funkcionální projekt -- 2023
## Autor: Dominik Nejedlý (xnejed09)

Všechny části zadání byly implementovány. Program dále umožňuje:

- zadání více vstupních přepínačů současně. Ty jsou poté vyhodnocovány v pořadí: `-i`, `-b`, `-o`.
- správné načtení vstupu i s přeházenými políčky -- stačí tedy dodržení základní struktury a klíčových slov (bílé znaky nejsou vyžadovány)

Prohledávání hrubou silou odřezává ty části stavového prostoru, které již překročili maximální kapacitu batohu `maxWeight` nebo již nemohou
splnit minimální cenu `minCost`.

Genetický algoritmus nevykazuje v současném nastavení přílišnou úspěšnost. Jeho úspěšnost však doně záleží na jeho přednastavených parametrech.
Aktuálně je velikost populace i počet generací nastaven tak, aby byl celkový výpočet netrval příliš dlouho a stabilněji procházeli alespoň 3-4
přiložené testovací případy. Všechny parametry lze však jednoduše upravovat v souboru `src/GeneticAlg.hs`. Nutno poznamenat, že při větší
velikosti populace nebo vyšším počtu generací je výpočet znatelně pomalejší.
