# FLP -- funkcionální projekt -- 2023
## Autor: Dominik Nejedlý (xnejed09)

Adresář `test/` obsahuje testovací skript `test.sh` pro a dva podadresáře:

- `in/` -- obsahuje testovací vstupy (soubory tvaru `*.in`)
- `out/` -- je rozdělen na podadresáře dle jednotlivých přepínačů, přičemž každý z nich obsahuje odpovídající testovací výstupy (soubory tvaru `*.out`)

Automatické testy lze spustit pomocí souboru `Makefile` příkazem `make tests` (spustí testování všech přepínačů -- `-i`, `-b` a `-o`),
případně spuštěním skriptu `test.sh`, který na svém vstupu bere stejné přepínače jako testovaný program (tedy `-i`, `-b`, `-o`). Ty určují,
jaké přepínače májí být testovány (může jich být zadáno i více). Testování jednotlivých přepínačů pak vždy probíhá v pořadí: `-i`, `-b`, `-o`.
Výsledky testů jsou tisknuty na standardní výstup. V případě neúspěšného testu je vypsán i získaný a očekávaný výsledek. Nakonec je vypsán
celkový počet úspěšných a neúspěšných testů.
