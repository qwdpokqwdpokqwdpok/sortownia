(** Sortowanie topologiczne *)

exception Cykliczne
(** wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)

val topol : ('a * 'a list) list -> 'a list
(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il *)
    
    
    
(* usuwanie niezaleznych wierzcholkow (Kahn's algorithm) *)

open PMap


let topol lista_sasiadow =
(* mapa z modulu PMap, w wezlach znajduja sie pary (wierzcholek, lista sasiadow tego wierzcholka) *)
    let mapa_sasiadow =
        List.fold_left (fun acc (wierzcholek, lista) -> add wierzcholek lista acc) empty lista_sasiadow in
(* mapa z modulu PMap, w wezlach znajduja sie pary (wierzcholek, stopien wchodzacy tego wierzcholka) *)
    let mapa_stopni =
(* funkcja dodajaca do mapy stopni pare (wierzcholek, lista sasiadow tego wierzcholka) *)
        let dodaj_pare (wierzcholek, lista) mapa =
(* najpierw dodawanie samego wierzcholka *)
(* dodajac wierzcholek do mapy nie zwiekszamy oczywiscie stopnia wchodzacego tego wierzcholka *)
            let mapa_po_dodaniu_wierzcholka =
                if exists wierzcholek mapa then mapa
                else add wierzcholek (ref 0) mapa in
(* dodawanie sasiada *)
            let dodaj_sasiada sasiad mapa =
(* jezeli sasiad byl w mapie, jego stopien wchodzacy zwieksza sie o 1 *)
                if exists sasiad mapa then
                    let stopien = find sasiad mapa in
                    begin
                        stopien =: !stopien +1;
                        mapa
                    end
(* jezeli sasiada nie bylo w mapie, to nalezy dodac go ze stopniem wchodzacym 1 *)
                else add sasiad (ref 1) mapa in
            List.fold_left dodaj_sasiada mapa_po_dodaniu_wierzcholka lista in
        ref (List.fold_left dodaj_pare empty list_sasiadow) in
(* zbior wierzcholkow o stopniu wchodzacym rownym 0 *)
    let zbior1 = ref [] in
(* zbior wierzcholkow o stopniu wchodzacym rownym 0 *)
    let zbior2 = ref [] in
(* odwrocony wynik *)
    let wynik = ref [] in
(* funkcja usuwa wierzcholek z mapy *)
(* dodaje wierzcholek do wyniku *)
(* zmniejsza stopien wszysktich jego sasiadow o 1 *)
    let usun_wierzcholek wierzcholek =
        begin
            mapa_stopni =: remove wierzcholek !mapa_stopni;
            wynik =: wierzcholek::(!wynik);
(* jezeli wierzcholek ma sasiada *)
            if exists wierzcholek mapa_sasiadow then
(* funkcja zmniejszajaca stopien sasiada *)
(* jezeli stopien jest rowny 0, sasiad jest zapamietywany *)
                let zmniejsz_stopien sasiad =
                    let stopien = find sasiad mapa_stopni in
                    stopien =: !stopien - 1;
                    if stopien = 0 then zbior1 =: sasiad::(!zbior2) in
                List.iter zmniejsz_stopien (find wierzcholek mapa_sasiadow);
        end in
    begin




