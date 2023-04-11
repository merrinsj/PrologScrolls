:- style_check(-singleton).
:- dynamic current_node_is/1, equipped/2, located/2, health/2, defense/2, attack/2, magic_attack/2, magic_defense/2, prev_node/1, status/2,
necromancer_status/2, gold/1, potion_count/2, interactable/2, experience/1, level/1, current_character/1, game_begin/0, traverse_darkness/1, traverse_height/1, traverse_water/1.


/*
To play the game load this file in SWI-Prolog (or any equivalent)
and then type 'play.'
*/

different(X, X) :- !, fail.
different(X, Y).

/*
Create player's character.
*/

argonian :- create_character(argonian), set_current_character(argonian).
khajiit :- create_character(khajiit), set_current_character(khajiit).
kinko :- create_character(kinko), set_current_character(kinko).

set_current_character(Character) :-
    retractall(current_character(_)),
    assert(current_character(Character)).

get_current_character(Character) :-
    current_character(Character).

create_character(Race) :- 
    get_current_character(Character),
    write('You have already created your character!'), nl,
    write('You are currently a '), write(Character), nl.

create_character(Race) :-
    current_node_is(home),
    (Race = argonian ->
        assert(level(1)),
        assert(health(player, 50)),
        assert(defense(player, 1)),
        assert(attack(player, 1)),
        assert(magic_attack(player, 1)),
        assert(magic_defense(player, 1)),
        assert(traverse_water(true)),
        assert(traverse_darkness(false)),
        assert(traverse_height(false)),
        write('Argonian character created successfully!'), nl;
    Race = khajiit ->
        assert(level(1)),
        assert(health(player, 50)),
        assert(defense(player, 1)),
        assert(attack(player, 1)),
        assert(magic_attack(player, 1)),
        assert(magic_defense(player, 1)),
        assert(traverse_water(false)),
        assert(traverse_darkness(true)),
        assert(traverse_height(false)),
        write('Khajiit character created successfully!'), nl;
    Race = kinko ->
        assert(level(1)),
        assert(health(player, 50)),
        assert(defense(player, 1)),
        assert(attack(player, 1)),
        assert(magic_attack(player, 1)),
        assert(magic_defense(player, 1)),
        assert(traverse_water(false)),
        assert(traverse_darkness(false)),
        assert(traverse_height(true)),
        write('Kinko character created successfully!'), nl;
    write('Invalid race!'), nl), description(home).

/*
The players equipment.
*/
load_default_equipment :-
    assert(equipped(weapon_slot, nothing)),
    assert(equipped(armor_slot, nothing)),
    assert(equipped(head_slot, nothing)),
    assert(equipped(magic_slot, nothing)),
    assert(equipped(cape_slot, nothing)),
    assert(equipped(utility_slot, nothing)),
    assert(potion_count(health_potion, 0)),
    assert(experience(0)),
    assert(gold(0)).

add_gold(I) :- gold(X), NG is X + I, assert(gold(NG)), retract(gold(X)), format(" gold increased to : ~w", [NG]), nl.

add_exp(I) :- experience(X), NG is X + I, assert(experience(NG)), retract(experience(X)), format(" you gained : ~w exp!", [NG]), nl.

repair_boat :- 
    equipped(utility_slot, hammer), assert(traverse_water(true)), retract(traverse_water(false)),
    write("With the boat repaired, you can now cross the lake. Enter 'n.' to cross."), nl.

reset_stat(Stat) :-
    level(L),
    ( Stat = attack -> retract(attack(player, _)), assert(attack(player, L))
    ; Stat = defense -> retract(defense(player, _)), assert(defense(player, L))
    ; Stat = magic_attack -> retract(magic_attack(player, _)), assert(magic_attack(player, L))
    ; Stat = magic_defense -> retract(magic_defense(player, _)), assert(magic_defense(player, L))).

modify_stat(Stat, Modifier) :-
    ( Stat = attack -> attack(player, OldValue), NewValue is OldValue + Modifier, retract(attack(player, OldValue)), assert(attack(player, NewValue)), 
    format("Your ~w increased by ~w", [Stat, Modifier]), nl
    ; Stat = defense -> defense(player, OldValue), NewValue is OldValue + Modifier, retract(defense(player, OldValue)), assert(defense(player, NewValue)),
    format("Your ~w increased by ~w", [Stat, Modifier]), nl
    ; Stat = magic_attack -> magic_attack(player, OldValue), NewValue is OldValue + Modifier, retract(magic_attack(player, OldValue)), assert(magic_attack(player, NewValue)),
    format("Your ~w increased by ~w", [Stat, Modifier]), nl
    ; Stat = magic_defense -> magic_defense(player, OldValue), NewValue is OldValue + Modifier, retract(magic_defense(player, OldValue)), assert(magic_defense(player, NewValue)),
    format("Your ~w increased by ~w", [Stat, Modifier]), nl
    ).


increase_level :- 
    level(X),
    attack(player, A),
    defense(player, D),
    magic_attack(player, MA),
    magic_defense(player, MD),
    NL is X + 1,
    NA is A + 1,
    ND is D + 1,
    NMA is MA + 1,
    NMD is MD + 1,
    retract(level(X)),
    assert(level(NL)),

    retract(attack(player, A)),
    assert(attack(player, NA)),

    retract(defense(player, D)),
    assert(defense(player, ND)),

    retract(magic_defense(player, MD)),
    assert(magic_defense(player, NMD)),

    retract(magic_attack(player, MA)),
    assert(magic_attack(player, NMA)).

/*
stats for characters
*/

%necromancer_status(player, no).
health(mudcrab, 15).
health(ghost, 15).
health(necromancer, 50).
health(troll, 25).
health(skeleton, 20).

defense(mudcrab, 1).
defense(ghost, 100).
defense(necromancer, 5).
defense(troll, 3).
defense(skeleton, 4).

attack(mudcrab, 1).
attack(ghost, 0).
attack(necromancer, 7).
attack(troll, 3).
attack(skeleton, 1).

magic_attack(mudcrab, 1).
magic_attack(ghost, 2).
magic_attack(necromancer, 5).
magic_attack(troll, 0).
magic_attack(skeleton, 4).

magic_defense(mudcrab, 1).
magic_defense(ghost, 1).
magic_defense(necromancer, 5).
magic_defense(troll, 2).
magic_defense(skeleton, 4).

load_default_status :-
    assert(status(mudcrab, alive)),
    assert(status(ghost, alive)),
    assert(status(player, alive)),
    assert(status(necromancer, alive)),
    assert(status(troll, dead)),
    assert(status(skeleton, alive)).

/*
damage calculations
*/
deal_damage(X, Y) :- attack(X, AM), defense(Y, DM), health(Y, H), status(Y, alive), calc_damage(AM, DM, H, R), retract(health(Y, H)), assert(health(Y, R)),
 nl, format("~w has ~2f health after the attack from ~w", [Y, R, X]), nl, !.
deal_magic_damage(X, Y) :- magic_attack(X, AM), magic_defense(Y, DM), health(Y, H), status(Y, alive), calc_damage(AM, DM, H, R), retract(health(Y, H)), assert(health(Y, R)),
 nl, format("~w has ~2f health after the magic attack from ~w", [Y, R, X]), nl, !.

deal_damage_monster(X, Y) :- attack(X, AM), defense(Y, DM), magic_attack(X, MAM), magic_defense(Y, MDM), health(Y, H), status(Y, alive), calc_damage_physical(AM, DM, H, R1), calc_damage_magical(MAM, MDM, H, R2),
  R1 < R2, retract(health(Y, H)),assert(health(Y, R1)) , nl, 
  format("~w has ~2f health after the physical attack from ~w", [Y, R1, X]), nl, !.
deal_damage_monster(X, Y) :- attack(X, AM), defense(Y, DM), magic_attack(X, MAM), magic_defense(Y, MDM), health(Y, H), status(Y, alive), calc_damage_physical(AM, DM, H, R1), calc_damage_magical(MAM, MDM, H, R2),
  R1 > R2, retract(health(Y, H)),assert(health(Y, R2)) , nl, 
  format("~w has ~2f health after the magical attack from ~w", [Y, R2, X]), nl, !.
deal_damage_monster(X, Y) :- attack(X, AM), defense(Y, DM), magic_attack(X, MAM), magic_defense(Y, MDM), health(Y, H), status(Y, alive), calc_damage_physical(AM, DM, H, R1), calc_damage_magical(MAM, MDM, H, R2),
  R1 = R2, retract(health(Y, H)),assert(health(Y, R2)) , nl, 
  format("~w has ~2f health after the magical attack from ~w", [Y, R2, X]), nl, !.

calc_damage(AM, DM, H, R):-
 R is H - (10 * AM / DM).

calc_damage_physical(AM, DM, H, R):-
 R is H - (10 * AM / DM).

calc_damage_magical(MAM, MDM, H, R):-
 R is H - (10 * MAM / MDM).

/*
battle functions
*/
attack(Y):- deal_damage(player,Y), write("Player attacked!"), nl, deal_damage_monster(Y, player), write("Monster attacked!"), nl, check_dead(player), check_dead(Y).
magic_attack(Y):- deal_magic_damage(player,Y), deal_damage_monster(Y, player), check_dead(player), check_dead(Y).

check_dead(player):-  health(player, H), H < 1 , status(player, S), assert(status(player, dead)), retract(status(player, S)),
 nl, write("You are dead. Game Over."), nl, halt(1).

check_dead(X):- different(X, player), different(X, necromancer), health(X, H), H < 1 , status(X, S), assert(status(X, dead)), retract(status(X, S)),
 nl, write(X), write(" is dead. You are safe to travel again."), nl, current_node_is(N), prev_node(PN), dead_event(X),
 assert(current_node_is(PN)), retract(current_node_is(N)), retract(prev_node(PN)), !.

check_dead(X):- different(X, player), health(X, H), H < 1, status(X, S), assert(status(X, dead)), retract(status(X, S)), current_node_is(N), prev_node(PN),
 assert(current_node_is(PN)), retract(current_node_is(N)), retract(prev_node(PN)), scene(PN),
 nl, nl, !.

check_dead(_) :- !.

dead_event(mudcrab):- add_potion(health_potion), add_gold(10).

dead_event(ghost):- add_gold(10).

dead_event(troll):- add_potion(health_potion), add_gold(20).
dead_event(_).

battle(X):- current_node_is(PN), status(X, alive), assert(prev_node(PN)), different(battle_dimension, PN),
 assert(current_node_is(battle_dimension)), retract(current_node_is(PN)), describe_battle(player, X).

describe_battle(X, Y):- health(X, H1), health(Y, H2), nl, 
    format("You have ~0f health remaining.", [H1]), nl,
    format("~w has ~0f health remaining.", [Y, H2]), nl,
    write("your options are attack("), write(Y), write(") and magic_attack("), write(Y), write(")"), nl, !.
/*
events for each location.
*/

event(mudcrab_field):- 
status(mudcrab, alive), write("You are attacked by a mudcrab!"), nl, sleep(2), nl, battle(mudcrab), !. 

event(cave) :-
status(troll, alive), write("You slowly pick your way through the cave, but a troll jumps out to attack you!"), nl, sleep(2), nl, battle(troll), !.

event(wasteland) :-
status(skeleton, alive), write("After exiting the decaying forest, you notice skeletal remains littering the ground. Suddenly the skeletal remains of an ancient wizard attacks you!"), nl, sleep(2), nl, battle(skeleton), !.

event(boss):-
status(necromancer, alive), write("You enter the throne room; the evil necromancer is waiting with his staff at the ready. He is ancient and cunning, you will need all your strength to defeat him."), nl,
sleep(2), nl, write("You lunge forward to engage the evil necromancer in battle!"), nl, battle(necromancer), !.

event(_) :- !.



/*
edge(Start, Direction, Destination)
Represents an edge from the current node, travelling in the given direction and reaching the specified end node. 
*/

edge(home, north, crossroads).

edge(crossroads, north, cave) :- equipped(weapon_slot, short_sword), equipped(armor_slot, chainmail), traverse_darkness(true).
edge(crossroads, north, cave) :- equipped(weapon_slot, halberd), equipped(armor_slot, chainmail), traverse_darkness(true).
edge(crossroads, north, cave) :- traverse_darkness(true), write("It would be suicide to enter the cave wthout armor or weapons!"), nl, !, fail.
edge(crossroads, north, cave) :- write("It would be suicide to enter the cave wthout armor,weapons or a light source!"), nl, !, fail.
edge(crossroads, west, abandoned_house).
edge(crossroads, east, armory).

edge(cave, south, crossroads).
edge(cave, north, mountain_pass).

edge(mountain_pass, north, lake) :- traverse_height(true).       %need rope/kinko wings
edge(mountain_pass, north, lake) :- write("Navigating the steep decline without some appropriate equipment would be too dangerous!"), nl, !, fail.
edge(mountain_pass, south, cave).
edge(mountain_pass, west, climbers_camp).
edge(climbers_camp, east, mountain_pass).
edge(mountain_pass, east, shop).
edge(shop, west, mountain_pass).


edge(lake, north, lake_island) :- traverse_water(true).  %repair boat or be argonian
edge(lake, north, lake_island) :- write("Without some way to repair the boat, traversing the lake is impossible!"), nl, !, fail.
edge(lake, south, mountain_pass).  
edge(lake, west, dark_forest).
edge(lake_island, south, lake).

edge(dark_forest, east, lake).
edge(dark_forest, north, wasteland).

edge(wasteland, north, tower).
edge(wasteland, south, dark_forest).

edge(tower, north, boss).
edge(tower, south, wasteland).

edge(abandoned_house, east, crossroads).
edge(abandoned_house, west, cemetery).
edge(cemetery, east, abandoned_house).


edge(armory, west, crossroads).
edge(armory, south, mudcrab_field).
edge(mudcrab_field, north, armory).

edge(mudcrab_field, east, crag) :- located(halberd, weapon_slot), write("It would mean sure death to go back into that crag."), nl, !, fail.
edge(mudcrab_field, east, crag) :- located(halberd, narnia), write("It would mean sure death to go back into that crag."), nl, !, fail.
edge(mudcrab_field, east, crag).
edge(crag, west, mudcrab_field).


map :- current_node_is(X), nl, format("currently in ~w. ", [X]), nl.
map :- current_node_is(X), edge(X, north, N), nl, format("to the north is ~w.", [N]).
map :- current_node_is(X), edge(X, west, N), nl, format("to the west is ~w.", [N]).
map :- current_node_is(X), edge(X, south, N), nl, format("to the south is ~w.", [N]).
map :- current_node_is(X), edge(X, east, N), nl, format("to the east is ~w.", [N]).  
/*
Initialize the location of the many items in the game.
*/

load_default_locations :-
    assert(located(short_sword, armory)),
    assert(located(chainmail, armory)),

    assert(located(torch, abandoned_house)),
    assert(located(rope, climbers_camp)),
    assert(located(hammer, shop)),

    assert(located(halberd, crag)),

    assert(located(spellbook, wasteland)),
    assert(located(pristine_cloak, wasteland)),
    assert(located(frost_wand, cemetery)),
    assert(located(rugged_cloak, cemetery)),
    assert(located(magic_staff, cave)),
    assert(located(broadsword, shop)),
    assert(located(battle_axe, shop)),
    assert(located(breastplate, shop)),
    assert(located(fire_wand, shop)),
    assert(located(quality_cloak, shop)),
    assert(located(excalibur, lake_island)).

% add health potions to shop or somewhere else


equip_item(Stat, Modifier) :-
    reset_stat(Stat),
    modify_stat(Stat, Modifier).


/*
Initialize the stats of each item
*/

%Attack items
item(short_sword, weapon_slot, equip_item(attack, 3)).
item(halberd, weapon_slot, equip_item(attack, 5)).
item(battle_axe, weapon_slot, equip_item(attack, 7)).
item(broadsword, weapon_slot, equip_item(attack, 10)).
item(excalibur, weapon_slot, equip_item(attack, 15)).

%Defense items
item(chainmail, armor_slot, equip_item(defense, 3)).
item(breastplate, armor_slot, equip_item(defense, 7)).

%Utility items
item(torch, utility_slot, traverse_darkness(true)).
item(rope, utility_slot, traverse_height(true)).
item(hammer, utility_slot, traverse_water(true)).

%Magic attack items
item(frost_wand, magic_slot, equip_item(magic_attack, 3)).
item(magic_staff, magic_slot, equip_item(magic_attack, 5)).
item(fire_wand, magic_slot, equip_item(magic_attack, 7)).
item(spellbook, magic_slot, equip_item(magic_attack, 10)).

%Defense items
item(rugged_cloak, cape_slot, equip_item(magic_defense, 2)).
item(quality_cloak, cape_slot, equip_item(magic_defense, 4)).
item(pristine_cloak, cape_slot, equip_item(magic_defense, 5)).
item(health_potion, potion, 1).

item(nothing, weapon_slot, attack(player, 1)).
item(nothing, armor_slot, defense(player, 1)).
item(nothing, magic_slot, magic_attack(player, 1)).
item(nothing, cape_slot, magic_defense(player, 1)).
item(nothing, utility_slot, _).

search :- current_node_is(X), nl, format("searching ~w for items. ", [X]).
search :- current_node_is(X), located(I, X), nl, format("~w", [I]).
search :- current_node_is(X), nl, write("found everything useful").
/*
Handles movement from node to node.
*/
move(D) :- current_node_is(S), edge(S, D, E), retract(current_node_is(S)), assert(current_node_is(E)), scene(E), event(E), nl, !.
move(_) :- write("Unable to move in that direction!"), nl, !, fail.

% Directions

n :- move(north).
e :- move(east).
s :- move(south).
w :- move(west).
%l :- move(west).
%r :- move(east).
%f :- move(north).

drink_health_potion:- 
    current_node_is(X), different(battle_dimension, X), potion_count(health_potion, C), C > 0, heal(30), Y is C - 1, 
    retract(potion_count(health_potion, C)), assert(potion_count(health_potion, Y)), !.
drink_health_potion:-
    current_node_is(battle_dimension), nl,
    write("Can't drink potions during combat!"), nl, !.

heal(X) :- 
    health(player, Y), Z is X + Y, Z < 51, retract(health(player, Y)), assert(health(player, Z)), nl,
    format("Your health points have increased from ~w to ~w!", [Y, Z]), nl, !.
heal(X) :- 
    health(player, Y), Z is X + Y, Z > 50, retract(health(player, Y)), assert(health(player, 50)), nl,
    format("Your health points have increased from ~w to 50!", [Y]), nl, !.
/*
Initialize the iteractables of the game.
*/

interactable(coffin, cemetery).


interact(X):- interactable(X, L), current_node_is(L), interaction_event(X), retract(interactable(X,L)).

interaction_event(coffin):- status(ghost, alive), write("the coffin cover is cold and heavy. you notice the surrounding area is filled with cobwebs, it must have been closed for a while. with all your strength you bearly manage to open it."),nl,
sleep(2), nl, write("A ghost emerges from the fog and attacks you!"), battle(ghost), !.



uncover :- current_node_is(X), nl, format("searching ~w for intresting things. ", [X]).
uncover :- current_node_is(X), interactable(I, X), nl, format("~w", [I]).
uncover :- current_node_is(X), nl, write("found everything of interest.").
/*
Handles picking up items.
*/
take(chest) :- add_gold(50), assert(located(halberd, narnia)), retract(located(halberd, cave)), !.
take(torch) :- 
    item(torch, Y, S), equipped(Y, I2),
    assert(equipped(Y, torch)), retract(equipped(Y, I2)),
    assert(traverse_darkness(true)), retract(traverse_darkness(false)), 
    assert(located(torch,Y)), retract(located(torch,X)),
    write("Item acquired: torch"), !.
take(rope) :- 
    assert(traverse_height(true)), retract(traverse_height(false)), 
    item(rope, Y, S), equipped(Y, I2),
    assert(equipped(Y, rope)), retract(equipped(Y, I2)),
    assert(located(rope,Y)), retract(located(rope,X)),
    write("Item acquired: rope"), !.
take(hammer) :- 
    located(hammer, shop), current_node_is(shop), buy(hammer),
    item(hammer, Y, S), equipped(Y, I2),
    assert(equipped(Y, hammer)), retract(equipped(Y, I2)),
    assert(located(hammer,Y)), retract(located(hammer,X)),
    write("Item acquired: hammer"), nl, !.
take(I1) :-
    located(I1, shop), current_node_is(shop), item(I1, Y, S), equipped(Y, I2), different(item(I1, Y, S), item(_,potion,_)),
    buy(I1),
    assert(located(I1, Y)), assert(equipped(Y, I1)), retract(located(I1, X)), retract(equipped(Y, I2)), extract_stat(S, Stat, Modifier),
    equip_item(Stat, Modifier), nl, !.
take(I1) :- 
    located(I1, X), current_node_is(X), item(I1, Y, S), equipped(Y, I2), different(X, shop), different(item(I1, Y, S), item(_,potion,_)),
    assert(located(I1, Y)), assert(equipped(Y, I1)), retract(located(I1, X)), retract(equipped(Y, I2)), extract_stat(S, Stat, Modifier),
    equip_item(Stat, Modifier), format("Item Acquired: ~w", [I1]), nl, !.
take(I1):- located(I1, X), current_node_is(X), item(I1, potion, S), add_potion(I1), retract(located(I1, X)), !.
take(_) :- write("Unable to acquire that item."), !.


extract_stat(equip_item(Stat, Modifier), Stat, Modifier).
/*
Item purchasing.
*/
buy(broadsword) :- gold(X), X > 19, Y is X - 20, assert(gold(Y)), retract(gold(X)).
buy(broadsword) :- write("You don't have enough gold!"), nl, fail.

buy(battle_axe) :- gold(X), X > 19, Y is X - 20, assert(gold(Y)), retract(gold(X)).
buy(battle_axe) :- write("You don't have enough gold!"), nl, fail.

buy(breastplate) :- gold(X), X > 29, Y is X - 30, assert(gold(Y)), retract(gold(X)).
buy(breastplate) :- write("You don't have enough gold!"), nl, fail.

buy(fire_wand) :- gold(X), X > 49, Y is X - 50, assert(gold(Y)), retract(gold(X)).
buy(fire_wand) :- write("You don't have enough gold!"), nl, fail.

buy(quality_cloak) :- gold(X), X > 29, Y is X - 30, assert(gold(Y)), retract(gold(X)).
buy(quality_cloak) :- write("You don't have enough gold!"), nl, fail.

buy(hammer) :- 
    gold(X), X > 9, Y is X - 10, assert(gold(Y)), retract(gold(X)).
buy(hammer) :- write("You don't have enough gold!"), nl, fail.



add_potion(I):- item(I, potion, N), potion_count(I, C), NC is C + N, assert(potion_count(I, NC)), retract(potion_count(I, C)), nl
, write(I), format(" count increased to : ~w", [NC]), !.


/*
Describes the players gear.
*/

inspect(player) :-
    nl,
    level(L),
    gold(G),
    experience(E),
    potion_count(health_potion, P),
    equipped(head_slot, X),
    equipped(weapon_slot, Y),
    equipped(armor_slot, Z),
    equipped(magic_slot, M),
    equipped(cape_slot, C),
    equipped(utility_slot, U),
    health(player, H),
    defense(player, D),
    attack(player, A),
    magic_attack(player, MA),
    magic_defense(player, MD),
    get_current_character(Character),
    traverse_darkness(TD),
    traverse_height(TH),
    traverse_water(TW),
    format("Current level: ~w", [L]), nl,
    format("Current Character: ~w", [Character]),nl,
    format("Head slot: ~w", [X]), nl,
    format("Armor slot: ~w", [Z]), nl,
    format("Cape slot: ~w", [C]), nl,
    format("Weapon slot: ~w", [Y]), nl,
    format("Magic slot: ~w", [M]), nl,
    format("Utility slot: ~w", [U]), nl,
    format("You currently have ~2f HP, ~w Attack, ~w Defence, ~w Magic Attack and ~w Magic Defence.", [H, A, D, MA, MD]), nl,
    ( TW == true ->
    format("You can traverse water."), nl
    ; TW == false -> format("")),
    ( TD == true ->
    format("You can traverse darkness."), nl
    ; TD == false -> format("")),
    ( TH == true ->
    format("You can traverse heights."), nl
    ; TH == false -> format("")),
    format("You are carrying ~w gold coins and ~w health potion(s).", [G, P]), nl,
    format("You have ~w experience points.", [E]), nl, !.
inspect(health_potion) :-
    potion_count(I, C),
    C > 0,
    nl,
    write("A bright red health potion, it will heal a large amount of health. To use type 'drink_health_potion.'"),
    nl, !.
inspect(I1) :-
    located(I1, X), current_node_is(X), item(I1, Y, S1), equipped(Y, I2), item(I2, Y, S2), different(item(I1, Y, S), item(_,potion,_)),
    format("The item in your ~w has a value of ~w", [Y, S2]), nl,
    format("The item you see has a value of ~w", [S1]), nl, !.
inspect(_) :- write("You can't inspect that!"), nl, !.

/*
Describes the current location to the player
*/

scene(X) :- current_node_is(X), description(X).

look :- current_node_is(X), description(X), !.

description(character):-
    nl,
    write("First of all you will receive a description of each character you can choose to be. There will be three characters you can choose from."),nl,
    write("You can choose to be either Argonian(argonian), Khajiit(khajiit) or Kinko(kinko). "),nl,
    write("Pay close attention to the character descriptions, their attributes may help you in different situations"),nl,
    write("To choose a character type in their race. For example typing 'argonian.' would create your character as an argonian."),nl,nl,nl,
   description(argonian),nl,
   description(khajiit),nl,
   description(kinko),nl,
    read_character_option.

description(argonian):-
    write("Argonian: "),nl,
    write("A reptilian humanoid race that inhabits the province of Black Marsh. "),nl,
    write("They are well-known for their ability to breathe underwater, which allows them to traverse swamps, marshes and lakes with ease."),nl,
    write("Their scaly skin and sharp claws give them a fearsome appearance, but they are also known for their shrewdness and adaptability."),nl.

description(khajiit):-
    write("Khajiit: "),nl,
    write("A feline humanoid race that hail from the province of Elsweyr."),nl,
    write("They possess excellent night vision, allowing them to see clearly in the dark and making them skilled hunters and thieves. "),nl,
    write("Their fur-covered bodies and feline features make them appear both graceful and dangerous, and they have a reputation for being cunning and quick-witted. "),nl.

description(kinko):-
    write("Kinko: "),nl,
    write("A humanoid race with bat-like wings that allow them to glide for short distances."),nl,
    write("They are known for their agility and speed."),nl,
    write("Their wings also grant them a degree of aerial mobility in combat and exploration. "),nl.

description(home) :-
    nl,
    write("You look around at the destruction the the evil necromancers undead have caused to your home. Its ruined. "), nl,
    write("They swept through this valley like a plague, destroying all in their path"), nl,
    write("You must defeat the necromancer in order to prevent this from continuing to happen"), nl,
    write("To the north you see the mountain, on the other side of which the necromancers tower lies"), nl.

description(crossroads) :-
    nl,
    current_character(khajiit),
    write("You reach the crossroads, from here you can go in any direction."), nl,
    write("To the north lies a cave, beyond which is the evil necromancer's castle. It is very dark, however your cat-like eyes allow you to pierce the gloom."), nl,
    write("You can see evidence of a troll. It wouldn't be wise to venture through there unprepared."), nl.
description(crossroads) :-
    nl,
    write("You reach the crossroads, from here you can go in any direction."), nl,
    write("To the north lies a cave, beyond which is the evil necromancer's castle. It is very dark, and it would be unwise to venture there unprepared."), nl,
    write("To the west you see an abandoned house, perhaps the previous tenant left a torch behind?"), nl,
    write("To the south is your home, but your quest lies ahead of you."), nl,
    write("To the east you see an armory, there might still be supplies inside."), nl.


% East of Crossroads
description(armory) :-
    located(chainmail, armory),
    located(short_sword, armory),
    nl,
    write("You enter a decrepit old armory, out of the corner of you eye you spot a glint of metal."), nl,
    write("You discover [chainmail] and a [short_sword], this will definitely help you in your fight against the evil necromancer."), nl,
    write("Enter 'take(chainmail).' to pick up the armor."), nl,
    write("Enter 'take(short_sword).' to equip the weapon."), nl,
    write("There is a field to the south, and the crossroads are back to the west."), nl.
description(armory) :-
    equipped(armor_slot, chainmail),
    nl,
    write("There is a field to the south of the now empty armory, and the crossroads are to the west."), nl.

/*
description(mudcrab_field) :-
    located(halberd, cave),
    nl,
    write("This field has been infested with mudcrabs since the spread of the necromancer's chaos. You see a cave"), nl,
    write("to the east of the field's edge, or you can turn back north to the relative safety of the armory."), nl.
*/

description(mudcrab_field) :-
    nl,
    status(mudcrab, dead),
    write("The cave is too dangerous to return to, the only way out from here is back north to the armory."), nl.

description(mudcrab_field) :-
    nl,
    write("This field has been infested with mudcrabs since the spread of the necromancer's chaos."), nl.

description(crag) :-
    nl,
    write("You enter the crag, it is dark but you can see enough to spot two valuable items on the ground."), nl,
    write("A treasure chest and a steel [halberd] lie on opposite sides of the cave's mouth."), nl,
    write("But you hear a deep roar echo from the depths of the cave, there is only time to take one item."), nl,
    write("Enter 'take(chest).' or 'take(halberd).' and then run to the west!"), nl. % Fixed


% West of Crossroads
% If the torch is still in this location, display this description
description(abandoned_house) :-
    nl,
    current_character(khajiit),
    write("Whoever lived in this house has not been here for a long time, they must have been driven out by the necromancer's dark creatures."), nl,
    write("Their items are strewn about, but there is nothing here of use to you."),
    write("To the west is a small cemetery covered in fog, and to the east is the crossroads."), nl.
description(abandoned_house) :-
    nl,
    located(torch, abandoned_house),
    write("Whoever lived in this house has not been here for a long time, they must have been driven out by the necromancer's dark creatures."), nl,
    write("You spot an open trunk in the corner, with a [torch] and some oil inside. This could allow you to navigate the cave."), nl,
    write("Enter 'take(torch).' to pick up the torch."), nl,
    write("To the west is a small cemetery covered in fog, and to the east is the crossroads."), nl.
% Display default description
description(abandoned_house) :-
    nl,
    write("Whoever lived in this house has not been here for a long time, they must have been driven out by the necromancer's dark creatures."), nl,
    write("To the west is a small cemetery covered in fog, and to the east is the crossroads."), nl.

description(cemetery) :-
    located(frost_wand, cemetery),
    nl,
    write("An eerie fog has taken up residence around the deteriorating gravestones. Be on your guard."), nl,
    write("Inspecting the gravestones, you see a glimmering blue [frost_wand] leaning against one."), nl,
    write("Enter 'take(frost_wand)' to pick up the magic weapon."), nl.
description(cemetery) :-
    %equipped(magic_slot, frost_wand),
    nl,
    write("An eerie fog has taken up residence around the deteriorating gravestones. Be on your guard."), nl,
    write("The only way to go from here is back the way you come, to the east."), nl.

description(cemetery2) :-
    write("You leave the cemetery at once and go back to the old house."), nl,
    move(east).


% North of Crossroads
description(cave) :-
    status(troll, alive).
description(cave) :-
    located(magic_staff, cave),
    nl,
    write("With the troll dead, you continue to navigate the gloom with your torch held aloft. In the middle of the dark cave, you see a huge, gnarled root with a [magic_staff] carved out of it."), nl,
    write("Enter 'take(magic_staff)' to equip the item."), nl,
    write("To the north you can just see a small light indicating the caves exit."), nl.
description(cave) :-
    write("You continue to navigate the dark cave."), nl,
    write("To the north you can just see a small light indicating the caves exit."), nl.


description(shop) :-
    located(fire_wand, shop),
    located(battle_axe, shop),
    located(hammer, shop),
    located(broadsword, shop),
    located(breastplate, shop),
    located(quality_cloak, shop),
    write("You find a yordle selling her wares in the shelter of a rocky outcrop."), nl,
    write("For sale is a [broadsword] for 20 gold, a [battle_axe] for 20 gold, a [breastplate] for 30 gold, a [fire_wand] for 50 gold, a [hammer] for 10 gold, and a [quality_cloak] for 30 gold"), nl,
    write("Enter 'take(___)' for any items you'd like to buy."), nl, 
    write("The only exit is back west to the cliff edge."), nl.
description(shop) :-
    write("The yordle is nowhere to be seen, so the only thing to do is go back west."), nl.

description(mountain_pass) :-
    current_character(kinko),
    write("You feel a biting chill from the howling wind as you exit the cave onto a frigid mountain pass."), nl,
    write("To the north is a sheer descent to a distant lake. Your kinko wings should be enough to carry you there."), nl,
    write("To the west looks like a climbers camp, and to the east you can make out a shopkeep."), nl.
description(mountain_pass) :-
    nl,
    write("You feel a biting chill from the howling wind as you exit the cave onto a frigid mountain pass."), nl,
    write("To the north is a sheer descent to a distant lake. You'll need some equipment to make it down safely"), nl,
    write("To the west looks like a climbers camp. Maybe there is some leftover equipment?"), nl,
    write("To the east you can make out a shopkeep."), nl.

description(climbers_camp) :-
    nl,
    current_character(kinko),
    write("You stand upon a plateau with steep drops on all sides."), nl,
    write("There are boxes and old tents scattered around that have seen better days, but nothing here is of use to you."), nl.
description(climbers_camp) :-
    nl,
    located(rope, climbers_camp),
    write("You stand upon a plateau with steep drops on all sides."), nl,
    write("There are boxes and old tents scattered around that have seen better days. Sitting on one of the boxes is an old [rope]."), nl,
    write("Enter 'take(rope)' to equip the item."), nl,
    write("To the east lies the path forward."), nl.

description(climbers_camp)   :-
    nl,
    traverse_height(true),
    write("You stand upon a plateau with steep drops on all sides."), nl,
    write("There are boxes and old tents scattered around that have seen better days."), nl,
    write("You've taken everything you can from here."), nl,
    write("To the east lies the path forward."), nl.

description(lake) :-
    current_character(argonian),
    write("You find yourself at the edge of a still lake. Silence permeates the area, but is broken periodically by the cry of a loon."), nl,
    write("In the centre of the lake lies a small island. The water looks dark and sick, due to proximity to the necromancer's tower"), nl,
    write("Your Argonian physiology would allow you to reach the island. Enter 'n.' to go to the island."), nl,
    write("Surrounding you is dense forest, but you can see the tower looming. You can make out a path to the west."), nl.
description(lake) :-
    equipped(utility_slot, hammer),
    write("You find yourself at the edge of a still lake. Silence permeates the area, but is broken periodically by the cry of a loon."), nl,
    write("In the centre of the lake lies a small island. The water looks dark and sick, due to proximity to the necromancer's tower"), nl,
    write("Beside you lies a boat that has fallen into disrepair. Enter 'repair_boat.' to repair the boat with your hammer."), nl,
    write("Surrounding you is dense forest, but you can see the tower looming. You can make out a path to the west."), nl.
description(lake) :-
    nl,
    write("You find yourself at the edge of a still lake. Silence permeates the area, but is broken periodically by the cry of a loon."), nl,
    write("In the centre of the lake lies a small island. The water looks dark and sick, due to proximity to the necromancer's tower"), nl,
    write("Beside you lies a boat that has fallen into disrepair. Maybe you could repair it with something?"), nl,
    write("Surrounding you is dense forest, but you can see the tower looming. You can make out a path to the west."), nl.

description(lake_island) :-
    nl,
    located(excalibur, lake_island),
    write("You come ashore on a small island. Quaint and idyllic, it looks almost like it's escaped the touch of the necromancer"), nl,
    write("In the centre of the lake lies a small pedestal. A single ray of light from the dark sky above illuminates the hilt of a sword."), nl,
    write("A calmness settles upon you."), nl,
    write("Enter 'take(excalibur)' to take the sword. Enter 's.' to return to the lake side."), nl.
description(lake_island) :-
    nl,
    located(excalibur, lake_island),
    write("On this small island, the pedestal is empty. You have already taken excalibur."), nl,
    write("You are ready."), nl.

description(dark_forest) :-
    %equipped(magic_slot, spellbook),
    nl,
    write("The forest gets progressively thinner and more dead as you close in on the tower"), nl,
    write("Ahead you can see that the forest gives way to a desolate sight. All of the flora has withered away to leave a dry wasteland."), nl,
    write("The tower rises menacingly towards the sky in the distance to the north."), nl.

description(wasteland) :-
    %located(spellbook, tower),
    status(skeleton, alive).
description(wasteland) :-
    %located(spellbook, tower),
    nl,
    write("The corpses of the necromancers servants litter the ground."), nl,
    write("The tower streaks toward the sky before you, so tall you can barely see its summit."), nl,
    write("You feel a finality permeate the air. This is it. Steel yourself, and rush forward to defeat your enemy!"), nl,
    write("Enter 'n.' to enter the wizard's tower."), nl.
description(tower) :-
    %equipped(magic_slot, spellbook),
    nl,
    write("The resistance you were expecting doesn't come. The tower is eerily quiet. Your steps echo on black tiles."), nl,
    write("You see an ornate staircase that leads up the tower. You pass through empty room after empty room as you slowly scale the tower."), nl,
    write("You've broken a sweat by the time you reach the top floor. Before you is an enormous door. You can hear energy crackling on the other side."), nl,
    write("This is it. There's no turning back now."), nl,
    write("Enter 'n.' to face the necromancer.").


description(boss) :-
    status(necromancer, dead),
    nl,
    write("The necromancer drops to the ground, blood soaking his fetid robes."), nl,
    write("You take the idol he was using to enact his arcane machinations and crush it in your hand"), nl,
    write("The dark clouds above you begin to recede. You gaze out onto the horizon of a better world."), nl,
    write("You win!"), nl,
    write("Thanks for playing."),
    halt(1).
description(boss).

/*
Read character 
*/
read_character_option :-
    write("Please enter your character choice: "), nl, 
    read(Option),
    (
        Option == argonian ->
            argonian,
            nl
        ;
        Option == khajiit ->
            khajiit,
            nl
        ;
        Option == kinko ->
            kinko,
            nl
        ;
        write("Invalid character option. Please enter argonian, khajiit, or kinko."),
        nl,
        read_character_option
    ).
/*
Starts the game.
*/

play :-
    (   game_begin
    ->  game_in_play
    ;   assert(current_node_is(home)),
        options,
        assert(game_begin)
    ).


game_in_play :- 
    nl,
    write("Game has already started."), nl.
    
new :- 
    load_default_equipment, load_default_status, load_default_locations,
    tutorial, description(character).

load :- load_game, retract(current_node_is(home)), !.

options :-
    nl,
    write("Welcome to - PrologScrolls!"), nl,
    write("To begin you can either start a new game by typing 'new.'"), nl,
    write("Or you can load a previous save by typing 'load.'"), nl, !.

/*
Basic instructions for players
*/

tutorial :-
    nl,
    write("You will recieve a short description of each location when you arrive there, pay attention to what's presented."), nl,
    write("You can move in four directions. North (n), East (e), South (s), and West (w). To move simply type the shorthand for a direction followed by a period."), nl,
    write("For example typing 'n.' would move you North."), nl,
    write("Once you reach a new location a short description of what you observe will be given."), nl,
    write("If you miss this description simply type 'look.' to have it be shown again."), nl,
    write("to find all the paths you can take for an area and find your current location type 'map.'."), nl,
    write("to find all interesting things to interact with type uncover."), nl,
    write("to interact with things of interest type interact(X)."), nl,
    write("to find all items in your current location type 'search.'."), nl,
    write("To pick up or equip an item type 'take(itemName)' where itemName can be any object the game describes such as a sword or armor."), nl,
    write("To inspect your current equipment type 'inspect(player).', to compare items type 'inspect(itemName).'"), nl,
    write("That's all for now. Enjoy the game!"), nl, !.

/*
Handles saving game state
*/

save :- current_node_is(battle_dimension), nl, write("Can't save during battles!"), nl, !.
save :- save_unary_misc, save_binary_misc, save_equipped, save_status, save_located.

save_unary_misc :- current_node_is(X), gold(Y), csv_write_file('save/save_unary_misc.txt', [current_node_is(X), gold(Y)]).

save_binary_misc :- health(player, X), defense(player, Y), attack(player, Z), magic_attack(player, A), magic_defense(player, B), potion_count(health_potion, C),
    csv_write_file('save/save_binary_misc.txt', 
        [health(player, X), defense(player, Y), attack(player, Z), magic_attack(player, A), magic_defense(player, B), potion_count(health_potion, C)]).

save_equipped :- findall(equipped(X, Y), equipped(X, Y), L), csv_write_file('save/save_equipped.txt', L).

save_status :- findall(status(X, Y), status(X, Y), L), csv_write_file('save/save_status.txt', L).

save_located :- findall(located(X,Y), located(X, Y), L), csv_write_file('save/save_located.txt', L).

/*
Handles loading game state
*/

load_game :- 
    current_node_is(home), 
    catch(see('save/save_unary_misc.txt'), E, (write('No save found'), fail_load)), 
    load_unary_misc, load_binary_misc, load_equipped, load_status, load_located.
load_game :- nl, write("Failed to load. Either no save files exist, or you already started your adventure."), nl, !.

fail_load :- nl, play, fail.

load_unary_misc :- csv_read_file('save/save_unary_misc.txt', X), load_unary_misc_state(X).
load_unary_misc_state([row(X), row(Y)]) :- assert(current_node_is(X)), assert(gold(Y)).

load_binary_misc :- csv_read_file('save/save_binary_misc.txt', X), load_binary_misc_state(X).
load_binary_misc_state([row(X1, Y1), row(X2, Y2), row(X3, Y3), row(X4, Y4), row(X5, Y5), row(X6, Y6)]) :- 
    assert(health(X1, Y1)), assert(defense(X2, Y2)), assert(attack(X3, Y3)), assert(magic_attack(X4, Y4)), assert(magic_defense(X5, Y5)), assert(potion_count(X6, Y6)).

load_equipped :- csv_read_file('save/save_equipped.txt', X), load_equipped_state(X).
load_equipped_state([row(X, Y)|T]) :- assert(equipped(X, Y)), load_equipped_state(T).
load_equipped_state([]).

load_status :- csv_read_file('save/save_status.txt', X), load_status_state(X).
load_status_state([row(X, Y)|T]) :- assert(status(X, Y)), load_status_state(T).
load_status_state([]).

load_located :- csv_read_file('save/save_located.txt', X), load_located_state(X).
load_located_state([row(X, Y)|T]) :- assert(located(X, Y)), load_located_state(T).
load_located_state([]).