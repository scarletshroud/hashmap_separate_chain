-module(unit_tests).

-import(
  hashmap,
  [
    hashmap_init/0,
    hashmap_add/3,
    hashmap_remove/2,
    hashmap_get_value/2,
    hashmap_filter/2,
    hashmap_map/2,
    hashmap_fold/3,
    hashmap_is_equal/2,
    hashmap_merge/2
  ]
).

-include_lib("eunit/include/eunit.hrl").
-include_lib("src/hashmap.hrl").

add_test() ->
  Genders = hashmap_init(),
  OneGender = hashmap_add(Genders, "Male", 1),
  TwoGenders = hashmap_add(OneGender, "Female", 2),
  GenderValue = hashmap_get_value(TwoGenders, "Male"),
  ?assertEqual(1, GenderValue),
  ?assertEqual(2, TwoGenders#hashmap.buckets_size).


remove_test() ->
  Users = hashmap_init(),
  OneUser = hashmap_add(Users, "John", 1),
  TwoUsers = hashmap_add(OneUser, "Sam", 2),
  ThreeUsers = hashmap_add(TwoUsers, "Kate", 3),
  AfterRemove = hashmap_remove(ThreeUsers, "Sam"),
  SamValue = hashmap_get_value(AfterRemove, "Sam"),
  ?assertEqual(1, AfterRemove#hashmap.buckets_size),
  ?assertEqual(false, SamValue).


get_test() ->
  Users = hashmap_init(),
  OneUser = hashmap_add(Users, "John", 1),
  TwoUsers = hashmap_add(OneUser, "Sam", 2),
  SamValue = hashmap_get_value(TwoUsers, "Sam"),
  UnknownValue = hashmap_get_value(TwoUsers, "Unknown"),
  ?assertEqual(2, SamValue),
  ?assertEqual(false, UnknownValue).


filter_test() ->
  Users = hashmap_init(),
  OneUser = hashmap_add(Users, "John", 1),
  TwoUsers = hashmap_add(OneUser, "Sam", 2),
  ThreeUsers = hashmap_add(TwoUsers, "Kate", 3),
  FilteredUsers = hashmap_filter(ThreeUsers, fun (Element) -> Element#node.value > 2 end),
  ?assertEqual(FilteredUsers#hashmap.buckets_size, 1).


map_test() ->
  Users = hashmap_init(),
  OneUser = hashmap_add(Users, "John", 1),
  TwoUsers = hashmap_add(OneUser, "Sam", 2),
  ThreeUsers = hashmap_add(TwoUsers, "Kate", 3),
  NewUsers =
    hashmap_map(
      ThreeUsers,
      fun
        (Element) ->
          #node{
            key = Element#node.key,
            value = Element#node.value * 3,
            hashcode = Element#node.hashcode
          }
      end
    ),
  JohnValue = hashmap_get_value(NewUsers, "John"),
  SamValue = hashmap_get_value(NewUsers, "Sam"),
  KateValue = hashmap_get_value(NewUsers, "Kate"),
  ?assertEqual(3, JohnValue),
  ?assertEqual(6, SamValue),
  ?assertEqual(9, KateValue).


fold_test() ->
  Users = hashmap_init(),
  OneUser = hashmap_add(Users, "John", 1),
  TwoUsers = hashmap_add(OneUser, "Sam", 2),
  ThreeUsers = hashmap_add(TwoUsers, "Kate", 3),
  Result = hashmap_fold(ThreeUsers, fun (Element, Acc) -> Acc + Element#node.value end, 0),
  ?assertEqual(6, Result).


fill_hashmap(Hashmap, Index, Limit) when Index > Limit -> Hashmap;

fill_hashmap(Hashmap, Index, Limit) when Index =< Limit ->
  fill_hashmap(hashmap_add(Hashmap, Index, Index), Index + 1, Limit).

grow_test() ->
  Hashmap = hashmap_init(),
  FilledHashmap = fill_hashmap(Hashmap, 1, 15),
  ?assertEqual(20, FilledHashmap#hashmap.buckets_amount),
  ?assertEqual(15, FilledHashmap#hashmap.elements_size).


merge_test() ->
  Users = hashmap_init(),
  Persons = hashmap_init(),
  OneUser = hashmap_add(Users, "John", 1),
  TwoUsers = hashmap_add(OneUser, "Sam", 2),
  ThreeUsers = hashmap_add(TwoUsers, "Kate", 3),
  OnePerson = hashmap_add(Persons, "Klim", 30),
  TwoPersons = hashmap_add(OnePerson, "Alexander", 21),
  ThreePersons = hashmap_add(TwoPersons, "Andrew", 5),
  MergedHashmap = hashmap_merge(ThreeUsers, ThreePersons),
  ?assertEqual(6, MergedHashmap#hashmap.elements_size).


merge_associativity_test() ->
  Users = hashmap_init(),
  Persons = hashmap_init(),
  OneUser = hashmap_add(Users, "John", 1),
  TwoUsers = hashmap_add(OneUser, "Sam", 2),
  ThreeUsers = hashmap_add(TwoUsers, "Kate", 3),
  OnePerson = hashmap_add(Persons, "Klim", 30),
  TwoPersons = hashmap_add(OnePerson, "Alexander", 21),
  ThreePersons = hashmap_add(TwoPersons, "Andrew", 5),
  MergedLeftToRightHashmap = hashmap_merge(ThreeUsers, ThreePersons),
  MergedRightToLeftHashmap = hashmap_merge(ThreePersons, ThreeUsers),
  ?assert(hashmap_is_equal(MergedLeftToRightHashmap, MergedRightToLeftHashmap)).


merge_neutral_test() ->
  Users = hashmap_init(),
  Empty = hashmap_init(),
  OneUser = hashmap_add(Users, "John", 1),
  TwoUsers = hashmap_add(OneUser, "Sam", 2),
  ThreeUsers = hashmap_add(TwoUsers, "Kate", 3),
  MergedLeftHashmap = hashmap_merge(Empty, ThreeUsers),
  MergedRightHashmap = hashmap_merge(ThreeUsers, Empty),
  ?assert(hashmap_is_equal(MergedLeftHashmap, MergedRightHashmap)).


is_equal_test() ->
  Users = hashmap_init(),
  Persons = hashmap_init(),
  People = hashmap_init(),
  Humans = hashmap_init(),
  OneUser = hashmap_add(Users, "John", 1),
  TwoUsers = hashmap_add(OneUser, "Sam", 2),
  ThreeUsers = hashmap_add(TwoUsers, "Kate", 3),
  OnePerson = hashmap_add(Persons, "John", 1),
  TwoPersons = hashmap_add(OnePerson, "Sam", 2),
  ThreePersons = hashmap_add(TwoPersons, "Kate", 3),
  OneMan = hashmap_add(People, "John", 1),
  TwoMen = hashmap_add(OneMan, "Sam", 2),
  ThreeMen = hashmap_add(TwoMen, "Kate", 1),
  OneHuman = hashmap_add(Humans, "John", 1),
  TwoHuman = hashmap_add(OneHuman, "Sam", 2),
  ThreeHuman = hashmap_add(TwoHuman, "Egor", 2),
  ?assert(hashmap_is_equal(ThreeUsers, ThreePersons)),
  ?assertEqual(false, hashmap_is_equal(ThreeUsers, ThreeMen)),
  ?assertEqual(false, hashmap_is_equal(ThreeUsers, ThreeHuman)).
