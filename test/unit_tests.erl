-module(unit_tests).

-import(
  hashmap,
  [
    hashmap_init/0,
    hashmap_add/3,
    hashmap_remove/2,
    hashmap_get/2,
    hashmap_filter/2,
    hashmap_map/2,
    hashmap_foldl/3
  ]
).

-include_lib("eunit/include/eunit.hrl").
-include_lib("src/hashmap.hrl").

add_test() ->
  Genders = hashmap_init(),
  OneGender = hashmap_add(Genders, "Male", 1),
  TwoGenders = hashmap_add(OneGender, "Female", 2),
  Gender = hashmap_get(TwoGenders, "Male"),
  ?assertEqual("Male", Gender#node.key),
  ?assertEqual(2, TwoGenders#hashmap.size).


remove_test() ->
  Users = hashmap_init(),
  OneUser = hashmap_add(Users, "John", 1),
  TwoUsers = hashmap_add(OneUser, "Sam", 2),
  ThreeUsers = hashmap_add(TwoUsers, "Kate", 3),
  AfterRemove = hashmap_remove(ThreeUsers, "Sam"),
  Sam = hashmap_get(AfterRemove, "Sam"),
  ?assertEqual(1, AfterRemove#hashmap.size),
  ?assertEqual(false, Sam).


get_test() ->
  Users = hashmap_init(),
  OneUser = hashmap_add(Users, "John", 1),
  TwoUsers = hashmap_add(OneUser, "Sam", 2),
  Sam = hashmap_get(TwoUsers, "Sam"),
  Unknown = hashmap_get(TwoUsers, "Unknown"),
  ?assertEqual(Sam#node.key, "Sam"),
  ?assertEqual(Unknown, false).


filter_test() ->
  Users = hashmap_init(),
  OneUser = hashmap_add(Users, "John", 1),
  TwoUsers = hashmap_add(OneUser, "Sam", 2),
  ThreeUsers = hashmap_add(TwoUsers, "Kate", 3),
  FilteredUsers = hashmap_filter(ThreeUsers, fun (Element) -> Element#node.value > 2 end),
  ?assertEqual(FilteredUsers#hashmap.size, 1).


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
  John = hashmap_get(NewUsers, "John"),
  Sam = hashmap_get(NewUsers, "Sam"),
  Kate = hashmap_get(NewUsers, "Kate"),
  ?assertEqual(3, John#node.value),
  ?assertEqual(6, Sam#node.value),
  ?assertEqual(9, Kate#node.value).


fold_test() ->
  Users = hashmap_init(),
  OneUser = hashmap_add(Users, "John", 1),
  TwoUsers = hashmap_add(OneUser, "Sam", 2),
  ThreeUsers = hashmap_add(TwoUsers, "Kate", 3),
  Result = hashmap_foldl(ThreeUsers, fun (Element, Acc) -> Acc + Element#node.value end, 0),
  ?assertEqual(6, Result).


grow_test() -> ?assertEqual(1, 1).

merge_test() -> ?assertEqual(1, 1).
