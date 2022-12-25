-module(prop_tests).

-include_lib("stdlib/include/assert.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("src/hashmap.hrl").

-import(
  hashmap,
  [
    hashmap_init/0,
    hashmap_init_from_list/1,
    hashmap_add/3,
    hashmap_remove/2,
    hashmap_get_value/2,
    hashmap_is_equal/2,
    hashmap_merge/2
  ]
).

-export(
  [
    prop_add_element/0,
    prop_remove_element/0,
    prop_merge_neutral_element/0,
    prop_merge_associativity_element/0
  ]
).

check_hashmap_addition(OldHashmap, NewHashmap, Key) ->
  (OldHashmap#hashmap.elements_size + 1 == NewHashmap#hashmap.elements_size)
  and
  (hashmap_get_value(NewHashmap, Key) /= false).

prop_add_element() ->
  ?FORALL(
    {List, {Key, Value}},
    {list({integer(), integer()}), {integer(), integer()}},
    ?IMPLIES(
      begin Hashmap = hashmap_init_from_list(List), hashmap_get_value(Hashmap, Key) == false end,
      begin
        Hashmap = hashmap_init_from_list(List),
        NewHashmap = hashmap_add(Hashmap, Key, Value),
        check_hashmap_addition(Hashmap, NewHashmap, Key)
      end
    )
  ).

check_hashmap_removal(OldHashmap, NewHashmap, Key, OldElement) ->
  case OldElement of
    false ->
      (OldHashmap#hashmap.elements_size == NewHashmap#hashmap.elements_size)
      and
      (hashmap_get_value(NewHashmap, Key) == false);

    _ ->
      (OldHashmap#hashmap.elements_size - 1 == NewHashmap#hashmap.elements_size)
      and
      (hashmap_get_value(NewHashmap, Key) == false)
  end.


prop_remove_element() ->
  ?FORALL(
    {List, {Key, _}},
    {list({integer(), integer()}), {integer(), integer()}},
    begin
      Hashmap = hashmap_init_from_list(List),
      OldElementValue = hashmap_get_value(Hashmap, Key),
      NewHashmap = hashmap_remove(Hashmap, Key),
      check_hashmap_removal(Hashmap, NewHashmap, Key, OldElementValue)
    end
  ).

prop_merge_neutral_element() ->
  ?FORALL(
    {List},
    {list({integer(), integer()})},
    begin
      Empty = hashmap_init(),
      Hashmap = hashmap_init_from_list(List),
      MergedLeftHashmap = hashmap_merge(Hashmap, Empty),
      MergedRightHashmap = hashmap_merge(Empty, Hashmap),
      hashmap_is_equal(MergedLeftHashmap, MergedRightHashmap)
    end
  ).

prop_merge_associativity_element() ->
  ?FORALL(
    {FirstList, SecondList, ThirdList},
    {list({integer(), integer()}), list({integer(), integer()}), list({integer(), integer()})},
    begin
      FirstHashmap = hashmap_init_from_list(FirstList),
      SecondHashmap = hashmap_init_from_list(SecondList),
      ThirdHashmap = hashmap_init_from_list(ThirdList),
      MergedLeftHashmap = hashmap_merge(hashmap_merge(FirstHashmap, SecondHashmap), ThirdHashmap),
      MergedRightHashmap = hashmap_merge(FirstHashmap, hashmap_merge(SecondHashmap, ThirdHashmap)),
      hashmap_is_equal(MergedLeftHashmap, MergedRightHashmap)
    end
  ).
