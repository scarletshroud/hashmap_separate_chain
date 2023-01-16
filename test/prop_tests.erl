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

prop_add_element() ->
  ?FORALL(
    {List, {Key, Value}},
    {list({integer(), integer()}), {integer(), integer()}},
    ?IMPLIES(
      begin
        Hashmap = hashmap_init_from_list(List),
        {Status, _} = hashmap_get_value(Hashmap, Key),
        Status == badkey
      end,
      begin
        Hashmap = hashmap_init_from_list(List),
        NewHashmap = hashmap_add(Hashmap, Key, Value),
        case hashmap_get_value(NewHashmap, Key) of
          {ok, Value} -> Hashmap#hashmap.elements_size + 1 == NewHashmap#hashmap.elements_size;
          {badkey, Value} -> false
        end
      end
    )
  ).


prop_remove_element() ->
  ?FORALL(
    {List, {Key, _}},
    {list({integer(), integer()}), {integer(), integer()}},
    begin
      Hashmap = hashmap_init_from_list(List),
      {Status, _} = hashmap_get_value(Hashmap, Key),
      NewHashmap = hashmap_remove(Hashmap, Key),
      case Status of
        ok -> Hashmap#hashmap.elements_size - 1 == NewHashmap#hashmap.elements_size;
        badkey -> Hashmap#hashmap.elements_size == NewHashmap#hashmap.elements_size
      end
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
