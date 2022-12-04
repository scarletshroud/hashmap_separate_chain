-module(hashmap).

-include("hashmap.hrl").

-export(
  [
    hashmap_init/0,
    hashmap_init/1,
    hashmap_add/3,
    hashmap_get/2,
    hashmap_remove/2,
    hashmap_filter/2,
    hashmap_map/2,
    hashmap_foldl/3,
    hashmap_foldr/3
  ]
).

-import(erlang, [phash2/1]).

-opaque map_t() :: #hashmap{buckets :: array:new(10), buckets_amount :: 10, size :: 0}.

-export_type([map_t/0]).

hashmap_init() -> #hashmap{buckets = array:new(10), buckets_amount = 10, size = 0}.

hashmap_init(Size) -> #hashmap{buckets = array:new(Size), buckets_amount = Size, size = 0}.

get_bucket_index(Map, Key) ->
  HashCode = phash2(Key),
  Index = HashCode rem Map#hashmap.buckets_amount,
  if
    (Index < 0) -> -1 * Index;
    (Index >= 0) -> Index
  end.


search_chain_key(Chain, Key, HashCode) -> search_chain_key(Chain, Key, HashCode, 1).

search_chain_key([], _, _, _) -> false;

search_chain_key(Chain, Key, HashCode, I) ->
  [Node | Tail] = Chain,
  case ((Node#node.key == Key) and (Node#node.hashcode == HashCode)) of
    true -> {Node, I};
    false -> search_chain_key(Tail, Key, HashCode, I + 1)
  end.


hashmap_remove(Map, Key) ->
  Index = get_bucket_index(Map, Key),
  HashCode = phash2(Key),
  Bucket = array:get(Index, Map#hashmap.buckets),
  case Bucket of
    undefined -> Map;

    _ ->
      Node = search_chain_key(Bucket, Key, HashCode),
      case Node of
        false -> Map;

        _ ->
          NewBucket =
            lists:sublist(Bucket, element(2, Node) - 1) ++ lists:nthtail(element(2, Node), Bucket),
          case length(NewBucket) of
            0 -> NewBuckets = array:set(Index, undefined, Map#hashmap.buckets);
            _ -> NewBuckets = array:set(Index, NewBucket, Map#hashmap.buckets)
          end,
          #hashmap{
            buckets = NewBuckets,
            buckets_amount = Map#hashmap.buckets_amount,
            size = Map#hashmap.size - 1
          }
      end
  end.


hashmap_get(Map, Key) ->
  Index = get_bucket_index(Map, Key),
  HashCode = phash2(Key),
  Head = array:get(Index, Map#hashmap.buckets),
  case Head of
    undefined -> false;
    Chain -> element(1, search_chain_key(Chain, Key, HashCode))
  end.


rearrange_hashmap_bucket(Bucket, NewHashmap, Index) ->
  case Index =< length(Bucket) of
    true ->
      Element = lists:nth(Index, Bucket),
      TmpHashmap = hashmap_add(NewHashmap, Element#node.key, Element#node.value),
      rearrange_hashmap_bucket(Bucket, TmpHashmap, Index);

    false -> NewHashmap
  end.


rearrange_hashmap(OldHashmap, NewHashmap, Index) ->
  OldBucket = array:get(Index, OldHashmap#hashmap.buckets),
  case OldBucket of
    undefined -> rearrange_hashmap(OldHashmap, NewHashmap, Index + 1);
    Bucket -> rearrange_hashmap_bucket(Bucket, NewHashmap, Index)
  end.


rearrange_hashmap(OldHashmap, NewHashmap) -> rearrange_hashmap(OldHashmap, NewHashmap, 1).

grow_hashmap(Hashmap) ->
  NewHashmap = hashmap_init(Hashmap#hashmap.buckets_amount * ?GROW_COEFFICIENT),
  rearrange_hashmap(Hashmap, NewHashmap).


hashmap_add(Map, Key, Value) ->
  Index = get_bucket_index(Map, Key),
  HashCode = phash2(Key),
  Bucket = array:get(Index, Map#hashmap.buckets),
  case Bucket of
    undefined ->
      NewNode = #node{key = Key, value = Value, hashcode = HashCode},
      NewBuckets = array:set(Index, [NewNode], Map#hashmap.buckets),
      #hashmap{
        buckets = NewBuckets,
        buckets_amount = Map#hashmap.buckets_amount,
        size = Map#hashmap.size + 1
      };

    Chain ->
      Node = search_chain_key(Chain, Key, HashCode),
      case Node of
        false ->
          NewNode = #node{key = Key, value = Value, hashcode = HashCode},
          #hashmap{
            buckets = array:set(Index, [NewNode | Chain], Map#hashmap.buckets),
            buckets_amount = Map#hashmap.buckets_amount,
            size = Map#hashmap.size
          };

        _ ->
          NewNode = #node{key = Key, value = Value, hashcode = HashCode},
          NewBucket =
            lists:sublist(Bucket, element(2, Node) - 1)
            ++
            [NewNode]
            ++
            lists:nthtail(element(2, Node), Bucket),
          NewBuckets = array:set(Index, NewBucket, Map#hashmap.buckets),
          case Map#hashmap.size + 1 / Map#hashmap.buckets_amount > ?LOAD_LIMIT of
            true -> grow_hashmap(Map);

            false ->
              #hashmap{
                buckets = NewBuckets,
                buckets_amount = Map#hashmap.buckets_amount,
                size = Map#hashmap.size
              }
          end
      end
  end.


hashmap_filter(Map, Pred) ->
  NewBuckets =
    array:map(
      fun
        (_, Bucket) ->
          case Bucket of
            undefined -> Bucket;

            _ ->
              FilteredList = lists:filter(Pred, Bucket),
              case length(FilteredList) of
                0 -> undefined;
                _ -> FilteredList
              end
          end
      end,
      Map#hashmap.buckets
    ),
  Size =
    array:foldl(
      fun
        (_, Bucket, Acc) ->
          case Bucket of
            undefined -> Acc;
            _ -> Acc + 1
          end
      end,
      0,
      NewBuckets
    ),
  #hashmap{buckets = NewBuckets, buckets_amount = Map#hashmap.buckets_amount, size = Size}.


hashmap_map(Map, Fun) ->
  NewBuckets =
    array:map(
      fun
        (_, Bucket) ->
          case Bucket of
            undefined -> Bucket;
            _ -> lists:map(Fun, Bucket)
          end
      end,
      Map#hashmap.buckets
    ),
  #hashmap{
    buckets = NewBuckets,
    buckets_amount = Map#hashmap.buckets_amount,
    size = Map#hashmap.size
  }.


hashmap_foldl(Map, Fun, InitAcc) ->
  array:foldl(
    fun
      (_, Bucket, Acc) ->
        case Bucket of
          undefined -> Acc;
          _ -> lists:foldl(Fun, Acc, Bucket)
        end
    end,
    InitAcc,
    Map#hashmap.buckets
  ).


hashmap_foldr(Map, Fun, InitAcc) ->
  array:foldr(
    fun
      (_, Bucket, Acc) ->
        case Bucket of
          undefined -> Acc;
          _ -> lists:foldr(Fun, Acc, Bucket)
        end
    end,
    InitAcc,
    Map#hashmap.buckets
  ).


% copy_map(FirstMap, SecondMap, I) ->
%   FirstMapBucket = array:get(I, FirstMap#hashmap.buckets),
%   SecondMapBucket = array:get(I, SecondMap#hashmap.buckets),
%   Buckets = {FirstMapBucket, SecondMapBucket},
%   case Buckets of
%     {undefined, undefined} -> array:set(I, undefined, map);
%     {undefined, _} -> ;
%     {_, undefined} -> ;
%   end
% merge(FirstMap, SecondMap) ->
%   case FirstMap#hashmap.size >= SecondMap#map.size of
%     true -> copyMap(FirstMap, SecondMap),
%     false -> copyMap(SecondMap, FirstMap)
%   end.
compare_bucket(_, _, Index, BucketSize) when Index > BucketSize -> true;

compare_bucket(FirstBucket, SecondBucket, Index, BucketSize) when Index =< BucketSize ->
  FirstBucketNode = lists:nth(Index, FirstBucket),
  SecondBucketNode = lists:nth(Index, SecondBucket),
  case
  (FirstBucketNode#node.key == SecondBucketNode#node.key)
  and
  (FirstBucketNode#node.value == SecondBucketNode#node.value) of
    true -> compare_bucket(FirstBucket, SecondBucket, Index + 1, BucketSize);
    false -> false
  end.


compare_buckets(_, _, Index, BucketsSize) when Index > BucketsSize -> true;

compare_buckets(FirstHashmap, SecondHashmap, Index, BucketsSize) when Index =< BucketsSize ->
  FirstBucket = array:get(Index, FirstHashmap#hashmap.buckets),
  SecondBucket = array:get(Index, SecondHashmap#hashmap.buckets),
  BucketSize = length(FirstBucket),
  case compare_bucket(FirstBucket, SecondBucket, 1, BucketSize) of
    true ->
      %check undefined
      compare_buckets(FirstHashmap, SecondHashmap, Index + 1, BucketSize);

    false -> false
  end.


is_equal(FirstHashmap, SecondHashmap) ->
  case
  (FirstHashmap#hashmap.buckets_amount == SecondHashmap#hashmap.buckets_amount)
  and
  (FirstHashmap#hashmap.size == SecondHashmap#hashmap.size) of
    true -> compare_buckets(FirstHashmap, SecondHashmap, 1, FirstHashmap#hashmap.buckets_amount);
    false -> false
  end.


% start() ->
%   Users = hashmap_init(),
%   OneUser = hashmap_add(Users, "John", 1),
%   TwoUsers = hashmap_add(OneUser, "Sam", 2),
%   ThreeUsers = hashmap_add(TwoUsers, "Kate", 3),
%   Result = hashmap_foldl(ThreeUsers, fun(Element, Acc) -> Acc + Element#node.value end, 0),
%   io:format("~w~n", [Result]).
