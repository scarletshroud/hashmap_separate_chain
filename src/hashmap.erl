-module(hashmap).

-include("hashmap.hrl").

-export(
  [
    hashmap_init/0,
    hashmap_init_from_list/1,
    hashmap_init/1,
    hashmap_add/3,
    hashmap_get/2,
    hashmap_remove/2,
    hashmap_filter/2,
    hashmap_map/2,
    hashmap_foldl/3,
    hashmap_foldr/3,
    hashmap_is_equal/2,
    hashmap_merge/2
  ]
).

-import(erlang, [phash2/1]).

hashmap_fill(Buckets, Index, BucketsAmount) when Index >= BucketsAmount -> Buckets;

hashmap_fill(Buckets, Index, BucketsAmount) when Index < BucketsAmount ->
  hashmap_fill(array:set(Index, [], Buckets), Index + 1, BucketsAmount).

hashmap_fill(Hashmap, []) -> Hashmap;

hashmap_fill(Hashmap, [Element | Tail]) ->
  NewHashmap = hashmap_add(Hashmap, element(1, Element), element(2, Element)),
  hashmap_fill(NewHashmap, Tail).


hashmap_init() ->
  Buckets = array:new(10),
  FilledBuckets = hashmap_fill(Buckets, 0, 10),
  #hashmap{buckets = FilledBuckets, buckets_amount = 10, buckets_size = 0, elements_size = 0}.


hashmap_init(Size) ->
  Buckets = array:new(Size),
  FilledBuckets = hashmap_fill(Buckets, 0, Size),
  #hashmap{buckets = FilledBuckets, buckets_amount = Size, buckets_size = 0, elements_size = 0}.


hashmap_init_from_list(List) ->
  Empty = hashmap_init(),
  hashmap_fill(Empty, List).


get_bucket_index(Hashmap, Key) ->
  HashCode = phash2(Key),
  Index = HashCode rem Hashmap#hashmap.buckets_amount,
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


hashmap_remove(Hashmap, Key) ->
  Index = get_bucket_index(Hashmap, Key),
  HashCode = phash2(Key),
  Bucket = array:get(Index, Hashmap#hashmap.buckets),
  case Bucket of
    [] -> Hashmap;

    _ ->
      Node = search_chain_key(Bucket, Key, HashCode),
      case Node of
        false -> Hashmap;

        _ ->
          NewBucket =
            lists:sublist(Bucket, element(2, Node) - 1) ++ lists:nthtail(element(2, Node), Bucket),
          case length(NewBucket) of
            0 ->
              NewBuckets = array:set(Index, [], Hashmap#hashmap.buckets),
              BucketsSize = Hashmap#hashmap.buckets_size - 1;

            _ ->
              NewBuckets = array:set(Index, NewBucket, Hashmap#hashmap.buckets),
              BucketsSize = Hashmap#hashmap.buckets_size
          end,
          #hashmap{
            buckets = NewBuckets,
            buckets_amount = Hashmap#hashmap.buckets_amount,
            buckets_size = BucketsSize,
            elements_size = Hashmap#hashmap.elements_size - 1
          }
      end
  end.


hashmap_get(Hashmap, Key) ->
  Index = get_bucket_index(Hashmap, Key),
  HashCode = phash2(Key),
  Head = array:get(Index, Hashmap#hashmap.buckets),
  case Head of
    [] -> false;

    Chain ->
      case search_chain_key(Chain, Key, HashCode) of
        false -> false;
        Element -> element(1, Element)
      end
  end.


rearrange_hashmap(OldHashmap, NewHashmap) ->
  hashmap_foldl(
    OldHashmap,
    fun (Element, Hashmap) -> hashmap_add(Hashmap, Element#node.key, Element#node.value) end,
    NewHashmap
  ).

grow_hashmap(Hashmap) ->
  NewHashmap = hashmap_init(Hashmap#hashmap.buckets_amount * ?GROW_COEFFICIENT),
  rearrange_hashmap(Hashmap, NewHashmap).


hashmap_add(Hashmap, Key, Value) ->
  Index = get_bucket_index(Hashmap, Key),
  HashCode = phash2(Key),
  Bucket = array:get(Index, Hashmap#hashmap.buckets),
  case Bucket of
    [] ->
      NewNode = #node{key = Key, value = Value, hashcode = HashCode},
      NewBuckets = array:set(Index, [NewNode], Hashmap#hashmap.buckets),
      NewHashmap =
        #hashmap{
          buckets = NewBuckets,
          buckets_amount = Hashmap#hashmap.buckets_amount,
          buckets_size = Hashmap#hashmap.buckets_size + 1,
          elements_size = Hashmap#hashmap.elements_size + 1
        },
      case (Hashmap#hashmap.buckets_size + 1) / Hashmap#hashmap.buckets_amount >= ?LOAD_LIMIT of
        true -> grow_hashmap(NewHashmap);
        false -> NewHashmap
      end;

    Chain ->
      Node = search_chain_key(Chain, Key, HashCode),
      case Node of
        false ->
          NewNode = #node{key = Key, value = Value, hashcode = HashCode},
          #hashmap{
            buckets = array:set(Index, [NewNode | Chain], Hashmap#hashmap.buckets),
            buckets_amount = Hashmap#hashmap.buckets_amount,
            buckets_size = Hashmap#hashmap.buckets_size,
            elements_size = Hashmap#hashmap.elements_size + 1
          };

        _ ->
          NewNode = #node{key = Key, value = Value, hashcode = HashCode},
          NewBucket =
            lists:sublist(Bucket, element(2, Node) - 1)
            ++
            [NewNode]
            ++
            lists:nthtail(element(2, Node), Bucket),
          NewBuckets = array:set(Index, NewBucket, Hashmap#hashmap.buckets),
          #hashmap{
            buckets = NewBuckets,
            buckets_amount = Hashmap#hashmap.buckets_amount,
            buckets_size = Hashmap#hashmap.buckets_size,
            elements_size = Hashmap#hashmap.elements_size
          }
      end
  end.


hashmap_filter(Hashmap, Pred) ->
  NewBuckets =
    array:map(
      fun
        (_, Bucket) ->
          case Bucket of
            [] -> Bucket;

            _ ->
              FilteredList = lists:filter(Pred, Bucket),
              case length(FilteredList) of
                0 -> [];
                _ -> FilteredList
              end
          end
      end,
      Hashmap#hashmap.buckets
    ),
  Size =
    array:foldl(
      fun
        (_, Bucket, Acc) ->
          case Bucket of
            [] -> Acc;
            _ -> Acc + 1
          end
      end,
      0,
      NewBuckets
    ),
  #hashmap{
    buckets = NewBuckets,
    buckets_amount = Hashmap#hashmap.buckets_amount,
    buckets_size = Size,
    elements_size = Hashmap#hashmap.elements_size
  }.


hashmap_map(Hashmap, Fun) ->
  NewBuckets =
    array:map(
      fun
        (_, Bucket) ->
          case Bucket of
            [] -> Bucket;
            _ -> lists:map(Fun, Bucket)
          end
      end,
      Hashmap#hashmap.buckets
    ),
  #hashmap{
    buckets = NewBuckets,
    buckets_amount = Hashmap#hashmap.buckets_amount,
    buckets_size = Hashmap#hashmap.buckets_size,
    elements_size = Hashmap#hashmap.elements_size
  }.


hashmap_foldl(Hashmap, Fun, InitAcc) ->
  array:foldl(
    fun
      (_, Bucket, Acc) ->
        case Bucket of
          [] -> Acc;
          _ -> lists:foldl(Fun, Acc, Bucket)
        end
    end,
    InitAcc,
    Hashmap#hashmap.buckets
  ).


hashmap_foldr(Hashmap, Fun, InitAcc) ->
  array:foldr(
    fun
      (_, Bucket, Acc) ->
        case Bucket of
          [] -> Acc;
          _ -> lists:foldr(Fun, Acc, Bucket)
        end
    end,
    InitAcc,
    Hashmap#hashmap.buckets
  ).


compare_buckets(FirstHashmap, SecondHashmap) ->
  hashmap_foldl(
    FirstHashmap,
    fun
      (Element, Acc) ->
        case (hashmap_get(SecondHashmap, Element#node.key)) of
          false -> false;

          SecondElement ->
            case Element#node.value == SecondElement#node.value of
              true -> Acc;
              false -> false
            end
        end
    end,
    true
  ).


hashmap_is_equal(FirstHashmap, SecondHashmap) ->
  case
  (FirstHashmap#hashmap.buckets_amount == SecondHashmap#hashmap.buckets_amount)
  and
  (FirstHashmap#hashmap.elements_size == SecondHashmap#hashmap.elements_size) of
    true -> compare_buckets(FirstHashmap, SecondHashmap);
    false -> false
  end.


hashmap_merge(FirstHashmap, SecondHashmap) ->
  case FirstHashmap#hashmap.buckets_amount > SecondHashmap#hashmap.buckets_amount of
    true -> NewHashmap = hashmap_init(FirstHashmap#hashmap.buckets_amount);
    false -> NewHashmap = hashmap_init(SecondHashmap#hashmap.buckets_amount)
  end,
  FirstMerge = rearrange_hashmap(FirstHashmap, NewHashmap),
  rearrange_hashmap(SecondHashmap, FirstMerge).
