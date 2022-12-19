# Hashmap Separate Chain Implementation

## Basic Functions

### Add element

```erlang
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
```

### Remove element

```erlang
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
```

### Get element

```erlang
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
```

### Filter

```erlang
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
```

### Map

```erlang
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
```

### Fold

```erlang
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
```

### Comparison

```erlang
hashmap_is_equal(FirstHashmap, SecondHashmap) ->
  case
  (FirstHashmap#hashmap.buckets_amount == SecondHashmap#hashmap.buckets_amount)
  and
  (FirstHashmap#hashmap.elements_size == SecondHashmap#hashmap.elements_size) of
    true -> compare_buckets(FirstHashmap, SecondHashmap);
    false -> false
  end.
```

### Merge

```erlang
hashmap_merge(FirstHashmap, SecondHashmap) ->
  case FirstHashmap#hashmap.buckets_amount > SecondHashmap#hashmap.buckets_amount of
    true -> NewHashmap = hashmap_init(FirstHashmap#hashmap.buckets_amount);
    false -> NewHashmap = hashmap_init(SecondHashmap#hashmap.buckets_amount)
  end,
  FirstMerge = rearrange_hashmap(FirstHashmap, NewHashmap),
  rearrange_hashmap(SecondHashmap, FirstMerge).
```

