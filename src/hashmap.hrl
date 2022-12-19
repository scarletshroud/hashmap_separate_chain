-record(node, {key, value, hashcode}).
-record(hashmap, {buckets, buckets_amount, buckets_size, elements_size}).

-define(LOAD_LIMIT, 0.7).
-define(GROW_COEFFICIENT, 2).
