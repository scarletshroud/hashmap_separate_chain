-record(node, {key, value, hashcode}).
-record(hashmap, {buckets, buckets_amount, size}).

-define(LOAD_LIMIT, 0.8).
-define(GROW_COEFFICIENT, 1.5).
